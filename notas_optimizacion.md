# Optimización Shiny GRD — notas de la rama `worktree-shiny-opt`

## Baseline (Fase 1) — medido local, offline (fuentes en symlink, sin Drive)

Equipo: macOS arm64, R 4.5.3. En Posit Connect el **cold-start suma la descarga de
~97 MB de fuentes desde Google Drive** (no medible aquí), que es el mayor costo real.

### Arranque
| Métrica | Actual |
| --- | --- |
| Warm start (cache hit) | **2.10 s** (libs ≈1.9 s + leer caché 0.21 s) |
| Cold rebuild (libs+pipeline+guardar caché, sin Drive) | **11.95 s** |

### Lectura de fuentes (RDS gzip vs Parquet)
| Fuente | filas×cols | RDS gzip | Parquet zstd-9 | Lectura RDS | Lectura Parquet |
| --- | --- | --- | --- | --- | --- |
| det_inv | 182840×68 | 56.4 MB | **44.8 MB** | 1.48 s | **0.07 s** (col_select 32) |
| grd_2012_25 | 538787×39 | 6.2 MB | **4.6 MB** | 0.83 s | **0.02 s** |
| geoinv (sf POINT) | 404085×39 | 34.4 MB | **29.9 MB** | 1.91 s | **0.77 s** |
| deptos_geo (sf POLY) | 25×2 | 0.3 MB | 0.4 MB | 0.01 s | 0.00 s |
| **Total fuentes** | | **~97 MB** | **~79 MB** | **~4.2 s** | **~0.9 s** |

Parquet zstd-9: más chico que RDS gzip y ~4–20× más rápido de leer. Velocidad de
lectura snappy≈zstd, así que zstd-9 gana (mismo read, menor tamaño). col_select en
det_inv lee solo 32 de 68 columnas (menos RAM).

### Serialización de la caché de arranque (`_cache_app.rds`)
| Formato | Tamaño | Escribir | Leer |
| --- | --- | --- | --- |
| `saveRDS(compress=FALSE)` (actual) | **85.5 MB** | 0.20 s | 0.21 s |
| `saveRDS(compress="gzip")` | 7.2 MB | 0.77 s | 0.25 s |
| **`qs2::qs_save` (zstd)** | **6.6 MB** | 0.15 s | 0.12 s |

→ La caché actual ocupa 85.5 MB sin ventaja de velocidad. **qs2 la reduce 13× y lee
más rápido.** Cambio de bajo riesgo.

## Decisiones de implementación derivadas del baseline
1. Parquet de fuentes con **compresión zstd nivel 9** (`arrow::write_parquet`,
   `sfarrow::st_write_parquet`).
2. det_inv se lee con **col_select** (32 columnas usadas) en `global.R`.
3. Caché de arranque pasa de `saveRDS(compress=FALSE)` a **qs2** (mantiene objetos R:
   sf, closures de leaflet). Fallback de lectura a RDS si el `.qs2` no existe.
4. GeoParquet vía sfarrow escribe metadata geo-arrow 0.1.0 (legible por geopandas);
   roundtrip sf verificado (geom = `geom`, filas iguales). Caveat: spec antigua, OK
   para interop Python actual.

## Paquetes añadidos a renv
arrow 24.0.0 · sfarrow 0.4.1 · qs2 0.2.2 · reactlog 1.1.1 · bench 1.1.4
