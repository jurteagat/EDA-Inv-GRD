# EDA de Inversiones GRD — Perú

Cuaderno Quarto de R para la **depuración y análisis exploratorio de datos (EDA)** de las inversiones del programa *Gestión de Riesgos y Emergencias* (GRD) del Ministerio de Economía y Finanzas (MEF) del Perú. Tiene propósito educativo: cada paso está comentado y explicado.

---

## Estructura del proyecto

```
ai_dev_ubu_r3/
├── EDA_Inv_GRD_v1.qmd      # Cuaderno principal (fuente)
├── EDA_Inv_GRD_v1.html     # Último render del cuaderno
├── raw/                    # Datos brutos (no editar)
│   ├── DETALLE_INVERSIONES_YYYYMMDD.csv
│   ├── Detalle_Inversiones_Diccionario_YYYYMMDD.csv
│   └── df_pu_geoinvierte_inv_g.gpkg
├── midputs/                # Datos intermedios internos
│   └── grd_2012_2025_t.csv
├── articulo_grd_b/         # Visualizaciones complementarias
├── renv/                   # Entorno reproducible de R
├── renv.lock               # Versiones exactas de paquetes (R 4.5.3)
├── ai_dev_ubu_r3.Rproj     # Proyecto RStudio
├── AGENTS.md               # Reglas de programación del proyecto
└── CLAUDE.md               # Instrucciones para el asistente IA
```

---

## Fuentes de datos

| Objeto R | Archivo | Tamaño aprox. | Origen |
|---|---|---|---|
| `df_det_inv` | `raw/DETALLE_INVERSIONES_YYYYMMDD.csv` | ~388 MB | Portal datos abiertos MEF |
| `df_pu_geoinv_inv_g` | `raw/df_pu_geoinvierte_inv_g.gpkg` | ~265 MB | Capa ESRI del MEF (puntos georreferenciados) |
| `df_grd_2012_25` | `midputs/grd_2012_2025_t.csv` | ~266 MB | Archivo interno (serie temporal 2012-2025) |

Los datos de internet (`df_det_inv` y `df_pu_geoinv_inv_g`) se guardan en `raw/` con sufijo de fecha (`_YYYYMMDD`) para preservar versiones históricas. El archivo `grd_2012_2025_t.csv` nunca se re-descarga; siempre se lee desde `midputs/`.

---

## Requisitos

- **R ≥ 4.5.3**
- **Quarto ≥ 1.5** (para renderizar el `.qmd`)
- **renv** (para instalar paquetes en las versiones exactas del lock)

Paquetes principales gestionados por `renv`: `tidyverse`, `data.table`, `sf`, `janitor`, `plotly`, `leaflet`, `leaflet.extras`, `crosstalk`, `leafpop`, `rmapshaper`, `DT`, `skimr`, `glue`, `scales`, `esri2sf`, `sessioninfo`.

---

## Cómo ejecutar

### 1. Clonar / abrir el proyecto

Abrir `ai_dev_ubu_r3.Rproj` en RStudio o VS Code con la extensión de R.

### 2. Restaurar el entorno de paquetes

```r
renv::restore()
```

Esto instala exactamente las versiones del `renv.lock`. Solo es necesario la primera vez o cuando el lock cambie.

### 3. Editar las opciones del usuario (opcional)

Al inicio del cuaderno (`chunk opciones-usuario`) hay dos variables:

```r
# ============ OPCIONES DEL USUARIO ============
opcion_datos    <- 2L        # 1 = descargar de internet, 2 = usar raw/ local
codigo_inv_foco <- "2508148" # CODIGO_UNICO para la sección de inversión específica
# ==============================================
```

- `opcion_datos = 1L` descarga el CSV principal, el diccionario y la capa ESRI desde internet y los guarda en `raw/` con fecha del día. Si la descarga falla, cae automáticamente al archivo local más reciente.
- `opcion_datos = 2L` (por defecto) lee directamente el archivo más reciente de `raw/` sin conexión.

### 4. Renderizar

```r
quarto::quarto_render("EDA_Inv_GRD_v1.qmd")
```

O desde la terminal:

```bash
quarto render EDA_Inv_GRD_v1.qmd
```

El HTML resultante (`EDA_Inv_GRD_v1.html`) es autocontenido (`embed-resources: true`) y se puede compartir sin dependencias externas.

---

## Contenido del cuaderno

| Sección | Descripción |
|---|---|
| **1. Configuración** | Variables del usuario y carga de paquetes |
| **2. Adquisición de datos** | Descarga o lectura local de las tres fuentes |
| **3. Descripción inicial** | `glimpse()` y `tabyl()` de las bases brutas |
| **4. Depuración** | Filtro a GRD, selección de columnas, deduplicación geoespacial, cast de tipos |
| **5. Fusión geoespacial** | Left join desde puntos ESRI → añade atributos tabulares (`df_pu_geoinv_inv_g_dpf`) |
| **6. Fusión temporal** | Left join desde serie 2012-2025 → añade contexto presupuestario (`df_grd_2012_25_dpf`) |
| **7. EDA** | `skimr::skim()`, inversión específica con gráfica acumulada interactiva y mini-mapa, diccionario actualizado, promedios por tipología, tabla de crecimientos porcentuales |
| **8. Cierre** | `sessioninfo::session_info()` |

### Bases de datos producidas

| Objeto | Descripción |
|---|---|
| `df_det_inv_grd_dp` | Detalle MEF filtrado a GRD (32 columnas seleccionadas) |
| `df_pu_geoinv_inv_g_dp` | Puntos ESRI depurados, un punto por `COD_UNICO` |
| `df_pu_geoinv_inv_g_dpf` | Fusión geoespacial (puntos + atributos tabulares) |
| `df_grd_2012_25_dp` | Serie temporal depurada (6 columnas) |
| `df_grd_2012_25_dpf` | Fusión temporal (serie + contexto geoespacial, sin geometría) |

---

## Consideraciones de memoria

El CSV principal (~388 MB) se lee con `data.table::fread(..., select = c(...))` para cargar solo las 32 columnas necesarias y evitar un pico de ~2-3 GB en RAM. Inmediatamente después del filtro a GRD se ejecuta `rm(df_det_inv); gc()` para liberar el objeto completo.

---

## Convenciones del proyecto

- Sin notación científica: `options(scipen = 999)` en el chunk de setup.
- Referencias a funciones siempre con `paquete::funcion()` cuando hay riesgo de ambigüedad.
- Interpolación de strings con `glue::glue()`, nunca con `paste0`.
- Nombres de archivos en `snake_case`.
- Los archivos de `folder_xyz_ignore/` son sensibles y no deben leerse ni modificarse.

---

## Notas

- **Tildes en `PROGRAMA`**: el filtro usa el literal `"GESTIÓN DE RIESGOS Y EMERGENCIAS"`. Si el MEF cambia la codificación del CSV, puede ser necesario aplicar `stringi::stri_trans_general` para normalizar antes de comparar.
- **`pct_pim_vs_pia`**: mide el crecimiento del PIM respecto al PIA (`(PIM - PIA) / PIA × 100`). Un valor negativo indica que el presupuesto fue reducido durante la ejecución.
- **Gráfica acumulada**: la serie de PIA/PIM/DEVENGADO por proyecto muestra valores acumulados (no anuales) para facilitar la lectura del crecimiento histórico total.
