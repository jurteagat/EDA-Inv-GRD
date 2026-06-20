# EDA de Inversiones GRD — Perú

Cuaderno Quarto de R y dashboard Shiny para el **análisis exploratorio de datos (EDA)** de las inversiones del programa - división funcional de *Gestión de Riesgos y Emergencias* (GRD) en el marco del Invierte.pe del Ministerio de Economía y Finanzas (MEF) del Perú.

El proyecto combina tres fuentes grandes del MEF: el detalle de inversiones (CSV ~388 MB), los puntos georreferenciados (GeoPackage ~265 MB) y la serie de ejecución presupuestal 2012-2025 (CSV ~266 MB), extendida a 2026 con el devengado del año en curso proveniente del detalle de inversiones.

La selección de inversiones GRD no se limita al programa *Gestión de Riesgos y Emergencias*: también incluye las tipologías de **drenaje pluvial** (servicio/sistema) y las **IOARR de emergencia** (`ind_ioarr_emerg = "SI"`), y excluye el subprograma de defensa contra incendios y emergencias menores.

---

## Estructura del proyecto

```
EDA-Inv-GRD/
├── EDA_Inv_GRD_v1.qmd      # Cuaderno EDA principal (consume midputs/rds/)
├── 00_datos_entrada.qmd    # Preparación de datos (crudos → RDS → Drive)
├── reporte_inversion.qmd   # Plantilla Typst para el reporte PDF por inversión
├── app.R                   # Dashboard Shiny (UI bslib + server reactivo)
├── global.R                # Precarga, pipeline y caché de arranque de la app
├── R/                      # Funciones puras testeables
│   ├── helpers.R           #   label_var, fmt_soles, radios_log, agregaciones
│   ├── theme_jut.R         #   sistema de estilo visual "jut" (paletas, theme_jut)
│   ├── datos.R             #   descargas Drive, pipeline, caché
│   └── exportar.R          #   exportaciones CSV/GPKG/PDF
├── www/                    # Assets del estilo: estilos-jut.css, fonts/, iconos (logo + favicon)
├── tests/testthat/         # Tests unitarios + shinytest2
├── midputs/rds/            # Datos intermedios .rds (descargados de Drive)
├── raw/                    # Crudos del MEF (no editar, no rastreados)
├── manifest.json           # Despliegue en Posit Connect
├── renv/ · renv.lock       # Entorno reproducible (R 4.5.3)
└── CLAUDE.md               # Guía detallada del proyecto (local, no rastreada)
```

> Para el detalle completo de arquitectura, convenciones y pipeline, ver **`CLAUDE.md`** (documento local de desarrollo, no incluido en el repositorio).

---

## Requisitos

- **R ≥ 4.5.3**
- **Quarto ≥ 1.5** (para renderizar los `.qmd`)
- **renv** (instala los paquetes en las versiones exactas del lock)

---

## Comandos clave

```bash
# Restaurar las versiones exactas de los paquetes (primera vez o tras cambios en renv.lock)
Rscript -e 'renv::restore()'

# Renderizar el cuaderno EDA a HTML autocontenido
quarto render EDA_Inv_GRD_v1.qmd

# Lanzar el dashboard Shiny
Rscript -e 'shiny::runApp()'

# Tests unitarios (sin Chrome)
Rscript -e 'testthat::test_dir("tests/testthat", filter = "helpers|datos|exportar")'

# Todos los tests (shinytest2 requiere Chrome/chromote)
Rscript -e 'testthat::test_dir("tests/testthat")'
```

---

## Datos

Tanto el cuaderno EDA como el dashboard leen los `.rds` ya preparados desde `midputs/rds/`. Si faltan, `descargar_si_falta()` los baja automáticamente desde Google Drive (archivos públicos, sin OAuth); los IDs viven en `DRIVE_IDS` (`R/datos.R`).

La preparación de datos es opcional y vive en `00_datos_entrada.qmd`, que trabaja en dos modos según el param `leer_desde_local`:

- `TRUE` → **modo actualización**: descarga los crudos del MEF a `raw/`, los convierte a `.rds` y (opcionalmente) los sube a Drive.
- `FALSE` → **modo consumo** (default): baja los `.rds` ya preparados desde Drive. No requiere OAuth.

---

## Dashboard Shiny

Versión interactiva del cuaderno con filtros de tipología, departamento, situación, **IOARR/emergencia** (por defecto en "NO") y tipo de inversión. Comparte los mismos `.rds` de `midputs/rds/`.

**Pestañas:** Resumen · Mapa · Distribuciones · Departamentos · Inversión Seleccionada · Ficha Técnica.

- La lógica está extraída en funciones puras en `R/` (testeadas con `testthat` + `shinytest2`).
- En el primer arranque, `global.R` ejecuta el pipeline completo y guarda una **caché** (`midputs/rds/_cache_app.rds`, no rastreada). Para forzar su reconstrucción: `GRD_REBUILD_CACHE=1 Rscript -e 'source("global.R")'`.
- Exporta los datos filtrados a **CSV** (geo y temporal), **GeoPackage** y un **reporte PDF** por inversión (Typst vía `reporte_inversion.qmd`).
- **Estilo visual "jut"** (Bootswatch Lux + Nunito Sans + tema ggplot BBC-minimal): definido en `R/theme_jut.R` y `www/`. Navbar oscuro y compacto con logo, value boxes en progresión azul, paletas de marca y favicon.
- Desplegable en **Posit Connect** (`manifest.json`), donde la caché se reconstruye descargando desde Drive.

---

## Convenciones

- Idioma: toda la prosa, comentarios y nombres de variables en **español**.
- Sin notación científica: `options(scipen = 999)`.
- Namespacing explícito (`paquete::funcion()`) ante riesgo de ambigüedad.
- Interpolación de strings con `glue::glue()`, nunca `paste0`.
- Nombres de archivos en `snake_case`.
- Identificadores clave (`CODIGO_UNICO`, `COD_UNICO`, `PRODUCTO_PROYECTO`) siempre como `character` para evitar coerciones silenciosas en los joins.
- CSVs grandes con `data.table::fread(..., select = c(...))`; liberar objetos grandes con `rm(...); gc()` tras filtrar.
