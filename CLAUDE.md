# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Project overview

Quarto notebook (R) for cleaning and exploratory data analysis of Peru's disaster risk management (GRD) public investments. Combines three large datasets: MEF investment detail (~388 MB CSV), georeferenced investment points (~265 MB GeoPackage), and a 2012-2025 budget execution time series (~266 MB CSV). The notebook is educational — every step is commented and explained.

## Key commands

```bash
# Restore exact package versions (first time or after renv.lock changes)
Rscript -e 'renv::restore()'

# Render the notebook to self-contained HTML
quarto render EDA_Inv_GRD_v1.qmd

# Or from R console
quarto::quarto_render("EDA_Inv_GRD_v1.qmd")
```

## Requirements

- R >= 4.5.3
- Quarto >= 1.5
- renv (lockfile pins all package versions)

## Data layout

- `raw/` — raw data files downloaded from MEF portal, suffixed `_YYYYMMDD` for versioning. Never manually edit these.
- `midputs/` — intermediate datasets produced by this project (e.g. `grd_2012_2025_t.csv`). Read-only during EDA.
- `folder_xyz_ignore/` — sensitive files; never read or modify.

## Coding conventions

- Language: all prose, comments, and variable names in Spanish.
- `options(scipen = 999)` — no scientific notation anywhere.
- Explicit namespacing with `paquete::funcion()` when there's ambiguity risk.
- String interpolation via `glue::glue()`, never `paste0`.
- Filenames in `snake_case`.
- Key identifiers (`CODIGO_UNICO`, `COD_UNICO`, `PRODUCTO_PROYECTO`) must always be `character` — cast immediately after reading to prevent silent coercion in joins.
- Prefer `data.table::fread(..., select = c(...))` for large CSVs to avoid memory spikes.
- Free large objects with `rm(...); gc()` immediately after filtering/subsetting.
- Use `tidyverse` and `data.table` as primary data manipulation stacks.
- Package installation pattern: `if (!require("pkg")) install.packages("pkg")` (GitHub packages via `remotes::install_github()`).

## Notebook user options

Data preparation lives in `00_datos_entrada.qmd` (converts raw CSV/GeoPackage to `.rds` in `midputs/rds/`, optionally pushes to Google Drive). The EDA notebook reads only from `midputs/rds/`.

YAML params on `EDA_Inv_GRD_v1.qmd`:
- `preparar_datos: false` — when `true`, the EDA notebook triggers `00_datos_entrada.qmd` first.
- `datos_leer_desde_local: false` — forwarded to `00_datos_entrada.qmd`.

The chunk `opciones-usuario` only exposes:
- `codigo_inv_foco`: `CODIGO_UNICO` of the specific investment to describe in detail.

## Data pipeline (object naming)

All three datasets are first restricted to `codigos_grd_comunes` — the intersection of `codigo_unico` across the three sources. After that the joins are `inner_join`s and never produce NA-padding, so a row count mismatch becomes a load-bearing error rather than a silent issue.

```
df_det_inv          → filter GRD + intersection → df_det_inv_grd_dp
df_pu_geoinv_inv_g  → deduplicate + intersection → df_pu_geoinv_inv_g_dp
                                         ↓ inner_join on codigo_unico
                                      df_pu_geoinv_inv_g_dpf  (geospatial, merged)
df_grd_2012_25      → select cols + intersection → df_grd_2012_25_dp
                                         ↓ inner_join on codigo_unico (geometry dropped)
                                      df_grd_2012_25_dpf  (temporal, no geometry)
```

Suffix conventions: `_dp` = depurado (cleaned), `_dpf` = depurado + fusionado (cleaned + merged). Joins use `stopifnot()` on the expected row counts.

## Important notes

- The GRD filter uses normalized text (`stringi::stri_trans_general(..., "Latin-ASCII")`) because the MEF CSV encoding for tildes can vary.
- `pct_pim_vs_pia` measures PIM growth over PIA (PIM is the modified budget, PIA is initial). Negative values mean budget was cut.
- The `ubigeo` field follows INEI: first 2 digits = department (e.g. `15` = Lima, `06` = Cajamarca). Extract with `stringr::str_sub(ubigeo, 1, 2)` and join to an inline `tribble()` lookup; there's no need for an external file.

## Visualization conventions

Lessons from the "Análisis avanzado" section — apply them when adding new charts.

- **Fullscreen toggle for interactive widgets.** Wrap each `plotly`/`leaflet` widget in `bslib::card(full_screen = TRUE, card_header(...), card_body(widget, padding = 0))`. The card adds a small expand button at the bottom-right. Use `padding = 0` only for leaflet maps so the tiles fill the card; default padding is fine for plotly.

- **Color palettes ordered by frequency, not alphabetical.** For categorical maps with many levels, the most frequent categories deserve the most distinguishable colors. Compute `tipologias_por_freq <- count(..., sort = TRUE) |> pull(...)`, then build a palette = (12 hand-picked high-contrast hex codes) + `grDevices::gray.colors(n_tail, start = 0.5, end = 0.85)` for the long tail. Pass via `leaflet::colorFactor(palette = paleta_ordenada, levels = tipologias_por_freq)` — `levels` (not `domain`) controls assignment order.

- **Overlapping points → spiderfy, not just clustering.** When multiple investments share the same coordinates, their colors blend and clicks become ambiguous. Add `clusterOptions = leaflet::markerClusterOptions(maxClusterRadius = 25, spiderfyOnMaxZoom = TRUE, spiderfyDistanceMultiplier = 1.8, showCoverageOnHover = FALSE, zoomToBoundsOnClick = TRUE)` to `addCircleMarkers()`. Low `maxClusterRadius` keeps the national-level view nearly unchanged; spiderfy kicks in only when overlapping.

- **Dense circle markers.** For nationwide point maps: `fillOpacity = 0.45`, `stroke = TRUE, weight = 0.4, opacity = 0.6`, and radius scaled by `log10()` of a monetary variable normalized to a tight `3–9 px` range. Higher opacity / larger radii saturate the map at country zoom.

- **Custom CSS for leaflet widgets needs high specificity.** Leaflet's `.info` class has specific font-size rules that override naive selectors. To shrink a legend, use:
  ```r
  css <- htmltools::tags$style(htmltools::HTML("
    .leaflet-container .mi-clase, .leaflet-container .mi-clase * {
      font-size: 9px !important;
      line-height: 1.15 !important;
    }
    .leaflet-container .mi-clase { max-width: 200px !important; white-space: normal !important; }
  "))
  ```
  Pass `className = "info legend mi-clase"` to `addLegend()` and emit `htmltools::tagList(css, card)` — the inline CSS travels in `embed-resources: true` HTML.

- **Log scale is mandatory for monetary distributions.** Inversion amounts span 6+ orders of magnitude. Always use `scale_x_log10(labels = scales::label_comma())` for histograms of `monto_viable`, `costo_actualizado`, `pia_*`, `pim_*`. Filter `valor > 0` first because `log10(0) = -Inf`.

- **Outlier rule per category, not global.** Tipologías have very different cost scales (e.g. defensas ribereñas vs. compañías de bomberos). Use Tukey's `Q3 + 3·IQR` **within each tipología** for outlier detection.
