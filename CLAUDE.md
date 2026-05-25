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

The chunk `opciones-usuario` at the top of the `.qmd` controls runtime behavior:
- `opcion_datos`: `1L` = download fresh data from internet, `2L` = use most recent files in `raw/`
- `codigo_inv_foco`: `CODIGO_UNICO` of the specific investment to inspect in detail

## Data pipeline (object naming)

```
df_det_inv          → (filter GRD) → df_det_inv_grd_dp
df_pu_geoinv_inv_g  → (deduplicate) → df_pu_geoinv_inv_g_dp
                                         ↓ left_join on COD_UNICO = CODIGO_UNICO
                                      df_pu_geoinv_inv_g_dpf  (geospatial, merged)
df_grd_2012_25      → (select cols) → df_grd_2012_25_dp
                                         ↓ left_join on PRODUCTO_PROYECTO = COD_UNICO
                                      df_grd_2012_25_dpf  (temporal, no geometry)
```

Suffix conventions: `_dp` = depurado (cleaned), `_dpf` = depurado + fusionado (cleaned + merged).

## Important notes

- The GRD filter uses normalized text (`stringi::stri_trans_general(..., "Latin-ASCII")`) because the MEF CSV encoding for tildes can vary.
- `pct_pim_vs_pia` measures PIM growth over PIA (PIM is the modified budget, PIA is initial). Negative values mean budget was cut.
- The geospatial join is a left join from points (preserves all georeferenced investments); unmatched rows show as NA in tabular columns.
- The temporal join is a left join from the time series (preserves all year-rows); geometry is dropped via `st_drop_geometry()` before joining.
