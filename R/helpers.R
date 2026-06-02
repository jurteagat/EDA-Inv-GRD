# R/helpers.R — Funciones de formato, agregación y tema gráfico
# Cargado por global.R y tests; sin dependencias de estado global.

# --- Etiquetas de variables ---------------------------------------------------
nombres_comunes <- tibble::tribble(
  ~variable,                  ~nombre_comun,
  "codigo_unico",             "Cód. Único",
  "codigo_pro",               "Cód. Programa",
  "link_ssi",                 "Enlace SSI",
  "des_servicio",             "Servicio",
  "des_tipologia_esri",       "Tipología (ESRI)",
  "geometry",                 "Geometría",
  "monto_viable",             "Monto viable",
  "costo_actualizado",        "Costo actual.",
  "des_tipologia",            "Tipología",
  "nivel",                    "Nivel",
  "entidad",                  "Entidad",
  "nombre_inversion",         "Nombre inversión",
  "nombre_abreviado",         "Nombre abreviado",
  "estado",                   "Estado",
  "situacion",                "Situación",
  "alternativa",              "Alternativa",
  "fecha_viabilidad",         "Fecha viab.",
  "programa",                 "Programa",
  "subprograma",              "Subprograma",
  "marco",                    "Marco",
  "tipo_inversion",           "Tipo inversión",
  "des_modalidad",            "Modalidad",
  "registrado_pmi",           "En PMI",
  "expediente_tecnico",       "Exp. técnico",
  "informe_cierre",           "Inf. cierre",
  "deven_acumul_anio_ant",    "Deveng. acum. ant.",
  "pia_anio_actual",          "PIA actual",
  "pim_anio_actual",          "PIM actual",
  "saldo_ejecutar",           "Saldo ejec.",
  "tiene_f12b",               "Tiene F-12B",
  "avance_fisico",            "Av. físico",
  "avance_ejecucion",         "Av. ejecución",
  "ind_ioarr_emerg",          "IOARR/Emerg.",
  "ubigeo",                   "Ubigeo",
  "fec_ini_ejecucion",        "Ini. ejecución",
  "fec_ini_ejec_fisica",      "Ini. ejec. física",
  "num_habitantes_benef",     "Hab. benef.",
  "monto_et_f8",              "Monto ET/F8",
  "n_proyectos",              "N° inversiones",
  "costo_actualizado_prom",   "Costo actual. (prom.)",
  "monto_viable_prom",        "Monto viable (prom.)",
  "pct_costo_vs_viable",      "% Costo vs. viable",
  "pct_pim_vs_pia",           "% PIM vs. PIA",
  "departamento",             "Departamento",
  "pct_ejecucion",            "% Ejecución (Deveng./PIM)",
  "pct_pim_pia",              "% PIM vs. PIA (portafolio)",
  "devengado_acum",           "Deveng. acum. 2012-2025",
  "costo_total",              "Costo total (S/)",
  "n_inversiones",            "N° inversiones",
  "pct_ejecucion_prom",       "% Ejec. promedio"
)

label_var <- function(vars) {
  lookup <- stats::setNames(nombres_comunes$nombre_comun, nombres_comunes$variable)
  unname(dplyr::coalesce(lookup[vars], vars))
}

# --- Formateo -----------------------------------------------------------------
fmt_soles <- function(x) {
  if (is.na(x) || !is.finite(x)) return("—")
  scales::label_comma(prefix = "S/ ")(x)
}

# --- Escalado de radios para el mapa ------------------------------------------
radios_log <- function(valores, min_r = 3, max_r = 9) {
  lv  <- log10(pmax(valores, 1, na.rm = TRUE))
  rng <- range(lv, na.rm = TRUE)
  if (diff(rng) == 0) return(rep((min_r + max_r) / 2, length(lv)))
  min_r + (lv - rng[1]) / (rng[2] - rng[1] + 1e-9) * (max_r - min_r)
}

# --- Tema gráfico GRD (estilo NYT/BBC) ----------------------------------------
paleta_grd <- c(
  verde    = "#1A9850",
  azul     = "#4393C3",
  rojo     = "#D6604D",
  azul_osc = "#2166AC",
  naranja  = "#F46D43",
  morado   = "#762A83"
)

theme_grd <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.title         = ggplot2::element_text(face = "bold", hjust = 0,
                                                  size = base_size * 1.15),
      plot.subtitle      = ggplot2::element_text(color = "#555555", hjust = 0,
                                                  size = base_size * 0.9),
      axis.title         = ggplot2::element_text(color = "#444444",
                                                  size = base_size * 0.85),
      axis.text          = ggplot2::element_text(color = "#666666"),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "#EBEBEB", linewidth = 0.4),
      legend.position    = "bottom",
      legend.key.size    = ggplot2::unit(0.8, "lines"),
      legend.text        = ggplot2::element_text(size = base_size * 0.8),
      strip.text         = ggplot2::element_text(face = "bold", color = "#333333")
    )
}

# --- Agregaciones -------------------------------------------------------------

#' Promedios de costo y monto viable por tipología
tabla_promedios_tipologia <- function(df) {
  df |>
    dplyr::filter(!is.na(des_tipologia)) |>
    dplyr::group_by(des_tipologia) |>
    dplyr::summarise(
      n_proyectos            = dplyr::n(),
      costo_actualizado_prom = mean(costo_actualizado, na.rm = TRUE),
      monto_viable_prom      = mean(monto_viable,      na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(costo_actualizado_prom))
}

#' Cortes por departamento: n, costo total y % ejecución
tabla_cortes_departamento <- function(df_plain, depto_lkp) {
  df_plain |>
    dplyr::mutate(
      cod_depto   = stringr::str_sub(ubigeo, 1, 2),
      cod_depto   = dplyr::if_else(!is.na(cod_depto) & nchar(cod_depto) == 2,
                                   cod_depto, "98")
    ) |>
    dplyr::left_join(depto_lkp, by = "cod_depto") |>
    dplyr::mutate(
      departamento = dplyr::coalesce(departamento, glue::glue("Cod {cod_depto}"))
    ) |>
    dplyr::group_by(cod_depto, departamento) |>
    dplyr::summarise(
      n_inversiones      = dplyr::n(),
      costo_total        = sum(costo_actualizado,  na.rm = TRUE),
      pct_ejecucion_prom = mean(avance_ejecucion,  na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::arrange(dplyr::desc(costo_total))
}

#' Serie portafolio anual agregada
serie_portafolio <- function(df_serie) {
  data.table::as.data.table(df_serie)[
    , .(pia       = sum(pia,       na.rm = TRUE),
        pim       = sum(pim,       na.rm = TRUE),
        devengado = sum(devengado, na.rm = TRUE)),
    by = anio
  ][order(anio)]
}

#' Ratios anuales de ejecución y ampliación PIM/PIA
ratios_portafolio <- function(sp) {
  sp |>
    dplyr::mutate(
      pct_ejecucion = dplyr::if_else(pim > 0, devengado / pim * 100, NA_real_),
      pct_pim_pia   = dplyr::if_else(pia > 0, pim       / pia * 100, NA_real_)
    ) |>
    dplyr::select(anio, pct_ejecucion, pct_pim_pia)
}

#' Top n filas ordenadas por variable descendente
top_por <- function(df, var, n = 10) {
  df |>
    dplyr::arrange(dplyr::desc(.data[[var]])) |>
    dplyr::slice_head(n = n)
}
