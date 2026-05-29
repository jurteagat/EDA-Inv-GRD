# global.R — Precarga de datos y precómputo de objetos compartidos
# para la Shiny app de Inversiones GRD.
#
# Replica el pipeline didáctico de EDA_Inv_GRD_v1.qmd (líneas 228-501)
# UNA sola vez al arrancar la app, no por sesión. Los objetos quedan
# disponibles globalmente; el server solo los filtra reactivamente.

options(scipen = 999)

# --- Paquetes --------------------------------------------------------------
suppressPackageStartupMessages({
  library(here)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(stringi)
  library(forcats)
  library(glue)
  library(scales)
  library(tibble)
  library(janitor)
  library(data.table)
  library(sf)
  library(ggplot2)
  library(plotly)
  library(leaflet)
  library(leaflet.extras)
  library(htmltools)
  library(DT)
  library(bslib)
  library(shiny)
})

# --- Etiquetas de variables (de EDA_Inv_GRD_v1.qmd L162-225) --------------
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
  "n_proyectos",              "N° proyectos",
  "costo_actualizado_prom",   "Costo actual. (prom.)",
  "monto_viable_prom",        "Monto viable (prom.)",
  "pct_costo_vs_viable",      "% Costo vs. viable",
  "pct_pim_vs_pia",           "% PIM vs. PIA",
  "departamento",             "Departamento",
  "pct_ejecucion",            "% Ejecución (Deveng./PIM)",
  "pct_pim_pia",              "% PIM vs. PIA (portafolio)",
  "devengado_acum",           "Deveng. acum. 2012-2025"
)

definiciones_locales <- tibble::tribble(
  ~variable,              ~definicion_local,
  "codigo_pro",           "Código interno del programa de inversiones (capa ESRI).",
  "link_ssi",             "URL a la inversión en el SSI del MEF (capa ESRI).",
  "des_servicio",         "Descripción del servicio público asociado (capa ESRI).",
  "des_tipologia_esri",   "Tipología según la capa ESRI; puede diferir de des_tipologia del diccionario MEF.",
  "geometry",             "Geometría (POINT) en WGS84 con la ubicación del proyecto."
)

label_var <- function(vars) {
  lookup <- setNames(nombres_comunes$nombre_comun, nombres_comunes$variable)
  dplyr::coalesce(lookup[vars], vars)
}

# --- Lectura de archivos RDS desde midputs/rds/ ---------------------------
ruta_rds <- function(f) here::here("midputs", "rds", f)

cols_det_inv <- c(
  "CODIGO_UNICO", "MONTO_VIABLE", "COSTO_ACTUALIZADO", "DES_TIPOLOGIA",
  "NIVEL", "ENTIDAD", "NOMBRE_INVERSION", "ESTADO", "SITUACION",
  "ALTERNATIVA", "FECHA_VIABILIDAD", "PROGRAMA", "SUBPROGRAMA",
  "MARCO", "TIPO_INVERSION", "DES_MODALIDAD", "REGISTRADO_PMI",
  "EXPEDIENTE_TECNICO", "INFORME_CIERRE", "DEVEN_ACUMUL_ANIO_ANT",
  "PIA_ANIO_ACTUAL", "PIM_ANIO_ACTUAL", "SALDO_EJECUTAR",
  "TIENE_F12B", "AVANCE_FISICO", "AVANCE_EJECUCION",
  "IND_IOARR_EMERG", "UBIGEO", "FEC_INI_EJECUCION",
  "FEC_INI_EJEC_FISICA", "NUM_HABITANTES_BENEF", "MONTO_ET_F8"
)
cols_det_inv_lower <- janitor::make_clean_names(cols_det_inv)

df_det_inv <- data.table::as.data.table(readRDS(ruta_rds("det_inv.rds")))
data.table::setnames(df_det_inv, janitor::make_clean_names(names(df_det_inv)))
df_det_inv <- df_det_inv[, ..cols_det_inv_lower]
df_det_inv[, codigo_unico := as.character(codigo_unico)]

df_pu_geoinv_inv_g <- readRDS(ruta_rds("geoinv.rds"))
local({
  n <- janitor::make_clean_names(names(df_pu_geoinv_inv_g))
  n[n == "cod_unico"]     <- "codigo_unico"
  n[n == "des_tipologia"] <- "des_tipologia_esri"
  names(df_pu_geoinv_inv_g) <<- n
})

cols_grd_12_25 <- c(
  "PRODUCTO_PROYECTO", "PRODUCTO_PROYECTO_NOMBRE",
  "ANIO", "PIA", "PIM", "DEVENGADO"
)
cols_grd_12_25_lower <- janitor::make_clean_names(cols_grd_12_25)

df_grd_2012_25 <- data.table::as.data.table(readRDS(ruta_rds("grd_2012_25.rds")))
data.table::setnames(df_grd_2012_25, janitor::make_clean_names(names(df_grd_2012_25)))
df_grd_2012_25 <- df_grd_2012_25[, ..cols_grd_12_25_lower]
data.table::setnames(df_grd_2012_25, "producto_proyecto", "codigo_unico")
df_grd_2012_25[, codigo_unico := as.character(codigo_unico)]

# --- Universo GRD común (intersección de las tres bases) ------------------
programa_objetivo <- "GESTION DE RIESGOS Y EMERGENCIAS"

codigos_grd_det <- unique(as.character(
  df_det_inv[
    stringi::stri_trans_general(programa, "Latin-ASCII") == programa_objetivo,
    codigo_unico
  ]
))
codigos_grd_geo <- unique(as.character(df_pu_geoinv_inv_g$codigo_unico))
codigos_grd_ts  <- unique(as.character(df_grd_2012_25[["codigo_unico"]]))

codigos_grd_comunes <- base::Reduce(
  intersect, list(codigos_grd_det, codigos_grd_geo, codigos_grd_ts)
)

# --- Depuración (replica L386-501 del cuaderno) ---------------------------
df_det_inv_grd <- df_det_inv[
  stringi::stri_trans_general(programa, "Latin-ASCII") == programa_objetivo
]
rm(df_det_inv); invisible(gc())

df_det_inv_grd_dp <- df_det_inv_grd[codigo_unico %in% codigos_grd_comunes]
df_det_inv_grd_dp[, codigo_unico := as.character(codigo_unico)]
rm(df_det_inv_grd)

df_pu_geoinv_inv_g_dp <- df_pu_geoinv_inv_g |>
  dplyr::filter(codigo_unico %in% codigos_grd_comunes) |>
  dplyr::select(codigo_unico, codigo_pro, link_ssi,
                des_servicio, des_tipologia_esri) |>
  dplyr::distinct(codigo_unico, .keep_all = TRUE) |>
  dplyr::mutate(codigo_unico = as.character(codigo_unico))
rm(df_pu_geoinv_inv_g); invisible(gc())

df_pu_geoinv_inv_g_dpf <- df_pu_geoinv_inv_g_dp |>
  dplyr::inner_join(df_det_inv_grd_dp, by = "codigo_unico")

stopifnot(nrow(df_pu_geoinv_inv_g_dpf) == length(codigos_grd_comunes))
stopifnot(inherits(df_pu_geoinv_inv_g_dpf, "sf"))

df_grd_2012_25_dp <- df_grd_2012_25[codigo_unico %in% codigos_grd_comunes]
rm(df_grd_2012_25); invisible(gc())

df_grd_2012_25_dpf <- df_grd_2012_25_dp |>
  dplyr::inner_join(
    sf::st_drop_geometry(df_pu_geoinv_inv_g_dpf),
    by = "codigo_unico"
  )

# Tabla plana sin geometría — usada por la mayoría de visualizaciones
df_geo_plain <- sf::st_drop_geometry(df_pu_geoinv_inv_g_dpf)

# --- Lookup departamental (ubigeo de 2 dígitos → nombre INEI) -------------
depto_lookup <- tibble::tribble(
  ~cod_depto, ~departamento,
  "01", "Amazonas",     "02", "Áncash",       "03", "Apurímac",
  "04", "Arequipa",     "05", "Ayacucho",      "06", "Cajamarca",
  "07", "Callao",       "08", "Cusco",          "09", "Huancavelica",
  "10", "Huánuco",      "11", "Ica",            "12", "Junín",
  "13", "La Libertad",  "14", "Lambayeque",     "15", "Lima",
  "16", "Loreto",       "17", "Madre de Dios",  "18", "Moquegua",
  "19", "Pasco",        "20", "Piura",           "21", "Puno",
  "22", "San Martín",   "23", "Tacna",           "24", "Tumbes",
  "25", "Ucayali",      "98", "Extranjero/SD"
)

# --- Paleta de tipologías ordenada por frecuencia (L848-894 del cuaderno)
tipologias_por_freq <- df_geo_plain |>
  dplyr::count(des_tipologia, sort = TRUE) |>
  dplyr::pull(des_tipologia)

colores_alto_contraste <- c(
  "#E41A1C", "#377EB8", "#4DAF4A", "#984EA3",
  "#FF7F00", "#FFD92F", "#A65628", "#F781BF",
  "#00CED1", "#1F78B4", "#6A3D9A", "#B15928"
)

colores_cola <- grDevices::gray.colors(
  n     = max(0, length(tipologias_por_freq) - length(colores_alto_contraste)),
  start = 0.5, end = 0.85
)

paleta_ordenada <- c(
  colores_alto_contraste[seq_len(min(length(colores_alto_contraste),
                                     length(tipologias_por_freq)))],
  colores_cola
)

pal_tipologia <- leaflet::colorFactor(
  palette  = paleta_ordenada,
  levels   = tipologias_por_freq,
  na.color = "#BBBBBB"
)

# --- Umbrales de outlier por tipología (Q3 + 3·IQR; L1202-1210) ----------
limites_iqr <- df_geo_plain |>
  dplyr::filter(!is.na(des_tipologia) & !is.na(costo_actualizado)) |>
  dplyr::group_by(des_tipologia) |>
  dplyr::summarise(
    q3  = quantile(costo_actualizado, 0.75, na.rm = TRUE),
    iqr = IQR(costo_actualizado,           na.rm = TRUE),
    .groups = "drop"
  ) |>
  dplyr::mutate(limite_outlier = q3 + 3 * iqr)

# --- Diccionario oficial (opcional; fallback a tibble vacío) -------------
ruta_dicc <- ruta_rds("diccionario.rds")
dicc_oficial <- if (file.exists(ruta_dicc)) {
  tryCatch(readRDS(ruta_dicc), error = function(e) NULL)
} else {
  NULL
}

vars_finales <- names(df_pu_geoinv_inv_g_dpf)
diccionario_final <- tibble::tibble(variable = vars_finales) |>
  dplyr::left_join(
    if (!is.null(dicc_oficial)) {
      tibble::tibble(
        variable   = janitor::make_clean_names(as.character(dicc_oficial[[1]])),
        definicion = as.character(dicc_oficial[[3]])
      )
    } else {
      tibble::tibble(variable = character(), definicion = character())
    },
    by = "variable"
  ) |>
  dplyr::left_join(definiciones_locales, by = "variable") |>
  dplyr::left_join(nombres_comunes, by = "variable") |>
  dplyr::mutate(
    definicion = dplyr::coalesce(
      definicion_local, definicion,
      "No está en el diccionario original (revisar)."
    )
  ) |>
  dplyr::select(variable, nombre_comun, definicion)

# --- Opciones precomputadas para los filtros del sidebar ------------------
opciones_tipologia <- sort(unique(df_geo_plain$des_tipologia))

opciones_depto <- df_geo_plain |>
  dplyr::mutate(cod_depto = stringr::str_sub(ubigeo, 1, 2)) |>
  dplyr::mutate(cod_depto = dplyr::if_else(
    !is.na(cod_depto) & nchar(cod_depto) == 2, cod_depto, "98"
  )) |>
  dplyr::distinct(cod_depto) |>
  dplyr::left_join(depto_lookup, by = "cod_depto") |>
  dplyr::mutate(departamento = dplyr::coalesce(departamento, glue::glue("Cod {cod_depto}"))) |>
  dplyr::arrange(cod_depto) |>
  dplyr::transmute(value = cod_depto,
                   label = glue::glue("{cod_depto} — {departamento}"))

opciones_estado <- sort(unique(stats::na.omit(df_geo_plain$estado)))

# --- Opciones para el buscador de inversión específica --------------------
opciones_codigo_inv <- df_geo_plain |>
  dplyr::transmute(
    value = codigo_unico,
    label = glue::glue("{codigo_unico} — {nombre_inversion}")
  ) |>
  dplyr::arrange(label)

# --- Serie portafolio total (referencia sin filtro) -----------------------
serie_portafolio_total <- data.table::as.data.table(df_grd_2012_25_dpf)[
  , .(pia       = sum(pia,       na.rm = TRUE),
      pim       = sum(pim,       na.rm = TRUE),
      devengado = sum(devengado, na.rm = TRUE)),
  by = anio
][order(anio)]

# --- CSS para la leyenda del mapa (alta especificidad sobre .info) -------
css_leyenda <- htmltools::tags$style(htmltools::HTML("
  .leaflet-container .leyenda-tipologia,
  .leaflet-container .leyenda-tipologia * {
    font-size: 9px !important;
    line-height: 1.15 !important;
  }
  .leaflet-container .leyenda-tipologia {
    max-width: 200px !important;
    padding: 4px 6px !important;
    white-space: normal !important;
  }
  .leaflet-container .leyenda-tipologia i {
    width: 10px !important;
    height: 10px !important;
    margin-right: 4px !important;
  }
"))
