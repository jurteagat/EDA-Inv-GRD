# global.R — Precarga de datos y precómputo de objetos compartidos
# para la Shiny app de Inversiones GRD.
#
# Replica el pipeline de EDA_Inv_GRD_v1.qmd una sola vez al arrancar.
# Los objetos quedan disponibles globalmente; el server solo los filtra.

options(scipen = 999)

# --- Paquetes ------------------------------------------------------------------
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
  library(bsicons)
  library(rmapshaper)
  library(shiny)
  library(readr)
  library(googledrive)
})

# --- Cargar módulos R/ ---------------------------------------------------------
source(here::here("R/helpers.R"),  local = FALSE)
source(here::here("R/datos.R"),    local = FALSE)
source(here::here("R/exportar.R"), local = FALSE)

# --- Evaluar caché ANTES de cualquier descarga --------------------------------
ruta_rds <- function(f) here::here("midputs", "rds", f)

.ruta_cache <- ruta_cache_app()

.fuentes_cache <- c(
  ruta_rds("det_inv.rds"),
  ruta_rds("diccionario.rds"),
  ruta_rds("geoinv.rds"),
  ruta_rds("grd_2012_25.rds"),
  ruta_rds("nombres_abreviados.csv"),
  ruta_rds("deptos_geo.rds"),
  here::here("midputs", "rds", "fechas_fuentes.csv"),
  list.files(here::here("R"), pattern = "\\.R$", full.names = TRUE),
  here::here("global.R")
)

if (cache_app_vigente(.ruta_cache, .fuentes_cache) &&
    Sys.getenv("GRD_REBUILD_CACHE") != "1") {

  # --- Ruta rápida: cargar caché prehorneada ---------------------------------
  message("Cargando caché de arranque…")
  .cache <- cargar_cache_app(.ruta_cache)
  list2env(.cache, envir = globalenv())
  rm(.cache)
  message("Caché cargada — arranque rápido.")

} else {

  # --- Ruta completa: pipeline + guardar caché --------------------------------

  # Descarga de archivos si faltan
  descargar_si_falta(DRIVE_IDS$det_inv,            ruta_rds("det_inv.rds"))
  descargar_si_falta(DRIVE_IDS$diccionario,         ruta_rds("diccionario.rds"))
  descargar_si_falta(DRIVE_IDS$geoinv,              ruta_rds("geoinv.rds"))
  descargar_si_falta(DRIVE_IDS$grd_12_25,           ruta_rds("grd_2012_25.rds"))
  descargar_si_falta(DRIVE_IDS$nombres_abreviados,  ruta_rds("nombres_abreviados.csv"))

  # Shapefile departamental: generar si no existe
  if (!file.exists(ruta_rds("deptos_geo.rds"))) {
    message("Generando deptos_geo.rds desde Drive…")
    preparar_deptos_geo(
      ruta_descarga = here::here("raw", "deptos_shp.zip"),
      ruta_rds      = ruta_rds("deptos_geo.rds")
    )
  }
  deptos_geo <- readRDS(ruta_rds("deptos_geo.rds"))

  # Lectura de RDS
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
  df_det_inv[, ubigeo := zero_pad_ubigeo(ubigeo)]

  df_pu_geoinv_inv_g <- readRDS(ruta_rds("geoinv.rds"))
  local({
    n <- janitor::make_clean_names(names(df_pu_geoinv_inv_g))
    n[n == "cod_unico"]     <- "codigo_unico"
    n[n == "des_tipologia"] <- "des_tipologia_esri"
    names(df_pu_geoinv_inv_g) <<- n
  })

  cols_grd_12_25       <- c("PRODUCTO_PROYECTO", "PRODUCTO_PROYECTO_NOMBRE",
                             "ANIO", "PIA", "PIM", "DEVENGADO")
  cols_grd_12_25_lower <- janitor::make_clean_names(cols_grd_12_25)

  df_grd_2012_25 <- data.table::as.data.table(readRDS(ruta_rds("grd_2012_25.rds")))
  data.table::setnames(df_grd_2012_25, janitor::make_clean_names(names(df_grd_2012_25)))
  df_grd_2012_25 <- df_grd_2012_25[, ..cols_grd_12_25_lower]
  data.table::setnames(df_grd_2012_25, "producto_proyecto", "codigo_unico")
  df_grd_2012_25[, codigo_unico := as.character(codigo_unico)]

  # Universo GRD común
  codigos_grd_comunes <- construir_universo_comun(df_det_inv, df_pu_geoinv_inv_g,
                                                   df_grd_2012_25)

  # Depuración
  programa_objetivo <- "GESTION DE RIESGOS Y EMERGENCIAS"

  df_det_inv_grd <- df_det_inv[
    stringi::stri_trans_general(programa, "Latin-ASCII") == programa_objetivo
  ]
  rm(df_det_inv); invisible(gc())

  df_det_inv_grd_dp <- df_det_inv_grd[codigo_unico %in% codigos_grd_comunes]
  df_det_inv_grd_dp[, codigo_unico := as.character(codigo_unico)]
  rm(df_det_inv_grd)

  df_det_inv_grd_dp <- join_nombres_abreviados(
    df_det_inv_grd_dp,
    ruta_rds("nombres_abreviados.csv")
  )

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

  # Recodificar tipología vacía → "Indeterminada" (obs 1)
  df_pu_geoinv_inv_g_dpf <- df_pu_geoinv_inv_g_dpf |>
    dplyr::mutate(
      des_tipologia = dplyr::if_else(
        is.na(des_tipologia) | trimws(des_tipologia) == "",
        "Indeterminada",
        des_tipologia
      )
    )

  df_grd_2012_25_dp <- df_grd_2012_25[codigo_unico %in% codigos_grd_comunes]
  rm(df_grd_2012_25); invisible(gc())

  df_grd_2012_25_dpf <- df_grd_2012_25_dp |>
    dplyr::inner_join(
      sf::st_drop_geometry(df_pu_geoinv_inv_g_dpf),
      by = "codigo_unico"
    )

  df_geo_plain <- sf::st_drop_geometry(df_pu_geoinv_inv_g_dpf)

  # Lookup departamental
  depto_lookup <- tibble::tribble(
    ~cod_depto, ~departamento,
    "01", "Amazonas",     "02", "Áncash",        "03", "Apurímac",
    "04", "Arequipa",     "05", "Ayacucho",       "06", "Cajamarca",
    "07", "Callao",       "08", "Cusco",           "09", "Huancavelica",
    "10", "Huánuco",      "11", "Ica",             "12", "Junín",
    "13", "La Libertad",  "14", "Lambayeque",      "15", "Lima",
    "16", "Loreto",       "17", "Madre de Dios",   "18", "Moquegua",
    "19", "Pasco",        "20", "Piura",            "21", "Puno",
    "22", "San Martín",   "23", "Tacna",            "24", "Tumbes",
    "25", "Ucayali",      "98", "Extranjero/SD"
  )

  # Paleta de tipologías por frecuencia
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

  # Umbrales de outlier por tipología (Q3 + 3·IQR)
  limites_iqr <- df_geo_plain |>
    dplyr::filter(!is.na(des_tipologia) & !is.na(costo_actualizado)) |>
    dplyr::group_by(des_tipologia) |>
    dplyr::summarise(
      q3  = quantile(costo_actualizado, 0.75, na.rm = TRUE),
      iqr = IQR(costo_actualizado,           na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(limite_outlier = q3 + 3 * iqr)

  # Diccionario oficial (opcional)
  definiciones_locales <- tibble::tribble(
    ~variable,              ~definicion_local,
    "codigo_pro",           "Código interno del programa de inversiones (capa ESRI).",
    "link_ssi",             "URL a la inversión en el SSI del MEF (capa ESRI).",
    "des_servicio",         "Descripción del servicio público asociado (capa ESRI).",
    "des_tipologia_esri",   "Tipología según la capa ESRI; puede diferir de des_tipologia del diccionario MEF.",
    "geometry",             "Geometría (POINT) en WGS84 con la ubicación del proyecto.",
    "nombre_abreviado",     "Nombre abreviado curado (coalesce de nom_inv_corto + nombre_inversion)."
  )

  ruta_dicc    <- ruta_rds("diccionario.rds")
  dicc_oficial <- if (file.exists(ruta_dicc)) {
    tryCatch(readRDS(ruta_dicc), error = function(e) NULL)
  } else { NULL }

  vars_finales    <- names(df_pu_geoinv_inv_g_dpf)
  diccionario_mef <- tibble::tibble(variable = vars_finales) |>
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
    dplyr::mutate(
      definicion = dplyr::coalesce(definicion, "No está en el diccionario original.")
    ) |>
    dplyr::select(variable, definicion)

  diccionario_final <- tibble::tibble(variable = vars_finales) |>
    dplyr::left_join(definiciones_locales, by = "variable") |>
    dplyr::left_join(nombres_comunes, by = "variable") |>
    dplyr::left_join(diccionario_mef, by = "variable") |>
    dplyr::mutate(
      definicion_efectiva = dplyr::coalesce(
        definicion_local, definicion,
        "Sin definición disponible."
      )
    ) |>
    dplyr::select(variable, nombre_comun, definicion_efectiva)

  # Opciones para filtros del sidebar
  opciones_tipologia <- sort(unique(df_geo_plain$des_tipologia))

  opciones_depto <- df_geo_plain |>
    dplyr::mutate(cod_depto = stringr::str_sub(ubigeo, 1, 2),
                  cod_depto = dplyr::if_else(
                    !is.na(cod_depto) & nchar(cod_depto) == 2, cod_depto, "98"
                  )) |>
    dplyr::distinct(cod_depto) |>
    dplyr::left_join(depto_lookup, by = "cod_depto") |>
    dplyr::mutate(departamento = dplyr::coalesce(
      departamento, glue::glue("Cod {cod_depto}")
    )) |>
    dplyr::arrange(cod_depto) |>
    dplyr::transmute(value = cod_depto,
                     label = glue::glue("{cod_depto} — {departamento}"))

  opciones_situacion <- sort(unique(stats::na.omit(df_geo_plain$situacion)))
  opciones_ioarr     <- c("Todos", sort(unique(stats::na.omit(
    as.character(df_geo_plain$ind_ioarr_emerg)
  ))))

  opciones_codigo_inv <- df_geo_plain |>
    dplyr::transmute(
      value = codigo_unico,
      label = glue::glue("{codigo_unico} — {nombre_abreviado}")
    ) |>
    dplyr::arrange(label)

  # Fecha de los datos (generada por 00_datos_entrada.qmd)
  ruta_fechas <- here::here("midputs", "rds", "fechas_fuentes.csv")
  fechas_fuentes <- if (file.exists(ruta_fechas)) {
    data.table::fread(ruta_fechas)
  } else { NULL }

  # Serie portafolio total (sin filtro)
  serie_portafolio_total <- serie_portafolio(df_grd_2012_25_dpf)

  # CSS leyenda del mapa
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

  # Guardar caché para el próximo arranque
  guardar_cache_app(list(
    df_pu_geoinv_inv_g_dpf  = df_pu_geoinv_inv_g_dpf,
    df_grd_2012_25_dpf      = df_grd_2012_25_dpf,
    df_geo_plain            = df_geo_plain,
    depto_lookup            = depto_lookup,
    deptos_geo              = deptos_geo,
    tipologias_por_freq     = tipologias_por_freq,
    paleta_ordenada         = paleta_ordenada,
    pal_tipologia           = pal_tipologia,
    limites_iqr             = limites_iqr,
    diccionario_final       = diccionario_final,
    opciones_tipologia      = opciones_tipologia,
    opciones_depto          = opciones_depto,
    fechas_fuentes          = fechas_fuentes,
    opciones_situacion      = opciones_situacion,
    opciones_ioarr          = opciones_ioarr,
    opciones_codigo_inv     = opciones_codigo_inv,
    serie_portafolio_total  = serie_portafolio_total,
    css_leyenda             = css_leyenda
  ), .ruta_cache)

  message("Caché de arranque guardada en ", .ruta_cache)
}

rm(.ruta_cache, .fuentes_cache)
