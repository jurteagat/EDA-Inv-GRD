# global.R — Precarga de datos y precómputo de objetos compartidos
# para la Shiny app de Inversiones GRD.
#
# Replica el pipeline de EDA_Inv_GRD_v1.qmd una sola vez al arrancar.
# Los objetos quedan disponibles globalmente; el server solo los filtra.

options(scipen = 999)
options(shiny.useragg = TRUE)   # ragg honra la fuente Nunito Sans en renderPlot

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
source(here::here("R/helpers.R"),   local = FALSE)
source(here::here("R/theme_jut.R"), local = FALSE)
source(here::here("R/datos.R"),     local = FALSE)
source(here::here("R/exportar.R"),  local = FALSE)

# Tema gráfico por defecto de todos los ggplot de la app (marca "jut").
ggplot2::theme_set(theme_jut())

# --- Evaluar caché ANTES de cualquier descarga --------------------------------
ruta_rds  <- function(f) here::here("data", "processed", f)  # con extensión (CSV/legacy)
base_proc <- function(f) here::here("data", "processed", f)  # base SIN extensión

.ruta_cache <- ruta_cache_app()

# Fuentes que invalidan la caché por mtime. ruta_fuente() apunta al .parquet si
# existe (lo normal) o al .rds heredado durante la transición.
.fuentes_cache <- c(
  ruta_fuente(base_proc("det_inv")),
  ruta_fuente(base_proc("diccionario")),
  ruta_fuente(base_proc("geoinv")),
  ruta_fuente(base_proc("grd_2012_25")),
  ruta_rds("nombres_abreviados.csv"),
  ruta_fuente(base_proc("deptos_geo")),
  here::here("data", "processed", "fechas_fuentes.csv"),
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

  # Asegura las fuentes localmente. asegurar_fuente() NO descarga si ya hay
  # Parquet en el bundle (caso Connect optimizado); si no, baja el .rds de Drive
  # como red de seguridad. Los CSV pequeños se manejan aparte.
  asegurar_fuente(base_proc("det_inv"),     DRIVE_IDS$det_inv)
  asegurar_fuente(base_proc("diccionario"), DRIVE_IDS$diccionario)
  asegurar_fuente(base_proc("geoinv"),      DRIVE_IDS$geoinv)
  asegurar_fuente(base_proc("grd_2012_25"), DRIVE_IDS$grd_12_25)
  descargar_si_falta(DRIVE_IDS$nombres_abreviados,  ruta_rds("nombres_abreviados.csv"))
  descargar_si_falta(DRIVE_IDS$fechas_fuentes,      ruta_rds("fechas_fuentes.csv"))

  # Shapefile departamental: generar si no existe (ni Parquet ni RDS)
  if (!file.exists(ruta_fuente(base_proc("deptos_geo")))) {
    message("Generando deptos_geo.parquet desde Drive…")
    preparar_deptos_geo(
      ruta_descarga = here::here("data", "raw", "deptos_shp.zip"),
      ruta_salida   = base_proc("deptos_geo")
    )
  }
  deptos_geo <- leer_fuente(base_proc("deptos_geo"), espacial = TRUE)

  # Lectura de fuentes (Parquet preferido). En det_inv se usa col_select para
  # leer solo las columnas que la app necesita (menos RAM y tiempo).
  cols_det_inv <- c(
    "CODIGO_UNICO", "MONTO_VIABLE", "COSTO_ACTUALIZADO", "DES_TIPOLOGIA",
    "NIVEL", "ENTIDAD", "NOMBRE_INVERSION", "ESTADO", "SITUACION", "NOMBRE_UEP",
    "ALTERNATIVA", "FECHA_VIABILIDAD", "PROGRAMA", "SUBPROGRAMA",
    "MARCO", "TIPO_INVERSION", "DES_MODALIDAD", "REGISTRADO_PMI",
    "EXPEDIENTE_TECNICO", "INFORME_CIERRE", "DEVEN_ACUMUL_ANIO_ANT",
    "PIA_ANIO_ACTUAL", "PIM_ANIO_ACTUAL", "DEV_ANIO_ACTUAL",
    "TIENE_F12B", "AVANCE_FISICO",
    "IND_IOARR_EMERG", "UBIGEO", "FEC_INI_EJECUCION",
    "FEC_INI_EJEC_FISICA", "NUM_HABITANTES_BENEF", "MONTO_ET_F8"
  )
  cols_det_inv_lower <- janitor::make_clean_names(cols_det_inv)

  df_det_inv <- data.table::as.data.table(
    leer_fuente(base_proc("det_inv"), col_select = cols_det_inv)
  )
  data.table::setnames(df_det_inv, janitor::make_clean_names(names(df_det_inv)))
  df_det_inv <- df_det_inv[, ..cols_det_inv_lower]
  df_det_inv[, codigo_unico := as.character(codigo_unico)]
  df_det_inv[, ubigeo := zero_pad_ubigeo(ubigeo)]

  df_pu_geoinv_inv_g <- leer_fuente(base_proc("geoinv"), espacial = TRUE)
  local({
    n <- janitor::make_clean_names(names(df_pu_geoinv_inv_g))
    n[n == "cod_unico"]     <- "codigo_unico"
    # Renombramos la tipología del geopackage solo para evitar que colisione
    # con des_tipologia del detalle MEF en el inner_join. Esta columna se
    # descarta aguas abajo (no se selecciona en df_pu_geoinv_inv_g_dp): por
    # decisión, la tipología de análisis es únicamente des_tipologia del MEF.
    n[n == "des_tipologia"] <- "des_tipologia_esri"
    names(df_pu_geoinv_inv_g) <<- n
  })

  cols_grd_12_25       <- c("PRODUCTO_PROYECTO", "PRODUCTO_PROYECTO_NOMBRE",
                             "ANIO", "PIA", "PIM", "DEVENGADO", "PLIEGO_NOMBRE")
  cols_grd_12_25_lower <- janitor::make_clean_names(cols_grd_12_25)

  df_grd_2012_25 <- data.table::as.data.table(leer_fuente(base_proc("grd_2012_25")))
  data.table::setnames(df_grd_2012_25, janitor::make_clean_names(names(df_grd_2012_25)))
  df_grd_2012_25 <- df_grd_2012_25[, ..cols_grd_12_25_lower]
  data.table::setnames(df_grd_2012_25, "producto_proyecto", "codigo_unico")
  df_grd_2012_25[, codigo_unico := as.character(codigo_unico)]

  # Universo GRD común
  codigos_grd_comunes <- construir_universo_comun(df_det_inv, df_pu_geoinv_inv_g,
                                                   df_grd_2012_25)

  # Depuración. Selección GRD: programa objetivo + drenaje pluvial +
  # ind_ioarr_emerg == "SI", excluyendo el subprograma de incendios (ver
  # filtrar_det_grd en R/datos.R, fuente única compartida con el cuaderno EDA).
  df_det_inv_grd <- filtrar_det_grd(df_det_inv)
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
    dplyr::select(codigo_unico, codigo_pro, link_ssi) |>
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

  # Pliego del último año con dato no vacío por inversión. El pliego solo existe
  # en esta serie SIAF y cambia con el tiempo (reorganizaciones); fijamos el
  # valor más reciente como atributo único que viajará por la base geoespacial.
  pliego_x_inv <- df_grd_2012_25_dp[
    !is.na(pliego_nombre) & pliego_nombre != "",
    .SD[which.max(anio)], by = codigo_unico,
    .SDcols = c("anio", "pliego_nombre")
  ][, .(codigo_unico, pliego_nombre)]

  # La versión per-año se descarta para evitar choque de nombres en el join.
  df_grd_2012_25_dp[, pliego_nombre := NULL]

  # Fila 2026 (año en curso) desde el detalle: PIA/PIM/devengado del año actual a
  # nivel de inversión. Extiende la serie SIAF 2012-2025 al año en curso y permite
  # conservar CUIs que aún no tienen historia 2012-2025. df_det_inv_grd_dp ya no es
  # data.table (pasó por join_nombres_abreviados), por eso se usa dplyr::transmute.
  df_grd_2026_dp <- df_det_inv_grd_dp |>
    dplyr::transmute(
      codigo_unico,
      producto_proyecto_nombre = NA_character_,
      anio      = 2026L,
      pia       = as.numeric(pia_anio_actual),
      pim       = as.numeric(pim_anio_actual),
      devengado = dplyr::coalesce(as.numeric(dev_anio_actual), 0)
    )

  # Serie 2012-2026: filas históricas (SIAF) + fila 2026 (detalle).
  df_grd_serie_dp <- data.table::rbindlist(
    list(df_grd_2012_25_dp, df_grd_2026_dp), use.names = TRUE, fill = TRUE
  )

  # Devengado acumulado 2012-2026 por inversión (misma definición canónica que las
  # tablas top-10): suma directa sobre la serie, que ya incluye la fila 2026. Se
  # une a la base geoespacial para derivar avance_financiero de forma consistente.
  deveng_acum_x_inv <- df_grd_serie_dp |>
    dplyr::group_by(codigo_unico) |>
    dplyr::summarise(devengado_acum = sum(devengado, na.rm = TRUE),
                     .groups = "drop")

  # avance_financiero = devengado acumulado 2012-2026 / costo actualizado * 100.
  # Si el costo actualizado es 0 o NA, queda NA (evita 0/0 e Inf).
  df_pu_geoinv_inv_g_dpf <- df_pu_geoinv_inv_g_dpf |>
    dplyr::left_join(deveng_acum_x_inv, by = "codigo_unico") |>
    dplyr::mutate(
      devengado_acum = dplyr::coalesce(devengado_acum, 0),
      avance_financiero = dplyr::if_else(
        is.na(as.numeric(costo_actualizado)) | as.numeric(costo_actualizado) == 0,
        NA_real_,
        devengado_acum / as.numeric(costo_actualizado) * 100
      )
    )

  # Adjuntar el pliego (último año) a la base geoespacial ANTES del merge
  # temporal, para que la columna viaje a df_grd_2012_25_dpf vía el inner_join.
  df_pu_geoinv_inv_g_dpf <- df_pu_geoinv_inv_g_dpf |>
    dplyr::left_join(pliego_x_inv, by = "codigo_unico")

  # La serie fusionada (2012-2026) conserva el nombre df_grd_2012_25_dpf por
  # compatibilidad con el resto del app.
  df_grd_2012_25_dpf <- df_grd_serie_dp |>
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

  # Paleta de marca "jut": pasteles cualitativos para las tipologías más
  # frecuentes + cola de grises para la larga (lo hace colores_jut).
  paleta_ordenada <- colores_jut(length(tipologias_por_freq))
  pal_tipologia <- leaflet::colorFactor(
    palette  = paleta_ordenada,
    levels   = tipologias_por_freq,
    na.color = "#BBBBBB"
  )

  # Umbrales de outlier por tipología (Q3 + 3·IQR) sobre el sobrecosto relativo:
  # la variación % del costo actualizado respecto del monto viable. Así se
  # señalan las inversiones cuyo costo se disparó frente a lo aprobado, sin que
  # el tamaño de la obra distorsione el resultado.
  limites_iqr <- df_geo_plain |>
    dplyr::mutate(pct_costo_vs_viable = dplyr::if_else(
      is.finite(monto_viable) & monto_viable > 0,
      (costo_actualizado - monto_viable) / monto_viable * 100,
      NA_real_
    )) |>
    dplyr::filter(!is.na(des_tipologia) & !is.na(pct_costo_vs_viable)) |>
    dplyr::group_by(des_tipologia) |>
    dplyr::summarise(
      q3  = quantile(pct_costo_vs_viable, 0.75, na.rm = TRUE),
      iqr = IQR(pct_costo_vs_viable,           na.rm = TRUE),
      .groups = "drop"
    ) |>
    dplyr::mutate(limite_outlier = q3 + 3 * iqr)

  # Diccionario oficial (opcional)
  definiciones_locales <- tibble::tribble(
    ~variable,              ~definicion_local,
    # Edita aquí la definición de codigo_unico: este override tiene prioridad
    # sobre la del diccionario oficial del MEF (diccionario.rds).
    "codigo_unico",         "Código Único de Inversión (CUI) en el marco del Invierte.pe.",
    "codigo_pro",           "Código interno del programa de inversiones (GeoInvierte).",
    "link_ssi",             "URL a la inversión en el SSI del MEF (GeoInvierte.",
    # "des_servicio",         "Descripción del servicio público asociado (GeoInvierte).",
    "avance_financiero",    "Avance Financiero. Corresponde al porcentaje de devengado acumulado, respecto del costo actualizado",
    # "des_tipologia_esri",   "Tipología según la capa GeoInvierte puede diferir de des_tipologia del diccionario MEF.",
    # La columna de geometría del objeto sf se llama "geom" (no "geometry").
    "geom",                 "Geometría (POINT) en WGS84 con la ubicación del proyecto.",
    "nombre_abreviado",     "Nombre abreviado generado con inteligencia artificial a partir del nombre completo de la inversión.",
    "devengado_acum",           "Devengado acumulado periodo 2012-2026",
    "pliego_nombre",        "Pliego presupuestal al que pertenece la Entidad (~90% faltante en la fuente SIAF)."
  )

  ruta_dicc    <- ruta_fuente(base_proc("diccionario"))
  dicc_oficial <- if (file.exists(ruta_dicc)) {
    tryCatch(leer_fuente(base_proc("diccionario")), error = function(e) NULL)
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
  opciones_tipo_inversion <- sort(unique(stats::na.omit(
    df_geo_plain$tipo_inversion
  )))

  opciones_codigo_inv <- df_geo_plain |>
    dplyr::transmute(
      value = codigo_unico,
      label = glue::glue("{codigo_unico} — {nombre_abreviado}")
    ) |>
    dplyr::arrange(label)

  # Fecha de los datos (generada por 00_datos_entrada.qmd, descargada de Drive)
  ruta_fechas <- ruta_rds("fechas_fuentes.csv")
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
    opciones_tipo_inversion = opciones_tipo_inversion,
    opciones_codigo_inv     = opciones_codigo_inv,
    serie_portafolio_total  = serie_portafolio_total,
    css_leyenda             = css_leyenda
  ), .ruta_cache)

  message("Caché de arranque guardada en ", .ruta_cache)
}

rm(.ruta_cache, .fuentes_cache)
