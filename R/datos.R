# R/datos.R — Descarga desde Drive y pipeline de datos (funciones puras)

# IDs de Drive (archivos públicos sin OAuth)
DRIVE_IDS <- list(
  det_inv           = "1Q0BYJcVM6zBvbj-5sLXctW9KThS4oZ6u",
  diccionario       = "13J6fKucKgNJ8Kngtq4gGGsnvMenC1A8i",
  geoinv            = "1V1jRSqqgjeX7jJkhSag0b1ufB5PP9X2Z",
  grd_12_25         = "1ZQHQOMFD8MG0r5pgDgcniUbZR_XGDGVq",
  nombres_abreviados = "1_ielTIIhPjN-6jd9SF7w-LZrmSJRos04",
  deptos_shp        = "1T0vOdYJLdigP1C4aykrEgJAINxit1PTA",
  fechas_fuentes    = "18jPh1hreHOM2_4VonCOlzt5IiU15STOx"
)

#' Descarga un archivo de Google Drive solo si no existe localmente.
descargar_si_falta <- function(id, destino) {
  if (file.exists(destino)) return(invisible(destino))
  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
  message("Descargando desde Drive → ", basename(destino))
  googledrive::drive_deauth()
  googledrive::drive_download(
    googledrive::as_id(id),
    path      = destino,
    overwrite = FALSE
  )
  invisible(destino)
}

#' Aplica zero-pad de 6 dígitos al campo ubigeo.
zero_pad_ubigeo <- function(x) {
  stringr::str_pad(as.character(x), width = 6, side = "left", pad = "0")
}

# --- Lectura/escritura de fuentes en Parquet (con fallback a RDS) -------------
# Las fuentes canónicas son Parquet/GeoParquet: más livianas que el RDS gzip y
# de lectura ~4–20× más rápida (ver notas_optimizacion.md), y legibles por
# pandas/geopandas para futuros complementos en Python. Durante la transición se
# mantiene el fallback a `.rds` para no romper entornos que aún no migraron.

# Compresión por defecto de los Parquet del proyecto: zstd nivel 9 (más chico
# que RDS gzip y misma velocidad de lectura que snappy).
.COMPRESION_PARQUET <- "zstd"
.NIVEL_PARQUET      <- 9L

#' Devuelve la ruta de una fuente priorizando `.parquet` sobre `.rds`.
#' `base` es la ruta SIN extensión (o con una extensión que se ignora),
#' p.ej. `ruta_fuente(here::here("data/processed/det_inv"))`.
ruta_fuente <- function(base) {
  base <- sub("\\.(parquet|rds)$", "", base)
  pq <- paste0(base, ".parquet")
  if (file.exists(pq)) pq else paste0(base, ".rds")
}

#' Lee una fuente: `.parquet` (preferido) o `.rds` (fallback de transición).
#' `espacial = TRUE` usa GeoParquet/sf. `col_select` (solo aplica al leer
#' Parquet no espacial) limita las columnas leídas para ahorrar RAM y tiempo;
#' en el camino RDS se ignora y el llamador selecciona columnas después.
leer_fuente <- function(base, espacial = FALSE, col_select = NULL) {
  f <- ruta_fuente(base)
  if (grepl("\\.parquet$", f)) {
    if (espacial) {
      sfarrow::st_read_parquet(f)
    } else if (!is.null(col_select)) {
      arrow::read_parquet(f, col_select = dplyr::all_of(col_select))
    } else {
      arrow::read_parquet(f)
    }
  } else {
    readRDS(f)
  }
}

#' Escribe una fuente como Parquet/GeoParquet con la compresión del proyecto.
#' Devuelve la ruta del `.parquet` escrito.
escribir_fuente <- function(obj, base, espacial = FALSE) {
  destino <- paste0(sub("\\.(parquet|rds)$", "", base), ".parquet")
  dir.create(dirname(destino), recursive = TRUE, showWarnings = FALSE)
  if (espacial) {
    sfarrow::st_write_parquet(obj, destino,
      compression = .COMPRESION_PARQUET, compression_level = .NIVEL_PARQUET)
  } else {
    arrow::write_parquet(obj, destino,
      compression = .COMPRESION_PARQUET, compression_level = .NIVEL_PARQUET)
  }
  invisible(destino)
}

#' Garantiza que una fuente exista localmente. Si NI `.parquet` NI `.rds` están
#' presentes, baja el `.parquet` de Drive como red de seguridad (p.ej. en Posit
#' Connect sin Parquet empaquetado). No descarga nada si ya hay una fuente local
#' (lo normal cuando el bundle trae los `.parquet`). El formato canónico en Drive
#' es Parquet (lo sube `00_datos_entrada.qmd`); por eso se descarga a `.parquet`.
asegurar_fuente <- function(base, drive_id) {
  base <- sub("\\.(parquet|rds)$", "", base)
  if (file.exists(paste0(base, ".parquet")) || file.exists(paste0(base, ".rds")))
    return(invisible())
  descargar_si_falta(drive_id, paste0(base, ".parquet"))
}

#' Une nombres_abreviados.csv a df y crea nombre_abreviado via coalesce.
join_nombres_abreviados <- function(df, ruta_csv) {
  nombres <- data.table::fread(ruta_csv,
                               colClasses = list(character = "codigo_unico"),
                               select = c("codigo_unico", "nom_inv_corto"))
  nombres <- janitor::clean_names(nombres)
  df <- dplyr::left_join(df, nombres, by = "codigo_unico")
  df <- dplyr::mutate(df,
    nombre_abreviado = dplyr::coalesce(
      dplyr::if_else(nchar(trimws(nom_inv_corto)) > 0, nom_inv_corto, NA_character_),
      nombre_inversion
    )
  )
  dplyr::select(df, -nom_inv_corto)
}

# --- Caché de arranque --------------------------------------------------------

#' Ruta canónica del archivo de caché de arranque.
#' Formato qs2 (zstd): ~13× más chico que el RDS sin comprimir y de lectura más
#' rápida. La caché guarda objetos R (sf, closures de leaflet), por eso NO migra
#' a Parquet; solo cambia el serializador.
ruta_cache_app <- function() {
  here::here("data", "processed", "_cache_app.qs2")
}

#' Devuelve TRUE si la caché existe y ninguna fuente *existente* es más nueva.
#' Fuentes que no existen en disco se ignoran (clave para Posit Connect).
cache_app_vigente <- function(ruta_cache, fuentes) {
  if (!file.exists(ruta_cache)) return(FALSE)
  mtime_cache <- file.mtime(ruta_cache)
  fuentes_presentes <- fuentes[file.exists(fuentes)]
  if (length(fuentes_presentes) == 0L) return(TRUE)
  all(file.mtime(fuentes_presentes) <= mtime_cache)
}

#' Serializa la lista nombrada de objetos a disco (qs2/zstd).
guardar_cache_app <- function(objetos, ruta_cache = ruta_cache_app()) {
  dir.create(dirname(ruta_cache), recursive = TRUE, showWarnings = FALSE)
  qs2::qs_save(objetos, ruta_cache)
  invisible(ruta_cache)
}

#' Carga la caché y devuelve la lista nombrada de objetos.
#' Detecta el formato por contenido: intenta qs2 (formato actual) y cae a un
#' `.rds` heredado durante la transición.
cargar_cache_app <- function(ruta_cache = ruta_cache_app()) {
  tryCatch(qs2::qs_read(ruta_cache), error = function(e) readRDS(ruta_cache))
}

#' Descarga el shapefile departamental, simplifica y guarda como GeoParquet.
#' El campo de código departamental se detecta automáticamente buscando
#' nombres comunes: CCDD, IDDPTO, COD_DPTO, UBIGEO (2 primeros dígitos).
#' `ruta_salida` es la ruta destino (la extensión se normaliza a `.parquet`).
preparar_deptos_geo <- function(ruta_descarga, ruta_salida) {
  dir.create(dirname(ruta_descarga), recursive = TRUE, showWarnings = FALSE)
  googledrive::drive_deauth()
  googledrive::drive_download(
    googledrive::as_id(DRIVE_IDS$deptos_shp),
    path      = ruta_descarga,
    overwrite = TRUE
  )

  if (endsWith(tolower(ruta_descarga), ".zip")) {
    dir_out <- file.path(dirname(ruta_descarga), "deptos_shp_raw")
    utils::unzip(ruta_descarga, exdir = dir_out)
    shp_file <- list.files(dir_out, pattern = "\\.shp$",
                           full.names = TRUE, recursive = TRUE)[1]
    sf_raw <- sf::st_read(shp_file, quiet = TRUE)
  } else {
    sf_raw <- sf::st_read(ruta_descarga, quiet = TRUE)
  }

  sf_wgs84 <- sf::st_transform(sf_raw, crs = 4326)

  campos <- toupper(names(sf_wgs84))
  campo_cod <- NULL
  for (candidato in c("CCDD", "IDDPTO", "COD_DPTO", "DEPARTAMEN")) {
    idx <- match(candidato, campos)
    if (!is.na(idx)) { campo_cod <- names(sf_wgs84)[idx]; break }
  }

  sf_simp <- rmapshaper::ms_simplify(sf_wgs84, keep = 0.02, keep_shapes = TRUE)

  if (!is.null(campo_cod)) {
    sf_simp$cod_depto <- stringr::str_pad(
      as.character(sf_simp[[campo_cod]]), width = 2, side = "left", pad = "0"
    )
  } else {
    # Fallback: si hay UBIGEO tomar primeros 2 dígitos
    idx_ub <- match("UBIGEO", campos)
    if (!is.na(idx_ub)) {
      sf_simp$cod_depto <- stringr::str_sub(
        stringr::str_pad(as.character(sf_simp[[names(sf_wgs84)[idx_ub]]]),
                         width = 6, pad = "0"),
        1, 2
      )
    } else {
      stop("No se encontró campo de código departamental en el shapefile: ",
           paste(names(sf_raw), collapse = ", "))
    }
  }

  sf_final <- sf_simp |>
    dplyr::select(cod_depto, geometry) |>
    dplyr::filter(!is.na(cod_depto))

  escribir_fuente(sf_final, ruta_salida, espacial = TRUE)
  invisible(sf_final)
}

#' Selecciona las inversiones GRD del detalle MEF.
#'
#' Incluye las del programa objetivo (GESTIÓN DE RIESGOS Y EMERGENCIAS), las de
#' tipología de drenaje pluvial (servicio/sistema) y las que tienen indicador de
#' emergencia (`ind_ioarr_emerg == "SI"`), sin importar el programa o tipología.
#' Excluye **siempre** las del subprograma de defensa contra incendios y
#' emergencias menores: la exclusión prevalece sobre cualquier criterio de
#' inclusión. Todas las comparaciones de texto se normalizan (Latin-ASCII +
#' mayúsculas) porque las tildes y mayúsculas del CSV MEF varían.
filtrar_det_grd <- function(det_inv_dt,
                            programa_objetivo = "GESTION DE RIESGOS Y EMERGENCIAS") {
  norm <- function(x) stringi::stri_trans_general(as.character(x), "Latin-ASCII")
  eq   <- function(x, valor) !is.na(x) & x == valor

  tipologias_drenaje  <- c("SERVICIO DE DRENAJE PLUVIAL", "SISTEMA DE DRENAJE PLUVIAL")
  subprograma_excluir <- "DEFENSA CONTRA INCENDIOS Y EMERGENCIAS MENORES"

  programa_n    <- norm(det_inv_dt$programa)
  tipologia_n   <- norm(det_inv_dt$des_tipologia)
  subprograma_n <- norm(det_inv_dt$subprograma)
  ioarr_n       <- toupper(trimws(as.character(det_inv_dt$ind_ioarr_emerg)))

  incluir <- eq(programa_n, programa_objetivo) |
             tipologia_n %in% tipologias_drenaje |
             eq(ioarr_n, "SI")
  excluir <- eq(subprograma_n, subprograma_excluir)

  det_inv_dt[incluir & !excluir]
}

#' Construye el universo de códigos GRD comunes a las tres fuentes.
construir_universo_comun <- function(det_inv_dt, geoinv_sf, grd_ts_dt,
                                     programa_objetivo = "GESTION DE RIESGOS Y EMERGENCIAS") {
  codigos_det <- unique(as.character(
    filtrar_det_grd(det_inv_dt, programa_objetivo)$codigo_unico
  ))
  codigos_geo <- unique(as.character(geoinv_sf$codigo_unico))
  codigos_ts  <- unique(as.character(grd_ts_dt[["codigo_unico"]]))
  # La serie SIAF solo llega hasta 2025. Los CUIs del detalle que pasan el filtro
  # se completan con una fila 2026 (ver pipeline en global.R / cuaderno EDA), así
  # que su presencia temporal se garantiza por unión: no se excluyen por carecer
  # de historia 2012-2025. El universo queda, en la práctica, det ∩ geo.
  codigos_ts_ext <- base::union(codigos_ts, codigos_det)
  universo <- base::Reduce(intersect, list(codigos_det, codigos_geo, codigos_ts_ext))
  # Descarta CUIs vacíos/NA: un registro sin código no debe propagarse a filtros,
  # tablas ni buscador.
  universo[!is.na(universo) & nzchar(trimws(universo))]
}
