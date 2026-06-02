# R/datos.R — Descarga desde Drive y pipeline de datos (funciones puras)

# IDs de Drive (archivos públicos sin OAuth)
DRIVE_IDS <- list(
  det_inv           = "1Q0BYJcVM6zBvbj-5sLXctW9KThS4oZ6u",
  diccionario       = "13J6fKucKgNJ8Kngtq4gGGsnvMenC1A8i",
  geoinv            = "1V1jRSqqgjeX7jJkhSag0b1ufB5PP9X2Z",
  grd_12_25         = "1ZQHQOMFD8MG0r5pgDgcniUbZR_XGDGVq",
  nombres_abreviados = "1_ielTIIhPjN-6jd9SF7w-LZrmSJRos04",
  deptos_shp        = "1T0vOdYJLdigP1C4aykrEgJAINxit1PTA"
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
ruta_cache_app <- function() {
  here::here("midputs", "rds", "_cache_app.rds")
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

#' Serializa la lista nombrada de objetos a disco.
guardar_cache_app <- function(objetos, ruta_cache = ruta_cache_app()) {
  dir.create(dirname(ruta_cache), recursive = TRUE, showWarnings = FALSE)
  saveRDS(objetos, file = ruta_cache, compress = FALSE)
  invisible(ruta_cache)
}

#' Carga la caché y devuelve la lista nombrada de objetos.
cargar_cache_app <- function(ruta_cache = ruta_cache_app()) {
  readRDS(ruta_cache)
}

#' Descarga el shapefile departamental, simplifica y guarda como RDS.
#' El campo de código departamental se detecta automáticamente buscando
#' nombres comunes: CCDD, IDDPTO, COD_DPTO, UBIGEO (2 primeros dígitos).
preparar_deptos_geo <- function(ruta_descarga, ruta_rds) {
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

  dir.create(dirname(ruta_rds), recursive = TRUE, showWarnings = FALSE)
  saveRDS(sf_final, file = ruta_rds)
  invisible(sf_final)
}

#' Construye el universo de códigos GRD comunes a las tres fuentes.
construir_universo_comun <- function(det_inv_dt, geoinv_sf, grd_ts_dt,
                                     programa_objetivo = "GESTION DE RIESGOS Y EMERGENCIAS") {
  codigos_det <- unique(as.character(
    det_inv_dt[
      stringi::stri_trans_general(programa, "Latin-ASCII") == programa_objetivo,
      codigo_unico
    ]
  ))
  codigos_geo <- unique(as.character(geoinv_sf$codigo_unico))
  codigos_ts  <- unique(as.character(grd_ts_dt[["codigo_unico"]]))
  base::Reduce(intersect, list(codigos_det, codigos_geo, codigos_ts))
}
