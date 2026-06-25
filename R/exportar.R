# R/exportar.R — Exportación de CSV y renderizado de PDF vía Quarto/Typst

#' Escribe CSV de la base geoespacial (sin geometría, con lon/lat).
#' Respeta el filtro activo recibido como df_sf.
exportar_csv_geo <- function(df_sf, ruta_destino) {
  coords <- sf::st_coordinates(df_sf)
  df_out <- sf::st_drop_geometry(df_sf)
  df_out$lon <- coords[, "X"]
  df_out$lat <- coords[, "Y"]
  options(scipen = 999)
  readr::write_csv(df_out, ruta_destino)
  invisible(ruta_destino)
}

#' Escribe la base geoespacial como GeoPackage (conserva la geometría POINT).
#' Respeta el filtro activo recibido como df_sf.
exportar_gpkg_geo <- function(df_sf, ruta_destino) {
  sf::st_write(
    df_sf, ruta_destino,
    driver     = "GPKG",
    delete_dsn = TRUE,
    quiet      = TRUE
  )
  invisible(ruta_destino)
}

#' Escribe CSV de la base temporal filtrada.
exportar_csv_temporal <- function(df, ruta_destino) {
  options(scipen = 999)
  readr::write_csv(df, ruta_destino)
  invisible(ruta_destino)
}

#' Renderiza el reporte PDF de una inversión específica via Quarto + Typst.
#' Devuelve la ruta del PDF generado.
renderizar_reporte_pdf <- function(codigo, datos_inv, datos_serie,
                                   qmd_path = here::here("notebooks", "reporte_inversion.qmd")) {
  tmp_dir   <- tempdir()
  rds_path  <- file.path(tmp_dir, glue::glue("inv_{codigo}.rds"))
  pdf_path  <- file.path(tmp_dir, glue::glue("reporte_{codigo}.pdf"))

  saveRDS(list(inv = datos_inv, serie = datos_serie), rds_path)

  quarto::quarto_render(
    input         = qmd_path,
    output_format = "typst",
    output_file   = glue::glue("reporte_{codigo}.pdf"),
    execute_params = list(
      codigo    = as.character(codigo),
      datos_rds = rds_path
    ),
    quiet = TRUE
  )

  # quarto_render produce el PDF junto al .qmd; moverlo a tmp
  pdf_junto_qmd <- file.path(dirname(qmd_path),
                              glue::glue("reporte_{codigo}.pdf"))
  if (file.exists(pdf_junto_qmd) && pdf_junto_qmd != pdf_path) {
    file.copy(pdf_junto_qmd, pdf_path, overwrite = TRUE)
    file.remove(pdf_junto_qmd)
  }

  invisible(pdf_path)
}
