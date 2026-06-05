library(testthat)
source(here::here("R/exportar.R"))

# Fixture sf mínimo
make_sf <- function() {
  sf::st_sf(
    codigo_unico      = c("001", "002"),
    nombre_inversion  = c("Proy A", "Proy B"),
    costo_actualizado = c(1e6, 2e6),
    geometry = sf::st_sfc(
      sf::st_point(c(-77.0, -12.0)),
      sf::st_point(c(-75.0, -9.5))
    ),
    crs = 4326
  )
}

# --- exportar_csv_geo --------------------------------------------------------
test_that("exportar_csv_geo incluye columnas lon y lat", {
  tmp <- tempfile(fileext = ".csv")
  exportar_csv_geo(make_sf(), tmp)
  df  <- readr::read_csv(tmp, show_col_types = FALSE)
  expect_true("lon" %in% names(df))
  expect_true("lat" %in% names(df))
  expect_false("geometry" %in% names(df))
  expect_equal(nrow(df), 2)
  unlink(tmp)
})

test_that("exportar_csv_geo tiene las filas esperadas", {
  sf_in <- make_sf()
  tmp   <- tempfile(fileext = ".csv")
  exportar_csv_geo(sf_in, tmp)
  df    <- readr::read_csv(tmp, show_col_types = FALSE)
  expect_equal(nrow(df), nrow(sf_in))
  unlink(tmp)
})

# --- exportar_gpkg_geo -------------------------------------------------------
test_that("exportar_gpkg_geo conserva la geometría y las filas", {
  sf_in <- make_sf()
  tmp   <- tempfile(fileext = ".gpkg")
  exportar_gpkg_geo(sf_in, tmp)
  res   <- sf::st_read(tmp, quiet = TRUE)
  expect_s3_class(res, "sf")
  expect_equal(nrow(res), nrow(sf_in))
  expect_true(all(sf::st_is(res, "POINT")))
  unlink(tmp)
})

# --- exportar_csv_temporal ---------------------------------------------------
test_that("exportar_csv_temporal escribe el CSV correctamente", {
  df  <- tibble::tibble(codigo_unico = "001", anio = 2024L, devengado = 1e6)
  tmp <- tempfile(fileext = ".csv")
  exportar_csv_temporal(df, tmp)
  res <- readr::read_csv(tmp, show_col_types = FALSE)
  expect_equal(nrow(res), 1)
  unlink(tmp)
})

# --- renderizar_reporte_pdf (marcado lento) ----------------------------------
test_that("renderizar_reporte_pdf produce un PDF no vacío", {
  testthat::skip_on_cran()
  testthat::skip_if_not(quarto::quarto_available(), "Quarto no disponible")
  testthat::skip_if_not(
    file.exists(here::here("reporte_inversion.qmd")),
    "reporte_inversion.qmd no existe"
  )

  sf_inv <- make_sf()[1, ]
  serie  <- tibble::tibble(
    codigo_unico = "001",
    anio = 2022L:2024L,
    pia = c(1e5, 1e5, 1e5),
    pim = c(1.2e5, 1.2e5, 1.2e5),
    devengado = c(8e4, 9e4, 1e5)
  )
  pdf_path <- renderizar_reporte_pdf(
    codigo    = "001",
    datos_inv = sf_inv,
    datos_serie = serie,
    qmd_path  = here::here("reporte_inversion.qmd")
  )
  expect_true(file.exists(pdf_path))
  expect_gt(file.size(pdf_path), 1000)
  unlink(pdf_path)
})
