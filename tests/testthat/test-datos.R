library(testthat)
library(here)
source(here::here("R/datos.R"))

# --- zero_pad_ubigeo ---------------------------------------------------------
test_that("zero_pad_ubigeo rellena a 6 dígitos", {
  expect_equal(zero_pad_ubigeo("1234"),   "001234")
  expect_equal(zero_pad_ubigeo("150101"), "150101")
  expect_equal(zero_pad_ubigeo("10101"),  "010101")
})

test_that("ubigeos con zero-pad: str_sub(,1,2) no clasifica mal depts 01-09", {
  padded <- zero_pad_ubigeo("10101")  # debería ser "010101"
  expect_equal(stringr::str_sub(padded, 1, 2), "01")
})

# --- join_nombres_abreviados -------------------------------------------------
test_that("join_nombres_abreviados usa nom_inv_corto cuando existe", {
  tmp_csv <- tempfile(fileext = ".csv")
  writeLines("codigo_unico,nom_inv_corto\n001,Nombre Corto A\n002,", tmp_csv)

  df <- tibble::tibble(
    codigo_unico     = c("001", "002", "003"),
    nombre_inversion = c("Nombre Largo A", "Nombre Largo B", "Nombre Largo C")
  )

  res <- join_nombres_abreviados(df, tmp_csv)
  expect_equal(res$nombre_abreviado[res$codigo_unico == "001"], "Nombre Corto A")
  expect_equal(res$nombre_abreviado[res$codigo_unico == "002"], "Nombre Largo B")
  expect_equal(res$nombre_abreviado[res$codigo_unico == "003"], "Nombre Largo C")
  expect_false("nom_inv_corto" %in% names(res))
  unlink(tmp_csv)
})

# --- descargar_si_falta -------------------------------------------------------
test_that("descargar_si_falta no descarga si el archivo ya existe", {
  tmp <- tempfile()
  writeLines("existe", tmp)
  # Si el archivo existe no debe llamar a googledrive (no hay mock, solo verifica
  # que la función retorna la ruta sin error)
  expect_equal(descargar_si_falta("fake_id", tmp), tmp)
  unlink(tmp)
})

# --- cache_app_vigente -------------------------------------------------------

test_that("cache_app_vigente: FALSE cuando la caché no existe", {
  expect_false(cache_app_vigente(tempfile(), character(0)))
})

test_that("cache_app_vigente: TRUE cuando la caché es más nueva que todas las fuentes", {
  fuente <- tempfile()
  writeLines("datos", fuente)
  Sys.sleep(0.05)
  cache <- tempfile()
  saveRDS(list(), cache)
  expect_true(cache_app_vigente(cache, fuente))
  unlink(c(fuente, cache))
})

test_that("cache_app_vigente: FALSE cuando una fuente es más nueva que la caché", {
  cache <- tempfile()
  saveRDS(list(), cache)
  Sys.sleep(0.05)
  fuente <- tempfile()
  writeLines("datos nuevos", fuente)
  expect_false(cache_app_vigente(cache, fuente))
  unlink(c(cache, fuente))
})

test_that("cache_app_vigente: fuente inexistente se ignora → TRUE", {
  cache <- tempfile()
  saveRDS(list(), cache)
  fuente_inexistente <- tempfile()  # nunca creado
  expect_true(cache_app_vigente(cache, fuente_inexistente))
  unlink(cache)
})

# --- guardar_cache_app / cargar_cache_app (round-trip) -----------------------

test_that("round-trip guardar/cargar preserva los objetos", {
  ruta <- tempfile(fileext = ".rds")
  objetos <- list(x = 1:5, y = "hola", z = data.frame(a = 1))
  guardar_cache_app(objetos, ruta)
  expect_true(file.exists(ruta))
  recuperado <- cargar_cache_app(ruta)
  expect_equal(recuperado$x, objetos$x)
  expect_equal(recuperado$y, objetos$y)
  expect_equal(recuperado$z, objetos$z)
  unlink(ruta)
})

# --- construir_universo_comun ------------------------------------------------
test_that("construir_universo_comun retorna la intersección correcta", {
  det <- data.table::data.table(
    codigo_unico = c("A", "B", "C"),
    programa     = c("GESTION DE RIESGOS Y EMERGENCIAS",
                     "GESTION DE RIESGOS Y EMERGENCIAS",
                     "OTRO PROGRAMA")
  )
  geo <- sf::st_sf(
    codigo_unico = c("A", "B", "D"),
    geometry     = sf::st_sfc(
      sf::st_point(c(0, 0)),
      sf::st_point(c(1, 1)),
      sf::st_point(c(2, 2))
    ),
    crs = 4326
  )
  ts <- data.table::data.table(
    codigo_unico = c("A", "B", "E"),
    anio         = 2024L,
    pia = 0, pim = 0, devengado = 0
  )

  resultado <- construir_universo_comun(det, geo, ts)
  expect_setequal(resultado, c("A", "B"))
})
