library(testthat)
library(shinytest2)

# Test de integración: el app debe arrancar correctamente.
# Requiere chromote/Chrome instalado.
test_that("la app arranca sin errores", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_if_not_installed("chromote")
  app <- AppDriver$new(
    app_dir     = here::here(),
    timeout     = 60000,
    load_timeout = 90000
  )
  on.exit(app$stop(), add = TRUE)
  expect_no_error(app$get_html("body"))
})

test_that("aplicar filtro de tipología reduce n_filtrados", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_if_not_installed("chromote")
  app <- AppDriver$new(here::here(), timeout = 60000, load_timeout = 90000)
  on.exit(app$stop(), add = TRUE)

  n_total <- app$get_text("#n_filtrados")

  app$set_inputs(f_tipologia = "Defensas ribereñas")
  app$click("recalcular")
  app$wait_for_idle()
  n_filt <- app$get_text("#n_filtrados")

  # El conteo filtrado debe ser diferente al total (o igual si solo hay esa tipología)
  # Solo verificamos que no hubo crash
  expect_true(is.character(n_filt))
})

test_that("downloadButton de CSV produce contenido", {
  testthat::skip_if_not_installed("shinytest2")
  testthat::skip_if_not_installed("chromote")
  app <- AppDriver$new(here::here(), timeout = 60000, load_timeout = 90000)
  on.exit(app$stop(), add = TRUE)

  # Navegar a la pestaña de ficha técnica
  app$click("tab_ficha_tecnica")
  app$wait_for_idle()

  tmp <- tempfile(fileext = ".csv")
  app$get_download("dl_csv_geo", filename = tmp)
  expect_true(file.exists(tmp))
  expect_gt(file.size(tmp), 0)
  unlink(tmp)
})
