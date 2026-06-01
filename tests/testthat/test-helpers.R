library(testthat)
source(here::here("R/helpers.R"))

# Fixture mínimo en memoria
fixture <- tibble::tibble(
  codigo_unico       = c("001", "002", "003"),
  des_tipologia      = c("Defensas", "Defensas", "Sirenas"),
  costo_actualizado  = c(1e6, 2e6, 5e5),
  monto_viable       = c(9e5, 1.8e6, 4.5e5),
  avance_ejecucion   = c(50, 75, 10),
  avance_fisico      = c(45, 70, 8),
  ubigeo             = c("150101", "060201", "010101"),
  entidad            = c("GOB REG LIMA", "GOB REG CAJAMARCA", "GOB REG AMAZONAS"),
  nombre_inversion   = c("Proy A", "Proy B", "Proy C"),
  nombre_abreviado   = c("A", "B", "C")
)

depto_lkp <- tibble::tribble(
  ~cod_depto, ~departamento,
  "15", "Lima",
  "06", "Cajamarca",
  "01", "Amazonas"
)

# --- label_var ---------------------------------------------------------------
test_that("label_var devuelve etiqueta conocida", {
  expect_equal(label_var("codigo_unico"), "Cód. Único")
})

test_that("label_var hace passthrough para variable desconocida", {
  expect_equal(label_var("variable_inexistente"), "variable_inexistente")
})

# --- fmt_soles ---------------------------------------------------------------
test_that("fmt_soles formatea números positivos con prefijo", {
  resultado <- fmt_soles(1000000)
  expect_true(grepl("S/", resultado))
  expect_true(grepl("1,000,000", resultado))
})

test_that("fmt_soles devuelve guion para NA", {
  expect_equal(fmt_soles(NA_real_), "—")
})

# --- radios_log --------------------------------------------------------------
test_that("radios_log produce radios en rango [3, 9]", {
  r <- radios_log(c(1e4, 1e6, 1e8))
  expect_true(all(r >= 3 - 1e-9 & r <= 9 + 1e-9))
})

test_that("radios_log devuelve valor medio cuando todos iguales", {
  r <- radios_log(rep(1e6, 5))
  expect_true(all(abs(r - 6) < 1e-9))
})

# --- tabla_promedios_tipologia -----------------------------------------------
test_that("tabla_promedios_tipologia agrega correctamente", {
  res <- tabla_promedios_tipologia(fixture)
  expect_equal(nrow(res), 2)  # 2 tipologías
  defensas <- res[res$des_tipologia == "Defensas", ]
  expect_equal(defensas$n_proyectos, 2)
  expect_equal(defensas$costo_actualizado_prom, 1.5e6)
})

# --- tabla_cortes_departamento -----------------------------------------------
test_that("tabla_cortes_departamento produce una fila por departamento", {
  res <- tabla_cortes_departamento(fixture, depto_lkp)
  expect_equal(nrow(res), 3)
  lima <- res[res$departamento == "Lima", ]
  expect_equal(lima$n_inversiones, 1)
  expect_equal(lima$costo_total, 1e6)
})

# --- top_por -----------------------------------------------------------------
test_that("top_por devuelve las n filas más altas", {
  res <- top_por(fixture, "costo_actualizado", n = 2)
  expect_equal(nrow(res), 2)
  expect_equal(res$costo_actualizado[1], 2e6)
})
