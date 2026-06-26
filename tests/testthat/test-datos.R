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

# --- ruta_fuente / leer_fuente / escribir_fuente -----------------------------

test_that("ruta_fuente prioriza .parquet sobre .rds", {
  dir <- tempfile(); dir.create(dir)
  base <- file.path(dir, "x")
  saveRDS(data.frame(a = 1), paste0(base, ".rds"))
  expect_equal(ruta_fuente(base), paste0(base, ".rds"))       # solo rds
  arrow::write_parquet(data.frame(a = 1), paste0(base, ".parquet"))
  expect_equal(ruta_fuente(base), paste0(base, ".parquet"))   # ahora parquet
  expect_equal(ruta_fuente(paste0(base, ".rds")), paste0(base, ".parquet")) # ignora ext
  unlink(dir, recursive = TRUE)
})

test_that("escribir_fuente + leer_fuente: round-trip no espacial", {
  dir <- tempfile(); dir.create(dir)
  base <- file.path(dir, "tab")
  df <- data.frame(codigo = c("001","002"), monto = c(10, 20), stringsAsFactors = FALSE)
  ruta <- escribir_fuente(df, base)
  expect_true(file.exists(ruta))
  expect_match(ruta, "\\.parquet$")
  leido <- as.data.frame(leer_fuente(base))
  expect_equal(leido, df)
  # col_select limita columnas
  sel <- as.data.frame(leer_fuente(base, col_select = "monto"))
  expect_equal(names(sel), "monto")
  unlink(dir, recursive = TRUE)
})

test_that("leer_fuente cae a .rds cuando no hay parquet", {
  dir <- tempfile(); dir.create(dir)
  base <- file.path(dir, "y")
  saveRDS(data.frame(a = 1:3), paste0(base, ".rds"))
  expect_equal(leer_fuente(base)$a, 1:3)
  unlink(dir, recursive = TRUE)
})

test_that("escribir_fuente + leer_fuente: round-trip espacial (GeoParquet)", {
  dir <- tempfile(); dir.create(dir)
  base <- file.path(dir, "geo")
  g <- sf::st_sf(
    id = c("A","B"),
    geometry = sf::st_sfc(sf::st_point(c(0,0)), sf::st_point(c(1,1))), crs = 4326
  )
  escribir_fuente(g, base, espacial = TRUE)
  leido <- leer_fuente(base, espacial = TRUE)
  expect_s3_class(leido, "sf")
  expect_equal(nrow(leido), 2)
  expect_setequal(leido$id, c("A","B"))
  unlink(dir, recursive = TRUE)
})

test_that("asegurar_fuente no descarga si ya hay fuente local", {
  dir <- tempfile(); dir.create(dir)
  base <- file.path(dir, "z")
  saveRDS(data.frame(a = 1), paste0(base, ".rds"))
  # Con .rds presente no debe intentar descargar de Drive (id falso, sin red)
  expect_invisible(asegurar_fuente(base, "id_falso"))
  unlink(dir, recursive = TRUE)
})

# --- filtrar_det_grd ---------------------------------------------------------
test_that("filtrar_det_grd: incluye programa GRD, drenaje e ioarr; excluye incendios", {
  det <- data.table::data.table(
    codigo_unico    = c("A", "C", "D", "E", "F", "Z"),
    programa        = c("GESTION DE RIESGOS Y EMERGENCIAS", "OTRO", "OTRO",
                        "GESTIÓN DE RIESGOS Y EMERGENCIAS", "OTRO", "OTRO"),
    des_tipologia   = c("X", "SERVICIO DE DRENAJE PLUVIAL", "Y", "Z",
                        "SISTEMA DE DRENAJE PLUVIAL", "W"),
    subprograma     = c(NA, NA, NA, "DEFENSA CONTRA INCENDIOS Y EMERGENCIAS MENORES",
                        "DEFENSA CONTRA INCENDIOS Y EMERGENCIAS MENORES", NA),
    ind_ioarr_emerg = c("NO", "NO", "SI", "NO", "SI", "NO")
  )

  res <- filtrar_det_grd(det)
  # A=programa GRD, C=drenaje servicio, D=ioarr SI → incluidos.
  # E=programa GRD pero incendios → excluido (exclusión prevalece pese a la tilde).
  # F=ioarr SI pero incendios → excluido (exclusión prevalece sobre inclusión).
  # Z=ninguna condición → excluido.
  expect_setequal(res$codigo_unico, c("A", "C", "D"))
})

# --- construir_universo_comun ------------------------------------------------
test_that("construir_universo_comun: intersección con selección GRD ampliada y unión 2026", {
  det <- data.table::data.table(
    codigo_unico    = c("A", "B", "C", "D", "E", "F", "G"),
    programa        = c("GESTION DE RIESGOS Y EMERGENCIAS",
                        "GESTION DE RIESGOS Y EMERGENCIAS",
                        "OTRO", "OTRO",
                        "GESTION DE RIESGOS Y EMERGENCIAS",
                        "OTRO",
                        "GESTION DE RIESGOS Y EMERGENCIAS"),
    des_tipologia   = c("X", "X", "SISTEMA DE DRENAJE PLUVIAL", "Y", "X", "X", "X"),
    subprograma     = c(NA, NA, NA, NA,
                        "DEFENSA CONTRA INCENDIOS Y EMERGENCIAS MENORES",
                        "DEFENSA CONTRA INCENDIOS Y EMERGENCIAS MENORES", NA),
    ind_ioarr_emerg = c("NO", "NO", "NO", "SI", "NO", "SI", "NO")
  )
  geo <- sf::st_sf(
    codigo_unico = c("A", "B", "C", "D", "E", "F"),
    geometry     = sf::st_sfc(
      sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), sf::st_point(c(2, 2)),
      sf::st_point(c(3, 3)), sf::st_point(c(4, 4)), sf::st_point(c(5, 5))
    ),
    crs = 4326
  )
  # B y D no tienen historia 2012-2025 (no están en ts): deben sobrevivir vía la
  # unión 2026. G está en ts pero no en geo: se excluye por faltar geo.
  ts <- data.table::data.table(
    codigo_unico = c("A", "C", "E", "F", "G"),
    anio         = 2024L,
    pia = 0, pim = 0, devengado = 0
  )

  resultado <- construir_universo_comun(det, geo, ts)
  # A=GRD, B=GRD (2026-only), C=drenaje, D=ioarr (2026-only) → incluidos.
  # E,F=incendios → excluidos. G=sin geo → excluido.
  expect_setequal(resultado, c("A", "B", "C", "D"))
})

test_that("construir_universo_comun: descarta CUIs vacíos/NA", {
  det <- data.table::data.table(
    codigo_unico    = c("A", "", NA_character_),
    programa        = rep("GESTION DE RIESGOS Y EMERGENCIAS", 3),
    des_tipologia   = c("X", "X", "X"),
    subprograma     = c(NA, NA, NA),
    ind_ioarr_emerg = c("NO", "NO", "NO")
  )
  geo <- sf::st_sf(
    codigo_unico = c("A", "", NA_character_),
    geometry     = sf::st_sfc(
      sf::st_point(c(0, 0)), sf::st_point(c(1, 1)), sf::st_point(c(2, 2))
    ),
    crs = 4326
  )
  ts <- data.table::data.table(
    codigo_unico = c("A", "", NA_character_),
    anio         = 2024L,
    pia = 0, pim = 0, devengado = 0
  )

  resultado <- construir_universo_comun(det, geo, ts)
  expect_setequal(resultado, "A")
  expect_false(any(is.na(resultado) | trimws(resultado) == ""))
})
