# deploy_connect.R — Despliegue controlado a Posit Connect del enfoque Parquet.
#
# Usa un allow-list EXPLÍCITO de archivos (appFiles) porque el .rscignore de
# rsconnect solo excluye entradas de nivel superior (no rutas anidadas como
# data/raw ni globs *.rds). Así el bundle lleva SOLO código + assets + los
# .parquet/.csv preparados, nunca los crudos de data/raw ni los .rds heredados.
#
# Uso:
#   1) Asegúrate de tener tu cuenta de Connect registrada:
#        rsconnect::accounts()   # debe listar tu servidor; si no:
#        # rsconnect::connectApiUser(server="<tu-connect>", apiKey="<API_KEY>")
#   2) Corre este script:  Rscript deploy_connect.R
#      Despliega como contenido SEPARADO (appName distinto) para NO tocar la
#      app productiva basada en .rds.

# --- Allow-list de archivos a empaquetar --------------------------------------
construir_app_files <- function() {
  todos <- list.files(".", recursive = TRUE, all.files = TRUE, no.. = TRUE)
  keep <- todos[
    grepl("^(app\\.R|global\\.R|\\.Rprofile|renv\\.lock|manifest\\.json|EDA-Inv-GRD\\.Rproj)$", todos) |
    grepl("^R/.*\\.R$", todos) |
    grepl("^www/", todos) |
    grepl("^renv/(activate\\.R|settings\\.json)$", todos) |
    grepl("^data/processed/.*\\.(parquet|csv)$", todos)
  ]
  # Excluir la caché de arranque: Connect la reconstruye desde el Parquet del
  # bundle en el primer arranque (así nunca queda desfasada).
  keep[!grepl("_cache_app", keep)]
}

app_files <- construir_app_files()
cat("Archivos a empaquetar:", length(app_files), "\n")
cat("Datos incluidos:\n"); cat(paste0("  ", grep("^data/", app_files, value = TRUE)), sep = "\n"); cat("\n")
stopifnot(!any(grepl("^data/raw", app_files)),
          !any(grepl("\\.rds$", app_files)),
          !any(grepl("_cache_app", app_files)),
          any(grepl("det_inv\\.parquet$", app_files)))

# --- Despliegue ----------------------------------------------------------------
rsconnect::deployApp(
  appDir   = ".",
  appFiles = app_files,
  appName  = "inversiones-grd-parquet-test",   # contenido SEPARADO de producción
  appTitle = "Inversiones GRD — Perú (test Parquet)",
  forceUpdate = TRUE,
  launch.browser = TRUE
)
