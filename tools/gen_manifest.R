# tools/gen_manifest.R — Regenera manifest.json para el despliegue en Posit Connect.
#
# Produce un manifiesto "lean": solo los archivos que la app Shiny necesita en
# runtime, más renv.lock (la sección `packages` se recalcula desde el entorno
# renv activo). NO incluye los datos (`data/`, que se descargan de Drive en el
# primer arranque con descargar_si_falta()), ni cuadernos de desarrollo, tests o
# documentación local. Excluye también los resource forks `._*` de macOS.
#
# Uso:  Rscript tools/gen_manifest.R
# Re-ejecutar tras añadir/quitar archivos de runtime (R/, www/, app.R, global.R)
# o tras actualizar paquetes (renv.lock).

# Archivos que viajan en el bundle (rutas relativas a la raíz del proyecto):
archivos_app <- c(
  # Entrypoints de la app
  "app.R",
  "global.R",
  # Pipeline y utilidades compartidas (única fuente de verdad)
  "R/helpers.R",
  "R/theme_jut.R",
  "R/datos.R",
  "R/exportar.R",
  # Assets de estilo servidos por la app
  "www/estilos-jut.css",
  "www/icono-inv-grd7.svg",
  "www/icono-jut.svg",
  "www/fonts/NunitoSans-Variable.ttf",
  "www/fonts/OFL-NunitoSans.txt",
  # Plantilla del reporte PDF (la usa renderizar_reporte_pdf() en runtime)
  "notebooks/reporte_inversion.qmd",
  # Entorno reproducible
  "renv.lock"
)

faltantes <- archivos_app[!file.exists(archivos_app)]
if (length(faltantes) > 0L) {
  stop("Faltan archivos esperados para el manifiesto:\n  ",
       paste(faltantes, collapse = "\n  "))
}

rsconnect::writeManifest(
  appDir   = ".",
  appFiles = archivos_app,
  quiet    = FALSE
)

message("manifest.json regenerado con ", length(archivos_app), " archivos de app.")
