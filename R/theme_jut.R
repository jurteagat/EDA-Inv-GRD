# R/theme_jut.R — Sistema de estilo visual "jut" para R (ggplot2 + plotly + DT).
#
# Única fuente de verdad de la marca visual: tipografía, paletas, tema ggplot y
# helpers de escala. Cargado por global.R (app Shiny) y por el chunk setup de
# reporte_inversion.qmd. Reemplaza al antiguo theme_grd()/paleta_grd.
#
# Cárgalo y fija el tema por defecto con:
#   source("R/theme_jut.R"); ggplot2::theme_set(theme_jut())
#
# Paquetes esperados: ggplot2, scales. Opcional: systemfonts (registro de fuente).
# Nota: radios_log() NO vive aquí — la versión canónica del proyecto está en
# R/helpers.R (única fuente de verdad de ese helper).

# --- Tipografía ---------------------------------------------------------------
# Nunito Sans (variable, OFL): fuente nativa del tema Bootswatch "Lux". Para que
# los gráficos del device de R (ragg/quartz) la usen, se registra en systemfonts
# apuntando al .ttf vendorizado en www/fonts/. Si el archivo no existe o
# systemfonts no está, ggplot cae a la sans del sistema sin romper.
fuente_jut <- "Nunito Sans"

ruta_fuente_jut <- here::here("www", "fonts", "NunitoSans-Variable.ttf")

registrar_fuente_jut <- function(ruta = ruta_fuente_jut) {
  if (!requireNamespace("systemfonts", quietly = TRUE)) return(invisible(FALSE))
  if (!file.exists(ruta)) return(invisible(FALSE))
  tryCatch({
    systemfonts::register_font(name = fuente_jut, plain = ruta, bold = ruta)
    invisible(TRUE)
  }, error = function(e) {
    message("No se pudo registrar Nunito Sans: ", conditionMessage(e))
    invisible(FALSE)
  })
}
registrar_fuente_jut()

# --- Paletas ------------------------------------------------------------------
# Paleta CUALITATIVA: 10 pasteles (matices apagados) que recorren la rueda de
# color ALTERNANDO oscuro/claro → máxima diferenciabilidad, sobre todo entre
# categorías contiguas por frecuencia. Es la fuente de verdad para colorear
# variables categóricas. Al ser nominal (no secuencial), asígnala a las
# categorías MÁS FRECUENTES; la cola larga hereda grises (ver `colores_jut`).
paleta_jut_cualitativa <- c(
  "#A8504C",   # terracota          (rojo, oscuro)
  "#E89C8A",   # salmón             (rojo-naranja, claro)
  "#BE8438",   # ocre               (naranja, oscuro)
  "#DCC079",   # arena              (amarillo, claro)
  "#4F8A57",   # verde salvia       (verde, oscuro)
  "#8FC58A",   # menta              (verde, claro)
  "#3D7E94",   # petróleo           (cian-azul, oscuro)
  "#82B4D6",   # celeste            (azul, claro)
  "#6B5A97",   # índigo             (violeta, oscuro)
  "#B597CE"    # lavanda            (violeta, claro)
)

# Acentos puntuales (1 color por gráfico monocromo / marcador). Tomados de los
# tonos OSCUROS de la paleta cualitativa (mejor contraste sobre blanco). Los
# nombres coinciden con la antigua paleta_grd para que el reemplazo sea directo.
paleta_jut <- c(
  azul     = "#3D7E94",   # petróleo (barras/histograma/scatter/boxplots)
  verde    = "#4F8A57",   # verde salvia
  rojo     = "#A8504C",   # terracota de realce (marcador foco)
  azul_osc = "#6B5A97",   # índigo
  naranja  = "#BE8438",   # ocre
  morado   = "#B597CE"    # lavanda
)

# Progresión oscura para "value boxes" / KPIs (azul que se aclara). Texto blanco.
paleta_jut_valuebox <- c("#1A1A1A", "#2C3E50", "#3D5A73", "#517A91")

# Marcador de foco/realce en mapas (magenta de alto contraste sobre CartoDB Positron).
color_jut_foco <- "#FF2AD4"

#' Devuelve un vector de N colores: la paleta cualitativa y, si faltan, una cola
#' de grises (50%–85%). Útil para asignar colores a categorías ordenadas por
#' frecuencia, manteniendo las más frecuentes con color de marca.
colores_jut <- function(n) {
  base <- paleta_jut_cualitativa
  if (n <= length(base)) return(unname(base[seq_len(n)]))
  cola <- grDevices::gray.colors(n = n - length(base), start = 0.5, end = 0.85)
  c(unname(base), cola)
}

# --- Tema ggplot --------------------------------------------------------------
#' Tema ggplot unificado estilo BBC (variante "moderada"): título en negrita,
#' solo gridlines horizontales gris, sin ticks ni líneas de eje, fondo blanco,
#' leyenda arriba. A diferencia del bbc_style() estricto, CONSERVA los títulos de
#' eje y usa tamaños adaptados a cards de dashboard.
theme_jut <- function(base_size = 13, base_family = fuente_jut) {
  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      text               = ggplot2::element_text(family = base_family, color = "#222222"),
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title         = ggplot2::element_text(face = "bold", hjust = 0,
                                                 color = "#222222",
                                                 size = base_size * 1.25,
                                                 margin = ggplot2::margin(b = 6)),
      plot.subtitle      = ggplot2::element_text(color = "#3A3A3A", hjust = 0,
                                                 size = base_size * 1.05,
                                                 margin = ggplot2::margin(b = 9)),
      plot.caption       = ggplot2::element_blank(),
      axis.title         = ggplot2::element_text(color = "#3A3A3A",
                                                 size = base_size * 0.9),
      axis.text          = ggplot2::element_text(color = "#222222",
                                                 size = base_size * 0.85),
      axis.ticks         = ggplot2::element_blank(),
      axis.line          = ggplot2::element_blank(),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor   = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb", linewidth = 0.4),
      panel.background   = ggplot2::element_blank(),
      plot.background    = ggplot2::element_blank(),
      legend.position    = "top",
      legend.title       = ggplot2::element_blank(),
      legend.key         = ggplot2::element_blank(),
      legend.background  = ggplot2::element_blank(),
      legend.text        = ggplot2::element_text(size = base_size * 0.9, color = "#222222"),
      strip.background   = ggplot2::element_rect(fill = "white", color = NA),
      strip.text         = ggplot2::element_text(face = "bold", hjust = 0,
                                                 color = "#222222",
                                                 size = base_size * 0.95)
    )
}

# --- Escalas de color de marca ------------------------------------------------
# Wrappers sobre scale_*_manual con la paleta cualitativa. Para más categorías
# que colores, pásales `n` explícito o usa `colores_jut(n)` como `values`.
escala_color_jut <- function(..., na.value = "#BBBBBB") {
  ggplot2::scale_color_manual(values = paleta_jut_cualitativa, na.value = na.value, ...)
}
escala_fill_jut <- function(..., na.value = "#BBBBBB") {
  ggplot2::scale_fill_manual(values = paleta_jut_cualitativa, na.value = na.value, ...)
}

# Recomendado fijar al cargar:
#   options(scipen = 999)              # nunca notación científica
#   options(shiny.useragg = TRUE)      # (solo Shiny) ragg honra las fuentes
#   ggplot2::theme_set(theme_jut())
