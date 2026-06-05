# app.R — Dashboard Shiny de Inversiones GRD del Perú
#
# Versión interactiva de EDA_Inv_GRD_v1.qmd.
# Los datos se precomputan en global.R; aquí solo se filtran reactivamente.

# Primer uso — calentar la caché desde la consola (solo una vez):
#   Rscript -e 'source("global.R")'   # ~11 s; escribe midputs/rds/_cache_app.rds
#   A partir de ahí el botón "Run App" en VS Code arranca en ~1-2 s. Para forzar rebuild:
#   GRD_REBUILD_CACHE=1 Rscript -e 'source("global.R")'.

source("global.R", local = FALSE)

# ============================================================================
# Helpers de UI
# ============================================================================

card_widget <- function(titulo, ..., padding = NULL) {
  bslib::card(
    full_screen = TRUE,
    bslib::card_header(titulo),
    if (is.null(padding))
      bslib::card_body(...)
    else
      bslib::card_body(..., padding = padding)
  )
}

# ============================================================================
# UI
# ============================================================================

ui <- bslib::page_navbar(
  title = "Inversiones GRD — Perú",
  theme = bslib::bs_theme(
    version    = 5,
    bootswatch = "flatly",
    font_scale = 0.7,
    base_font  = bslib::font_google("Inter", wght = c(400, 600),
                                    local = FALSE),
    heading_font = bslib::font_google("Inter", wght = 700, local = FALSE)
  ),
  fillable = TRUE,
  header   = htmltools::tagList(
    css_leyenda,
    htmltools::tags$style(htmltools::HTML(
      ".bslib-value-box .value-box-value { white-space: nowrap; font-size: 1.3rem !important; }"
    ))
  ),

  sidebar = bslib::sidebar(
    id     = "sidebar_principal",
    width  = 300,
    title  = "Filtros",
    open   = list(desktop = "open", mobile = "closed"),
    shiny::selectizeInput(
      "f_tipologia", "Tipología",
      choices  = opciones_tipologia,
      multiple = TRUE,
      options  = list(placeholder = "Todas las tipologías")
    ),
    shiny::selectizeInput(
      "f_depto", "Departamento",
      choices  = stats::setNames(opciones_depto$value, opciones_depto$label),
      multiple = TRUE,
      options  = list(placeholder = "Todos los departamentos")
    ),
    shiny::selectizeInput(
      "f_situacion", "Situación",
      choices  = opciones_situacion,
      multiple = TRUE,
      options  = list(placeholder = "Todas las situaciones")
    ),
    shiny::selectInput(
      "f_ioarr", "IOARR/Emergencia",
      choices  = opciones_ioarr,
      selected = "Todos"
    ),
    shiny::selectizeInput(
      "f_tipo_inv", "Tipo de inversión",
      choices  = opciones_tipo_inversion,
      multiple = TRUE,
      options  = list(placeholder = "Todos los tipos")
    ),
    shiny::actionButton("recalcular", "Recalcular",
                        icon  = shiny::icon("arrows-rotate"),
                        class = "btn-primary btn-sm w-100"),
    shiny::actionButton("reset", "Limpiar filtros",
                        icon  = shiny::icon("rotate-left"),
                        class = "btn-outline-secondary btn-sm w-100"),
    htmltools::hr(),
    shiny::textOutput("n_filtrados"),
    htmltools::tags$small(
      htmltools::tags$em(
        "Filtros activos en Resumen, Mapa, Distribuciones ",
        "y Departamentos al presionar Recalcular. ",
        "Inversión Seleccionada usa el universo completo."
      ),
      style = "color:#777;"
    ),
    htmltools::hr(),
    shiny::selectizeInput(
      "cod_foco", "Buscar inversión",
      choices = NULL, multiple = FALSE,
      width   = "100%",
      options = list(placeholder = "Escribe código o palabras del nombre…",
                     maxOptions  = 50)
    ),
    htmltools::tags$small(
      htmltools::tags$em(
        "Esta búsqueda solo aplica a la pestaña «Inversión Seleccionada»."
      ),
      style = "color:#777;"
    )
  ),

  # ---- Pestaña: Resumen ----------------------------------------------------
  bslib::nav_panel(
    "Resumen",
    bslib::layout_columns(
      fill = FALSE,
      col_widths = bslib::breakpoints(sm = c(6, 6, 6, 6), lg = c(3, 3, 3, 3)),
      bslib::value_box("Inversiones",
                       shiny::textOutput("kpi_n", inline = TRUE),
                       showcase = bsicons::bs_icon("folder"),
                       theme = "primary"),
      bslib::value_box("Costo actual. total (S/)",
                       shiny::textOutput("kpi_costo", inline = TRUE),
                       showcase = bsicons::bs_icon("cash-stack"),
                       theme = "success"),
      bslib::value_box("Monto viable total (S/)",
                       shiny::textOutput("kpi_viable", inline = TRUE),
                       showcase = bsicons::bs_icon("check-circle"),
                       theme = "info"),
      bslib::value_box("Av. financiero medio",
                       shiny::textOutput("kpi_avance", inline = TRUE),
                       showcase = bsicons::bs_icon("speedometer2"),
                       theme = "warning")
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)),
      card_widget("PIA, PIM y Devengado anual",
                  plotly::plotlyOutput("g_serie", height = "540px")),
      bslib::navset_card_tab(
        full_screen = TRUE,
        height      = "540px",
        bslib::nav_panel("Promedios por tipología",
                         DT::DTOutput("tbl_tipologia")),
        bslib::nav_panel("Top 10 UEP por devengado",
                         DT::DTOutput("tbl_top_entidades"))
      )
    ),
    bslib::layout_columns(
      col_widths = 12,
      card_widget("Tabla de inversiones (navegable)",
                  DT::DTOutput("tbl_inversiones"))
    )
  ),

  # ---- Pestaña: Mapa -------------------------------------------------------
  bslib::nav_panel(
    "Mapa",
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)),
      card_widget("Mapa global de inversiones GRD",
                  leaflet::leafletOutput("mapa", height = "560px"),
                  padding = 0),
      card_widget("Top 15 departamentos por costo",
                  plotly::plotlyOutput("g_deptos", height = "560px"))
    )
  ),

  # ---- Pestaña: Distribuciones --------------------------------------------
  bslib::nav_panel(
    "Distribuciones",
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)),
      card_widget("Distribuciones de montos (log₁₀)",
                  shiny::plotOutput("g_montos", height = "380px")),
      card_widget("Boxplots de costo por tipología (log₁₀)",
                  shiny::plotOutput("g_boxplots_tipo", height = "440px"))
    ),
    bslib::layout_columns(
      col_widths = 12,
      card_widget("Outliers de sobrecosto (% Costo vs Monto viable > Q3 + 3·IQR por tipología)",
                  DT::DTOutput("tbl_outliers"))
    )
  ),

  # ---- Pestaña: Departamentos ---------------------------------------------
  bslib::nav_panel(
    "Departamentos",
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)),
      card_widget("Mapa departamental (av. financiero / costo total)",
                  leaflet::leafletOutput("mapa_deptos", height = "480px"),
                  padding = 0),
      card_widget("Cortes por departamento",
                  DT::DTOutput("tbl_deptos"))
    )
  ),

  # ---- Pestaña: Inversión Seleccionada ------------------------------------
  bslib::nav_panel(
    "Inversión Seleccionada",
    htmltools::div(
      style = "margin-bottom:10px;",
      shiny::uiOutput("encabezado_foco"),
      htmltools::tags$small(
        htmltools::tags$em(
          "Para seleccionar una inversión, usa el buscador ",
          "«Buscar inversión» en la barra lateral izquierda."
        ),
        style = "color:#777;"
      )
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)),
      card_widget("Ejecución acumulada — PIA/PIM/Devengado",
                  plotly::plotlyOutput("g_foco_serie", height = "340px")),
      card_widget("Ubicación",
                  leaflet::leafletOutput("mapa_foco",  height = "340px"),
                  padding = 0)
    ),
    bslib::card(
      full_screen = TRUE,
      bslib::card_header(
        htmltools::div(
          style = "display:flex; justify-content:space-between; align-items:center;",
          htmltools::span("Atributos de la inversión"),
          shiny::downloadButton("dl_pdf_reporte", "PDF",
                                icon  = shiny::icon("file-pdf"),
                                class = "btn-outline-danger btn-sm")
        )
      ),
      bslib::card_body(DT::DTOutput("tbl_foco"))
    )
  ),

  # ---- Pestaña: Ficha Técnica ---------------------------------------------
  bslib::nav_panel(
    title = "Ficha Técnica",
    value = "tab_ficha_tecnica",
    bslib::layout_columns(
      col_widths = 12,
      bslib::card(
        max_height = "220px",
        bslib::card_header("Fuentes de datos"),
        bslib::card_body(
          if (!is.null(fechas_fuentes) && "fecha_dato" %in% names(fechas_fuentes)) {
            DT::DTOutput("tbl_fechas_fuentes")
          } else {
            htmltools::p(
              htmltools::em(
                "Fecha de datos no disponible. ",
                "Ejecuta 00_datos_entrada.qmd para generarla."
              )
            )
          }
        )
      )
    ),
    bslib::layout_columns(
      col_widths = 12,
      card_widget(
        "Diccionario de variables",
        DT::DTOutput("tbl_diccionario")
      )
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)),
      bslib::card(
        bslib::card_header("Base geoespacial (filtrada)"),
        bslib::card_body(
          htmltools::div(
            style = "display:flex; align-items:center; gap:12px;",
            htmltools::p(
              "GeoPackage (",
              htmltools::code(".gpkg"),
              ") con todos los atributos de la inversión y la geometría",
              "POINT en WGS84. Respeta los filtros activos.",
              style = "margin:0; flex:1;"
            ),
            shiny::downloadButton("dl_gpkg_geo", "GPKG geoespacial",
                                  icon  = shiny::icon("download"),
                                  class = "btn-primary btn-sm")
          )
        )
      ),
      bslib::card(
        bslib::card_header("Base temporal (filtrada)"),
        bslib::card_body(
          htmltools::div(
            style = "display:flex; align-items:center; gap:12px;",
            htmltools::p(
              "Serie 2012-2025 de PIA/PIM/Devengado por inversión.",
              "Filtrada a los códigos activos según filtros globales.",
              style = "margin:0; flex:1;"
            ),
            shiny::downloadButton("dl_csv_temporal", "CSV temporal",
                                  icon  = shiny::icon("download"),
                                  class = "btn-success btn-sm")
          )
        )
      )
    )
  ),

  bslib::nav_spacer(),
  bslib::nav_item(
    htmltools::div(
      style = "display:flex; align-items:center; gap:8px;",
      htmltools::span("Desarrollado por Juan Urteaga Tirado"),
      htmltools::tags$a(
        href   = "https://www.linkedin.com/in/jnut/",
        target = "_blank",
        title  = "LinkedIn",
        bsicons::bs_icon("linkedin")
      ),
      htmltools::tags$a(
        href   = "https://jurteagat.github.io/",
        target = "_blank",
        title  = "Sitio web",
        bsicons::bs_icon("globe")
      )
    )
  )
)

# ============================================================================
# Server
# ============================================================================

server <- function(input, output, session) {

  # ---- Filtros diferidos: solo se aplican al presionar Recalcular ----------
  filtros <- shiny::reactiveValues(
    tip  = NULL,
    dep  = NULL,
    sit  = NULL,
    ioarr = "Todos",
    tinv = NULL
  )

  shiny::observeEvent(input$recalcular, {
    filtros$tip  <- input$f_tipologia
    filtros$dep  <- input$f_depto
    filtros$sit  <- input$f_situacion
    filtros$ioarr <- input$f_ioarr
    filtros$tinv <- input$f_tipo_inv
  })

  shiny::observeEvent(input$reset, {
    shiny::updateSelectizeInput(session, "f_tipologia", selected = character(0))
    shiny::updateSelectizeInput(session, "f_depto",     selected = character(0))
    shiny::updateSelectizeInput(session, "f_situacion", selected = character(0))
    shiny::updateSelectInput(   session, "f_ioarr",     selected = "Todos")
    shiny::updateSelectizeInput(session, "f_tipo_inv",  selected = character(0))
    filtros$tip  <- NULL
    filtros$dep  <- NULL
    filtros$sit  <- NULL
    filtros$ioarr <- "Todos"
    filtros$tinv <- NULL
  })

  datos_filt <- shiny::reactive({
    df <- df_pu_geoinv_inv_g_dpf
    if (length(filtros$tip))
      df <- dplyr::filter(df, des_tipologia %in% filtros$tip)
    if (length(filtros$dep))
      df <- dplyr::filter(df,
                          stringr::str_sub(ubigeo, 1, 2) %in% filtros$dep)
    if (length(filtros$sit))
      df <- dplyr::filter(df, situacion %in% filtros$sit)
    if (!is.null(filtros$ioarr) && filtros$ioarr != "Todos")
      df <- dplyr::filter(df,
                          as.character(ind_ioarr_emerg) == filtros$ioarr)
    if (length(filtros$tinv))
      df <- dplyr::filter(df, tipo_inversion %in% filtros$tinv)
    df
  })

  datos_filt_plain <- shiny::reactive(sf::st_drop_geometry(datos_filt()))
  codigos_filt     <- shiny::reactive(unique(datos_filt_plain()$codigo_unico))
  serie_filt       <- shiny::reactive(
    dplyr::filter(df_grd_2012_25_dpf, codigo_unico %in% codigos_filt())
  )

  # ---- Sidebar: contador ---------------------------------------------------
  output$n_filtrados <- shiny::renderText({
    n   <- nrow(datos_filt_plain())
    tot <- nrow(df_pu_geoinv_inv_g_dpf)
    glue::glue("{scales::comma(n)} de {scales::comma(tot)} inversiones")
  })

  # ---- KPIs ---------------------------------------------------------------
  output$kpi_n      <- shiny::renderText(scales::comma(nrow(datos_filt_plain())))
  output$kpi_costo  <- shiny::renderText(
    scales::label_number(scale = 1e-6, accuracy = 0.1,
                         suffix = " M", prefix = "S/ ")(
      sum(datos_filt_plain()$costo_actualizado, na.rm = TRUE)
    ))
  output$kpi_viable <- shiny::renderText(
    scales::label_number(scale = 1e-6, accuracy = 0.1,
                         suffix = " M", prefix = "S/ ")(
      sum(datos_filt_plain()$monto_viable, na.rm = TRUE)
    ))
  output$kpi_avance <- shiny::renderText({
    v <- mean(datos_filt_plain()$avance_financiero, na.rm = TRUE)
    if (is.nan(v)) "—" else paste0(round(v, 1), "%")
  })

  # ---- Tabla: promedios por tipología ------------------------------------
  output$tbl_tipologia <- DT::renderDT({
    df <- datos_filt_plain()
    shiny::validate(shiny::need(nrow(df) > 0, "Sin datos."))
    tabla <- tabla_promedios_tipologia(df) |> dplyr::rename_with(label_var)
    DT::datatable(tabla, rownames = FALSE,
                  options = list(pageLength = 10, scrollX = TRUE)) |>
      DT::formatRound(label_var(c("costo_actualizado_prom", "monto_viable_prom")),
                      digits = 0, mark = ",")
  })

  # ---- Tabla: top 10 unidades ejecutoras (UEP) por devengado -------------
  output$tbl_top_entidades <- DT::renderDT({
    s <- serie_filt()
    shiny::validate(shiny::need(nrow(s) > 0, "Sin datos."))
    top <- data.table::as.data.table(s)[
      , .(devengado_acum = sum(devengado, na.rm = TRUE)), by = nombre_uep
    ][order(-devengado_acum)][seq_len(min(10, .N))]
    DT::datatable(dplyr::rename_with(top, label_var),
                  rownames = FALSE,
                  options  = list(pageLength = 10, dom = "t")) |>
      DT::formatRound(label_var("devengado_acum"), digits = 0, mark = ",")
  })

  # ---- Tabla navegable de inversiones ------------------------------------
  output$tbl_inversiones <- DT::renderDT({
    df <- datos_filt_plain()
    shiny::validate(shiny::need(nrow(df) > 0, "Sin datos."))
    df |>
      dplyr::select(codigo_unico, nombre_abreviado, des_tipologia,
                    tipo_inversion, entidad, costo_actualizado,
                    avance_financiero) |>
      dplyr::rename_with(label_var) |>
      DT::datatable(
        rownames = FALSE, filter = "top",
        options  = list(pageLength = 15, scrollX = TRUE,
                        columnDefs = list(
                          list(width = "300px",
                               targets = which(
                                 names(dplyr::rename_with(
                                   dplyr::select(df, codigo_unico, nombre_abreviado,
                                                 des_tipologia, tipo_inversion,
                                                 entidad, costo_actualizado,
                                                 avance_financiero),
                                   label_var
                                 )) == label_var("nombre_abreviado")
                               ) - 1
                          )
                        ))
      ) |>
      DT::formatRound(label_var("costo_actualizado"), digits = 0, mark = ",") |>
      DT::formatRound(label_var("avance_financiero"), digits = 1)
  })

  # ---- Mapa global --------------------------------------------------------
  output$mapa <- leaflet::renderLeaflet({
    shiny::withProgress(message = "Cargando mapa…", value = 0.5, {
      leaflet::leaflet() |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
        leaflet::setView(lng = -75, lat = -9.5, zoom = 5) |>
        leaflet::addLayersControl(
          overlayGroups = tipologias_por_freq,
          options       = leaflet::layersControlOptions(collapsed = TRUE)
        ) |>
        leaflet::addLegend(
          position  = "bottomright",
          pal       = pal_tipologia,
          values    = tipologias_por_freq,
          title     = "Tipología",
          opacity   = 0.8,
          className = "info legend leyenda-tipologia"
        )
    })
  })
  shiny::observe({
    shiny::req(input$mapa_bounds)  # espera a que Leaflet esté montado en el browser
    df    <- datos_filt()
    plain <- sf::st_drop_geometry(df)

    p <- leaflet::leafletProxy("mapa")
    for (tip in tipologias_por_freq) {
      p <- leaflet::clearGroup(p, tip)
    }
    if (nrow(df) == 0) return(invisible())

    radios <- radios_log(plain$costo_actualizado)

    for (tip in tipologias_por_freq) {
      idx <- which(plain$des_tipologia == tip)
      if (length(idx) == 0) next
      df_tip  <- df[idx, ]
      rad_tip <- radios[idx]
      p <- leaflet::addCircleMarkers(
        p,
        data        = df_tip,
        group       = tip,
        radius      = rad_tip,
        color       = pal_tipologia(tip),
        fillOpacity = 0.45,
        stroke      = TRUE,
        weight      = 0.4,
        opacity     = 0.6,
        popup       = ~glue::glue(
          "<b>{nombre_abreviado}</b><br>",
          "<i>{des_tipologia}</i><br>",
          "Entidad: {entidad}<br>",
          "Costo actual.: S/ {scales::label_comma()(costo_actualizado)}<br>",
          "Av. financiero: {round(avance_financiero, 1)}%"
        ),
        clusterOptions = leaflet::markerClusterOptions(
          maxClusterRadius           = 25,
          spiderfyOnMaxZoom          = TRUE,
          spiderfyDistanceMultiplier = 1.8,
          showCoverageOnHover        = FALSE,
          zoomToBoundsOnClick        = TRUE
        )
      )
    }
  })

  # ---- Barplot: top 15 departamentos -------------------------------------
  output$g_deptos <- plotly::renderPlotly({
    plain <- datos_filt_plain()
    shiny::validate(shiny::need(nrow(plain) > 0, "Sin datos."))
    if (length(filtros$dep) == 1L)
      shiny::validate("Filtrado a 1 departamento — la vista comparativa no aplica.")

    tabla <- plain |>
      dplyr::mutate(
        cod_depto = stringr::str_sub(ubigeo, 1, 2),
        cod_depto = dplyr::if_else(
          !is.na(cod_depto) & nchar(cod_depto) == 2, cod_depto, "98"
        )
      ) |>
      dplyr::left_join(depto_lookup, by = "cod_depto") |>
      dplyr::mutate(departamento = dplyr::coalesce(
        departamento, glue::glue("Cod {cod_depto}")
      )) |>
      dplyr::group_by(departamento) |>
      dplyr::summarise(costo_total = sum(costo_actualizado, na.rm = TRUE),
                       .groups = "drop") |>
      dplyr::arrange(dplyr::desc(costo_total)) |>
      dplyr::slice_head(n = 15) |>
      dplyr::mutate(departamento = forcats::fct_reorder(departamento, costo_total))

    g <- ggplot2::ggplot(
      tabla,
      ggplot2::aes(x = costo_total, y = departamento,
                   text = scales::label_comma()(costo_total))
    ) +
      ggplot2::geom_col(fill = paleta_grd["azul_osc"]) +
      ggplot2::scale_x_continuous(labels = scales::label_comma()) +
      ggplot2::labs(x = "Costo actualizado total (S/)", y = NULL) +
      theme_grd()

    plotly::ggplotly(g, tooltip = c("y", "text"))
  })

  # ---- Distribuciones de montos -----------------------------------------
  output$g_montos <- shiny::renderPlot({
    plain <- datos_filt_plain()
    shiny::validate(shiny::need(nrow(plain) > 0, "Sin datos."))

    df_largo <- plain |>
      dplyr::select(monto_viable, costo_actualizado,
                    pia_anio_actual, pim_anio_actual) |>
      tidyr::pivot_longer(dplyr::everything(),
                          names_to = "variable", values_to = "valor") |>
      dplyr::filter(is.finite(valor) & valor > 0) |>
      dplyr::mutate(variable = dplyr::recode(
        variable,
        monto_viable      = "Monto viable",
        costo_actualizado = "Costo actual.",
        pia_anio_actual   = "PIA actual",
        pim_anio_actual   = "PIM actual"
      ))

    shiny::validate(shiny::need(nrow(df_largo) > 0,
                                "Sin valores > 0 para log₁₀."))

    ggplot2::ggplot(df_largo, ggplot2::aes(x = valor)) +
      ggplot2::geom_histogram(bins = 40, fill = paleta_grd["azul"],
                              color = "white", alpha = 0.8) +
      ggplot2::scale_x_log10(labels = scales::label_comma()) +
      ggplot2::facet_wrap(~variable, scales = "free_y", ncol = 2) +
      ggplot2::labs(x = "Soles (escala log₁₀)", y = "Frecuencia") +
      theme_grd()
  })

  # ---- Boxplots por tipología (top 10) -----------------------------------
  output$g_boxplots_tipo <- shiny::renderPlot({
    plain <- datos_filt_plain()
    shiny::validate(shiny::need(nrow(plain) > 0, "Sin datos."))

    top_tipos <- plain |>
      dplyr::count(des_tipologia, sort = TRUE) |>
      dplyr::slice_head(n = 10) |>
      dplyr::pull(des_tipologia)

    df_box <- plain |>
      dplyr::filter(des_tipologia %in% top_tipos,
                    is.finite(costo_actualizado), costo_actualizado > 0) |>
      dplyr::mutate(
        des_tipologia = stringr::str_wrap(des_tipologia, width = 25),
        des_tipologia = forcats::fct_reorder(des_tipologia, costo_actualizado,
                                             .fun = median, na.rm = TRUE)
      )

    shiny::validate(shiny::need(nrow(df_box) > 0, "Sin datos de costo."))

    ggplot2::ggplot(df_box,
                    ggplot2::aes(x = costo_actualizado, y = des_tipologia)) +
      ggplot2::geom_boxplot(fill = paleta_grd["azul"], alpha = 0.6,
                            outlier.size = 0.6, outlier.alpha = 0.3) +
      ggplot2::scale_x_log10(labels = scales::label_comma()) +
      ggplot2::labs(x = "Costo actualizado (log₁₀, S/)", y = NULL,
                    subtitle = "Top 10 tipologías por frecuencia") +
      theme_grd() +
      ggplot2::theme(axis.text.y = ggplot2::element_text(size = 8,
                                                         lineheight = 0.85))
  })

  # ---- Outliers de costo -------------------------------------------------
  output$tbl_outliers <- DT::renderDT({
    plain <- datos_filt_plain()
    shiny::validate(shiny::need(nrow(plain) > 0, "Sin datos."))

    df_out <- plain |>
      dplyr::mutate(pct_costo_vs_viable = dplyr::if_else(
        is.finite(monto_viable) & monto_viable > 0,
        (costo_actualizado - monto_viable) / monto_viable * 100,
        NA_real_
      )) |>
      dplyr::left_join(limites_iqr, by = "des_tipologia") |>
      dplyr::filter(!is.na(limite_outlier), !is.na(pct_costo_vs_viable),
                    pct_costo_vs_viable > limite_outlier) |>
      dplyr::select(codigo_unico, nombre_abreviado, entidad,
                    des_tipologia, costo_actualizado, pct_costo_vs_viable) |>
      dplyr::arrange(dplyr::desc(pct_costo_vs_viable))

    shiny::validate(shiny::need(nrow(df_out) > 0,
                                "Ninguna inversión supera Q3 + 3·IQR."))

    DT::datatable(
      dplyr::rename_with(df_out, label_var),
      rownames = FALSE, filter = "top",
      options  = list(pageLength = 10, scrollX = TRUE)
    ) |>
      DT::formatRound(label_var("costo_actualizado"),   digits = 0, mark = ",") |>
      DT::formatRound(label_var("pct_costo_vs_viable"), digits = 1)
  })

  # ---- Evolución temporal ------------------------------------------------
  serie_portafolio_filt <- shiny::reactive({
    s <- serie_filt()
    shiny::req(nrow(s) > 0)
    serie_portafolio(s)
  })

  output$g_serie <- plotly::renderPlotly({
    sp <- serie_portafolio_filt()
    shiny::validate(shiny::need(nrow(sp) > 0, "Sin datos."))

    s_largo <- sp |>
      tidyr::pivot_longer(c(pia, pim, devengado),
                          names_to = "metrica", values_to = "valor") |>
      dplyr::mutate(metrica = dplyr::recode(
        metrica, pia = "PIA", pim = "PIM", devengado = "Devengado"
      ))

    g <- ggplot2::ggplot(
      s_largo,
      ggplot2::aes(x = anio, y = valor, color = metrica, group = metrica,
                   text = paste0(metrica, ": S/ ",
                                 scales::label_number(scale = 1e-6,
                                                      accuracy = 0.1,
                                                      suffix = " M")(valor)))
    ) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::scale_color_manual(
        values = c(PIA = paleta_grd[["verde"]], PIM = paleta_grd[["azul"]],
                   Devengado = paleta_grd[["rojo"]])
      ) +
      ggplot2::scale_y_continuous(
        labels = scales::label_number(scale = 1e-6, accuracy = 0.1, suffix = " M")
      ) +
      ggplot2::scale_x_continuous(breaks = scales::breaks_width(1)) +
      ggplot2::labs(x = NULL, y = "Soles (millones)", color = NULL) +
      theme_grd() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1,
                                                         size = 8))

    plotly::ggplotly(g, tooltip = c("x", "text"))
  })

  # ---- Mapa departamental (% ejecución + costo) --------------------------
  output$mapa_deptos <- leaflet::renderLeaflet({
    plain <- datos_filt_plain()
    shiny::validate(shiny::need(nrow(plain) > 0, "Sin datos."))

    tabla_d <- tabla_cortes_departamento(plain, depto_lookup)

    sf_deptos <- deptos_geo |>
      dplyr::left_join(tabla_d, by = "cod_depto")

    pal_ejec <- leaflet::colorNumeric(
      palette  = "RdYlGn",
      domain   = c(0, 100),
      na.color = "#CCCCCC"
    )

    centroides <- suppressWarnings(sf::st_point_on_surface(sf_deptos))
    coords_c   <- sf::st_coordinates(centroides)
    sf_c       <- sf_deptos |>
      sf::st_drop_geometry() |>
      dplyr::mutate(lon = coords_c[, "X"], lat = coords_c[, "Y"]) |>
      dplyr::filter(!is.na(costo_total) & costo_total > 0)

    radios_d <- radios_log(sf_c$costo_total, min_r = 4, max_r = 18)

    leaflet::leaflet(sf_deptos) |>
      leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
      leaflet::addPolygons(
        fillColor   = ~pal_ejec(pct_ejecucion_prom),
        fillOpacity = 0.7,
        color       = "white",
        weight      = 0.6,
        opacity     = 0.8,
        popup       = ~dplyr::if_else(
          !is.na(departamento),
          glue::glue(
            "<b>{departamento}</b><br>",
            "N° inversiones: {scales::comma(n_inversiones)}<br>",
            "Costo total: S/ {scales::label_comma()(costo_total)}<br>",
            "Av. financiero prom.: {round(pct_ejecucion_prom, 1)}%"
          ),
          "Sin inversiones en este departamento"
        )
      ) |>
      leaflet::addCircleMarkers(
        data        = sf_c,
        lng         = ~lon,
        lat         = ~lat,
        radius      = radios_d,
        color       = paleta_grd["azul_osc"],
        fillOpacity = 0.5,
        stroke      = TRUE,
        weight      = 0.5,
        label       = ~glue::glue("{departamento}: S/ {scales::label_comma()(costo_total)}")
      ) |>
      leaflet::addLegend(
        position  = "bottomright",
        pal       = pal_ejec,
        values    = c(0, 100),
        title     = "Av. financiero prom.",
        opacity   = 0.8,
        className = "info legend leyenda-tipologia"
      )
  })

  # ---- Tabla: cortes por departamento ------------------------------------
  output$tbl_deptos <- DT::renderDT({
    df <- datos_filt_plain()
    shiny::validate(shiny::need(nrow(df) > 0, "Sin datos."))
    tabla <- tabla_cortes_departamento(df, depto_lookup) |>
      dplyr::select(-cod_depto) |>
      dplyr::rename_with(label_var)
    DT::datatable(tabla, rownames = FALSE, filter = "top",
                  options = list(pageLength = 15, scrollX = TRUE)) |>
      DT::formatRound(label_var("costo_total"),        digits = 0, mark = ",") |>
      DT::formatRound(label_var("pct_ejecucion_prom"), digits = 1)
  })

  # ---- Inversión específica: buscador server-side -----------------------
  shiny::updateSelectizeInput(
    session, "cod_foco",
    choices  = stats::setNames(opciones_codigo_inv$value,
                               opciones_codigo_inv$label),
    server   = TRUE,
    selected = "2508148"
  )

  fila_foco <- shiny::reactive({
    shiny::req(input$cod_foco)
    dplyr::filter(df_pu_geoinv_inv_g_dpf,
                  codigo_unico == as.character(input$cod_foco))
  })

  output$encabezado_foco <- shiny::renderUI({
    f <- fila_foco()
    shiny::validate(shiny::need(nrow(f) > 0, "Selecciona una inversión."))
    htmltools::tagList(
      htmltools::tags$h4(
        glue::glue("{f$nombre_abreviado[1]} (CUI {f$codigo_unico[1]})"),
        style = "font-weight:700; margin-bottom:2px;"
      ),
      htmltools::tags$p(
        f$nombre_inversion[1],
        style = "color:#555; margin:0;"
      )
    )
  })

  output$tbl_foco <- DT::renderDT({
    f <- fila_foco()
    shiny::validate(shiny::need(nrow(f) > 0, "Selecciona una inversión."))

    tabla <- f |>
      sf::st_drop_geometry() |>
      # avance_financiero se formatea como "%" antes del across numérico para
      # que no se muestre como número con comas.
      dplyr::mutate(
        avance_financiero = dplyr::if_else(
          is.na(avance_financiero), NA_character_,
          paste0(round(avance_financiero, 1), "%")
        )
      ) |>
      dplyr::mutate(dplyr::across(where(is.numeric),
                                  ~scales::comma(., accuracy = 1))) |>
      tidyr::pivot_longer(
        dplyr::everything(),
        names_to = "variable", values_to = "valor",
        values_transform = list(valor = as.character)
      ) |>
      dplyr::mutate(variable = label_var(variable))

    DT::datatable(
      tabla, rownames = FALSE,
      colnames = c("Variable", "Valor"),
      options  = list(pageLength = 25, dom = "tip", scrollX = TRUE)
    )
  })

  output$g_foco_serie <- plotly::renderPlotly({
    cod  <- shiny::req(input$cod_foco)
    fila <- fila_foco()
    shiny::validate(shiny::need(nrow(fila) > 0, "Selecciona una inversión."))

    serie <- dplyr::filter(df_grd_2012_25_dpf,
                           codigo_unico == as.character(cod)) |>
      dplyr::group_by(anio) |>
      dplyr::summarise(pia       = sum(pia,       na.rm = TRUE),
                       pim       = sum(pim,       na.rm = TRUE),
                       devengado = sum(devengado, na.rm = TRUE),
                       .groups   = "drop") |>
      dplyr::arrange(anio)

    shiny::validate(shiny::need(nrow(serie) > 0,
                                "Sin serie temporal para esta inversión."))

    # Agregar fila 2026 desde el detalle del año actual (devengado real, no
    # estimado): PIA/PIM/Devengado del año en curso.
    fila_2026 <- tibble::tibble(
      anio      = 2026L,
      pia       = dplyr::coalesce(fila$pia_anio_actual[1], 0),
      pim       = dplyr::coalesce(fila$pim_anio_actual[1], 0),
      devengado = dplyr::coalesce(fila$dev_anio_actual[1], 0)
    )
    serie <- dplyr::bind_rows(serie, fila_2026) |> dplyr::arrange(anio)

    costo_foco <- fila$costo_actualizado[1]
    shiny::validate(shiny::need(
      !is.na(costo_foco) && costo_foco > 0,
      "Sin costo actualizado disponible para calcular el porcentaje."
    ))

    s_largo <- serie |>
      dplyr::mutate(dplyr::across(c(pia, pim, devengado), cumsum)) |>
      tidyr::pivot_longer(c(pia, pim, devengado),
                          names_to = "metrica", values_to = "valor_soles") |>
      dplyr::mutate(
        metrica   = dplyr::recode(metrica,
                                  pia = "PIA", pim = "PIM", devengado = "Devengado"),
        valor_pct = valor_soles / costo_foco * 100,
        # El hover muestra el monto acumulado en soles junto al % del costo.
        text = paste0(
          "Año: ", anio, "<br>", metrica,
          "<br>Acumulado: S/ ", scales::label_comma()(valor_soles),
          "<br>% del costo: ", round(valor_pct, 1), "%"
        )
      )

    g <- ggplot2::ggplot(
      s_largo,
      ggplot2::aes(x = anio, y = valor_pct, color = metrica, group = metrica,
                   text = text)
    ) +
      ggplot2::geom_hline(yintercept = 100, linetype = "dashed",
                          color = "grey50", linewidth = 0.6) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::scale_color_manual(
        values = c(PIA = paleta_grd[["verde"]], PIM = paleta_grd[["azul"]],
                   Devengado = paleta_grd[["rojo"]])
      ) +
      ggplot2::scale_y_continuous(labels = ~paste0(round(., 1), "%")) +
      ggplot2::scale_x_continuous(breaks = scales::breaks_width(1)) +
      ggplot2::labs(x = NULL, y = "% del costo actualizado", color = NULL) +
      theme_grd() +
      ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1,
                                                          size = 8))

    plotly::ggplotly(g, tooltip = "text")
  })

  output$mapa_foco <- leaflet::renderLeaflet({
    f <- fila_foco()
    shiny::validate(shiny::need(nrow(f) > 0, "Selecciona una inversión."))

    cod_d <- stringr::str_sub(f$ubigeo[1], 1, 2)
    depto_sf <- dplyr::filter(deptos_geo, cod_depto == cod_d)

    mapa <- leaflet::leaflet() |>
      leaflet::addTiles()

    if (nrow(depto_sf) > 0) {
      bbox <- sf::st_bbox(depto_sf)
      mapa <- mapa |>
        leaflet::addPolygons(
          data        = depto_sf,
          fillColor   = paleta_grd[["azul"]],
          fillOpacity = 0.15,
          color       = paleta_grd[["azul_osc"]],
          weight      = 1.5
        ) |>
        leaflet::fitBounds(
          lng1 = bbox[["xmin"]], lat1 = bbox[["ymin"]],
          lng2 = bbox[["xmax"]], lat2 = bbox[["ymax"]]
        )
    }

    mapa |> leaflet::addCircleMarkers(
      data       = f,
      radius     = 8,
      color      = "#D6604D",
      fillOpacity = 0.8,
      popup      = ~glue::glue(
        "<b>{nombre_abreviado}</b><br>",
        "Tipología: {des_tipologia}<br>",
        "Monto viable: S/ {scales::label_comma()(monto_viable)}<br>",
        "Costo actualizado: S/ {scales::label_comma()(costo_actualizado)}"
      )
    )
  })

  # ---- Descarga PDF del reporte ------------------------------------------
  output$dl_pdf_reporte <- shiny::downloadHandler(
    filename = function() {
      glue::glue("reporte_inversion_{input$cod_foco}.pdf")
    },
    content  = function(file) {
      shiny::withProgress(
        message = "Generando PDF…",
        detail  = "Preparando datos…",
        value   = 0.2,
        {
          f <- fila_foco()
          shiny::validate(shiny::need(nrow(f) > 0, "Sin datos de inversión."))

          serie_inv <- dplyr::filter(df_grd_2012_25_dpf,
                                     codigo_unico == as.character(input$cod_foco))

          shiny::incProgress(0.4, detail = "Renderizando Quarto/Typst…")

          pdf_path <- renderizar_reporte_pdf(
            codigo      = input$cod_foco,
            datos_inv   = f,
            datos_serie = serie_inv
          )

          shiny::incProgress(0.4, detail = "Listo.")
          file.copy(pdf_path, file)
        }
      )
    }
  )

  # ---- Descargas ----------------------------------------------------------
  output$dl_gpkg_geo <- shiny::downloadHandler(
    filename = function() {
      glue::glue("inversiones_grd_geo_{format(Sys.Date(), '%Y%m%d')}.gpkg")
    },
    content = function(file) {
      shiny::withProgress(message = "Exportando GPKG geoespacial…", value = 0.5, {
        exportar_gpkg_geo(datos_filt(), file)
      })
    }
  )

  output$dl_csv_temporal <- shiny::downloadHandler(
    filename = function() {
      glue::glue("inversiones_grd_temporal_{format(Sys.Date(), '%Y%m%d')}.csv")
    },
    content = function(file) {
      shiny::withProgress(message = "Exportando CSV temporal…", value = 0.5, {
        exportar_csv_temporal(serie_filt(), file)
      })
    }
  )

  # ---- Diccionario -------------------------------------------------------
  output$tbl_diccionario <- DT::renderDT({
    DT::datatable(
      dplyr::select(diccionario_final, -variable), rownames = FALSE,
      colnames = c("Nombre común", "Definición"),
      options  = list(pageLength = 15, scrollX = TRUE)
    )
  })

  output$tbl_fechas_fuentes <- DT::renderDT({
    shiny::req(!is.null(fechas_fuentes))
    DT::datatable(
      fechas_fuentes[, c("fuente", "archivo", "fecha_dato"),
                     with = FALSE],
      rownames = FALSE,
      colnames = c("Fuente", "Archivo", "Fecha del dato"),
      options  = list(dom = "t", pageLength = 20, scrollX = TRUE)
    )
  })
}

shiny::shinyApp(ui, server)
