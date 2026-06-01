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
  header   = htmltools::tagList(css_leyenda),

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
      "f_estado", "Estado",
      choices  = opciones_estado,
      multiple = TRUE,
      options  = list(placeholder = "Todos los estados")
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
    shiny::actionButton("reset", "Limpiar filtros",
                        icon  = shiny::icon("rotate-left"),
                        class = "btn-outline-secondary btn-sm w-100"),
    htmltools::hr(),
    shiny::textOutput("n_filtrados"),
    htmltools::tags$small(
      htmltools::tags$em(
        "Filtros activos en Resumen, Mapa, Distribuciones, Evolución temporal ",
        "y Departamentos. Inversión específica usa el universo completo."
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
      bslib::value_box("Proyectos",
                       shiny::textOutput("kpi_n", inline = TRUE),
                       theme = "primary"),
      bslib::value_box("Costo actual. total (S/)",
                       shiny::textOutput("kpi_costo", inline = TRUE),
                       theme = "success"),
      bslib::value_box("Monto viable total (S/)",
                       shiny::textOutput("kpi_viable", inline = TRUE),
                       theme = "info"),
      bslib::value_box("Av. ejecución medio",
                       shiny::textOutput("kpi_avance", inline = TRUE),
                       theme = "warning")
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)),
      card_widget("Promedios por tipología",
                  DT::DTOutput("tbl_tipologia")),
      card_widget("Top 10 entidades por devengado 2012-2025",
                  DT::DTOutput("tbl_top_entidades"))
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
      col_widths = 12,
      card_widget("Mapa global de inversiones GRD",
                  leaflet::leafletOutput("mapa", height = "560px"),
                  padding = 0)
    ),
    bslib::layout_columns(
      col_widths = 12,
      card_widget("Top 15 departamentos por costo",
                  plotly::plotlyOutput("g_deptos", height = "380px"))
    )
  ),

  # ---- Pestaña: Distribuciones --------------------------------------------
  bslib::nav_panel(
    "Distribuciones",
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)),
      card_widget("Distribuciones de montos (log₁₀)",
                  shiny::plotOutput("g_montos", height = "380px")),
      card_widget("Avance físico y de ejecución por estado",
                  shiny::plotOutput("g_avances", height = "380px"))
    ),
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)),
      card_widget("Boxplots de costo por tipología (log₁₀)",
                  shiny::plotOutput("g_boxplots_tipo", height = "380px")),
      card_widget("Matriz de correlación (montos y avances)",
                  shiny::plotOutput("g_corr_matrix",   height = "380px"))
    ),
    bslib::layout_columns(
      col_widths = 12,
      card_widget("Outliers de costo (> Q3 + 3·IQR por tipología)",
                  DT::DTOutput("tbl_outliers"))
    )
  ),

  # ---- Pestaña: Evolución temporal ----------------------------------------
  bslib::nav_panel(
    "Evolución temporal",
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(7, 5)),
      card_widget("PIA, PIM y Devengado anual",
                  plotly::plotlyOutput("g_serie",  height = "380px")),
      card_widget("Ratios de ejecución y ampliación",
                  plotly::plotlyOutput("g_ratios", height = "380px"))
    ),
    bslib::layout_columns(
      col_widths = 12,
      card_widget("Top 10 tipologías por devengado 2012-2025",
                  DT::DTOutput("tbl_top_tipologias"))
    )
  ),

  # ---- Pestaña: Departamentos ---------------------------------------------
  bslib::nav_panel(
    "Departamentos",
    bslib::layout_columns(
      col_widths = 12,
      card_widget("Cortes por departamento",
                  DT::DTOutput("tbl_deptos"))
    )
  ),

  # ---- Pestaña: Inversión específica --------------------------------------
  bslib::nav_panel(
    "Inversión específica",
    bslib::card(
      bslib::card_header("Buscar inversión por código o nombre"),
      bslib::card_body(
        bslib::layout_columns(
          col_widths = bslib::breakpoints(sm = 12, lg = c(10, 2)),
          shiny::selectizeInput(
            "cod_foco", NULL,
            choices = NULL, multiple = FALSE,
            width   = "100%",
            options = list(placeholder = "Escribe código o palabras del nombre…",
                           maxOptions  = 50)
          ),
          shiny::downloadButton("dl_pdf_reporte", "PDF",
                                icon  = shiny::icon("file-pdf"),
                                class = "btn-outline-danger btn-sm")
        )
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
    card_widget("Atributos del proyecto",
                DT::DTOutput("tbl_foco"))
  ),

  # ---- Pestaña: Datos y descargas -----------------------------------------
  bslib::nav_panel(
    value = "tab_datos_descargas",
    title = "Datos y descargas",
    bslib::layout_columns(
      col_widths = bslib::breakpoints(sm = 12, lg = c(6, 6)),
      bslib::card(
        bslib::card_header("Base geoespacial (filtrada)"),
        bslib::card_body(
          htmltools::p(
            "Incluye todos los atributos de la inversión más columnas",
            htmltools::code("lon"), "/", htmltools::code("lat"),
            "extraídas de la geometría. Respeta los filtros activos."
          ),
          shiny::downloadButton("dl_csv_geo", "Descargar CSV geoespacial",
                                icon  = shiny::icon("download"),
                                class = "btn-primary btn-sm")
        )
      ),
      bslib::card(
        bslib::card_header("Base temporal (filtrada)"),
        bslib::card_body(
          htmltools::p(
            "Serie 2012-2025 de PIA/PIM/Devengado por inversión.",
            "Filtrada a los códigos activos según filtros globales."
          ),
          shiny::downloadButton("dl_csv_temporal", "Descargar CSV temporal",
                                icon  = shiny::icon("download"),
                                class = "btn-success btn-sm")
        )
      )
    )
  ),

  # ---- Pestaña: Diccionario -----------------------------------------------
  bslib::nav_panel(
    "Diccionario",
    bslib::layout_columns(
      col_widths = 12,
      card_widget(
        "Diccionario actualizado (variables etiquetadas)",
        DT::DTOutput("tbl_diccionario")
      )
    ),
    bslib::layout_columns(
      col_widths = 12,
      card_widget(
        "Diccionario oficial MEF (sin modificar)",
        DT::DTOutput("tbl_diccionario_mef")
      )
    )
  )
)

# ============================================================================
# Server
# ============================================================================

server <- function(input, output, session) {

  # ---- Reactivos centrales ------------------------------------------------
  datos_filt <- shiny::reactive({
    df <- df_pu_geoinv_inv_g_dpf
    if (length(input$f_tipologia))
      df <- dplyr::filter(df, des_tipologia %in% input$f_tipologia)
    if (length(input$f_depto))
      df <- dplyr::filter(df,
                          stringr::str_sub(ubigeo, 1, 2) %in% input$f_depto)
    if (length(input$f_estado))
      df <- dplyr::filter(df, estado %in% input$f_estado)
    if (length(input$f_situacion))
      df <- dplyr::filter(df, situacion %in% input$f_situacion)
    if (!is.null(input$f_ioarr) && input$f_ioarr != "Todos")
      df <- dplyr::filter(df,
                          as.character(ind_ioarr_emerg) == input$f_ioarr)
    df
  })

  datos_filt_plain <- shiny::reactive(sf::st_drop_geometry(datos_filt()))
  codigos_filt     <- shiny::reactive(unique(datos_filt_plain()$codigo_unico))
  serie_filt       <- shiny::reactive(
    dplyr::filter(df_grd_2012_25_dpf, codigo_unico %in% codigos_filt())
  )

  # ---- Sidebar: contador + reset ------------------------------------------
  output$n_filtrados <- shiny::renderText({
    n   <- nrow(datos_filt_plain())
    tot <- nrow(df_pu_geoinv_inv_g_dpf)
    glue::glue("{scales::comma(n)} de {scales::comma(tot)} inversiones")
  })

  shiny::observeEvent(input$reset, {
    shiny::updateSelectizeInput(session, "f_tipologia", selected = character(0))
    shiny::updateSelectizeInput(session, "f_depto",     selected = character(0))
    shiny::updateSelectizeInput(session, "f_estado",    selected = character(0))
    shiny::updateSelectizeInput(session, "f_situacion", selected = character(0))
    shiny::updateSelectInput(   session, "f_ioarr",     selected = "Todos")
  })

  # ---- KPIs ---------------------------------------------------------------
  output$kpi_n      <- shiny::renderText(scales::comma(nrow(datos_filt_plain())))
  output$kpi_costo  <- shiny::renderText(
    fmt_soles(sum(datos_filt_plain()$costo_actualizado, na.rm = TRUE)))
  output$kpi_viable <- shiny::renderText(
    fmt_soles(sum(datos_filt_plain()$monto_viable, na.rm = TRUE)))
  output$kpi_avance <- shiny::renderText({
    v <- mean(datos_filt_plain()$avance_ejecucion, na.rm = TRUE)
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

  # ---- Tabla: top 10 entidades por devengado -----------------------------
  output$tbl_top_entidades <- DT::renderDT({
    s <- serie_filt()
    shiny::validate(shiny::need(nrow(s) > 0, "Sin datos."))
    top <- data.table::as.data.table(s)[
      , .(devengado_acum = sum(devengado, na.rm = TRUE)), by = entidad
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
                    entidad, estado, costo_actualizado, avance_ejecucion) |>
      dplyr::rename_with(label_var) |>
      DT::datatable(
        rownames = FALSE, filter = "top",
        options  = list(pageLength = 15, scrollX = TRUE,
                        columnDefs = list(
                          list(width = "300px",
                               targets = which(
                                 names(dplyr::rename_with(
                                   dplyr::select(df, codigo_unico, nombre_abreviado,
                                                 des_tipologia, entidad, estado,
                                                 costo_actualizado, avance_ejecucion),
                                   label_var
                                 )) == label_var("nombre_abreviado")
                               ) - 1
                          )
                        ))
      ) |>
      DT::formatRound(label_var("costo_actualizado"), digits = 0, mark = ",") |>
      DT::formatRound(label_var("avance_ejecucion"),  digits = 1)
  })

  # ---- Mapa global --------------------------------------------------------
  output$mapa <- leaflet::renderLeaflet({
    shiny::withProgress(message = "Cargando mapa…", value = 0.5, {
      leaflet::leaflet() |>
        leaflet::addProviderTiles(leaflet::providers$CartoDB.Positron) |>
        leaflet::setView(lng = -75, lat = -9.5, zoom = 5) |>
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
    df <- datos_filt()
    p  <- leaflet::leafletProxy("mapa") |> leaflet::clearMarkers()
    if (nrow(df) == 0) return(invisible())

    plain  <- sf::st_drop_geometry(df)
    radios <- radios_log(plain$costo_actualizado)

    p |> leaflet::addCircleMarkers(
      data        = df,
      radius      = radios,
      color       = ~pal_tipologia(des_tipologia),
      fillOpacity = 0.45,
      stroke      = TRUE,
      weight      = 0.4,
      opacity     = 0.6,
      popup       = ~glue::glue(
        "<b>{nombre_abreviado}</b><br>",
        "<i>{des_tipologia}</i><br>",
        "Entidad: {entidad}<br>",
        "Estado: {estado}<br>",
        "Costo actual.: S/ {scales::label_comma()(costo_actualizado)}<br>",
        "Av. ejecución: {avance_ejecucion}%"
      ),
      clusterOptions = leaflet::markerClusterOptions(
        maxClusterRadius           = 25,
        spiderfyOnMaxZoom          = TRUE,
        spiderfyDistanceMultiplier = 1.8,
        showCoverageOnHover        = FALSE,
        zoomToBoundsOnClick        = TRUE
      )
    )
  })

  # ---- Barplot: top 15 departamentos -------------------------------------
  output$g_deptos <- plotly::renderPlotly({
    plain <- datos_filt_plain()
    shiny::validate(shiny::need(nrow(plain) > 0, "Sin datos."))
    if (length(input$f_depto) == 1L)
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
      ggplot2::geom_density(ggplot2::aes(y = ggplot2::after_stat(count)),
                            color = paleta_grd["rojo"], linewidth = 0.8) +
      ggplot2::scale_x_log10(labels = scales::label_comma()) +
      ggplot2::facet_wrap(~variable, scales = "free_y", ncol = 2) +
      ggplot2::labs(x = "Soles (escala log₁₀)", y = "Frecuencia") +
      theme_grd()
  })

  # ---- Avances por estado ------------------------------------------------
  output$g_avances <- shiny::renderPlot({
    plain <- datos_filt_plain()
    shiny::validate(shiny::need(nrow(plain) > 0, "Sin datos."))

    df_av <- plain |>
      dplyr::filter(!is.na(estado)) |>
      dplyr::select(estado, avance_fisico, avance_ejecucion) |>
      tidyr::pivot_longer(c(avance_fisico, avance_ejecucion),
                          names_to = "tipo_avance", values_to = "valor") |>
      dplyr::mutate(tipo_avance = dplyr::recode(
        tipo_avance,
        avance_fisico    = "Avance físico (%)",
        avance_ejecucion = "Avance ejecución (%)"
      ))

    shiny::validate(shiny::need(nrow(df_av) > 0, "Sin datos de avance."))

    ggplot2::ggplot(df_av, ggplot2::aes(x = estado, y = valor, fill = estado)) +
      ggplot2::geom_boxplot(outlier.shape = NA, alpha = 0.6) +
      ggplot2::geom_jitter(width = 0.2, alpha = 0.15, size = 0.8) +
      ggplot2::facet_wrap(~tipo_avance, ncol = 2) +
      ggplot2::scale_y_continuous(limits = c(0, 100)) +
      ggplot2::labs(x = "Estado", y = "Avance (%)") +
      theme_grd() +
      ggplot2::theme(legend.position = "none",
                     axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
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
      theme_grd()
  })

  # ---- Matriz de correlación-dispersión (GGally) -------------------------
  output$g_corr_matrix <- shiny::renderPlot({
    shiny::withProgress(message = "Calculando matriz de correlación…", {
      plain <- datos_filt_plain()
      shiny::validate(shiny::need(nrow(plain) >= 10, "Necesitas al menos 10 inversiones."))

      df_mat <- plain |>
        dplyr::select(costo_actualizado, monto_viable,
                      pia_anio_actual, pim_anio_actual,
                      avance_fisico, avance_ejecucion) |>
        dplyr::filter(dplyr::if_all(
          c(costo_actualizado, monto_viable),
          ~ is.finite(.) & . > 0
        )) |>
        dplyr::mutate(
          log_costo    = log10(costo_actualizado),
          log_viable   = log10(monto_viable),
          log_pia      = log10(pmax(pia_anio_actual, 1, na.rm = TRUE)),
          log_pim      = log10(pmax(pim_anio_actual, 1, na.rm = TRUE)),
          av_fisico    = avance_fisico,
          av_ejecucion = avance_ejecucion
        ) |>
        dplyr::select(log_costo, log_viable, log_pia, log_pim,
                      av_fisico, av_ejecucion)

      col_labels <- c(
        log_costo    = "log₁₀\nCosto",
        log_viable   = "log₁₀\nViable",
        log_pia      = "log₁₀\nPIA",
        log_pim      = "log₁₀\nPIM",
        av_fisico    = "Av.\nFísico",
        av_ejecucion = "Av.\nEjec."
      )

      shiny::validate(shiny::need(nrow(df_mat) >= 5, "Datos insuficientes."))

      GGally::ggpairs(
        df_mat,
        columnLabels = col_labels,
        upper = list(continuous = GGally::wrap("cor", size = 2.5)),
        lower = list(continuous = GGally::wrap("points",
                                               alpha = 0.2, size = 0.5,
                                               color = paleta_grd["azul"])),
        diag  = list(continuous = GGally::wrap("densityDiag",
                                               fill = paleta_grd["azul"],
                                               alpha = 0.5))
      ) +
        theme_grd(base_size = 8)
    })
  })

  # ---- Outliers de costo -------------------------------------------------
  output$tbl_outliers <- DT::renderDT({
    plain <- datos_filt_plain()
    shiny::validate(shiny::need(nrow(plain) > 0, "Sin datos."))

    df_out <- plain |>
      dplyr::left_join(limites_iqr, by = "des_tipologia") |>
      dplyr::filter(!is.na(limite_outlier),
                    costo_actualizado > limite_outlier) |>
      dplyr::mutate(pct_costo_vs_viable = dplyr::if_else(
        is.finite(monto_viable) & monto_viable > 0,
        (costo_actualizado - monto_viable) / monto_viable * 100,
        NA_real_
      )) |>
      dplyr::select(codigo_unico, nombre_abreviado, entidad,
                    des_tipologia, costo_actualizado, pct_costo_vs_viable) |>
      dplyr::arrange(dplyr::desc(costo_actualizado))

    shiny::validate(shiny::need(nrow(df_out) > 0,
                                "Ningún proyecto supera Q3 + 3·IQR."))

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
                   text = paste0(metrica, ": S/ ", scales::label_comma()(valor)))
    ) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point(size = 2) +
      ggplot2::scale_color_manual(
        values = c(PIA = paleta_grd["verde"], PIM = paleta_grd["azul"],
                   Devengado = paleta_grd["rojo"])
      ) +
      ggplot2::scale_y_continuous(labels = scales::label_comma()) +
      ggplot2::labs(x = "Año", y = "Soles", color = NULL) +
      theme_grd()

    plotly::ggplotly(g, tooltip = c("x", "text"))
  })

  output$g_ratios <- plotly::renderPlotly({
    sp <- serie_portafolio_filt()
    shiny::validate(shiny::need(nrow(sp) > 0, "Sin datos."))

    s_ratios <- ratios_portafolio(sp) |>
      tidyr::pivot_longer(c(pct_ejecucion, pct_pim_pia),
                          names_to = "ratio", values_to = "valor") |>
      dplyr::mutate(ratio = dplyr::recode(
        ratio,
        pct_ejecucion = "% Ejecución (Dev./PIM)",
        pct_pim_pia   = "% PIM vs. PIA"
      ))

    g <- ggplot2::ggplot(
      s_ratios,
      ggplot2::aes(x = anio, y = valor, color = ratio, group = ratio,
                   text = paste0(ratio, ": ", round(valor, 1), "%"))
    ) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point(size = 2) +
      ggplot2::scale_color_manual(
        values = c("% Ejecución (Dev./PIM)" = paleta_grd["rojo"],
                   "% PIM vs. PIA"           = paleta_grd["azul"])
      ) +
      ggplot2::scale_y_continuous(labels = ~paste0(., "%")) +
      ggplot2::labs(x = "Año", y = "Porcentaje (%)", color = NULL) +
      theme_grd()

    plotly::ggplotly(g, tooltip = c("x", "text"))
  })

  # ---- Tabla: top 10 tipologías -----------------------------------------
  output$tbl_top_tipologias <- DT::renderDT({
    s <- serie_filt()
    shiny::validate(shiny::need(nrow(s) > 0, "Sin datos."))
    top <- data.table::as.data.table(s)[
      , .(devengado_acum = sum(devengado, na.rm = TRUE)), by = des_tipologia
    ][order(-devengado_acum)][seq_len(min(10, .N))]
    DT::datatable(dplyr::rename_with(top, label_var),
                  rownames = FALSE,
                  options  = list(pageLength = 10, dom = "t")) |>
      DT::formatRound(label_var("devengado_acum"), digits = 0, mark = ",")
  })

  # ---- Tabla: cortes por departamento ------------------------------------
  output$tbl_deptos <- DT::renderDT({
    df <- datos_filt_plain()
    shiny::validate(shiny::need(nrow(df) > 0, "Sin datos."))
    tabla <- tabla_cortes_departamento(df, depto_lookup) |>
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

  output$tbl_foco <- DT::renderDT({
    f <- fila_foco()
    shiny::validate(shiny::need(nrow(f) > 0, "Selecciona una inversión."))

    tabla <- f |>
      sf::st_drop_geometry() |>
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
    cod   <- shiny::req(input$cod_foco)
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

    s_largo <- serie |>
      dplyr::mutate(dplyr::across(c(pia, pim, devengado), cumsum)) |>
      tidyr::pivot_longer(c(pia, pim, devengado),
                          names_to = "metrica", values_to = "valor") |>
      dplyr::mutate(metrica = dplyr::recode(
        metrica, pia = "PIA", pim = "PIM", devengado = "Devengado"
      ))

    g <- ggplot2::ggplot(s_largo,
                         ggplot2::aes(x = anio, y = valor, color = metrica)) +
      ggplot2::geom_line(linewidth = 1) +
      ggplot2::geom_point() +
      ggplot2::scale_y_continuous(labels = scales::label_comma()) +
      ggplot2::scale_color_manual(
        values = c(PIA = paleta_grd["verde"], PIM = paleta_grd["azul"],
                   Devengado = paleta_grd["rojo"])
      ) +
      ggplot2::labs(x = "Año", y = "Soles acumulados", color = NULL) +
      theme_grd()

    plotly::ggplotly(g)
  })

  output$mapa_foco <- leaflet::renderLeaflet({
    f <- fila_foco()
    shiny::validate(shiny::need(nrow(f) > 0, "Selecciona una inversión."))

    leaflet::leaflet(f) |>
      leaflet::addTiles() |>
      leaflet::addCircleMarkers(
        radius = 8, color = "#D6604D", fillOpacity = 0.8,
        popup  = ~glue::glue(
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

  # ---- Descargas CSV ------------------------------------------------------
  output$dl_csv_geo <- shiny::downloadHandler(
    filename = function() {
      glue::glue("inversiones_grd_geo_{format(Sys.Date(), '%Y%m%d')}.csv")
    },
    content = function(file) {
      shiny::withProgress(message = "Exportando CSV geoespacial…", value = 0.5, {
        exportar_csv_geo(datos_filt(), file)
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
      diccionario_final, rownames = FALSE,
      colnames = c("Variable", "Nombre común", "Definición"),
      options  = list(pageLength = 15, scrollX = TRUE)
    )
  })

  output$tbl_diccionario_mef <- DT::renderDT({
    shiny::validate(shiny::need(
      !is.null(dicc_oficial),
      "Diccionario oficial MEF no disponible (diccionario.rds no encontrado)."
    ))
    DT::datatable(
      diccionario_mef, rownames = FALSE,
      colnames = c("Variable", "Definición MEF"),
      options  = list(pageLength = 15, scrollX = TRUE)
    )
  })
}

shiny::shinyApp(ui, server)
