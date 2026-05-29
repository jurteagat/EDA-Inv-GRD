# app.R — Dashboard Shiny de Inversiones GRD del Perú
#
# Versión interactiva de EDA_Inv_GRD_v1.qmd. Los datos y lookups se
# precomputan una sola vez en global.R; aquí solo se filtran reactivamente.

source("global.R", local = FALSE)

# ============================================================================
# UI
# ============================================================================

card_widget <- function(titulo, ..., padding = NULL) {
  bslib::card(
    full_screen = TRUE,
    bslib::card_header(titulo),
    if (is.null(padding)) bslib::card_body(...) else bslib::card_body(..., padding = padding)
  )
}

ui <- bslib::page_navbar(
  title = "Inversiones GRD — Perú",
  theme = bslib::bs_theme(version = 5, bootswatch = "flatly"),
  fillable = TRUE,
  header = htmltools::tagList(css_leyenda),

  sidebar = bslib::sidebar(
    width = 320,
    title = "Filtros globales",
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
    shiny::actionButton("reset", "Limpiar filtros",
                        icon = shiny::icon("rotate-left"),
                        class = "btn-outline-secondary btn-sm"),
    htmltools::hr(),
    shiny::textOutput("n_filtrados"),
    htmltools::tags$small(
      htmltools::tags$em(
        "Los filtros aplican a las pestañas Resumen, Mapa, ",
        "Distribuciones y Evolución temporal. ",
        "La pestaña Inversión específica opera sobre el universo completo."
      ),
      style = "color:#666;"
    )
  ),

  # ---- Pestaña: Resumen ---------------------------------------------------
  bslib::nav_panel(
    "Resumen",
    bslib::layout_columns(
      fill = FALSE,
      bslib::value_box(
        "Proyectos",
        shiny::textOutput("kpi_n", inline = TRUE),
        theme = "primary"
      ),
      bslib::value_box(
        "Costo actual. total (S/)",
        shiny::textOutput("kpi_costo", inline = TRUE),
        theme = "success"
      ),
      bslib::value_box(
        "Monto viable total (S/)",
        shiny::textOutput("kpi_viable", inline = TRUE),
        theme = "info"
      ),
      bslib::value_box(
        "Av. ejecución medio",
        shiny::textOutput("kpi_avance", inline = TRUE),
        theme = "warning"
      )
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      card_widget("Promedios por tipología", DT::DTOutput("tbl_tipologia")),
      card_widget("Top 10 entidades por devengado 2012-2025",
                  DT::DTOutput("tbl_top_entidades"))
    )
  ),

  # ---- Pestaña: Mapa ------------------------------------------------------
  bslib::nav_panel(
    "Mapa",
    bslib::layout_columns(
      col_widths = c(12),
      card_widget("Mapa global de inversiones GRD",
                  leaflet::leafletOutput("mapa", height = "560px"),
                  padding = 0)
    ),
    bslib::layout_columns(
      col_widths = c(12),
      card_widget("Top 15 departamentos por costo",
                  plotly::plotlyOutput("g_deptos", height = "420px"))
    )
  ),

  # ---- Pestaña: Distribuciones -------------------------------------------
  bslib::nav_panel(
    "Distribuciones",
    bslib::layout_columns(
      col_widths = c(6, 6),
      card_widget("Distribuciones de montos (log₁₀)",
                  shiny::plotOutput("g_montos", height = "420px")),
      card_widget("Avance físico y de ejecución por estado",
                  shiny::plotOutput("g_avances", height = "420px"))
    ),
    bslib::layout_columns(
      col_widths = c(12),
      card_widget("Outliers de costo (> Q3 + 3·IQR por tipología)",
                  DT::DTOutput("tbl_outliers"))
    )
  ),

  # ---- Pestaña: Evolución temporal ---------------------------------------
  bslib::nav_panel(
    "Evolución temporal",
    bslib::layout_columns(
      col_widths = c(7, 5),
      card_widget("PIA, PIM y Devengado anual",
                  plotly::plotlyOutput("g_serie", height = "420px")),
      card_widget("Ratios de ejecución y ampliación",
                  plotly::plotlyOutput("g_ratios", height = "420px"))
    ),
    bslib::layout_columns(
      col_widths = c(12),
      card_widget("Top 10 tipologías por devengado 2012-2025",
                  DT::DTOutput("tbl_top_tipologias"))
    )
  ),

  # ---- Pestaña: Inversión específica -------------------------------------
  bslib::nav_panel(
    "Inversión específica",
    bslib::card(
      bslib::card_header("Buscar inversión por código o nombre"),
      bslib::card_body(
        shiny::selectizeInput(
          "cod_foco", NULL,
          choices = NULL, multiple = FALSE,
          width = "100%",
          options = list(placeholder = "Escribe código o palabras del nombre…",
                         maxOptions = 50)
        )
      )
    ),
    bslib::layout_columns(
      col_widths = c(7, 5),
      card_widget("Ejecución acumulada — PIA/PIM/Devengado",
                  plotly::plotlyOutput("g_foco_serie", height = "360px")),
      card_widget("Ubicación",
                  leaflet::leafletOutput("mapa_foco", height = "360px"),
                  padding = 0)
    ),
    card_widget("Atributos del proyecto",
                DT::DTOutput("tbl_foco"))
  ),

  # ---- Pestaña: Diccionario ----------------------------------------------
  bslib::nav_panel(
    "Diccionario",
    card_widget(
      "Diccionario de variables (df_pu_geoinv_inv_g_dpf)",
      DT::DTOutput("tbl_diccionario")
    )
  )
)

# ============================================================================
# Server
# ============================================================================

server <- function(input, output, session) {

  # ---- Reactivos centrales ----------------------------------------------
  datos_filt <- shiny::reactive({
    df <- df_pu_geoinv_inv_g_dpf
    if (length(input$f_tipologia))
      df <- df |> dplyr::filter(des_tipologia %in% input$f_tipologia)
    if (length(input$f_depto))
      df <- df |> dplyr::filter(stringr::str_sub(ubigeo, 1, 2) %in% input$f_depto)
    if (length(input$f_estado))
      df <- df |> dplyr::filter(estado %in% input$f_estado)
    df
  })

  datos_filt_plain <- shiny::reactive({
    sf::st_drop_geometry(datos_filt())
  })

  codigos_filt <- shiny::reactive(unique(datos_filt_plain()$codigo_unico))

  serie_filt <- shiny::reactive({
    df_grd_2012_25_dpf |> dplyr::filter(codigo_unico %in% codigos_filt())
  })

  # ---- Sidebar: contador + reset ----------------------------------------
  output$n_filtrados <- shiny::renderText({
    n   <- nrow(datos_filt_plain())
    tot <- nrow(df_pu_geoinv_inv_g_dpf)
    glue::glue("{scales::comma(n)} de {scales::comma(tot)} inversiones")
  })

  shiny::observeEvent(input$reset, {
    shiny::updateSelectizeInput(session, "f_tipologia", selected = character(0))
    shiny::updateSelectizeInput(session, "f_depto",     selected = character(0))
    shiny::updateSelectizeInput(session, "f_estado",    selected = character(0))
  })

  # ---- Pestaña Resumen: KPIs --------------------------------------------
  fmt_soles <- function(x) {
    if (is.na(x) || !is.finite(x)) return("—")
    scales::label_comma(prefix = "S/ ")(x)
  }

  output$kpi_n      <- shiny::renderText(scales::comma(nrow(datos_filt_plain())))
  output$kpi_costo  <- shiny::renderText(fmt_soles(sum(datos_filt_plain()$costo_actualizado, na.rm = TRUE)))
  output$kpi_viable <- shiny::renderText(fmt_soles(sum(datos_filt_plain()$monto_viable,      na.rm = TRUE)))
  output$kpi_avance <- shiny::renderText({
    v <- mean(datos_filt_plain()$avance_ejecucion, na.rm = TRUE)
    if (is.nan(v)) "—" else paste0(round(v, 1), "%")
  })

  # ---- Tabla: promedios por tipología -----------------------------------
  output$tbl_tipologia <- DT::renderDT({
    df <- datos_filt_plain()
    shiny::validate(shiny::need(nrow(df) > 0, "Sin datos con los filtros actuales."))
    tabla <- df |>
      dplyr::filter(!is.na(des_tipologia)) |>
      dplyr::group_by(des_tipologia) |>
      dplyr::summarise(
        n_proyectos            = dplyr::n(),
        costo_actualizado_prom = mean(costo_actualizado, na.rm = TRUE),
        monto_viable_prom      = mean(monto_viable,      na.rm = TRUE),
        .groups = "drop"
      ) |>
      dplyr::arrange(dplyr::desc(costo_actualizado_prom)) |>
      dplyr::rename_with(label_var)

    DT::datatable(
      tabla, rownames = FALSE,
      options  = list(pageLength = 10, scrollX = TRUE)
    ) |>
      DT::formatRound(label_var(c("costo_actualizado_prom", "monto_viable_prom")),
                      digits = 0, mark = ",")
  })

  # ---- Tabla: top 10 entidades por devengado ----------------------------
  output$tbl_top_entidades <- DT::renderDT({
    s <- serie_filt()
    shiny::validate(shiny::need(nrow(s) > 0, "Sin datos con los filtros actuales."))
    top <- data.table::as.data.table(s)[
      , .(devengado_acum = sum(devengado, na.rm = TRUE)), by = entidad
    ][order(-devengado_acum)][1:10]

    DT::datatable(
      dplyr::rename_with(top, label_var),
      rownames = FALSE,
      options  = list(pageLength = 10, dom = "t")
    ) |>
      DT::formatRound(label_var("devengado_acum"), digits = 0, mark = ",")
  })

  # ---- Tabla: top 10 tipologías -----------------------------------------
  output$tbl_top_tipologias <- DT::renderDT({
    s <- serie_filt()
    shiny::validate(shiny::need(nrow(s) > 0, "Sin datos con los filtros actuales."))
    top <- data.table::as.data.table(s)[
      , .(devengado_acum = sum(devengado, na.rm = TRUE)), by = des_tipologia
    ][order(-devengado_acum)][1:10]

    DT::datatable(
      dplyr::rename_with(top, label_var),
      rownames = FALSE,
      options  = list(pageLength = 10, dom = "t")
    ) |>
      DT::formatRound(label_var("devengado_acum"), digits = 0, mark = ",")
  })

  # ---- Mapa global: render base + leyenda; marcadores con leafletProxy --
  output$mapa <- leaflet::renderLeaflet({
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

  shiny::observe({
    df <- datos_filt()
    p  <- leaflet::leafletProxy("mapa") |> leaflet::clearMarkers()
    if (nrow(df) == 0) return(invisible())

    plain <- sf::st_drop_geometry(df)
    log_costo <- log10(pmax(plain$costo_actualizado, 1, na.rm = TRUE))
    rng <- range(log_costo, na.rm = TRUE)
    radios <- if (diff(rng) == 0) rep(5, length(log_costo)) else
      3 + (log_costo - rng[1]) / (rng[2] - rng[1] + 1e-9) * (9 - 3)

    p |> leaflet::addCircleMarkers(
      data        = df,
      radius      = radios,
      color       = ~pal_tipologia(des_tipologia),
      fillOpacity = 0.45,
      stroke      = TRUE,
      weight      = 0.4,
      opacity     = 0.6,
      popup       = ~glue::glue(
        "<b>{nombre_inversion}</b><br>",
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

  # ---- Barplot: top 15 departamentos ------------------------------------
  output$g_deptos <- plotly::renderPlotly({
    plain <- datos_filt_plain()
    shiny::validate(shiny::need(nrow(plain) > 0, "Sin datos con los filtros actuales."))

    if (length(input$f_depto) == 1L) {
      shiny::validate("Filtrado a 1 departamento — la vista de comparación no aplica.")
    }

    tabla <- plain |>
      dplyr::mutate(cod_depto = stringr::str_sub(ubigeo, 1, 2),
                    cod_depto = dplyr::if_else(
                      !is.na(cod_depto) & nchar(cod_depto) == 2, cod_depto, "98")) |>
      dplyr::left_join(depto_lookup, by = "cod_depto") |>
      dplyr::mutate(departamento = dplyr::coalesce(departamento, glue::glue("Cod {cod_depto}"))) |>
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
      ggplot2::geom_col(fill = "#2166AC") +
      ggplot2::scale_x_continuous(labels = scales::label_comma()) +
      ggplot2::labs(x = "Costo actualizado total (S/)", y = NULL) +
      ggplot2::theme_minimal()

    plotly::ggplotly(g, tooltip = c("y", "text"))
  })

  # ---- Distribuciones de montos -----------------------------------------
  output$g_montos <- shiny::renderPlot({
    plain <- datos_filt_plain()
    shiny::validate(shiny::need(nrow(plain) > 0, "Sin datos con los filtros actuales."))

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
                                "Sin valores > 0 para graficar en log₁₀."))

    ggplot2::ggplot(df_largo, ggplot2::aes(x = valor)) +
      ggplot2::geom_histogram(bins = 40, fill = "#4393C3",
                              color = "white", alpha = 0.8) +
      ggplot2::geom_density(ggplot2::aes(y = ggplot2::after_stat(count)),
                            color = "#D6604D", linewidth = 0.8) +
      ggplot2::scale_x_log10(labels = scales::label_comma()) +
      ggplot2::facet_wrap(~variable, scales = "free_y", ncol = 2) +
      ggplot2::labs(x = "Soles (escala log₁₀)", y = "Frecuencia") +
      ggplot2::theme_minimal()
  })

  # ---- Avances por estado ------------------------------------------------
  output$g_avances <- shiny::renderPlot({
    plain <- datos_filt_plain()
    shiny::validate(shiny::need(nrow(plain) > 0, "Sin datos con los filtros actuales."))

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
      ggplot2::theme_minimal() +
      ggplot2::theme(legend.position = "none",
                     axis.text.x = ggplot2::element_text(angle = 30, hjust = 1))
  })

  # ---- Outliers de costo ------------------------------------------------
  output$tbl_outliers <- DT::renderDT({
    plain <- datos_filt_plain()
    shiny::validate(shiny::need(nrow(plain) > 0, "Sin datos con los filtros actuales."))

    df_out <- plain |>
      dplyr::left_join(limites_iqr, by = "des_tipologia") |>
      dplyr::filter(!is.na(limite_outlier),
                    costo_actualizado > limite_outlier) |>
      dplyr::mutate(pct_costo_vs_viable = dplyr::if_else(
        is.finite(monto_viable) & monto_viable > 0,
        (costo_actualizado - monto_viable) / monto_viable * 100,
        NA_real_
      )) |>
      dplyr::select(codigo_unico, nombre_inversion, entidad,
                    des_tipologia, costo_actualizado, pct_costo_vs_viable) |>
      dplyr::arrange(dplyr::desc(costo_actualizado))

    shiny::validate(shiny::need(nrow(df_out) > 0,
                                "Ningún proyecto supera Q3 + 3·IQR en su tipología."))

    DT::datatable(
      dplyr::rename_with(df_out, label_var),
      rownames = FALSE, filter = "top",
      options  = list(pageLength = 10, scrollX = TRUE)
    ) |>
      DT::formatRound(label_var("costo_actualizado"), digits = 0, mark = ",") |>
      DT::formatRound(label_var("pct_costo_vs_viable"), digits = 1)
  })

  # ---- Evolución temporal -----------------------------------------------
  serie_portafolio_filt <- shiny::reactive({
    s <- serie_filt()
    shiny::req(nrow(s) > 0)
    data.table::as.data.table(s)[
      , .(pia       = sum(pia,       na.rm = TRUE),
          pim       = sum(pim,       na.rm = TRUE),
          devengado = sum(devengado, na.rm = TRUE)),
      by = anio
    ][order(anio)]
  })

  output$g_serie <- plotly::renderPlotly({
    sp <- serie_portafolio_filt()
    shiny::validate(shiny::need(nrow(sp) > 0, "Sin datos para graficar."))

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
        values = c(PIA = "#1A9850", PIM = "#4393C3", Devengado = "#D6604D")
      ) +
      ggplot2::scale_y_continuous(labels = scales::label_comma()) +
      ggplot2::labs(x = "Año", y = "Soles", color = NULL) +
      ggplot2::theme_minimal()

    plotly::ggplotly(g, tooltip = c("x", "text"))
  })

  output$g_ratios <- plotly::renderPlotly({
    sp <- serie_portafolio_filt()
    shiny::validate(shiny::need(nrow(sp) > 0, "Sin datos para graficar."))

    s_ratios <- sp |>
      dplyr::mutate(
        pct_ejecucion = dplyr::if_else(pim > 0, devengado / pim * 100, NA_real_),
        pct_pim_pia   = dplyr::if_else(pia > 0, pim       / pia * 100, NA_real_)
      ) |>
      dplyr::select(anio, pct_ejecucion, pct_pim_pia) |>
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
        values = c("% Ejecución (Dev./PIM)" = "#D6604D",
                   "% PIM vs. PIA"           = "#4393C3")
      ) +
      ggplot2::scale_y_continuous(labels = ~paste0(., "%")) +
      ggplot2::labs(x = "Año", y = "Porcentaje (%)", color = NULL) +
      ggplot2::theme_minimal()

    plotly::ggplotly(g, tooltip = c("x", "text"))
  })

  # ---- Inversión específica: buscador server-side -----------------------
  shiny::updateSelectizeInput(
    session, "cod_foco",
    choices  = stats::setNames(opciones_codigo_inv$value, opciones_codigo_inv$label),
    server   = TRUE,
    selected = "2508148"
  )

  fila_foco <- shiny::reactive({
    shiny::req(input$cod_foco)
    df_pu_geoinv_inv_g_dpf |>
      dplyr::filter(codigo_unico == as.character(input$cod_foco))
  })

  output$tbl_foco <- DT::renderDT({
    f <- fila_foco()
    shiny::validate(shiny::need(nrow(f) > 0, "Selecciona una inversión."))

    tabla <- f |>
      sf::st_drop_geometry() |>
      dplyr::mutate(dplyr::across(where(is.numeric),
                                  ~scales::comma(., accuracy = 1))) |>
      tidyr::pivot_longer(dplyr::everything(),
                          names_to = "variable", values_to = "valor",
                          values_transform = list(valor = as.character)) |>
      dplyr::mutate(variable = label_var(variable))

    DT::datatable(
      tabla, rownames = FALSE,
      colnames = c("Variable", "Valor"),
      options  = list(pageLength = 25, dom = "tip", scrollX = TRUE)
    )
  })

  output$g_foco_serie <- plotly::renderPlotly({
    cod <- shiny::req(input$cod_foco)
    serie <- df_grd_2012_25_dpf |>
      dplyr::filter(codigo_unico == as.character(cod)) |>
      dplyr::group_by(anio) |>
      dplyr::summarise(pia       = sum(pia,       na.rm = TRUE),
                       pim       = sum(pim,       na.rm = TRUE),
                       devengado = sum(devengado, na.rm = TRUE),
                       .groups   = "drop") |>
      dplyr::arrange(anio)

    shiny::validate(shiny::need(nrow(serie) > 0, "Sin serie temporal para esta inversión."))

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
        values = c(PIA = "#1A9850", PIM = "#4393C3", Devengado = "#D6604D")
      ) +
      ggplot2::labs(x = "Año", y = "Soles acumulados", color = NULL) +
      ggplot2::theme_minimal()

    plotly::ggplotly(g)
  })

  output$mapa_foco <- leaflet::renderLeaflet({
    f <- fila_foco()
    shiny::validate(shiny::need(nrow(f) > 0, "Selecciona una inversión."))

    leaflet::leaflet(f) |>
      leaflet::addTiles() |>
      leaflet::addCircleMarkers(
        radius = 8, color = "red", fillOpacity = 0.8,
        popup  = ~glue::glue(
          "<b>{nombre_inversion}</b><br>",
          "Tipología: {des_tipologia}<br>",
          "Monto viable: {scales::label_comma()(monto_viable)}<br>",
          "Costo actualizado: {scales::label_comma()(costo_actualizado)}"
        )
      )
  })

  # ---- Diccionario ------------------------------------------------------
  output$tbl_diccionario <- DT::renderDT({
    DT::datatable(
      diccionario_final, rownames = FALSE,
      options  = list(pageLength = 15, scrollX = TRUE),
      colnames = c("Variable", "Nombre común", "Definición")
    )
  })
}

shiny::shinyApp(ui, server)
