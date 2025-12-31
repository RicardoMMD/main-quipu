# ==============================================================================
# MÓDULO: Datos Poblacionales (Censales) - Versión Pro
# ==============================================================================

# 1. UI DEL MÓDULO -------------------------------------------------------------
mod_censales_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Fila de Indicadores Rápidos (Resumen Estadístico)
    fluidRow(
      valueBoxOutput(ns("vbox_avg"), width = 3),
      valueBoxOutput(ns("vbox_max"), width = 3),
      valueBoxOutput(ns("vbox_min"), width = 3),
      valueBoxOutput(ns("vbox_total_pob"), width = 3)
    ),
    
    fluidRow(
      # Panel de Control Izquierdo
      column(
        style = "padding:0rem;",
        width = 4,
        box(
          title = "Configuración de Análisis", status = "primary", solidHeader = TRUE, width = 12,
          shinyWidgets::pickerInput(
            ns("censo_interes"), 
            "Variable Demográfica:", 
            choices = chs_censales,
            options = list(`live-search` = TRUE, size = 10),
            selected = chs_censales[1]
          ),
          helpText("Las variables se calculan automáticamente como % del total si corresponde."),
          hr(),
          downloadBttn(ns("dw_censal_full"), "Exportar Reporte .CSV", 
                       style = "simple", color = "success", block = TRUE, size = "sm")
        ),
        box(
          title = "Distribución del Dato", status = "info", solidHeader = TRUE, width = 12,
          plotlyOutput(ns("hist_distribucion"), height = "250px")
        )
      ),
      
      # Panel Principal de Visualización
      column(
        style = "padding:0rem;",
        width = 8,
        tabBox(
          id = ns("tabs_censales"), width = 12,
          
          # Tab 1: Mapa Coroplético
          tabPanel(
            title = "Geografía del Indicador", icon = icon("map-marked-alt"),
            withSpinner(leafletOutput(ns("mapa_censales"), height = "70vh")),
            absolutePanel(
              bottom = 20, left = 20, zIndex = 1000,
              actionButton(ns("mapFullscreen"), "Pantalla Completa", icon = icon("expand"))
            )
          ),
          
          # Tab 2: Ranking y Comparativa
          tabPanel(
            title = "Ranking de Secciones", icon = icon("list-ol"),
            fluidRow(
              column(6, h4("Secciones con Mayor Valor"), DTOutput(ns("dt_top"))),
              column(6, h4("Secciones con Menor Valor"), DTOutput(ns("dt_bottom")))
            )
          ),
          
          # Tab 3: Base de Datos Completa
          tabPanel(
            title = "Explorador de Datos", icon = icon("table"),
            DTOutput(ns("dt_full"))
          )
        )
      )
    )
  )
}

# 2. SERVER DEL MÓDULO ---------------------------------------------------------
mod_censales_server <- function(id, secciones_reactivas) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # --- 1. PROCESAMIENTO DE DATOS ---
    
    # Identificación inteligente de variables que deben ser porcentajes
    es_porcentaje <- reactive({
      vars_proporcionales <- c("PHOG_IND", "POB_AFRO", "PCON_DISC", "POCUPADA", 
                               "PDESOCUP", "VIVPAR_HAB", "VIVPAR_DES")
      input$censo_interes %in% vars_proporcionales || grepl("_F$|_M$", input$censo_interes)
    })
    
    # Unión de Geometría con Datos Censales
    data_final <- reactive({
      req(secciones_reactivas(), input$censo_interes)
      
      # 1. Preparar Geometría (SF)
      shp <- secciones_reactivas() %>% 
        mutate(SECCION = as.character(SECCION)) %>%
        st_transform(4326)
      
      # 2. Preparar Datos (Tabular)
      # Nota: Usamos data_secc_cpv2020 de los metadatos compartidos
      censal_sub <- data_secc_cpv2020 %>%
        mutate(SECCION = as.character(SECCION)) %>%
        filter(SECCION %in% shp$SECCION) %>%
        select(SECCION, POBTOT, GRAPROES_NIVEL, Val = all_of(input$censo_interes))
      
      # 3. Join y Cálculos
      res <- shp %>%
        left_join(censal_sub, by = "SECCION") %>%
        mutate(
          POBTOT = coalesce(as.numeric(POBTOT), 0),
          Val = coalesce(as.numeric(Val), 0),
          # Cálculo de tasa/porcentaje o valor absoluto
          valor_final = if(es_porcentaje()){
            ifelse(POBTOT > 0, (Val / POBTOT) * 100, 0)
          } else { Val }
        )
      return(res)
    })
    
    # --- 2. RENDERIZADO DE INDICADORES (ValueBoxes) ---
    
    output$vbox_avg <- renderValueBox({
      val <- mean(data_final()$valor_final, na.rm = TRUE)
      valueBox(
        value = paste0(round(val, 1), if(es_porcentaje()) "%" else ""),
        subtitle = "Promedio en la Zona", icon = icon("calculator"), color = "blue"
      )
    })
    
    output$vbox_max <- renderValueBox({
      val <- max(data_final()$valor_final, na.rm = TRUE)
      valueBox(
        value = paste0(round(val, 1), if(es_porcentaje()) "%" else ""),
        subtitle = "Valor Máximo Detec.", icon = icon("arrow-up"), color = "red"
      )
    })
    
    output$vbox_min <- renderValueBox({
      val <- min(data_final()$valor_final, na.rm = TRUE)
      valueBox(
        value = paste0(round(val, 1), if(es_porcentaje()) "%" else ""),
        subtitle = "Valor Mínimo Detec.", icon = icon("arrow-up"), color = "red"
      )
    })
    
    output$vbox_total_pob <- renderValueBox({
      val <- sum(data_final()$POBTOT, na.rm = TRUE)
      valueBox(
        value = format(val, big.mark = ","),
        subtitle = "Población Total Alcanzada", icon = icon("users"), color = "olive"
      )
    })
    
    # --- 3. VISUALIZACIONES (MAPA Y GRÁFICA) ---
    
    output$hist_distribucion <- renderPlotly({
      # 1. Preparación de datos y etiquetas
      dta <- data_final()
      req(nrow(dta) > 0)
      
      # Obtener el nombre humano de la variable seleccionada
      nombre_humano <- names(chs_censales)[which(chs_censales == input$censo_interes)]
      if(length(nombre_humano) == 0) nombre_humano <- input$censo_interes
      
      unidad <- if(es_porcentaje()) "(%)" else "(Cant. Absoluta)"
      media_val <- mean(dta$valor_final, na.rm = TRUE)
      
      # 2. Creación del gráfico con ggplot2
      p <- ggplot(dta, aes(x = valor_final)) +
        # Histograma interactivo
        # Usamos 'text' para personalizar el tooltip de Plotly
        geom_histogram(
          aes(text = paste0(
            "<b>Rango: </b>", round(after_stat(xmin), 1), " a ", round(after_stat(xmax), 1), unidad, "<br>",
            "<b>Secciones: </b>", after_stat(count), " secciones"
          )),
          fill = APP_CONFIG$color_principal %||% "#3498db", 
          color = "white", # Bordes para distinguir las barras
          bins = 20
        ) +
        # Línea vertical de la media aritmética
        geom_vline(
          aes(xintercept = media_val), 
          color = "#e74c3c", linetype = "dashed", size = 0.8
        ) +
        # Etiquetas profesionales
        labs(
          title = paste("Análisis de:", nombre_humano),
          x = paste(nombre_humano, unidad),
          y = "Frecuencia (Secciones)"
        ) +
        theme_minimal(base_size = 11) +
        theme(
          plot.title = element_text(size = 12, face = "bold", hjust = 0),
          panel.grid.minor = element_blank()
        )
      
      # 3. Conversión a Plotly
      ggplotly(p, tooltip = "text") %>%
        layout(
          # Añadimos una anotación para explicar la línea roja
          annotations = list(
            x = media_val, y = 0, text = "Promedio", 
            showarrow = T, arrowhead = 1, ax = 40, ay = -30,
            font = list(color = "#e74c3c", size = 10)
          ),
          margin = list(t = 50) # Espacio para el título
        ) %>%
        config(displayModeBar = FALSE) # Limpiar la barra de herramientas de plotly
    })
    
    output$mapa_censales <- renderLeaflet({
      dta <- data_final()
      pal <- colorNumeric(palette = "YlGnBu", domain = dta$valor_final)
      
      nombre_var <- names(chs_censales)[chs_censales == input$censo_interes]
      
      leaflet(dta) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          fillColor = ~pal(valor_final),
          fillOpacity = 0.8, weight = 1, color = "#495057",
          label = ~paste0("Sección: ", SECCION, " | ", round(valor_final, 1)),
          popup = ~sprintf(
            "<b>Sección: %s</b><br>Población: %s<br>Escolaridad: %s<br><hr><b>%s: %s</b>",
            SECCION, format(POBTOT, big.mark=","), GRAPROES_NIVEL, 
            nombre_var, paste0(round(valor_final,2), if(es_porcentaje())"%" else "")
          ),
          highlightOptions = highlightOptions(weight = 3, color = "#ff0000", bringToFront = TRUE)
        ) %>%
        addLegend(pal = pal, values = ~valor_final, title = "Escala", position = "bottomright")
    })
    
    # --- 4. TABLAS Y RANKINGS ---
    
    output$dt_top <- renderDT({
      data_final() %>% st_drop_geometry() %>%
        select(SECCION, NOMBRE, valor_final) %>%
        arrange(desc(valor_final)) %>% head(10) %>%
        datatable(options = list(dom = 't'), rownames = FALSE)
    })
    
    output$dt_full <- renderDT({
      data_final() %>% st_drop_geometry() %>%
        select(SECCION, MUNICIPIO, NOMBRE, POBTOT, valor_final) %>%
        datatable(extensions = 'Buttons', options = list(dom = 'Bfrtip', buttons = c('copy', 'excel')))
    })
    
    # --- 5. FUNCIONALIDADES EXTRA ---
    
    observeEvent(input$mapFullscreen, {
      shinyjs::runjs(sprintf("document.getElementById('%s').requestFullscreen();", ns("mapa_censales")))
    })
    
    output$dw_censal_full <- downloadHandler(
      filename = function() { paste0("reporte_demografico_", input$censo_interes, "_", Sys.Date(), ".csv") },
      content = function(file) { write.csv(st_drop_geometry(data_final()), file, row.names = FALSE) }
    )
  })
}