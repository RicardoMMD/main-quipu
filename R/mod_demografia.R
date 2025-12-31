# ==============================================================================
# MÓDULO: Demografía Espacial - Versión Pro
# ==============================================================================

# 1. UI DEL MÓDULO -------------------------------------------------------------
mod_demografia_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(
      # KPIs Superiores
      valueBoxOutput(ns("vbox_total"), width = 4),
      valueBoxOutput(ns("vbox_pct_total"), width = 4),
      valueBoxOutput(ns("vbox_dominante"), width = 4)
    ),
    
    fluidRow(
      # Columna de Controles
      column(width = 3,
             style  = "padding:0rem;",
             box(
               title = "Filtros Demográficos", status = "primary", solidHeader = TRUE, width = 12,
               
               
               # --- BOTÓN DE AYUDA ---
               div(style = "text-align: right; margin-bottom: 10px;",
                   actionButton(ns("btn_info"), " ¿Cómo funciona?", 
                                icon = icon("question-circle"), 
                                class = "btn-info btn-sm") 
               ),
               
               shinyWidgets::pickerInput(
                 ns("edades"), "Grupos de Edad:",
                 choices = c("18 a 24" = "_18_24", "25 a 29" = "_25_29", "30 a 39" = "_30_39", 
                             "40 a 49" = "_40_49", "50 a 59" = "_50_59", "60 y más" = "mayores"),
                 selected = c("_18_24", "_25_29"), multiple = TRUE,
                 options = list(`actions-box` = TRUE, `none-selected-text` = "Seleccione edad")
               ),
               
               shinyWidgets::pickerInput(
                 ns("genero"), "Género:",
                 choices = c("Hombres" = "hombres", "Mujeres" = "mujeres"),
                 selected = c("hombres", "mujeres"), multiple = TRUE
               ),
               
               radioButtons(ns("metrica_mapa"), "Visualizar en mapa por:",
                            choices = c("Concentración (%)" = "prop", "Volumen (Personas)" = "pob_seleccionada"),
                            inline = TRUE),
               
               hr(),
               downloadBttn(ns("download_edades"), "Descargar Base", style = "simple", color = "success", block = TRUE, size = "sm")
             ),
             
             box(
               title = "Composición de Selección", status = "info", solidHeader = TRUE, width = 12,
               plotlyOutput(ns("plot_composicion"), height = "250px")
             )
      ),
      
      # Columna del Mapa
      column(width = 9,
             style  = "padding:0rem;",
             box(
               title = "Análisis Geográfico de Población", status = "primary", solidHeader = TRUE, width = 12,
               leafletOutput(ns("mapa_demografia"), height = "75vh") %>% withSpinner(),
               absolutePanel(
                 bottom = 30, left = 30,
                 actionButton(ns("mapFullscreen"), "", icon = icon("expand"), class = "btn-light")
               )
             )
      )
    )
  )
}

# 2. SERVER DEL MÓDULO ---------------------------------------------------------
mod_demografia_server <- function(id, secciones_reactivas) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Procesamiento de Datos ------------------------------------------------
    datos_completos <- reactive({
      req(input$edades, input$genero, secciones_reactivas())
      
      # Patrones para filtrado
      pat_edad <- paste(input$edades, collapse = "|")
      pat_gen  <- paste(input$genero, collapse = "|")
      
      # Procesar base 'edades' (Long format para cálculos)
      dta_long <- edades %>%
        filter(seccion %in% secciones_reactivas()$SECCION) %>%
        pivot_longer(cols = matches("hombres|mujeres"), names_to = "cat", values_to = "n") %>%
        mutate(
          match_edad = stringr::str_detect(cat, pat_edad),
          match_gen  = stringr::str_detect(cat, pat_gen)
        )
      
      # Agregado por sección
      res_seccion <- dta_long %>%
        group_by(seccion) %>%
        summarise(
          pob_seleccionada = sum(n[match_edad & match_gen], na.rm = TRUE),
          padron_total = max(padron_total, na.rm = TRUE),
          .groups = "drop"
        ) %>%
        mutate(prop = ifelse(padron_total > 0, (pob_seleccionada / padron_total), 0))
      
      # Agregado para Gráfica (Desglose)
      res_plot <- dta_long %>%
        filter(match_edad, match_gen) %>%
        group_by(cat) %>%
        summarise(total = sum(n, na.rm = TRUE)) %>%
        separate(cat, into = c("genero", "rango"), sep = "_", extra = "merge")
      
      return(list(secciones = res_seccion, plot = res_plot))
    })
    
    # 2. KPIs ------------------------------------------------------------------
    output$vbox_total <- renderValueBox({
      total <- sum(datos_completos()$secciones$pob_seleccionada)
      valueBox(scales::comma(total), "Personas Seleccionadas", icon = icon("users"), color = "purple")
    })
    
    output$vbox_pct_total <- renderValueBox({
      secc <- datos_completos()$secciones
      pct <- (sum(secc$pob_seleccionada) / sum(secc$padron_total)) * 100
      valueBox(paste0(round(pct, 1), "%"), "del Padrón en la Zona", icon = icon("percent"), color = "blue")
    })
    
    # 2.2. ValueBox para el Grupo Dominante
    output$vbox_dominante <- renderValueBox({
      req(datos_completos())
      df_plot <- datos_completos()$plot
      
      # Si no hay datos seleccionados
      if (nrow(df_plot) == 0) {
        return(valueBox("N/A", "Grupo Dominante", icon = icon("info"), color = "black"))
      }
      
      # 1. Encontrar la fila con el total máximo
      dominante <- df_plot[which.max(df_plot$total), ]
      
      # 2. Traducción de etiquetas técnicas a nombres amigables
      lbl_gen <- ifelse(dominante$genero == "hombres", "Hombres", "Mujeres")
      
      lbl_edad <- case_when(
        dominante$rango == "_18_24"  ~ "18-24 años",
        dominante$rango == "_25_29"  ~ "25-29 años",
        dominante$rango == "_30_39"  ~ "30-39 años",
        dominante$rango == "_40_49"  ~ "40-49 años",
        dominante$rango == "_50_59"  ~ "50-59 años",
        dominante$rango == "mayores" ~ "60+ años",
        TRUE ~ dominante$rango
      )
      
      # 3. Renderizado del ValueBox
      valueBox(
        value = paste(lbl_gen, lbl_edad),
        subtitle = "Grupo Mayoritario en la Zona",
        icon = icon("medal"),
        color = "orange"
      )
    })
    
    # 3. Gráfico de Composición ------------------------------------------------
    output$plot_composicion <- renderPlotly({
      df <- datos_completos()$plot
      p <- ggplot(df, aes(x = rango, y = total, fill = genero)) +
        geom_col(position = "dodge") +
        scale_fill_manual(values = c("hombres" = "#3498db", "mujeres" = "#e74c3c")) +
        theme_minimal() + labs(x = NULL, y = NULL) +
        theme(legend.position = "none")
      ggplotly(p) %>% config(displayModeBar = F)
    })
    
    # 4. Mapa ------------------------------------------------------------------
    output$mapa_demografia <- renderLeaflet({
      dta <- datos_completos()$secciones
      geo <- secciones_reactivas() %>% 
        mutate(SECCION = as.character(SECCION)) %>%
        left_join(dta %>% mutate(seccion = as.character(seccion)), by = c("SECCION" = "seccion")) %>%
        st_transform(4326)
      
      metrica <- input$metrica_mapa
      pal <- colorNumeric(palette = "viridis", domain = geo[[metrica]])
      
      leaflet(geo) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          fillColor = ~pal(get(metrica)), fillOpacity = 0.7, weight = 1, color = "white",
          popup = ~sprintf(
            "<b>Sección: %s</b><br>Población Seleccionada: %s<br>Padron Total: %s<br>Representación: %s%%",
            SECCION, scales::comma(pob_seleccionada), scales::comma(padron_total), round(prop*100,1)
          ),
          highlightOptions = highlightOptions(weight = 3, color = "cyan", bringToFront = TRUE)
        ) %>%
        addLegend(pal = pal, values = geo[[metrica]], 
                  title = if(metrica == "prop") "Concentración" else "Personas", 
                  position = "bottomright")
    })
    
    # 5. Modal Ayuda -----------------------------------------------------------
    observeEvent(input$btn_info, {
      showModal(modalDialog(
        title = tagList(icon("id-card"), "Guía de Análisis: Demografía Espacial"),
        size = "l",
        easyClose = TRUE,
        footer = modalButton("Entendido"),
        
        fluidRow(
          column(12,
                 h4("Objetivo del Módulo", style = "color: #2c3e50; font-weight: bold;"),
                 p("Este módulo permite segmentar y localizar geográficamente a la población según su rango de edad y género. Es la base para el ", 
                   strong("Micro-targeting"), ", permitiendo que los mensajes de campaña lleguen a las personas adecuadas en las secciones correctas."),
                 
                 hr(),
                 
                 h4("¿Cómo interpretar el mapa?", style = "color: #2c3e50; font-weight: bold;"),
                 p("Dependiendo de tu objetivo, puedes cambiar la métrica de visualización:"),
                 tags$ul(
                   tags$li(strong("Concentración (%):"), " Muestra qué peso tiene el grupo seleccionado frente al total de la sección. ", 
                           em("Útil para decidir contenido de lonas o publicidad exterior.")),
                   tags$li(strong("Volumen (Personas):"), " Muestra el número bruto de personas. ", 
                           em("Útil para logística de tierra y rutas de brigadas.") )
                 ),
                 
                 div(style = "background-color: #fcf8e3; padding: 15px; border-left: 5px solid #8a6d3b; margin: 15px 0;",
                     h5(icon("lightbulb"), strong(" Tip Estratégico")),
                     p("Si el gráfico de composición muestra que las ", strong("Mujeres de 40-49"), " son el grupo dominante en una zona, 
              tus propuestas de salud o economía deben priorizar ese perfil en las visitas domiciliarias de esas secciones.")
                 ),
                 
                 h4("Consideraciones Importantes", style = "color: #c0392b; font-weight: bold;"),
                 tags$ul(
                   tags$li("Los datos provienen del Padrón Electoral / Censo, segmentados por las secciones oficiales vigentes."),
                   tags$li("Las secciones en ", strong("Gris"), " o transparentes indican que no hay población reportada para los criterios seleccionados o son zonas no habitacionales."),
                   tags$li("Recuerda que la demografía te da el potencial de contacto, pero la ", em("Fortaleza Electoral"), " te dirá si ese contacto es propenso a votarte.")
                 )
          )
        )
      ))
    })
    
    # 6. Fullscreen y Descarga
    observeEvent(input$mapFullscreen, {
      shinyjs::runjs(sprintf("document.getElementById('%s').requestFullscreen();", ns("mapa_demografia")))
    })
    
    output$download_edades <- downloadHandler(
      filename = function() { paste0("demografia_", Sys.Date(), ".csv") },
      content = function(file) { write.csv(st_drop_geometry(datos_completos()$secciones), file, row.names = FALSE) }
    )
  })
}