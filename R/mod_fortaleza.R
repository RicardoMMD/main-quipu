# ==============================================================================
# MÓDULO: Fortaleza Electoral (Modelo Predictivo Pro)
# ==============================================================================

# 1. UI DEL MÓDULO -------------------------------------------------------------
mod_fortaleza_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    fluidRow(uiOutput(ns("kpi_cards"))),
    
    fluidRow(
      column(width = 4,
             style  = "padding:0rem;",
             box(
               title = "Configuración del Modelo", status = "primary", solidHeader = TRUE, width = 12,
               
               div(style = "text-align: right; margin-bottom: 10px;",
                   actionButton(ns("btn_info"), " ¿Cómo funciona?", 
                                icon = icon("question-circle"), 
                                class = "btn-info btn-sm") 
               ),
               
               shinyWidgets::pickerInput(
                 ns("partido_predecir"), "Partido a Analizar:", 
                 choices = c("morena", "PAN", "PRI", "MC", "VERDE", "PT"),
                 options = list(`live-search` = TRUE), selected = "morena"
               ),
               # --- ACTUALIZACIÓN DE TEXTO ---
               helpText("El modelo calcula la probabilidad de victoria basándose en la Presidencial 2024 y su relación con la historia (2018-2021) y el contexto local actual."),
               hr(),
               downloadButton(ns("download_fortalezas"), "Exportar Probabilidades", class = "btn-block")
             ),
             box(
               title = "Peso de las Variables", status = "info", solidHeader = TRUE, width = 12,
               plotlyOutput(ns("plot_importance"), height = "300px")
             )
      ),
      
      column(width = 8,
             style  = "padding:0rem;",
             tabBox(
               title = tagList(icon("shield-alt"), "Fortaleza 2024"),
               id = ns("tabs_fortaleza"), width = 12,
               tabPanel("Mapa de Probabilidades", 
                        leafletOutput(ns("mapa_fortaleza"), height = "70vh")),
               tabPanel("Ranking de Secciones", 
                        DTOutput(ns("tabla_prioridad")))
             )
      )
    )
  )
}

# 2. SERVER DEL MÓDULO ---------------------------------------------------------
mod_fortaleza_server <- function(id, secciones_reactivas) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Lógica del Modelo (Objetivo: Presidencial 2024) -----------------------
    resultados_modelo <- reactive({
      req(input$partido_predecir)
      
      # Selección de variables según tus metadatos
      df_model <- base_ganadores %>%
        select(
          seccion, 
          
          # --- VARIABLE OBJETIVO (TARGET) ---
          pres_24, 
          
          # --- PREDICTORES 2024 (Contexto concurrente) ---
          alcalde_24, 
          dip_local_24, 
          dip_fed24,  # Ojo: en tus metadatos aparece sin guion bajo
          sen_24,
          
          # --- PREDICTORES 2021 (Historia reciente) ---
          alcalde_21, 
          gober_21, 
          dip_local_21, 
          dip_fed_21,
          
          # --- PREDICTORES 2018 (Historia base) ---
          alcalde_18, 
          dip_local_18, 
          senado_18, 
          pres_18, 
          dip_fed18   # Ojo: en tus metadatos aparece sin guion bajo
        ) %>%
        # Definir éxito (1) o fracaso (0) en la Presidencial 2024
        mutate(
          target = ifelse(!is.na(pres_24) & pres_24 == input$partido_predecir, 1, 0)
        ) %>%
        # Eliminamos la columna original del target para no predecir Y con Y
        select(-pres_24) %>%
        # Convertimos todo a factores para la regresión logística
        mutate(across(where(is.character), as.factor))
      
      # Entrenamiento del Modelo
      # Usamos na.exclude para que las filas con NAs no rompan el modelo pero se mantengan en el df
      fit <- tryCatch({
        glm(target ~ ., 
            data = df_model %>% select(-seccion), 
            family = binomial(link = "logit"),
            na.action = na.exclude)
      }, error = function(e) return(NULL))
      
      if(is.null(fit)) return(NULL)
      
      # Cálculo de Probabilidades
      df_model$prob <- predict(fit, type = "response")
      
      # Categorización (Semaforización estratégica)
      df_model <- df_model %>%
        mutate(
          nivel = case_when(
            is.na(prob) ~ "Sin datos suficientes",
            prob >= 0.85 ~ "Bastión (Muy Alta)",
            prob >= 0.65 ~ "Fortaleza Alta",
            prob >= 0.40 ~ "Zona de Disputa",
            prob >= 0.20 ~ "Debilidad / Oposición",
            TRUE ~ "Territorio Perdido"
          ),
          nivel = factor(nivel, levels = c("Bastión (Muy Alta)", "Fortaleza Alta", "Zona de Disputa", 
                                           "Debilidad / Oposición", "Territorio Perdido", "Sin datos suficientes"))
        )
      
      # Extracción de Importancia de Variables (Coeficientes)
      importance <- as.data.frame(summary(fit)$coefficients)
      importance$variable <- rownames(importance)
      
      return(list(data = df_model, model = fit, importance = importance))
    })
    
    # 2. Modal de Ayuda (Actualizado) ------------------------------------------
    observeEvent(input$btn_info, {
      showModal(modalDialog(
        title = tagList(icon("info-circle"), "Modelo de Fortaleza Electoral 2024"),
        size = "l", easyClose = TRUE,
        footer = modalButton("Cerrar"),
        
        fluidRow(
          column(12,
                 h4("Objetivo", style = "color: #2c3e50; font-weight: bold;"),
                 p("Este módulo entrena un algoritmo de Inteligencia Artificial (Regresión Logística) para entender ",
                   strong("qué hace que una sección vote por el partido seleccionado en la Presidencial 2024.")),
                 
                 hr(),
                 h4("Variables Analizadas", style = "color: #2c3e50; font-weight: bold;"),
                 tags$ul(
                   tags$li(strong("Históricas (2018-2021):"), "Miden la lealtad y la inercia del territorio."),
                   tags$li(strong("Concurrentes (2024):"), "Miden el 'voto parejo'. Por ejemplo, ¿qué tanto ayuda ganar la alcaldía local para ganar la presidencia?")
                 ),
                 
                 div(style = "background-color: #e8f4fd; padding: 15px; border-radius: 10px; margin: 15px 0;",
                     h5(icon("lightbulb"), strong(" Interpretación:")),
                     p("Si una sección aparece como 'Bastión' pero en la realidad se perdió, indica una anomalía grave (traición operativa o cambio demográfico abrupto). Si aparece como 'Zona de Disputa' y se ganó, fue un excelente trabajo de campaña.")
                 )
          )
        )
      ))
    })
    
    # 3. Renders (KPIs, Plot, Mapa) --------------------------------------------
    output$kpi_cards <- renderUI({
      res_list <- resultados_modelo(); req(res_list)
      res <- res_list$data
      
      # KPIs simples
      bastiones <- sum(res$nivel == "Bastión (Muy Alta)", na.rm=T)
      disputa   <- sum(res$nivel == "Zona de Disputa", na.rm=T)
      prob_avg  <- round(mean(res$prob, na.rm=T)*100, 1)
      
      tagList(
        valueBox(bastiones, "Bastiones Identificados", icon=icon("chess-rook"), color="green", width=4),
        valueBox(disputa, "Secciones en Disputa", icon=icon("balance-scale"), color="orange", width=4),
        valueBox(paste0(prob_avg, "%"), "Probabilidad Promedio", icon=icon("chart-line"), color="blue", width=4)
      )
    })
    
    output$plot_importance <- renderPlotly({
      res_list <- resultados_modelo(); req(res_list)
      
      # Limpieza de nombres para el gráfico
      imp <- res_list$importance %>% 
        filter(variable != "(Intercept)") %>%
        arrange(desc(abs(Estimate))) %>% 
        head(12) %>%
        mutate(variable = str_remove_all(variable, "partido_predecir|TRUE|FALSE")) # Limpieza visual
      
      p <- ggplot(imp, aes(x = reorder(variable, Estimate), y = Estimate, fill = Estimate > 0)) +
        geom_col() + 
        coord_flip() +
        scale_fill_manual(values = c("#e74c3c", "#2ecc71"), guide = "none") +
        theme_minimal() + 
        labs(x = NULL, y = "Influencia en Voto Presidencial '24")
      
      ggplotly(p) %>% config(displayModeBar = F)
    })
    
    output$mapa_fortaleza <- renderLeaflet({
      res_list <- resultados_modelo(); req(res_list)
      res <- res_list$data
      
      # Join Espacial: Usamos secciones_reactivas() para respetar permisos de usuario
      geo <- secciones_reactivas() %>% 
        mutate(SECCION = as.character(SECCION)) %>%
        left_join(res %>% mutate(seccion = as.character(seccion)), by = c("SECCION" = "seccion"))
      # st_transform(4326) ya no es necesario si ya convertiste todo
      
      pal <- colorFactor(
        palette = c("#006d2c", "#74c476", "#fed976", "#fb6a4a", "#de2d26", "#bdc3c7"),
        domain = geo$nivel
      )
      
      leaflet(geo) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          fillColor = ~pal(nivel), 
          fillOpacity = 0.7, 
          weight = 1, 
          color = "white",
          popup = ~sprintf(
            "<b>Sección: %s</b><br>
             Estatus: <b>%s</b><br>
             Probabilidad Modelo: %s%%<br>
             <i>Ganador Pres. '24 (Real): %s</i>", 
            SECCION, nivel, round(prob*100,1), target
          ),
          highlightOptions = highlightOptions(weight = 3, color = "#666", bringToFront = TRUE)
        ) %>%
        addLegend(pal = pal, values = ~nivel, title = "Fortaleza Electoral (Modelo 2024)", position = "bottomright")
    })
    
    output$tabla_prioridad <- renderDT({
      res_list <- resultados_modelo(); req(res_list)
      res_list$data %>% 
        select(Sección = seccion, `Prob. Victoria` = prob, Estatus = nivel) %>%
        arrange(desc(`Prob. Victoria`)) %>% 
        datatable(options = list(pageLength = 10)) %>% 
        formatPercentage("Prob. Victoria", 1)
    })
    
    output$download_fortalezas <- downloadHandler(
      filename = function() { paste0("fortaleza_2024_", input$partido_predecir, "_", Sys.Date(), ".csv") },
      content = function(file) { write.csv(resultados_modelo()$data, file, row.names = FALSE) }
    )
  })
}