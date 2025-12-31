# ==============================================================================
# UI - Módulo Colonias (Perfilador Táctico)
# ==============================================================================

mod_colonias_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyjs(),
    # 1. Panel de Control y Búsqueda
    fluidRow(
      box(
        width = 12,
        title = "Buscador de Inteligencia Territorial",
        status = "primary",
        solidHeader = TRUE,
        collapsible = FALSE,
        column(
          width = 8,
          # Selectize es mejor para búsquedas con miles de opciones
          selectizeInput(ns("sel_colonia"), 
                         "Escribe o selecciona la Colonia:", 
                         choices = NULL, 
                         options = list(placeholder = 'Escribe para buscar...', maxOptions = 100))
        ),
        column(
          width = 4,
          # Mostrar qué sección electoral representa esta colonia
          h5("Sección Electoral Asociada:"),
          h3(textOutput(ns("txt_seccion_id")), style = "margin-top:0; color:#3c8dbc; font-weight:bold;")
        )
      )
    ),
    
    # 2. Tarjetas de Indicadores (KPIs) - "La Radiografía"
    fluidRow(
      # Nivel Socioeconómico Proxy
      div(
        id = ns("box_nse_click"), # ID para detectar el clic
        style = "cursor: pointer;", # Cambia el cursor a 'manita' para indicar clic
        valueBoxOutput(ns("kpi_nse"), width = 3)
      ),
      # Población Potencial
      valueBoxOutput(ns("kpi_poblacion"), width = 3),
      # Escolaridad
      valueBoxOutput(ns("kpi_educacion"), width = 3),
      # Ganador Reciente
      valueBoxOutput(ns("kpi_ganador24"), width = 3)
    ),
    
    # 3. Cuerpo Principal: Mapa y Análisis
    fluidRow(
      # Columna Izquierda: Ubicación y Estrategia
      column(
        width = 6,
        box(
          width = 12,
          title = "Ubicación Geográfica (Sección)",
          status = "info",
          height = "400px",
          leafletOutput(ns("mapa_colonia"), height = "350px")
        ),
        # Caja de Recomendación Estratégica
        box(
          width = 12,
          title = "Tip de Campaña (Micro-Targeting)",
          status = "warning",
          solidHeader = TRUE,
          uiOutput(ns("txt_estrategia"))
        ),
        style = "padding-right: 0.1rem;padding-left: 0.1rem;"
      ),
      
      # Columna Derecha: Datos Duros
      column(
        width = 6,
        # Pestañas para no saturar
        tabBox(
          width = 12,
          height = "550px",
          title = "Perfil Detallado",
          # Pestaña 1: Infraestructura (¿Tienen auto? ¿Internet?)
          tabPanel(
            "Entorno y Conectividad",
            icon = icon("wifi"),
            plotOutput(ns("plot_conectividad"), height = "200px"),
            hr(),
            p("Datos del Censo 2020 por Sección Electoral.", style="font-size:0.8em; color:gray;")
          ),
          # Pestaña 2: Historia Electoral
          tabPanel(
            "Historial de Voto",
            icon = icon("vote-yea"),
            tableOutput(ns("tbl_historia_politica"))
          )
        ),
        style = "padding-right: 0.1rem;padding-left: 0.1rem;"
      )
    )
  )
}


# ==============================================================================
# SERVER - Módulo Colonias
# ==============================================================================

mod_colonias_server <- function(id, secciones_reactivas) {
  moduleServer(id, function(input, output, session) {
    
    # --------------------------------------------------------------------------
    # 1. ACTUALIZAR SELECTOR DE COLONIAS
    # --------------------------------------------------------------------------
    # Filtramos las colonias disponibles basándonos en el municipio seleccionado globalmente
    # (que viene filtrado dentro de secciones_reactivas)
    
    colonias_filtradas <- reactive({
      req(secciones_reactivas())
      
      # Obtenemos los municipios presentes en la geometría actual
      mun_ids <- unique(secciones_reactivas()$MUNICIPIO)
      
      # Filtramos el dataset 'colonias'
      colonias %>% 
        filter(MUNICIPIO %in% mun_ids) %>%
        select(COLONIA, SECCIÓN, MUNICIPIO) %>%
        arrange(COLONIA)
    })
    
    observeEvent(colonias_filtradas(), {
      # Usamos unique() por si una colonia aparece duplicada
      opts <- unique(colonias_filtradas()$COLONIA)
      updateSelectizeInput(session, "sel_colonia", choices = opts, server = TRUE)
    })
    
    
    # --------------------------------------------------------------------------
    # 2. CEREBRO REACTIVO: Obtener datos de la colonia seleccionada
    # --------------------------------------------------------------------------
    info_colonia_data <- reactive({
      req(input$sel_colonia)
      req(colonias_filtradas())
      
      # a) Identificar Sección
      # Nota: Una colonia puede estar asociada a una sección. 
      # Usamos la primera coincidencia o la más representativa.
      dato_colonia <- colonias_filtradas() %>% 
        filter(COLONIA == input$sel_colonia) %>%
        head(1) # Tomamos 1 para evitar duplicados en lógica simple
      
      seccion_id <- dato_colonia$SECCIÓN
      municipio_id <- dato_colonia$MUNICIPIO
      
      # b) Cruzar con Censo 2020 (data_secc_cpv2020)
      # Buscamos la fila que coincida con MUNICIPIO y SECCION
      censo_target <- data_secc_cpv2020 %>% 
        filter(MUNICIPIO == municipio_id, SECCION == seccion_id)
      
      # c) Cruzar con Electoral (base_ganadores)
      electoral_target <- base_ganadores %>% 
        filter(seccion == seccion_id) # OJO: Asegúrate que 'seccion' en base_ganadores sea compatible (numérico)
      
      # d) Cruzar con Geometría (secciones_reactivas) para el mapa
      geo_target <- secciones_reactivas() %>% 
        filter(SECCION == seccion_id) # Asegúrate de que coincida formato char/num
      
      return(list(
        meta = dato_colonia,
        censo = censo_target,
        electoral = electoral_target,
        geo = geo_target,
        seccion_id = seccion_id
      ))
    })
    
    
    # --------------------------------------------------------------------------
    # 3. VISUALIZACIONES Y SALIDAS
    # --------------------------------------------------------------------------
    
    output$txt_seccion_id <- renderText({
      req(info_colonia_data())
      paste0("SECCIÓN ", info_colonia_data()$seccion_id)
    })
    
    # --- KPI: NIVEL SOCIOECONÓMICO (Calculado) ---
    output$kpi_nse <- renderValueBox({
      req(info_colonia_data())
      dat <- info_colonia_data()$censo
      
      if(nrow(dat) == 0) return(valueBox("S/D", "Sin Datos Censo", color = "black"))
      
      # Lógica Proxy de NSE:
      # Usamos % de viviendas con Auto e Internet y Grado Escolaridad
      viv_totales <- as.numeric(dat$TVIVHAB)
      if(viv_totales == 0) viv_totales <- 1
      
      perc_auto <- (as.numeric(dat$VPH_AUTOM) / viv_totales) * 100
      perc_inter <- (as.numeric(dat$VPH_INTER) / viv_totales) * 100
      escolaridad <- as.numeric(dat$GRAPROES)
      
      # Clasificación simple
      etiqueta <- "NSE Popular (D/E)"
      color_box <- "red"
      icono <- icon("hand-holding-usd")
      
      if (perc_auto > 85 & escolaridad > 12) {
        etiqueta <- "NSE Alto (A/B)"
        color_box <- "navy"
        icono <- icon("gem")
      } else if (perc_auto > 50 & escolaridad > 9) {
        etiqueta <- "NSE Medio (C)"
        color_box <- "light-blue"
        icono <- icon("briefcase")
      }
      
      valueBox(
        value = etiqueta,
        subtitle = "Perfil Socioeconómico Est.",
        icon = icono,
        color = color_box
      )
    })
    
    shinyjs::onclick("box_nse_click", {
      
      req(info_colonia_data())
      dat <- info_colonia_data()$censo
      if(nrow(dat) == 0) return(NULL)
      
      # Datos de la colonia actual
      viv_totales <- as.numeric(dat$TVIVHAB)
      if(viv_totales == 0) viv_totales <- 1
      
      val_auto <- round((as.numeric(dat$VPH_AUTOM) / viv_totales) * 100, 1)
      val_esco <- round(as.numeric(dat$GRAPROES), 1)
      
      showModal(modalDialog(
        title = HTML("📊 <b>Metodología de Clasificación NSE</b>"),
        size = "m", # Tamaño mediano
        easyClose = TRUE,
        footer = modalButton("Cerrar"),
        
        tagList(
          # 1. Datos de ESTA Colonia (Resumen)
          h4("Diagnóstico de esta Colonia:", style = "color:#3c8dbc; font-weight:bold; margin-top:0;"),
          tags$ul(
            tags$li(strong("Viviendas con Auto: "), val_auto, "%"),
            tags$li(strong("Escolaridad Promedio: "), val_esco, " Años")
          ),
          
          hr(),
          
          # 2. La Tabla de Criterios (LO QUE PEDISTE)
          h5("Reglas de Clasificación Utilizadas:"),
          p("El sistema asigna la categoría basándose en si la colonia supera los siguientes umbrales:", style="font-size:0.9em; color:gray;"),
          
          tags$table(class = "table table-bordered table-condensed",
                     tags$thead(
                       tags$tr(style = "background-color: #f5f5f5;",
                               tags$th("Clasificación"),
                               tags$th("% Autos (Umbral)"),
                               tags$th("Escolaridad (Umbral)")
                       )
                     ),
                     tags$tbody(
                       # Fila NSE ALTO
                       tags$tr(style = "background-color: #e8f0fe;", # Azul muy claro
                               tags$td(strong("NSE Alto (A/B)"), icon("gem", style="color:navy; font-size:10px;")),
                               tags$td("> 85%"),
                               tags$td("> 12 Años (Prepa+)")
                       ),
                       # Fila NSE MEDIO
                       tags$tr(style = "background-color: #e0f7fa;", # Cyan muy claro
                               tags$td(strong("NSE Medio (C)"), icon("briefcase", style="color:#00c0ef; font-size:10px;")),
                               tags$td("> 50%"),
                               tags$td("> 9 Años (Secundaria+)")
                       ),
                       # Fila NSE POPULAR
                       tags$tr(style = "background-color: #ffebee;", # Rojo muy claro
                               tags$td(strong("NSE Popular (D/E)"), icon("hand-holding-usd", style="color:red; font-size:10px;")),
                               tags$td("Menor al 50%"),
                               tags$td("Menor a 9 Años")
                       )
                     )
          ),
          
          # 3. Nota al pie
          div(style = "background-color: #fff3cd; padding: 10px; border-radius: 5px; font-size: 0.85em;",
              icon("lightbulb"), 
              strong("Nota:"), " Si una colonia tiene muchos autos pero baja escolaridad (o viceversa), el algoritmo prioriza el escenario más conservador (hacia abajo) para evitar sobreestimar la riqueza."
          )
        )
      ))
    })
    
    # --- KPI: POBLACIÓN ---
    output$kpi_poblacion <- renderValueBox({
      req(info_colonia_data())
      dat <- info_colonia_data()$censo
      val <- if(nrow(dat) > 0) format(dat$POBTOT, big.mark=",") else "0"
      
      valueBox(
        value = val,
        subtitle = "Población Total (Censo 2020)",
        icon = icon("users"),
        color = "purple"
      )
    })
    
    # --- KPI: EDUCACIÓN ---
    output$kpi_educacion <- renderValueBox({
      req(info_colonia_data())
      dat <- info_colonia_data()$censo
      val <- if(nrow(dat) > 0) paste(round(as.numeric(dat$GRAPROES), 1), "Años") else "S/D"
      
      valueBox(
        value = val,
        subtitle = "Escolaridad Promedio",
        icon = icon("user-graduate"),
        color = "teal"
      )
    })
    
    # --- KPI: GANADOR 2024 (ALCALDE) ---
    output$kpi_ganador24 <- renderValueBox({
      req(info_colonia_data())
      elec <- info_colonia_data()$electoral
      
      # Ajusta el nombre de la columna según tu base_ganadores (ej. alcalde_24)
      ganador <- if(nrow(elec) > 0) elec$alcalde_24 else "S/D"
      
      # Color dinámico por partido
      color_p <- switch(tolower(ganador),
                        "pan" = "blue",
                        "pri" = "green",
                        "mc" = "orange",
                        "morena" = "maroon",
                        "black") # Default
      
      valueBox(
        value = ganador,
        subtitle = "Ganador Alcaldía 2024",
        icon = icon("trophy"),
        color = color_p
      )
    })
    
    # --- MAPA: LEAFLET ZOOM ---
    # output$mapa_colonia <- renderLeaflet({
    #   req(info_colonia_data())
    #   geo <- info_colonia_data()$geo
    #   
    #   if(nrow(geo) == 0) return(NULL)
    #   
    #   geo_transf <- sf::st_transform(geo, 4326)
    #   browser()
    #   leaflet(geo_transf) %>%
    #     addProviderTiles(providers$CartoDB.Positron) %>%
    #     addPolygons(
    #       color = "#d62728",
    #       weight = 3,
    #       opacity = 1,
    #       fillOpacity = 0.2,
    #       highlightOptions = highlightOptions(weight = 5, color = "#666", bringToFront = TRUE),
    #       popup = ~paste0("<b>Sección:</b> ", SECCION, "<br>",
    #                       "<b>Ganador '24:</b> ", winner_variable)
    #     )
    # })
    
    output$mapa_colonia <- renderLeaflet({
      req(info_colonia_data())
      
      # 1. Extraer datos (Unpacking)
      datos <- info_colonia_data()
      geo_raw <- datos$geo
      meta    <- datos$meta
      censo   <- datos$censo
      elec    <- datos$electoral
      
      if(nrow(geo_raw) == 0) return(NULL)
      
      # 2. Transformar Geo a Lat/Lon (IMPORTANTE para Leaflet)
      geo_transf <- sf::st_transform(geo_raw, 4326)
      
      # 3. Preparar Variables para el Popup
      # Usamos 'tryCatch' o validaciones simples por si faltan datos en alguna sección
      
      nombre_colonia <- meta$COLONIA
      seccion_id     <- meta$SECCIÓN
      
      # Población (con formato de miles: 1,342)
      pob_total <- if(nrow(censo) > 0) format(censo$POBTOT, big.mark=",") else "S/D"
      
      # Ganadores (Validar si existe el dato)
      ganador_alc <- if(nrow(elec) > 0) elec$alcalde_24 else "Sin Dato"
      ganador_gob <- if(nrow(elec) > 0) elec$gober_21 else "Sin Dato" # Ejemplo histórico
      
      # Colores para badges
      color_alc <- get_color_partido(ganador_alc)
      
      # 4. Construir el HTML del Popup (Estilo Tarjeta)
      contenido_popup <- paste0(
        "<div style='font-family: sans-serif; min-width: 200px;'>",
        
        # Encabezado con color de fondo gris suave
        "<div style='background-color: #f7f7f7; padding: 10px; border-bottom: 2px solid #3c8dbc; border-radius: 5px 5px 0 0;'>",
        "<h4 style='margin:0; font-weight:bold; color: #333;'>Sección ", seccion_id, "</h4>",
        "<small style='color: #777;'>", nombre_colonia, "</small>",
        "</div>",
        
        # Cuerpo de la tarjeta
        "<div style='padding: 10px;'>",
        
        # Dato Demográfico
        "<div style='margin-bottom: 8px;'>",
        "<i class='fa fa-users' style='color:#3c8dbc;'></i> <b>Población:</b> ", pob_total, " hab.",
        "</div>",
        
        "<hr style='margin: 5px 0;'>",
        
        # Dato Electoral (Badge del Ganador)
        "<div style='margin-top: 8px;'>",
        "<small>Ganador Alcaldía '24:</small><br>",
        "<span style='background-color:", color_alc, "; color: white; padding: 3px 8px; border-radius: 4px; font-weight:bold; display: inline-block; margin-top:3px;'>",
        ganador_alc,
        "</span>",
        "</div>",
        
        # Dato Histórico (Texto simple)
        "<div style='margin-top: 5px; font-size: 0.9em; color: #555;'>",
        "<i>Gobernador '21: ", ganador_gob, "</i>",
        "</div>",
        
        "</div>",
        "</div>"
      )
      
      # 5. Renderizar Mapa
      leaflet(geo_transf) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          color = "#d62728",      # Borde Rojo
          weight = 3,
          opacity = 1,
          fillColor = color_alc,  # Relleno del color del partido ganador (Opcional, muy visual)
          fillOpacity = 0.4,
          
          # --- LABEL (HOVER) ---
          # Muestra info rápida al pasar el mouse
          label = lapply(paste0("<strong>Sección ", seccion_id, "</strong><br>", nombre_colonia), htmltools::HTML),
          labelOptions = labelOptions(
            style = list("font-weight" = "normal", padding = "3px 8px"),
            textsize = "13px",
            direction = "auto"
          ),
          
          # --- POPUP (CLICK) ---
          # Muestra la tarjeta HTML detallada
          popup = contenido_popup,
          
          # Resaltado al pasar el mouse
          highlightOptions = highlightOptions(
            weight = 5,
            color = "#666",
            fillOpacity = 0.7,
            bringToFront = TRUE
          )
        )
    })
    
    # --- ESTRATEGIA: RECOMENDACIÓN AUTOMÁTICA ---
    output$txt_estrategia <- renderUI({
      req(info_colonia_data())
      dat <- info_colonia_data()$censo
      if(nrow(dat) == 0) return(HTML("Sin datos para generar estrategia."))
      
      # Calcular % Internet
      viv_totales <- as.numeric(dat$TVIVHAB)
      if(viv_totales == 0) viv_totales <- 1
      perc_inter <- (as.numeric(dat$VPH_INTER) / viv_totales) * 100
      
      texto <- ""
      
      if (perc_inter > 75) {
        texto <- "🎯 <b>Estrategia Digital:</b> Alta penetración de Internet. Priorizar pauta segmentada en redes sociales (Meta/Google). El mensaje llega directo al celular."
      } else if (perc_inter > 45) {
        texto <- "📢 <b>Estrategia Híbrida:</b> Combinar recorrido territorial con grupos de WhatsApp vecinales. Buen balance digital/físico."
      } else {
        texto <- "👟 <b>Estrategia Territorial:</b> Baja conectividad digital. Indispensable recorrido casa por casa (Tierra), volanteo y eventos presenciales."
      }
      
      HTML(texto)
    })
    
    # --- GRÁFICO: CONECTIVIDAD (GGPLOT) ---
    output$plot_conectividad <- renderPlot({
      req(info_colonia_data())
      dat <- info_colonia_data()$censo
      if(nrow(dat) == 0) return(NULL)
      
      viv_totales <- as.numeric(dat$TVIVHAB)
      if(viv_totales == 0) viv_totales <- 1
      
      # Crear DF para gráfico
      df_plot <- data.frame(
        Servicio = c("Internet", "Automóvil", "Celular", "PC/Laptop"),
        Valor = c(as.numeric(dat$VPH_INTER), as.numeric(dat$VPH_AUTOM), 
                  as.numeric(dat$VPH_CEL), as.numeric(dat$VPH_PC))
      ) %>%
        mutate(Porcentaje = (Valor / viv_totales) * 100)
      
      ggplot(df_plot, aes(x = reorder(Servicio, Porcentaje), y = Porcentaje)) +
        geom_col(fill = "#3c8dbc", width = 0.6) +
        coord_flip() +
        geom_text(aes(label = paste0(round(Porcentaje, 0), "%")), hjust = -0.2, fontface="bold") +
        theme_minimal() +
        labs(title = "% de Viviendas con Servicio", x = "", y = "") +
        ylim(0, 110) # Margen para etiquetas
    })
    
    # --- TABLA: HISTORIA ELECTORAL ---
    output$tbl_historia_politica <- renderTable({
      req(info_colonia_data())
      elec <- info_colonia_data()$electoral
      
      if(nrow(elec) == 0) return(data.frame(Mensaje = "Sin historia electoral registrada"))
      
      # Transponer para mostrar verticalmente (Elección | Ganador)
      elec %>%
        select(starts_with("alcalde"), starts_with("dip"), starts_with("pres"), starts_with("gober")) %>%
        pivot_longer(cols = everything(), names_to = "Elección", values_to = "Partido Ganador") %>%
        mutate(Elección = toupper(gsub("_", " ", Elección)))
    }, striped = TRUE, hover = TRUE, width = "100%")
    
  })
}



