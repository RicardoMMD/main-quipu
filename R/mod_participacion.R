# ==============================================================================
# MÓDULO: Participación Electoral - REFACTORIZADO
# ==============================================================================

# 1. UI DEL MÓDULO -------------------------------------------------------------
mod_participacion_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # --- CSS Específico para corregir el Scroll ---
    tags$head(
      tags$style(HTML("
        /* Tarjetas KPI */
        .kpi-card {
          background-color: #f8f9fa;
          border-left: 5px solid #0073b7;
          padding: 15px;
          border-radius: 4px;
          box-shadow: 0 1px 3px rgba(0,0,0,0.1);
          margin-bottom: 20px;
        }
        .kpi-title { font-size: 12px; text-transform: uppercase; color: #6c757d; font-weight: 600; }
        .kpi-value { font-size: 28px; font-weight: 700; color: #2c3e50; line-height: 1.2; }
        
        /* CORRECCIÓN SCROLL: Quitamos padding al cuerpo de la caja del mapa */
        .map-box .box-body {
          padding: 0 !important;
        }
        
        /* Sombra opcional para resaltar el mapa */
        .map-container {
          box-shadow: 0 4px 6px rgba(0,0,0,0.1);
        }
      "))
    ),
    
    fluidRow(
      style = "margin: 0;", # Evita scroll horizontal por márgenes negativos
      
      # --- Panel Lateral ---
      column(
        width = 3,
        style = "padding-left: 5px; padding-right: 5px;", # Ajuste fino de espaciado
        
        box(
          title = tagList(icon("sliders-h"), "Filtros"), 
          status = "primary", 
          solidHeader = TRUE, 
          width = 12,
          
          shinyWidgets::pickerInput(
            inputId = ns("tipo_dato"),
            label = "Variable a visualizar:",
            choices = c("Lista Nominal", "Tasa de Participación" = "participacion", "Votos Totales" = "votos"),
            selected = "Lista Nominal",
            options = list(`style` = "btn-primary", `icon-base` = "fa")
          ),
          
          hr(),
          uiOutput(ns("info_resumen")),
          hr(),
          
          shinyWidgets::downloadBttn(
            outputId = ns("descargar_data"), 
            label = "Exportar CSV", 
            style = "material-flat", 
            color = "success", 
            size = "sm", 
            block = TRUE
          )
        )
      ),
      
      # --- Mapa (Sin Scroll Doble) ---
      column(
        width = 9,
        style = "padding-left: 5px; padding-right: 5px;",
        
        div(
          class = "map-container map-box", # Clase personalizada agregada arriba
          box(
            width = 12, 
            title = tagList(icon("map-marked-alt"), "Geografía Electoral"), 
            status = "info", 
            solidHeader = TRUE,
            
            # --- CAMBIO IMPORTANTE AQUÍ ---
            # Usamos calc(100vh - Xpx) donde X es aprox la altura del header + márgenes
            shinycssloaders::withSpinner(
              leafletOutput(ns("mapa_participacion"), height = "calc(100vh - 130px)"),
              type = 4, 
              color = "#0073b7"
            )
          )
        )
      )
    )
  )
}




# 2. SERVER DEL MÓDULO ---------------------------------------------------------
mod_participacion_server <- function(id, secciones_base) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # 1. Preparación de Geometría
    geo_base <- reactive({
      req(secciones_base())
      shp <- secciones_base()
      
      cols <- names(shp)
      col_sec <- intersect(cols, c("SECCION", "Seccion", "CLAVE", "ID"))[1]
      col_nom <- intersect(cols, c("NOMBRE", "MUNICIPIO", "NOM_MUN", "NOMGEO"))[1]
      
      if (!is.na(col_sec)) {
        shp %>%
          transmute(
            SECCION_JOIN = as.character(.data[[col_sec]]), 
            NOMBRE_SHOW  = if(!is.na(col_nom)) .data[[col_nom]] else "Municipio"
          ) %>%
          st_transform(crs = 4326)
      } else {
        NULL 
      }
    })
    
    # 2. Preparación de Datos
    datos_mapa <- reactive({
      req(geo_base(), input$tipo_dato)
      geo <- geo_base()
      

      df_part <- participacion %>% 
        as.data.frame() %>%
        mutate(SECCION = as.character(SECCION)) %>% 
        select(-GEOMETRY1_)
      
      col_variable <- case_when(
        input$tipo_dato == "Lista Nominal" ~ "lista",
        input$tipo_dato == "participacion" ~ "participacion", 
        input$tipo_dato == "votos" ~ "votos",
        TRUE ~ "lista"
      )
      
      if(!col_variable %in% names(df_part)) return(NULL)
      
      geo %>%
        left_join(df_part, by = c("SECCION_JOIN" = "SECCION")) %>%
        mutate(
          valor_raw = .data[[col_variable]],
          valor_mapa = ifelse(is.finite(valor_raw), valor_raw, 0)
        )
    })
    
    # 3. Renderizado del Mapa (Actualizado con Plugin Fullscreen)
    output$mapa_participacion <- renderLeaflet({
      dta <- datos_mapa()
      shiny::validate(need(!is.null(dta) && nrow(dta) > 0, "Cargando datos geográficos..."))
      
      es_porcentaje <- input$tipo_dato == "participacion"
      pal <- colorNumeric(palette = "YlOrRd", domain = dta$valor_mapa, na.color = "#e0e0e0")
      
      txt_valor <- if(es_porcentaje) {
        paste0(format(round(dta$valor_mapa * 100, 1), nsmall = 1), "%")
      } else {
        format(dta$valor_mapa, big.mark = ",")
      }
      
      popup_html <- sprintf(
        "<div style='font-family: sans-serif;'>
           <h5 style='margin: 0; color: #333;'>%s</h5>
           <small style='color: #777;'>Sección %s</small>
           <hr style='margin: 5px 0;'>
           <strong>%s:</strong> <span style='color: #0073b7; font-size:1.1em;'>%s</span>
         </div>",
        dta$NOMBRE_SHOW, dta$SECCION_JOIN, input$tipo_dato, txt_valor
      )
      
      titulo_leyenda <- if(es_porcentaje) "Participación (%)" else input$tipo_dato
      lab_trans <- if(es_porcentaje) function(x) x * 100 else function(x) x
      sufijo    <- if(es_porcentaje) "%" else ""
      
      leaflet(dta) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          fillColor = ~pal(valor_mapa),
          color = "#999", weight = 0.5, opacity = 1,
          fillOpacity = 0.8,
          popup = popup_html,
          highlightOptions = highlightOptions(
            weight = 2, color = "#00FFFF", fillOpacity = 0.9, bringToFront = TRUE
          )
        ) %>%
        addLegend(
          position = "bottomright", pal = pal, values = dta$valor_mapa,
          title = titulo_leyenda,
          labFormat = labelFormat(transform = lab_trans, suffix = sufijo)
        ) %>%
        # Plugin nativo para pantalla completa (requiere leaflet.extras si no viene en leaflet base)
        leaflet.extras::addFullscreenControl(position = "topleft", pseudoFullscreen = FALSE)
    })
    
    # 4. Info Resumen (KPI Estilizado)
    output$info_resumen <- renderUI({
      req(datos_mapa())
      dta <- datos_mapa()
      
      # Cálculos
      promedio <- mean(dta$valor_mapa, na.rm = TRUE)
      total    <- sum(dta$valor_mapa, na.rm = TRUE)
      
      if(input$tipo_dato == "participacion") {
        valor_main <- paste0(round(promedio * 100, 1), "%")
        titulo_kpi <- "Tasa Promedio"
      } else {
        valor_main <- scales::comma(total)
        titulo_kpi <- paste("Total", input$tipo_dato)
      }
      
      # Generamos HTML que coincide con el CSS del UI
      div(class = "kpi-card",
          div(class = "kpi-title", titulo_kpi),
          div(class = "kpi-value", valor_main)
      )
    })
    
    # 5. Descarga
    output$descargar_data <- downloadHandler(
      filename = function() { paste0("participacion_", Sys.Date(), ".csv") },
      content = function(file) {
        write.csv(st_drop_geometry(datos_mapa()), file, row.names = FALSE)
      }
    )
  })
}