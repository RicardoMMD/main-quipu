# ==============================================================================
# MÓDULO: CLUSTERING DE SECCIONES ELECTORALES (K-MEANS + IA)
# ==============================================================================

# --- DEFINICIÓN DE VARIABLES CALCULADAS (ACTUALIZADO) ---
vars_secciones_config <- list(
  `Demografía` = c(
    "Población Joven (0-14 años) %" = "pct_joven",
    "Adultos Mayores (65+) %"       = "pct_mayores",
    "Relación Hombres/Mujeres"      = "REL_H_M"
  ),
  `Educación` = c(
    "Grado Promedio Escolaridad"    = "GRAPROES",
    "Asistencia Escolar (15-24) %"  = "pct_asistencia"
  ),
  `Socioeconómico (Vivienda)` = c(
    "Acceso a Internet %"           = "pct_internet",
    "Disponibilidad de Auto %"      = "pct_auto",
    "Hacinamiento (Ocup. x Dorm)"   = "prom_ocup_dorm",
    "Viviendas con PC %"            = "pct_pc"
  ),
  `Servicios` = c(
    "Cobertura de Salud %"          = "pct_salud",
    "Viviendas con Celular %"       = "pct_celular"
  ),
  # --- NUEVA CATEGORÍA ELECTORAL ---
  `Comportamiento Electoral (2024)` = c(
    "Participación Ciudadana %"     = "pct_participacion",
    "Voto MORENA %"                 = "voto_MORENA",
    "Voto PAN %"                    = "voto_PAN",
    "Voto MC %"                     = "voto_MC",
    "Voto PRI %"                    = "voto_PRI"
  )
)

# ------------------------------------------------------------------------------
# UI
# ------------------------------------------------------------------------------
mod_clustering_secciones_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    useShinyjs(),
    
    fluidRow(
      # --- PANEL IZQUIERDO: CONFIGURACIÓN ---
      column(width = 4,
             style  = "padding:0rem;",
             box(
               title = "Configuración del Modelo", width = 12, 
               status = "primary", solidHeader = TRUE,
               
               # --- BOTÓN DE AYUDA ---
               div(style = "text-align: right; margin-bottom: 10px;",
                   actionButton(ns("btn_info"), " ¿Cómo funciona?", 
                                icon = icon("question-circle"), 
                                class = "btn-info btn-sm") # Estilo pequeño y azul
               ),
               
               div(class = "alert alert-info", icon("info-circle"), 
                   "Este análisis agrupa las secciones filtradas actualmente según sus similitudes censales y electorales."),
               
               
               
               # 1. Selección de variables
               pickerInput(
                 inputId = ns("vars_select"),
                 label = "Variables para el Cluster:",
                 choices = vars_secciones_config,
                 selected = c("pct_joven", "GRAPROES", "pct_internet", "pct_auto"),
                 multiple = TRUE,
                 options = pickerOptions(actionsBox = TRUE, title = "Variables...", liveSearch = TRUE)
               ),
               
               # 2. Número de Clusters (K)
               sliderInput(ns("k_clusters"), "Número de Grupos (K):", min = 2, max = 8, value = 4),
               
               # 3. Botón de Ejecución
               actionButton(ns("btn_run"), "Generar Segmentación", icon = icon("cogs"), 
                            class = "btn-primary btn-block btn-lg"),
               
               hr(),
               
               # 4. Botones de IA
               h4(icon("robot"), "Inteligencia Artificial"),
               actionButton(ns("btn_interpretar"), "Interpretar con Gemini AI", 
                            icon = icon("magic"), class = "btn-success btn-block"),
               br(),
               actionButton(ns("btn_elbow"), "Calcular K Óptimo", 
                            icon = icon("chart-line"), class = "btn-info btn-block")
             )
      ),
      
      # --- PANEL DERECHO: RESULTADOS ---
      column(width = 8,
             style  = "padding:0rem;",
             tabBox(width = 12, id = ns("tabs_res"),
                    
                    # A) MAPA
                    tabPanel("Mapa de Clusters", icon = icon("map"),
                             shinycssloaders::withSpinner(
                               leafletOutput(ns("mapa_cluster"), height = "600px"),
                               type = 6, color = "#007bff"
                             )
                    ),
                    
                    # B) PERFILES
                    tabPanel("Perfil de los Grupos", icon = icon("chart-bar"),
                             shinycssloaders::withSpinner(
                               plotlyOutput(ns("plot_perfil"), height = "600px"),
                               type = 6, color = "#007bff"
                             )
                    ),
                    
                    # C) IA
                    tabPanel("Interpretación (IA)", icon = icon("align-left"),
                             shinycssloaders::withSpinner(
                               uiOutput(ns("ui_interpretacion")),
                               type = 6, color = "#007bff"
                             )
                             
                    ),
                    
                    # D) CODO
                    tabPanel("Análisis de K", icon = icon("chart-area"),
                             plotOutput(ns("plot_codo"), height = "500px")
                    ),
                    
                    # E) DATA
                    tabPanel("Datos", icon = icon("table"),
                             DTOutput(ns("tabla_datos"))
                    )
             )
      )
    )
  )
}

# ------------------------------------------------------------------------------
# SERVER
# ------------------------------------------------------------------------------
mod_clustering_secciones_server <- function(id, secciones_reactivas, contexto_geo) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # Variables reactivas
    res_cluster <- reactiveVal(NULL)
    res_elbow   <- reactiveVal(NULL)
    txt_ia      <- reactiveVal(NULL)
    
    # Verificar API Key al inicio (Solo UI check)
    api_key_env <- Sys.getenv("GEMINI_API_KEY")
    has_api <- nchar(api_key_env) > 5
    
    if(!has_api) {
      shinyjs::disable("btn_interpretar")
      # Opcional: Warning silencioso en consola
      message("Warning: GEMINI_API_KEY no encontrada. El módulo de IA estará deshabilitado.")
    }
    
    # 0. BOTÓN DE INFORMACIÓN / AYUDA ------------------------------------------------------------------------
    
    observeEvent(input$btn_info, {
      showModal(modalDialog(
        title = tagList(icon("brain"), "Guía de Segmentación y Clustering"),
        size = "l", # Tamaño grande para leer bien
        easyClose = TRUE,
        footer = modalButton("Cerrar"),
        
        # CONTENIDO HTML DEL MODAL
        HTML("
          <div style='font-family: sans-serif; line-height: 1.6;'>
            
            <div style='background-color: #f4f6f9; padding: 15px; border-left: 5px solid #3c8dbc; margin-bottom: 15px;'>
              <h4 style='margin-top:0;'><b>¿Qué hace este módulo?</b></h4>
              <p>Utiliza algoritmos de Aprendizaje Automático (<i>K-Means Clustering</i>) para encontrar 
              <b>patrones ocultos</b> en las secciones electorales. El sistema agrupa secciones que son matemáticamente similares 
              entre sí, permitiéndote identificar 'tipos de territorio' (ej: zonas residenciales panistas, zonas populares movilizadas, etc.).</p>
            </div>

            <h4><b>1. Fuentes de Datos</b></h4>
            <ul>
              <li><b>Demografía:</b> Censo de Población y Vivienda 2020 (INEGI) a nivel manzana, agregado a sección.</li>
              <li><b>Electoral:</b> Resultados de la Elección Presidencial 2024 (o la más reciente disponible) para determinar la identidad política.</li>
              <li><b>Participación:</b> Porcentaje de participación ciudadana real histórica.</li>
            </ul>

            <h4><b>2. ¿Cómo interpretar los resultados?</b></h4>
            <p>El algoritmo no sabe de política, solo de matemáticas. Agrupa por similitud numérica. Por eso incluimos herramientas de apoyo:</p>
            <ul>
              <li><b>Mapa:</b> Visualiza la distribución territorial. ¿Los clusters están contiguos o dispersos?</li>
              <li><b>Perfiles (Gráfica):</b> Compara qué variable destaca en cada grupo (ej: el Cluster 1 tiene internet alto pero participación baja).</li>
              <li><b>Inteligencia Artificial (Gemini):</b> Enviamos los datos estadísticos a la IA para que actúe como consultor y 'traduzca' los números a perfiles sociopolíticos con estrategias sugeridas.</li>
            </ul>

            <h4><b>3. Consideraciones Importantes</b></h4>
            <ul>
              <li><b>El Filtro Importa:</b> El análisis se hace SOLO sobre lo que seleccionaste en el menú lateral (Municipio o Distrito). La IA recibe el contexto de esa zona específica.</li>
              <li><b>Normalización:</b> Todas las variables se convierten a una escala común (Z-score) para que una variable con números grandes (Ingreso) no domine a una pequeña (Hijos promedio).</li>
              <li><b>Correlación no es Causalidad:</b> Que un cluster vote por un partido y tenga ciertas características no garantiza que esa sea la causa única, pero da pistas fuertes para la estrategia.</li>
            </ul>
            
            <hr>
            <p class='text-muted small'><i>Desarrollado con R Shiny, Leaflet y Google Gemini AI.</i></p>
          </div>
        ")
      ))
    })
    
    # 1. PREPARACIÓN DE DATOS (Reactive) ---------------------------------------
    datos_calculados <- reactive({
      req(secciones_reactivas())
      geo_filt <- secciones_reactivas()
      
      if(nrow(geo_filt) == 0) return(NULL)
      
      # Verificamos dependencias globales
      req(exists("data_secc_cpv2020"), exists("participacion"), exists("cant_votos_nl"))
      
      # --- A) PREPARACIÓN DATOS ELECTORALES (Al vuelo) ---
      # Calculamos % de voto por partido para la elección Presidencial 2024 (o la más reciente relevante)
      # Esto normaliza los datos para que el Cluster entienda "Intención de voto" y no "Cantidad de votos"
      
      df_elec_wide <- tryCatch({
        cant_votos_nl %>%
          filter(eleccion_año_id == "pres_24") %>% # Usamos Presidencial 2024 como proxy ideológico
          group_by(seccion) %>%
          mutate(total_seccion = sum(votos, na.rm = TRUE)) %>%
          ungroup() %>%
          # Filtramos solo los partidos principales para el cluster
          filter(partido %in% c("PAN", "PRI", "MC", "MORENA", "VERDE", "PT")) %>%
          mutate(pct = (votos / total_seccion) * 100) %>%
          select(seccion, partido, pct) %>%
          pivot_wider(
            names_from = partido, 
            values_from = pct, 
            values_fill = 0,
            names_prefix = "voto_" # Resultado: voto_PAN, voto_MORENA...
          )
      }, error = function(e) {
        print(paste("Error procesando electoral:", e$message))
        return(data.frame(seccion = character(0)))
      })
      
      # --- B) PREPARACIÓN PARTICIPACIÓN ---
      # La tabla 'participacion' es un SF, la convertimos a tabla simple
      df_part_simple <- participacion %>%
        st_drop_geometry() %>%
        select(SECCION, participacion) %>%
        mutate(pct_participacion = participacion * 100) %>% # Convertir 0.54 a 54%
        select(SECCION, pct_participacion)
      
      
      # --- C) JOINS Y CÁLCULOS FINALES ---
      
      # 1. Preparación Llaves (Numeric vs Character)
      geo_prep <- geo_filt %>%
        mutate(SECCION_NUM = as.numeric(as.character(SECCION))) 
      
      # 2. Join Censal
      data_full <- geo_prep %>%
        left_join(data_secc_cpv2020, by = c("SECCION_NUM" = "SECCION"))
      
      # 3. Join Electoral y Participación (Por código de sección Character)
      data_full <- data_full %>%
        left_join(df_elec_wide, by = c("SECCION" = "seccion")) %>%
        left_join(df_part_simple, by = c("SECCION" = "SECCION"))
      
      if(nrow(data_full) == 0) return(NULL)
      
      # 4. Feature Engineering
      tryCatch({
        df_temp <- data_full %>%
          mutate(
            pct_joven      = (POB0_14 / POBTOT) * 100,
            pct_mayores    = (POB65_MAS / POBTOT) * 100,
            REL_H_M        = as.numeric(REL_H_M),
            
            pob_15_24      = P_15A17 + P_18A24,
            asiste_15_24   = P15A17A + P18A24A,
            pct_asistencia = ifelse(pob_15_24 > 0, (asiste_15_24 / pob_15_24) * 100, 0),
            
            pct_internet   = (VPH_INTER / TVIVPARHAB) * 100,
            pct_auto       = (VPH_AUTOM / TVIVPARHAB) * 100,
            pct_pc         = (VPH_PC / TVIVPARHAB) * 100,
            prom_ocup_dorm = PRO_OCUP_C, 
            pct_salud      = (PDER_SS / POBTOT) * 100,
            pct_celular    = (VPH_CEL / TVIVPARHAB) * 100
          )
        
        # Selección dinámica de columnas
        vars_a_mantener <- unname(unlist(vars_secciones_config))
        
        # Filtramos solo las columnas que existen (para evitar error si falta algún partido en una zona)
        vars_existentes <- intersect(vars_a_mantener, names(df_temp))
        
        df_final <- df_temp %>%
          select(SECCION, SECCION_NUM, any_of(vars_existentes)) %>%
          mutate(across(where(is.numeric), ~ifelse(!is.finite(.), 0, .))) %>%
          mutate(across(where(is.numeric), ~replace_na(., 0)))
        
        return(df_final)
        
      }, error = function(e) {
        showNotification(paste("Error en cálculo de variables:", e$message), type = "error")
        return(NULL)
      })
    })
    
    # 2. EJECUCIÓN DEL CLUSTER (K-MEANS) ---------------------------------------
    observeEvent(input$btn_run, {
      req(datos_calculados(), input$vars_select)
      
      df_geo <- datos_calculados()
      n_rows <- nrow(df_geo)
      
      # --- CORRECCIÓN CRÍTICA 1: Validar tamaño de muestra ---
      if(n_rows < input$k_clusters) {
        showNotification(
          paste("No hay suficientes secciones (", n_rows, ") para generar", input$k_clusters, "grupos."), 
          type = "error"
        )
        return()
      }
      
      if(length(input$vars_select) < 2) {
        showNotification("Selecciona al menos 2 variables", type = "warning")
        return()
      }
      
      shinyjs::disable("btn_run")
      showNotification("Calculando segmentos...", id = "notif_proc", duration = NULL)
      
      tryCatch({
        vars_finales <- intersect(input$vars_select, names(df_geo))
        
        df_num <- df_geo %>% 
          st_drop_geometry() %>%
          select(all_of(vars_finales)) %>%
          scale() 
        
        set.seed(123)
        km <- kmeans(df_num, centers = input$k_clusters, nstart = 25)
        
        df_res <- df_geo %>% mutate(Cluster = as.factor(km$cluster))
        
        perfiles <- df_res %>% st_drop_geometry() %>% group_by(Cluster) %>%
          summarise(across(all_of(vars_finales), \(x) mean(x, na.rm = TRUE)))
        
        res_cluster(list(sf = df_res, perfiles = perfiles, vars = vars_finales))
        
        showNotification("Segmentación finalizada exitosamente.", type = "message")
        
      }, error = function(e) {
        showNotification(paste("Error Cluster:", e$message), type = "error")
      }, finally = {
        removeNotification("notif_proc")
        shinyjs::enable("btn_run")
      })
    })
    
    # 3. INTERPRETACIÓN CON IA (CORREGIDO HTTR2) -------------------------------
    observeEvent(input$btn_interpretar, {
      req(res_cluster(), has_api)
      
      # Capturamos datos y KEY en el hilo principal antes de entrar al futuro
      mis_perfiles <- res_cluster()$perfiles
      mis_vars     <- res_cluster()$vars
      my_api_key   <- Sys.getenv("GEMINI_API_KEY") # CRÍTICO: Leerla aquí
      
      # CAPTURAMOS EL LUGAR EXACTO
      lugar_analisis <- contexto_geo()
      
      shinyjs::disable("btn_interpretar")
      showNotification("Consultando a Gemini (Google)...", id = "notif_ai", duration = NULL)
      
      # Promesa Asíncrona
      future_promise({
        library(httr2)
        library(jsonlite)
        library(dplyr)
        library(knitr)
        
        # 1. Preparar Texto
        tabla_md <- mis_perfiles %>%
          mutate(across(where(is.numeric), round, 2)) %>%
          kable(format = "markdown") %>%
          paste(collapse = "\n")
        
        prompt_text <- paste0(
          "Eres un consultor político experto. Analiza estos clusters de secciones electorales en Nuevo León:\n\n",
          "Analiza la siguiente tabla de clusters (segmentos) de secciones electorales ", lugar_analisis, ".\n\n",
          
          "DATOS DE LOS PERFILES:\n",
          tabla_md,
          "\n\nVariables analizadas: ", paste(mis_vars, collapse=", "), ".\n",
          "Tarea:\n",
          "1. Asigna un NOMBRE corto a cada Cluster (ej: 'Panismo Acomodado', 'Bastión Popular Morena').\n",
          "2. Correlaciona el perfil demográfico con su comportamiento electoral.\n", # NUEVA INSTRUCCIÓN
          "3. Usa formato Markdown."
        )
        
        # 2. Llamada Directa a API (Más robusto que librerías wrapper)
        url <- paste0("https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent?key=", my_api_key)
        
        resp <- request(url) %>%
          req_method("POST") %>%
          req_headers("Content-Type" = "application/json") %>%
          req_body_json(list(
            contents = list(
              list(parts = list(list(text = prompt_text)))
            )
          )) %>%
          req_perform()
        
        # 3. Parsear Respuesta
        json_resp <- resp %>% resp_body_json()
        texto_generado <- json_resp$candidates[[1]]$content$parts[[1]]$text
        
        return(texto_generado)
        
      }) %...>% (function(resultado_texto) {
        
        # Éxito: Actualizar UI
        txt_ia(resultado_texto)
        updateTabsetPanel(session, "tabs_res", selected = "Interpretación (IA)")
        showNotification("Interpretación generada.", type = "message")
        
      }) %...!% (function(err) {
        
        # Error
        print(paste("Error Gemini:", err$message))
        showNotification("Error al conectar con la IA. Verifica tu API Key o conexión.", type = "error")
        
      }) %>% finally(~{
        removeNotification("notif_ai")
        shinyjs::enable("btn_interpretar")
      })
    })
    
    # 4. SALIDAS GRÁFICAS ------------------------------------------------------
    
    # A) Mapa Leaflet
    output$mapa_cluster <- renderLeaflet({
      req(res_cluster())
      
      # 1. Obtener datos y variables seleccionadas
      data <- res_cluster()$sf
      vars_selected <- res_cluster()$vars
      
      # 2. Preparar diccionario inverso (Interno -> Legible)
      # 'vars_secciones_config' es una lista anidada, la aplanamos para buscar fácil
      lista_plana <- unlist(vars_secciones_config)
      # Invertimos: Clave=CodigoInterno, Valor=NombreLegible
      nombres_legibles <- setNames(names(lista_plana), lista_plana)
      
      # 3. Construcción del Popup (Iterativo)
      # Iniciamos con el encabezado fijo (Sección y Cluster)
      popups_html <- paste0(
        "<div style='font-family: sans-serif; font-size: 12px;'>",
        "<h4 style='margin:0; color:#333; border-bottom:1px solid #ccc; padding-bottom:4px;'>",
        "Sección: <b>", data$SECCION, "</b></h4>",
        "<span style='background-color:#eee; padding:2px 5px; border-radius:3px; font-size:11px;'>",
        "Cluster: <b>", data$Cluster, "</b></span><br>",
        "<div style='margin-top:8px;'>"
      )
      
      # Iteramos sobre CADA variable seleccionada para agregarla al HTML
      for (var in vars_selected) {
        
        # Obtener etiqueta legible (si no existe, usa el nombre interno)
        label <- nombres_legibles[var]
        if(is.na(label)) label <- var
        
        # Obtener valores y formatear
        valores <- data[[var]]
        
        # Lógica de formato visual
        if (is.numeric(valores)) {
          # Si parece porcentaje (por nombre o rango), agregar "%"
          if (grepl("pct_|voto_|tasa", var) || max(valores, na.rm=T) <= 100) {
            val_fmt <- paste0(round(valores, 1), "%")
          } else {
            val_fmt <- round(valores, 2)
          }
        } else {
          val_fmt <- valores
        }
        
        # Concatenar línea al popup de todas las filas
        popups_html <- paste0(
          popups_html,
          "<b>", label, ":</b> ", val_fmt, "<br>"
        )
      }
      
      # Cerrar el div
      popups_html <- paste0(popups_html, "</div></div>")
      
      # 4. Renderizar Mapa
      pal <- colorFactor(palette = "Set1", domain = data$Cluster)
      
      leaflet(data) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        addPolygons(
          fillColor = ~pal(Cluster),
          fillOpacity = 0.6,
          color = "white", 
          weight = 1, # Un poco más de borde para separar secciones
          opacity = 1,
          # Pasamos el vector HTML construido
          popup = popups_html, 
          highlightOptions = highlightOptions(
            weight = 2,
            color = "#666",
            fillOpacity = 0.8,
            bringToFront = TRUE
          )
        ) %>%
        addLegend("bottomright", pal = pal, values = ~Cluster, title = "Segmento")
    })
    
    # B) Plotly Perfiles
    output$plot_perfil <- renderPlotly({
      req(res_cluster())
      perfiles <- res_cluster()$perfiles
      vars <- res_cluster()$vars
      
      # Normalizamos solo para visualización (0-1)
      perfiles_viz <- perfiles %>%
        mutate(across(all_of(vars), scales::rescale)) %>%
        pivot_longer(cols = all_of(vars), names_to = "Variable", values_to = "Valor")
      
      gg <- ggplot(perfiles_viz, aes(x = Variable, y = Valor, fill = Cluster)) +
        geom_bar(stat = "identity", position = "dodge") +
        coord_flip() +
        scale_fill_brewer(palette = "Set1") +
        theme_minimal() +
        labs(title = "Comparativa de Intensidad (Escala Relativa)", x = "", y = "")
      
      ggplotly(gg) %>% config(displayModeBar = FALSE)
    })
    
    # C) Output Texto IA
    output$ui_interpretacion <- renderUI({
      req(txt_ia())
      # Renderizar Markdown con estilo seguro
      wellPanel(
        style = "background-color: #f8f9fa; padding: 20px; border-left: 5px solid #007bff;",
        HTML(markdown::markdownToHTML(text = txt_ia(), fragment.only = TRUE))
      )
    })
    
    # 5. MÉTODO DEL CODO (ELBOW) -----------------------------------------------
    
    observeEvent(input$btn_elbow, {
      req(datos_calculados())
      shinyjs::disable("btn_elbow")
      
      df_geo <- datos_calculados()
      n_rows <- nrow(df_geo)
      
      # --- CORRECCIÓN CRÍTICA 2: Evitar K > N ---
      if(n_rows < 3) {
        showNotification("Datos insuficientes para análisis de K (mínimo 3 secciones).", type = "warning")
        shinyjs::enable("btn_elbow")
        return()
      }
      
      # Definir el límite máximo de clusters a probar (Máximo 8, pero nunca más que N-1)
      max_k_possible <- min(8, n_rows - 1)
      
      df_num <- df_geo %>% 
        st_drop_geometry() %>% 
        select(all_of(input$vars_select)) %>% 
        scale()
      
      # Calcular WSS dinámicamente hasta max_k_possible
      tryCatch({
        wss <- sapply(2:max_k_possible, function(k) {
          kmeans(df_num, centers = k, nstart = 10)$tot.withinss
        })
        
        res_elbow(data.frame(k = 2:max_k_possible, wss = wss))
        updateTabsetPanel(session, "tabs_res", selected = "Análisis de K")
        
      }, error = function(e){
        showNotification(paste("Error en Codo:", e$message), type = "error")
      }, finally = {
        shinyjs::enable("btn_elbow")
      })
    })
    
    output$plot_codo <- renderPlot({
      req(res_elbow())
      ggplot(res_elbow(), aes(x = k, y = wss)) +
        geom_line(color = "#007bff", size = 1) + 
        geom_point(size = 4, color = "#007bff") +
        theme_minimal() +
        scale_x_continuous(breaks = res_elbow()$k) + # Asegurar que solo muestre enteros
        labs(title = "Método del Codo (Elbow Method)", 
             subtitle = "Busca el punto donde la curva se aplana",
             x = "Número de Clusters (K)", y = "WSS (Error)")
    })
    
    # E) Tabla Datos
    output$tabla_datos <- renderDT({
      req(res_cluster())
      res_cluster()$sf %>% 
        st_drop_geometry() %>% 
        select(SECCION, Cluster, everything())
    }, options = list(pageLength = 10, scrollX = TRUE))
    
  })
}