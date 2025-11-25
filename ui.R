

# ==============================================================================
# Interfaz de Usuario (UI) de la Aplicación Shiny
# ==============================================================================

# options(shiny.reactlog = TRUE)

ui_code <- dashboardPage(
  
  # 1. Cabecera (Header) ----
  dashboardHeader(
    titleWidth = "25rem",
    title = 
      tags$span(
        tags$img(
          src = "boleta_logo.png", # Ruta relativa desde la carpeta www
          height = "40", # Ajusta la altura del logo
          style = "margin-top:-5px; margin-right:5px;" # Ajuste fino de la posición
        ),
        "BOLETA Electoral" 
      )
  ),
  
  # 2. Barra Lateral (Sidebar) ----
  dashboardSidebar(width = "25rem",
    disable = FALSE,
    includeCSS("www/styles.css"),
    includeScript("www/main.js"),
    useShinyjs(),
    # tags$head(
    #   tags$link(rel = "stylesheet", type = "text/css", href = "https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.2/css/all.min.css")
    # ),
    
    # --- Filtros Iniciales ---
    shiny::selectInput("tipo_filtro_inicial",
                       "Nivel geográfico:",
                       choices = c("Municipio", "DFederal", "DLocal", "Ninguno"),
                       multiple = FALSE),
    
    hidden(
      shiny::selectInput("municipio_inicial",
                         "Selecciona el municipio:",
                         choices = NULL),
      shiny::selectInput("federal_inicial",
                         "Selecciona el distrito federal:",
                         choices = NULL),
      shiny::selectInput("local_inicial",
                         "Selecciona el distrito local:",
                         choices = NULL)
    ),
    
    # --- Menú Principal ---
    sidebarMenu(
      # Menú de Inicio se mantiene en la parte superior
      shinydashboard::menuItem("Inicio", tabName = "home", icon = icon("home")),
      
      # 1. Menú principal "Información Electoral" con sub-elementos
      shinydashboard::menuItem("Información Electoral", icon = icon("vote-yea"), startExpanded = FALSE, # Cambia a TRUE si quieres que inicie abierto
               
               # NOTA: Los nombres han sido actualizados, pero los 'tabName' se mantienen
               # para asegurar la compatibilidad con tu código del servidor.
               shinydashboard::menuSubItem("Participación Electoral",           tabName = "participacion"), # Añadido desde el primer código
               shinydashboard::menuSubItem("Comparativo Resultados Históricos", tabName = "ganadores"),
               shinydashboard::menuSubItem("Distancia Entre Primero y Segundo", tabName = "diferencias"),
               shinydashboard::menuSubItem("Voto Sombra",                       tabName = "sombra"),
               shinydashboard::menuSubItem("Evolución Partidista",              tabName = "cambios_porcentuales"),
               shinydashboard::menuSubItem("Voto Diferenciado Partidista",      tabName = "rendimiento_historico"),
               shinydashboard::menuSubItem("Comparativa ganadas/perdidas",      tabName = "robados"),
               shinydashboard::menuSubItem("BD Resultados",                     tabName = "creacion_tablas"),
               shinydashboard::menuSubItem("Lealtad Partidista",                tabName = "lealtad"), 
               shinydashboard::menuSubItem("Simulador de Resultados",           tabName = "simulaciones")
      ),
      
      # 2. Renombramiento de "Datos censales" a "Datos Poblacionales"
      shinydashboard::menuItem("Datos Poblacionales",              tabName = "censales",             icon = icon("users-cog")),
      
      # 3. Resto de los menús en el nivel principal
      shinydashboard::menuItem("Demografía Espacial",              tabName = "edades",               icon = icon("id-card")),
      shinydashboard::menuItem("Población y votos por manzana",    tabName = "data_mza",             icon = icon("map-marker-alt")),
      shinydashboard::menuItem("Perfil de colonias",               tabName = "información_colonias", icon = icon("map-signs")),
      shinydashboard::menuItem("Gestión de tiempo",                tabName = "tiempoxsección",       icon = icon("calendar-alt"))
      
      # La línea de "Fortaleza Electoral" permanece comentada
      # menuItem("Fortaleza Electoral",           tabName = "probabilidades",       icon = icon("shield-alt"))
    )
  ),
  
  # 3. Cuerpo del Dashboard (Body) ----
  dashboardBody(
    extendShinyjs(
      script = "www/main.js", 
      functions = c("fullscreen", "fullscreenFortaleza", "fullscreenVisitas", 
                    "fullscreenGanaOPierde", "fullscreenGana", "fullscreenPierde", 
                    "fullscreenGanador1", "fullscreenGanador2", "fullscreenGenerosEdad", 
                    "fullscreenPri", "fullscreenSim1", "fullscreenCambiosPerc", 
                    "fullscreenManzanas", "fullscreenSombra", "fullscreenDiferencia")
    ),
    
    tabItems(
      
      # Pestaña: Inicio ----
      tabItem(tabName = "home",
              
              # Fila 1: Mensaje de bienvenida mejorado
              fluidRow(
                box(
                  title = tagList(shiny::icon("chart-pie"), "Bienvenidos a BOLETA Electoral"),
                  status = "primary", # Color del encabezado
                  solidHeader = TRUE, # Encabezado sólido
                  width = 12, # Ocupa todo el ancho
                  collapsible = FALSE,
                  
                  h3("Una herramienta de análisis GeoElectoral interactiva"),
                  p("Esta plataforma está diseñada para explorar, analizar y simular datos electorales de manera geográfica. Navega a través de las diferentes herramientas en el menú de la izquierda para descubrir patrones, comparar resultados históricos y obtener insights estratégicos.", style = "font-size: 16px;"),
                  textOutput("user_role") # Se mantiene el rol del usuario
                )
              ),
              
              # Fila 2: Guía de inicio rápido
              fluidRow(
                box(
                  title = tagList(shiny::icon("rocket"), "Cómo Empezar"),
                  status = "info",
                  solidHeader = TRUE,
                  width = 12,
                  collapsible = TRUE, # El usuario puede colapsarla si ya sabe usar la app
                  
                  p("Sigue estos sencillos pasos para comenzar tu análisis:", style = "font-size: 16px;"),
                  tags$ol(
                    tags$li(tags$strong("Elige tu área de interés:"), "Utiliza los selectores en el panel izquierdo para filtrar por Municipio, Distrito Federal o Distrito Local."),
                    tags$li(tags$strong("Navega por las herramientas:"), "Selecciona cualquiera de los módulos en el menú para acceder a mapas, gráficos y tablas interactivas."),
                    tags$li(tags$strong("Explora y descarga:"), "Interactúa con las visualizaciones y descarga los datos que necesites en formato CSV.")
                  )
                )
              ),
              
              # Fila 3: Título para la sección de funcionalidades
              fluidRow(
                column(width = 12,
                       h3("Principales Funcionalidades")
                )
              ),
              
              # Fila 4: Grid de funcionalidades
              fluidRow(
                # Usaremos infoBox para un look más de dashboard
                infoBox(
                  "Visualizador Histórico", 
                  "Observa los ganadores por sección a través del tiempo.",
                  icon = icon("history"), 
                  color = "aqua", 
                  width = 4
                ),
                infoBox(
                  "Demografía Espacial", 
                  "Analiza la distribución de edad y género por sección.",
                  icon = icon("id-card"), 
                  color = "light-blue", 
                  width = 4
                ),
                infoBox(
                  "Archivo Electoral", 
                  "Genera y descarga tablas de votos personalizadas.",
                  icon = icon("folder-open"), 
                  color = "teal", 
                  width = 4
                ),
                infoBox(
                  "Rendimiento Histórico", 
                  "Analiza el desempeño de un partido en múltiples elecciones.",
                  icon = icon("chart-line"), 
                  color = "green", 
                  width = 4
                ),
                infoBox(
                  "Simulador Electoral", 
                  "Modifica escenarios y predice resultados electorales.",
                  icon = icon("gamepad"), 
                  color = "yellow", 
                  width = 4
                ),
                infoBox(
                  "Evolución Electoral", 
                  "Mide el cambio porcentual de votos entre elecciones.",
                  icon = icon("chart-bar"), 
                  color = "orange", 
                  width = 4
                ),
                infoBox(
                  "Evaluación Comparativa", 
                  "Compara qué secciones ha ganado o perdido tu partido.",
                  icon = icon("balance-scale-right"), 
                  color = "red", 
                  width = 4
                ),
                infoBox(
                  "Perfil de Colonias", 
                  "Explora el perfil demográfico y electoral de cada colonia.",
                  icon = icon("map-signs"), 
                  color = "purple", 
                  width = 4
                ),
                infoBox(
                  "Fortaleza Electoral", 
                  "Mide la probabilidad de triunfo con modelos de Machine Learning.",
                  icon = icon("shield-alt"), 
                  color = "maroon", 
                  width = 4
                )
                # Puedes agregar las demás funcionalidades siguiendo este formato
              )
      ),
      
      # Pestaña: Población y Votos por Manzana ----
      tabItem(
        tabName = "data_mza",
        fluidRow(
          column(width = 1),
          column(width = 10,
                 box(
                   width = 12, title = "Instrucciones", status = "info", solidHeader = TRUE, collapsible = TRUE, collapsed = TRUE,
                   h3("Explora poblaciones y votos de secciones electorales a nivel manzana"),
                   p("1. Utilice los controles principales en la barra lateral para seleccionar la geografía de su interés."),
                   p("2. Utilice el selector de población (ej. 'Población total', 'Población de 18 o más años') ubicado sobre el mapa."),
                   p("3. Haga clic en una sección del mapa para visualizar las manzanas que la componen, coloreadas por densidad de población."),
                   p("4. A la derecha, se mostrarán opciones para elegir la elección y el año. Debajo, verá un resumen de la población y votos, junto a un gráfico de distribución."),
                   p("5. Para regresar, haga clic en la flecha amarilla a la izquierda del título de la sección."),
                   h4("Consideraciones:"),
                   p("Las poblaciones corresponden al Censo 2020. Únicamente se mapean y contabilizan manzanas urbanas.")
                 )
          ),
          column(width = 1)
        ),
        
        fluidRow(
          column(width = 1),
          column(width = 11,
                 fluidRow(
                   style = "display:flex; align-items:end; gap:1.2rem; font-size:x-large; font-weight:500;",
                   shinyjs::hidden(
                     actionBttn(inputId = "btt_return_secc_select", label = NULL, style = "material-circle", color = "warning", icon = icon("chevron-left"))
                   ),
                   textOutput("text_secc") 
                 )
          )
        ),
        br(),
        
        fluidRow(
          column(width = 1),
          column(width = 6,
                 fluidRow(
                   style="display:flex; align-items:end; gap:0.8rem;",
                   shinyjs::hidden(
                     pickerInput(inputId = "pik_pob_mza", label = "Seleccione población de interés", options = list(`live-search` = TRUE), choices = chs_poblaciones_manzanas, selected = chs_poblaciones_manzanas[1]),
                     pickerInput(inputId = "pik_secc_col_mza", label = "Búsqueda de sección", multiple = TRUE, options = list(`live-search` = TRUE, `deselect-all-text` = "Ninguno", `select-all-text` = "Todos"), choices = "", selected = "")
                   )
                 )
          ),
          column(width = 4,
                 fluidRow(
                   style="display:flex; align-items:end; gap:0.8rem;",
                   hidden(
                     pickerInput(inputId = "pik_elect_mza", label = "Seleccione elección", options = list(`live-search` = TRUE), choices = list_eleccion, selected = list_eleccion[1]),
                     sliderTextInput(inputId = "slid_año_mza", label = "Año de elección:", choices = "", grid = TRUE)
                   )
                 )
          ),
          column(width = 1)
        ),
        
        fluidRow(
          column(width = 1),
          column(width = 6,
                 materialSwitch(inputId = "swch_colonias_mzas", label = "Visualiza colonias", status = "success", value = FALSE),
                 leafletOutput("map_mzas", width = "100%", height = 600)
          ),
          column(width = 4,
                 fluidRow(
                   valueBox(textOutput("text_vb_pob_map"), strong(textOutput("text_titvb_pob_map")), icon = icon("users"), width = 6, color = "light-blue"),
                   valueBox(textOutput("text_vb_vots_map"), strong(textOutput("text_titvb_vots_map")), icon = icon("check-to-slot"), width = 6, color = "teal")
                 ),
                 plotlyOutput("py_ganadores", width = "100%")
          ),
          column(width = 1)
        )
      ),
      
      # Pestaña: Comparativo Resultados Históricos ----
      tabItem(
        tabName = "ganadores",
        fluidRow(
          box(
            width = 12,
            title = "Comparativo Resultados Históricos",
            collapsible = TRUE, collapsed = T, 
            solidHeader = F, status = "info",
            p("Explore y compare visualmente los datos electorales de diferentes periodos. Seleccione dos elecciones para generar mapas interactivos donde cada sección se colorea según el partido ganador.")
          )
        ),
        fluidRow( 
          box(
            status = "success",
            shiny::selectInput(
              "eleccion_1","Elección para el primer panel:",
              choices = choices_rendimiento_historico,
              selected = choices_rendimiento_historico$`Elecciones 2024`[1]),
            
            leafletOutput("mapa_ganador_1", height = "80vh"),
            shiny::actionButton("mapGanador1Fullscreen", "Pantalla Completa"),
            downloadButton("downloadganador_elec1", "Descargar Tabla (CSV)")
          ),
          box(
            status = "success",
            shiny::selectInput(
              "eleccion_2","Elección para el segundo panel:",
              choices = choices_rendimiento_historico,
              selected = choices_rendimiento_historico$`Elecciones 2021`[1]),
            
            leafletOutput("mapa_ganador_2", height = "80vh"),
            shiny::actionButton("mapGanador2Fullscreen", "Pantalla Completa"),
            downloadButton("downloadganador_elec2", "Descargar Tabla (CSV)")
          )
        )
      ), 
      
      # Pestaña: Voto Sombra ----
      tabItem(
        tabName = "sombra",
        
        fluidRow(
          column(
            width = 3,
            box(
              title = "Visualizador del Voto Sombra", width = 12, collapsible = TRUE, collapsed = TRUE, status = "info",
              p("Esta herramienta permite analizar cuál partido político quedó en segundo lugar en cada sección para una elección específica. Seleccione una elección para generar un mapa interactivo.")
            ),
            box(width = 12, status = "warning",

                
                
                
                shiny::selectInput("eleccion_sombra",
                                   "Seleccione la elección a analizar:",
                                   choices = choices_rendimiento_historico)
                )
            
          ),
          column(
            width = 9,
            tabBox(
              width = 12,
              
              # --- Primer Tab: Contiene el mapa ---
              tabPanel(
                title = "Mapa Interactivo", 
                icon = icon("map-location-dot"), 

                leafletOutput("mapa_sombra", height = "80vh", width = "100%"),
                br(),
                shiny::actionButton("mapSombraFullscreen", "Pantalla Completa", icon = icon("expand"))
              ),
              
              # --- Segundo Tab: Contiene la tabla de datos ---
              tabPanel(
                title = "Tabla de Datos", 
                icon = icon("table"), 
                DT::DTOutput("dt_sombra_sombra"),
                downloadBttn(
                  outputId = "dw_csv_sombra", 
                  label = "Descargar CSV", 
                  size = "sm", 
                  style = "simple",
                  color = "warning"
                )
              )
            )
          )
        )
      ),
      
      # Pestaña: Distancia Primer-Segundo ----
      tabItem(
        tabName = "diferencias",
        fluidRow(
          box(
            title = "Distancia Histórica: Primer vs. Segundo Lugar", collapsible = TRUE, collapsed = TRUE, width = 12, status = "info",
            p("Analiza la diferencia de votos entre el primer y segundo lugar en cada sección para la elección seleccionada.")
          )
        ),
        fluidRow(
          column(
            width = 3,
            box(width = 12, status = "warning",
                shiny::selectInput("eleccion_diferencia",
                                   "Seleccione la elección a analizar:",
                                   choices = choices_elections_sombra)
            )
          ),
          column(
            width = 9,
            tabBox(
              id = "resultados_tabs", 
              width = 12, # Este width se aplica a todo el tabBox
              
              # Primer Tab: Mapa
              tabPanel(
                title = "Mapa", 
                icon = icon("map"),
                # Los argumentos 'width' y 'status' se han eliminado de aquí
                leafletOutput("mapa_diferencia", height = "80vh"),
                # Sugerencia: Colocar el botón dentro de un tag para mejor control del estilo
                tags$div(
                  style = "position: absolute; top: 10px; right: 10px; z-index: 1000;",
                  shiny::actionButton("mapDiferenciaFullscreen", "Pantalla Completa")
                )
              ),
              
              # Segundo Tab: Tabla
              tabPanel(
                title = "Tabla", 
                icon = icon("table"),
                # Los argumentos 'width' y 'status' se han eliminado de aquí
                
                # Sugerencia: Agrupar los controles para un mejor layout
                tags$div(
                  style = "padding-bottom: 10px;", # Un poco de espacio
                  downloadBttn(
                    outputId = "dw_csv_distancia", 
                    label = "CSV", 
                    icon = icon("file-csv"), 
                    size = "sm", 
                    color = "warning", 
                    style = "simple"
                  )
                ),
                
                DT::DTOutput("dt_diferencia_diferencia")
              )
            )
          )
        )
      ),
      
      # Pestaña: Demografía Espacial ----
      tabItem(
        tabName = "edades",
        fluidRow(
          
          
          column(
            width = 3,
            box(
              width = 12,
              title = "Demografía Espacial por Edad y Género", 
              collapsible = TRUE, collapsed = TRUE, status = "info",
              p("Visualice la distribución demográfica en el mapa. Seleccione los grupos de edad y géneros de interés para ver qué secciones tienen mayor concentración de dicha población.")
            ),
            box(
              width = 12,
              shiny::selectInput("edades",
                                 "Seleccione grupos de edad:",
                                 choices =  c("18 a 24" = "_18_24", "25 a 29" = "_25_29", "30 a 39" = "_30_39", 
                                              "40 a 49" = "_40_49", "50 a 59" = "_50_59", "Mayores" = "mayores"),
                                 selected = c("_18_24", "_25_29", "_30_39", "_40_49", "_50_59", "mayores"),
                                 multiple = TRUE),
              shiny::selectInput("genero",
                                 "Seleccione géneros:",
                                 choices =  c("Hombre" = "hombres", "Mujer" = "mujeres"),
                                 selected = c("hombres", "mujeres"),
                                 multiple = TRUE)
            )
          ),
          column(
            width = 9,
            box(
              width = 12,
              leafletOutput("mapa_generos_edad", height = "80vh"),
              shiny::actionButton("mapGenerosEdadFullscreen", "Pantalla Completa"),
              downloadButton("download_edades", "Descargar Información (CSV)")
            )
          )
          
          
        )
      ),
      
      # Pestaña: Archivo Electoral ----
      tabItem(
        tabName = "creacion_tablas",
        fluidRow(
          box(
            title = "Archivo Electoral", collapsible = TRUE, collapsed = TRUE, width = 12, status = "success",
            p("Genere tablas con la cantidad de votos por partido. Utilice los filtros para seleccionar las secciones, partidos, elecciones y años de su interés.")
          ),
          box(
            width = 3,  
            shinyWidgets::pickerInput(inputId = "pik_secc_archivo", label = "Seleccione secciones:", options = list(size = 10, `live-search` = TRUE, `actions-box` = TRUE, `deselect-all-text` = "Ninguno", `select-all-text` = "Todos"), choices = sort(unique(cant_votos_nl$seccion)), selected = unique(cant_votos_nl$seccion), multiple = TRUE),
            shinyWidgets::pickerInput(inputId = "pik_partido_archivo", label = "Seleccione partidos:", options = list(size = 10, `live-search` = TRUE, `actions-box` = TRUE, `deselect-all-text` = "Ninguno", `select-all-text` = "Todos"), choices = unique(cant_votos_nl$partido), selected = unique(cant_votos_nl$partido), multiple = TRUE),
            shinyWidgets::pickerInput(inputId = "pik_elecc_archivo", label = "Seleccione elección:", options = list(size = 10, `live-search` = TRUE, `actions-box` = TRUE, `deselect-all-text` = "Ninguno", `select-all-text` = "Todos"), choices = unique(cant_votos_nl$eleccion), selected = unique(cant_votos_nl$eleccion), multiple = TRUE),
            shinyWidgets::pickerInput(inputId = "pik_año_archivo", label = "Seleccione años:", multiple = TRUE, options = list(size = 10, `live-search` = TRUE, `actions-box` = TRUE, `deselect-all-text` = "Ninguno", `select-all-text` = "Todos"), choices = sort(unique(cant_votos_nl$año)), selected = unique(cant_votos_nl$año))
          ),
          tabBox(
            width = 9,
            tabPanel("Tabla Agregada",
                     div(style = "display:flex; gap:5px; padding:5px;",
                         downloadBttn(outputId = "dw_archivo_csv_archivo", label = "CSV", size = "sm", icon = icon("file-csv"), color = "warning", style = "simple")
                     ),
                     DT::dataTableOutput("dt_agregada_archivo")
            ),
            tabPanel("Tabla Desagregada",
                     div(style = "display:flex; gap:5px; padding:5px;",
                         downloadBttn(outputId = "dw_archivo_csv_2_archivo", label = "CSV", size = "sm", icon = icon("file-csv"), color = "warning", style = "simple")
                     ),
                     DT::dataTableOutput("dt_desagregada_archivo")
            )
          )
        )
      ),
      
      # Pestaña: Rendimiento Histórico ----
      tabItem(
        tabName = "rendimiento_historico",
        fluidRow(
          column(width = 3,
                 box(
                   title = "Rendimiento Histórico por Partido", collapsible = TRUE, collapsed = TRUE, width = 12, status = "success",
                   p("Descubre los bastiones históricos de un partido. Simplemente elige un partido y las elecciones que deseas analizar, y el mapa te mostrará al instante las secciones que ha ganado.")
                 ),
                 box(
                   width = 12, 
                   shiny::selectInput("partido_ganador", "Selecciona el partido de interés", 
                                      choices = c("MC","PAN","PRI","VERDE","PT","MORENA","INDEP")),
                   shiny::selectInput("slt_candidatura_rh", 
                                      "Selecciona las candidaturas ganadas a visualizar",
                                      multiple = TRUE, 
                                      selected = choices_rendimiento_historico[1][[1]][1],
                                      choices = choices_rendimiento_historico
                                      )
                   )
                 
          ),
          column(width = 9,
                 box(width = 12,
                     leafletOutput("mapa_rendimiento_historico",height = "80vh"), # height = "80vh" mapa_pri   rendimiento_historico
                     shiny::actionButton("mapPriFullscreen", "Pantalla Completa"),
                     downloadButton("download_quitar", "Descargar Tabla (CSV)")
                 )
          )
        )
      ),
      
      # Pestaña: Evolución Electoral ----
      tabItem(
        tabName = "cambios_porcentuales",
        fluidRow(
          column(
            width = 3,
            box(
              width = 12,
              title = "Evolución Electoral", collapsible = TRUE, collapsed = TRUE, status = "success",
              p("Analice el cambio porcentual en la votación de un partido entre dos elecciones. Seleccione una elección base, una de comparación y el partido de interés.")
            ),
            box(
              width = 12,
              shiny::selectInput("eleccion1_a", "Selecciona la elección base", choices = lista_elecciones_nombres, selected = "Diputado Local 2015"),
              shiny::selectInput("eleccion2_a", "Selecciona la segunda elección", choices = lista_elecciones_nombres, selected = "Diputado Local 2021"),
              shiny::selectInput("partido_analisis_a", "Selecciona el partido a analizar", choices =  c("pan", "pri", "morena", "mc", "verde", "pt"))
            )
            ),
          column(
            width = 9,
            box(
              width = 12,
              h3("Tasa de cambio entre las dos elecciones"),
              leafletOutput("mapa_cambios_perc", height = "80vh"),
              shiny::actionButton("mapCambiosPercFullscreen", "Pantalla Completa"),
              downloadButton("download_tasas", "Descargar Tabla (CSV)")
            )
            )
          
          
        )
      ),
      
      # Pestaña: Fortaleza Electoral ----
      tabItem(
        tabName = "probabilidades",
        fluidRow(
          box(
            title = "Fortaleza Electoral (Machine Learning)", collapsible = TRUE, collapsed = TRUE, width = 12, status = "success",
            p("Explore la fortaleza relativa de un partido en cada sección, calculada con modelos de machine learning basados en datos históricos. Seleccione el partido para ver su probabilidad de ganar en el mapa.")
          )
        ),
        fluidRow(
          box(
            width = 3,
            shiny::selectInput("partido_predecir", "Selecciona el partido a analizar:", c("PRI", "morena", "MC", "PAN", "VERDE", "pt"))
          )
        ),
        box(
          width = 12,
          h3("Fortaleza Electoral"),
          leafletOutput("mapa_fortaleza", height = "80vh"),
          shiny::actionButton("mapFortalezaFullscreen", "Pantalla Completa"),
          downloadButton("download_fortalezas", "Descargar Tabla (CSV)")
        )
      ),
      
      # Pestaña: Datos Censales ----
      tabItem(
        tabName = "censales",
        
        # He modificado el título para que coincida con el nuevo nombre del menú y he quitado el 'collapsed'
        box(
          title = "Datos Poblacionales por Sección", 
          collapsible = TRUE, 
          collapsed = FALSE, # Es mejor mostrar la instrucción inicial por defecto
          width = 12, 
          status = "success",
          solidHeader = TRUE,
          p("Seleccione una variable demográfica en el panel de control para visualizar su distribución geográfica en el mapa y explorar los datos detallados en la tabla.")
        ),
        
        fluidRow(
          # --- Columna Izquierda: Panel de Control ---
          # Aquí se quedan todos los inputs y botones de acción que afectan a ambas salidas.
          column(
            width = 3,
            box(
              title = "Controles",
              status = "info",
              solidHeader = TRUE,
              width = 12, # Ancho 12 para llenar la columna de 3
              
              selectInput("censo_interes", 
                          "Elige la variable de interés:", 
                          choices = chs_censales, 
                          selected = chs_censales[1]),
              
              hr(), # Un separador visual
              
              # Botón de descarga con texto más descriptivo
              downloadBttn(
                outputId = "dw_censal_csv_censales", 
                label = "Descargar datos de la tabla", 
                size = "sm", 
                icon = icon("download"), 
                color = "primary", 
                style = "simple",
                block = TRUE # Hace que el botón ocupe todo el ancho
              )
            )
          ),
          
          # --- Columna Derecha: Salidas con Pestañas ---
          # Aquí es donde el mapa y la tabla se agrupan en el tabBox.
          column(
            width = 9,
            tabBox(
              id = "censales_tabset",
              width = 12, # Ancho 12 para llenar la columna de 9
              
              # Pestaña 1: Mapa
              tabPanel(
                title = "Mapa de Distribución", 
                icon = icon("map-marked-alt"),
                leafletOutput("mapa_censales", height = "80vh"),
                # Colocamos el botón de pantalla completa aquí, asociado al mapa
                actionButton("mapFullscreen", "Pantalla Completa", icon = icon("expand-arrows-alt"))
              ),
              
              # Pestaña 2: Tabla de Datos
              tabPanel(
                title = "Tabla de Datos", 
                icon = icon("table"),
                # Movemos la tabla desde el panel de control a su propia pestaña
                DTOutput("dt_query_censales")
              )
            )
          )
        )
      ),
      
      
      # Pestaña: Simulador Electoral ----
      tabItem(tabName = "simulaciones",
              fluidRow(
                column(
                  width = 3, 
                  box(
                    width = 12,
                    title = "Simulador Electoral", 
                    collapsible = TRUE, 
                    collapsed = TRUE,
                    solidHeader = FALSE,
                    status = "info",
                    p("El simulador electoral te permite generar y visualizar escenarios hipotéticos de elecciones. Sigue estos pasos:"),
                    tags$ol(
                      tags$li(
                        tags$b("Selecciona la Elección a Simular:"),
                        " Elige el proceso electoral de interés."
                      ),
                      tags$li(
                        tags$b("Define el Partido a Eliminar:"),
                        " Indica qué partido no participaría en tu simulación."
                      ),
                      tags$li(
                        tags$b("Asigna el Destinatario de los Votos:"),
                        " Selecciona el partido que recibiría los votos del partido eliminado."
                      )
                    ),
                    p("El mapa interactivo resultante mostrará el partido ganador en cada sección bajo tu escenario simulado. Al pasar el cursor, verás detalles de los resultados. Si el partido eliminado es también el destinatario, se simulará su ausencia total sin redistribución de votos."),
                    p("Esta herramienta te ayuda a comprender cómo la redistribución de votos impactaría los resultados electorales.")
                  ),
                  box(width = 12, 
                      shiny::selectInput("eleccion_sim1", 
                                         "Selecciona la elección a simular",
                                         choices = 
                                           list(
                                             "Elecciones 2024" = c("Presidente 2024" = "pres24", "Senado 2024" = "sen24","Diputado Local 2024" = "dipl24", "Diputado Federal 2024" = "fed24", "Alcalde 2024" = "ayunt24"),
                                             "Elecciones 2021" = c("Diputado Local 2021" = "dl21" , "Diputado Federal 2021" = "fed21", "Alcalde 2021" = "ayunt21", "Gobernador 2021" = "gob21"),
                                             "Elecciones 2018" = c("Diputado Local 2018" = "dipl18", "Senado 2018" = "sen18", "Alcalde 2018" = "ayunt18","Presidente 2018" = "pres18", "Diputado Federal 2018" = "fed18"),
                                             "Elecciones 2015" = c("Gobernador 2015" = "gob15", "Alcalde 2015" = "ayunt15", "Diputado Local 2015" = "dipl15","Diputado Federal 2015" = "fed15")
                                           )
                      ),
                      shiny::selectInput("dejar_afuera", 
                                         "Selecciona al partido que quieres dejar afuera. ",
                                         choices =  c("PAN" = "pan", "PRI" = "pri", "Morena" = "morena", "MC" = "mc", "Verde" = "verde", "pt")
                      ),
                      shiny::selectInput("recibe_votos", 
                                         "Selecciona el destino de los votos de este partido",
                                         choices =  c("PAN" = "pan", "PRI" = "pri","Morena" = "morena", "MC" = "mc", "Verde" = "verde", "pt")
                      )
                  ),
                ),
                column(
                  width = 9,
                  box(width = 12,
                      leafletOutput("mapa_sim1", height = "80vh"),
                      shiny::actionButton("mapSim1Fullscreen", "Fullscreen"),
                      downloadButton("downloadganador_sim", "Descargar la tabla como CSV")
                      )
                  )
                )
              ),
      
      # Comparativa ganadas/perdidas ----
      tabItem(tabName = "robados",
              fluidRow(
                box(
                  title = "Comparativa ganadas/perdidas", 
                  collapsible = TRUE, collapsed = TRUE,
                  width = 12,
                  solidHeader = F, 
                  status = "primary",  
                  
                  p("Esta sección te permite realizar un análisis comparativo entre dos elecciones para entender dónde un partido específico ganó, perdió o mantuvo su posición."),
                  
                  h4("¿Cómo funciona?"),
                  p("Selecciona dos elecciones y un partido de interés en el panel de la izquierda. La aplicación analizará los resultados por sección electoral y los visualizará en tres mapas interactivos:"),
                  
                  tags$ul(
                    tags$li(strong("1. Mapa General:"), " Muestra el panorama completo. Las secciones ganadas por el partido se marcan en verde, las perdidas en rojo y las que no cambiaron de resultado en gris o blanco."),
                    tags$li(strong("2. Mapa de Secciones Ganadas:"), " Se enfoca en las victorias. Muestra a qué partido se le 'arrebató' cada sección ganada."),
                    tags$li(strong("3. Mapa de Secciones Perdidas:"), " Se enfoca en las derrotas. Muestra qué partido fue el que ganó la sección que antes tenía tu partido de interés.")
                  ),
                  
                  p("Estos insights son clave para entender la dinámica de votantes y desarrollar estrategias electorales más efectivas.")
                )
              ),
              
              fluidRow(
                box(
                  width = 3,
                  title = "Filtros de Análisis",
                  solidHeader = F,
                  status = "success",
                  
                  selectInput("eleccion1", 
                              "1. Selecciona la elección base",
                              choices = choices_rendimiento_historico
                              ),
                  
                  selectInput("eleccion2", 
                              "2. Selecciona la elección a comparar",
                              choices = choices_rendimiento_historico
                              ),
                  
                  selectInput("partido_analisis", 
                              "3. Selecciona el partido a analizar",
                              choices = c("PAN","PRI","VERDE","PT","MC","MORENA","INDEP")),
                  
                  hr(), 
                  
                  downloadButton("download_robados", "Descargar Datos (CSV)", class = "btn-block")
                ),
                
                tabBox(
                  width = 9,
                  id = "map_tabs",
                  title = "Visualización Comparativa",
                  
                  # Pestaña 1: Mapa General
                  tabPanel(
                    title = "Aspecto General", 
                    icon = icon("globe-americas"),
                    # Usamos withSpinner para el efecto de carga
                    shinycssloaders::withSpinner(
                      leafletOutput("mapa_gana_o_pierde", height = "78vh"),
                      type = 6, color = "#00c0ef"
                    ),
                    actionButton("mapGanaOPierdeFullscreen", "Pantalla Completa", icon = icon("expand-arrows-alt"), class = "pull-right")
                  ),
                  
                  # Pestaña 2: Mapa de Secciones Ganadas
                  tabPanel(
                    title = "Ganadas", 
                    icon = icon("plus-circle"),
                    shinycssloaders::withSpinner(
                      leafletOutput("mapa_gana", height = "78vh"),
                      type = 6, color = "#00a65a"
                    ),
                    actionButton("mapGanaFullscreen", "Pantalla Completa", icon = icon("expand-arrows-alt"), class = "pull-right")
                  ),
                  
                  # Pestaña 3: Mapa de Secciones Perdidas
                  tabPanel(
                    title = "Perdidas", 
                    icon = icon("minus-circle"),
                    shinycssloaders::withSpinner(
                      leafletOutput("mapa_pierde", height = "78vh"),
                      type = 6, color = "#f56954"
                    ),
                    actionButton("mapPierdeFullscreen", "Pantalla Completa", icon = icon("expand-arrows-alt"), class = "pull-right")
                  )
                )
              )
              ),
      
      
      
      
      
      
      # Participación Electoral ----
      tabItem(tabName = "participacion",
              fluidRow(
                box(
                  title = "Participación Electoral", collapsible = T,collapsed = T,
                  width = 12,
                  solidHeader = F,
                  status = "success"
                ),
                box(width = 3, 
                    shiny::selectInput("participacion", 
                                       "¿Qué quieres observar?",
                                       choices =  c(
                                         "Lista Nominal", "Tasa de participación", "Cantidad de votos"
                                       ),
                                       selected = c("Lista Nominal")
                    )
                ),
                box(width = 9,
                    leafletOutput("mapa_participacion", height = "80vh")
                )
              )
              
      ),
      
      # Lealtad Partidista ----
      tabItem(
        tabName = "lealtad",
        fluidRow(
          box(width = 12,
              leafletOutput("mapa_lealtad", height = "80vh")
          )
        )
      ),
      
      ### Perfil de colonias ----
      tabItem(tabName = "información_colonias",
              
              # --- CAJA DE INSTRUCCIONES MEJORADA ---
              # La mantenemos colapsada para no estorbar, pero con mejor formato.
              fluidRow(
                box(
                  title = "Guía de Uso: Perfiles de Colonias",
                  collapsible = TRUE, collapsed = TRUE,
                  width = 12,
                  solidHeader = TRUE, 
                  status = "primary", 
                  
                  # Usamos listas y texto en negrita para que sea más fácil de leer
                  p("Esta sección te permite explorar los datos demográficos y electorales a nivel de colonia."),
                  tags$ul(
                    tags$li(tags$strong("Paso 1:"), "Usa el 'Panel de Control' a la izquierda para comenzar."),
                    tags$li(tags$strong("Paso 2:"), "Elige una colonia en el primer selector. Su información demográfica aparecerá al instante."),
                    tags$li(tags$strong("Paso 3:"), "Explora las visualizaciones a la derecha, que se actualizarán según tus selecciones:"),
                    tags$ul(
                      tags$li("En la pestaña 'Resultados por Elección', puedes elegir una elección específica para ver cómo votó esa colonia."),
                      tags$li("En la pestaña 'Histórico por Partido', elige un partido para ver su evolución a lo largo de todas las elecciones en esa colonia.")
                    )
                  )
                )
              ),
              
              # --- DISEÑO PRINCIPAL EN DOS COLUMNAS ---
              fluidRow(
                
                # --- COLUMNA 1: PANEL DE CONTROL (ANCHO 4) ---
                column(width = 4,
                       box(
                         title = "Panel de Control",
                         status = "primary",
                         solidHeader = TRUE,
                         width = NULL, # El ancho NULL hace que la caja ocupe toda la columna
                         
                         # Selector principal de colonia
                         uiOutput("colonias_seleccionadas"),
                         
                         # Separador visual
                         hr(),
                         
                         # Texto con la información demográfica, ahora dentro del panel
                         h4("Información Demográfica"),
                         textOutput("texto_colonia"),
                         
                         hr(),
                         
                         # Controles para los gráficos
                         h4("Filtros para Gráficos"),
                         
                         shiny::selectInput("eleccion_colonia", 
                                            "1. Selecciona una elección:",
                                            choices =  c("Alcalde 2024" = "ayunt24", "Diputado Local 2024" = "dipl24", "Diputado Federal 2024" = "fed24", "Senado 2024" = "sen24",
                                                         "Alcalde 2021" = "ayunt21", "Gobernador 2021" = "gob21", "Diputado Local 2021" = "dl21" , "Diputado Federal 2021" = "fed21",
                                                         "Alcalde 2018" = "ayunt18", "Presidente 2018" = "pres18", "Senado 2018" = "sen18", "Diputado Local 2018" = "dipl18", "Diputado Federal 2018" = "fed18",
                                                         "Alcalde 2015" = "ayunt15", "Gobernador 2015" = "gob15")
                         ),
                         
                         shiny::selectInput(
                           "partido_colonia", 
                           "2. Selecciona un partido:",
                           choices =   c("PAN" = "pan", "PRI" = "pri", "Morena" = "morena", "MC" = "mc", "Verde" = "verde", "PT" = "pt", "Independiente" = "indep")
                         )
                       )
                ),
                
                # --- COLUMNA 2: VISUALIZACIONES (ANCHO 8) ---
                column(width = 8,
                       # Usamos un tabBox para organizar los dos gráficos
                       tabBox(
                         title = "Visualizaciones",
                         id = "tabset_colonias",
                         width = NULL,
                         
                         # Pestaña para el primer gráfico
                         tabPanel("Resultados por Elección",
                                  p("Muestra la proporción de votos que cada partido obtuvo en la elección seleccionada."),
                                  plotOutput("grafico_colonia"),
                                  downloadButton("download_votos_colonia", "Descargar datos como CSV")
                         ),
                         
                         # Pestaña para el segundo gráfico
                         tabPanel("Histórico por Partido",
                                  p("Muestra el desempeño histórico del partido seleccionado a través de todas las elecciones."),
                                  plotOutput("grafico_colonia_tiempo"),
                                  downloadButton("download_votos_colonia_tiempo", "Descargar datos como CSV")
                                  )
                         )
                       )
                )
              ),
      
      # Gestión de tiempo ---- 
      tabItem(tabName = "tiempoxsección",
              fluidRow(
                box(
                  title = "Gestión de tiempo de campaña", collapsible = T,collapsed = T,
                  width = 12,
                  solidHeader = F,
                  status = "success",
                  p("En esta pestaña podrás observar cuantas horas deben dedicarse en total a cada sección en los días de campaña.
                       El tiempo dedicado a cada sección depende de la cantidad de días de campaña y de la cantidad de equipos que existen 
                       para recorrer el municipio.")
                ),
                box(width = 3, 
                    shiny::sliderInput("dias_campaña", "Selecciona la cantidad de días de campaña", min = 0, max = 120, step = 10, value = 90),
                    shiny::sliderInput("equipos", 
                                       "Selecciona la cantidad de equipos que van a recorrer el municipio",
                                       min = 1, max = 20, step = 1, value = 3)
                ),
                box(width = 9,
                    h3("Aspecto general "),
                    leafletOutput("mapa_visitas_secciones", height = "80vh"),
                    shiny::actionButton("mapVisitasFullscreen", "Fullscreen"),
                    downloadButton("download_basej", "Descargar la base informativa")
                )
              )
      )
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
      
    ) # Cierre de tabItems
  ) # Cierre de dashboardBody
) # Cierre de dashboardPage





ui <- secure_app(
  ui_code, 
  background  = "linear-gradient(rgba(255, 246, 246, 1), 
                rgba(250, 240, 202, 0.5)),
                url('https://aleadomi.wordpress.com/wp-content/uploads/2025/06/boletia.png')  repeat center fixed;", 
  language = "es")















