# ==============================================================================
# MÓDULO: Página de Inicio (Home Dashboard)
# ==============================================================================

# 1. UI DEL MÓDULO -------------------------------------------------------------
mod_home_ui <- function(id) {
  ns <- NS(id)
  
  tagList(
    # Fila 1: Bienvenida
    fluidRow(
      box(
        
        title = tagList(shiny::icon("chart-pie"), "Bienvenidos a BOLETA Electoral"),
        status = "primary",
        
        solidHeader = TRUE,
        width = 12,
        collapsible = FALSE,
        
         h3(APP_CONFIG$welcome_subtitle),
        p("Esta plataforma está diseñada para explorar, analizar y simular datos electorales...", style = "font-size: 16px;"),
        
        
        # Salida de texto dinámica para el rol
        textOutput(ns("user_role_display"))
      )
    ),
    
    # Fila 2: Guía Rápida
    fluidRow(
      box(
        title = tagList(shiny::icon("rocket"), "Cómo Empezar"),
        status = "info",
        solidHeader = TRUE,
        width = 12,
        collapsible = TRUE,
        
        p("Sigue estos sencillos pasos para comenzar tu análisis:", style = "font-size: 16px;"),
        tags$ol(
          tags$li(tags$strong("Elige tu área de interés:"), "Utiliza los selectores en el panel izquierdo para filtrar por Municipio, Distrito Federal o Distrito Local."),
          tags$li(tags$strong("Navega por las herramientas:"), "Selecciona cualquiera de los módulos en el menú para acceder a mapas, gráficos y tablas interactivas."),
          tags$li(tags$strong("Explora y descarga:"), "Interactúa con las visualizaciones y descarga los datos que necesites en formato CSV.")
        )
      )
    ),
    
    # Fila 3: Título Funcionalidades
    fluidRow(
      column(width = 12, h3("Principales Funcionalidades"))
    ),
    
    # Fila 4: Grid de InfoBoxes (Accesos Directos Visuales)
    fluidRow(
      infoBox("Visualizador Histórico", "Observa los ganadores por sección.", icon = icon("history"), color = "aqua", width = 4, fill = TRUE),
      infoBox("Demografía Espacial", "Distribución de edad y género.", icon = icon("id-card"), color = "light-blue", width = 4, fill = TRUE),
      infoBox("Archivo Electoral", "Tablas de votos personalizadas.", icon = icon("folder-open"), color = "teal", width = 4, fill = TRUE),
      
      infoBox("Rendimiento Histórico", "Desempeño partidista.", icon = icon("chart-line"), color = "green", width = 4, fill = TRUE),
      infoBox("Simulador Electoral", "Escenarios hipotéticos.", icon = icon("gamepad"), color = "yellow", width = 4, fill = TRUE),
      infoBox("Evolución Electoral", "Cambio porcentual de votos.", icon = icon("chart-bar"), color = "orange", width = 4, fill = TRUE),
      
      infoBox("Comparativa", "Secciones ganadas y perdidas.", icon = icon("balance-scale-right"), color = "red", width = 4, fill = TRUE),
      infoBox("Perfil de Colonias", "Datos a nivel colonia.", icon = icon("map-signs"), color = "purple", width = 4, fill = TRUE),
      infoBox("Fortaleza Electoral", "Probabilidad de triunfo (ML).", icon = icon("shield-alt"), color = "maroon", width = 4, fill = TRUE)
    )
  )
}

# 2. SERVER DEL MÓDULO ---------------------------------------------------------
mod_home_server <- function(id, user_role_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Mostrar el rol del usuario (recibido desde el server principal)
    output$user_role_display <- renderText({
      paste("Rol de Usuario Activo:", user_role_reactive())
    })
    
  })
}