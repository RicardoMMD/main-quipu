# ==============================================================================
# UI.R - Estructura Principal Modularizada
# ==============================================================================

# Definición de la interfaz
ui_code <- dashboardPage(
  
  title = APP_CONFIG$browser_title, # Título de la pestaña del navegador
  skin = "blue",
  
  
  # 1. Header
  dashboardHeader(
    titleWidth = "25rem",
    title = tags$span(
      tags$img(src = APP_CONFIG$logo_path, 
               height = "40", 
               style = "margin-top:-5px; margin-right:5px;"),
      APP_CONFIG$app_title
    )
  ),
  
  # 2. Barra Lateral (Sidebar) -------------------------------------------------
  dashboardSidebar(
    width = "25rem",
    disable = FALSE,
    
    # Recursos globales
    tags$head(
      tags$style(
        generar_css_dinamico(APP_CONFIG)
      )
    ),
    # includeCSS("www/styles.css"),
    includeScript("www/main.js"),
    useShinyjs(),
    
    # --- FILTROS GLOBALES ---
    shiny::selectInput("tipo_filtro_inicial",
                       "Nivel geográfico:",
                       choices = c("Municipio", "DFederal", "DLocal", "Ninguno"),
                       multiple = FALSE),
    
    hidden(
      shiny::selectInput("municipio_inicial", "Selecciona el municipio:", choices = NULL),
      shiny::selectInput("federal_inicial", "Selecciona el distrito federal:", choices = NULL),
      shiny::selectInput("local_inicial", "Selecciona el distrito local:", choices = NULL)
    ),
    
    # --- MENÚ DE NAVEGACIÓN ---
    sidebarMenu(
      shinydashboard::menuItem("Inicio", tabName = "home", icon = icon("home")),
      
      shinydashboard::menuItem("Información Electoral", icon = icon("vote-yea"), startExpanded = FALSE,
                               shinydashboard::menuSubItem("Participación Electoral",           tabName = "participacion"),
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
      
      shinydashboard::menuItem("Datos Poblacionales",           tabName = "censales",             icon = icon("users-cog")),
      shinydashboard::menuItem("Demografía Espacial",           tabName = "edades",               icon = icon("id-card")),
      shinydashboard::menuItem("Población y votos por manzana", tabName = "data_mza",             icon = icon("map-marker-alt")),
      shinydashboard::menuItem("Perfil de colonias",            tabName = "información_colonias", icon = icon("map-signs")),
      shinydashboard::menuItem("Gestión de tiempo",             tabName = "tiempoxsección",       icon = icon("calendar-alt")),
      shinydashboard::menuItem("Fortaleza Electoral",           tabName = "probabilidades",       icon = icon("shield-alt")),
      
      shinydashboard::menuItem("Segmentación IA", tabName = "clustering_ia", icon = icon("brain"))
      
      
      
    )
  ),
  
  # 3. Cuerpo del Dashboard (Body) ---------------------------------------------
  dashboardBody(
    # Scripts JS personalizados para Fullscreen
    extendShinyjs(
      script = "www/main.js", 
      functions = c("fullscreen", "fullscreenFortaleza", "fullscreenVisitas", 
                    "fullscreenGanaOPierde", "fullscreenGana", "fullscreenPierde", 
                    "fullscreenGanador1", "fullscreenGanador2", "fullscreenGenerosEdad", 
                    "fullscreenPri", "fullscreenSim1", "fullscreenCambiosPerc", 
                    "fullscreenManzanas", "fullscreenSombra", "fullscreenDiferencia")
    ),
    
    # Contenido de las Pestañas (Llamadas a Módulos)
    tabItems(
      
      # 1. Inicio
      tabItem(tabName = "home", 
              mod_home_ui("home_1")),
      
      # 2. Datos Manzana
      tabItem(tabName = "data_mza", 
              mod_data_mza_ui("data_mza_1")),
      
      # 3. Comparativo Histórico
      tabItem(tabName = "ganadores", 
              mod_ganadores_ui("ganadores_1")),
      
      # 4. Voto Sombra
      tabItem(tabName = "sombra", 
              mod_sombra_ui("sombra_1")),
      
      # 5. Distancia 1ro vs 2do
      tabItem(tabName = "diferencias", 
              mod_diferencias_ui("diferencias_1")),
      
      # 6. Demografía Espacial
      tabItem(tabName = "edades", 
              mod_demografia_ui("demografia_1")),
      
      # 7. Archivo Electoral (BD Resultados)
      tabItem(tabName = "creacion_tablas", 
              mod_archivo_electoral_ui("archivo_1")),
      
      # 8. Rendimiento Histórico (Voto Diferenciado)
      tabItem(tabName = "rendimiento_historico", 
              mod_rendimiento_ui("rendimiento_1")),
      
      # 9. Evolución Partidista
      tabItem(tabName = "cambios_porcentuales", 
              mod_evolucion_ui("evolucion_1")),
      
      # 10. Fortaleza Electoral
      tabItem(tabName = "probabilidades", 
              mod_fortaleza_ui("fortaleza_1")),
      
      # 11. Datos Censales
      tabItem(tabName = "censales", 
              mod_censales_ui("censales_1")),
      
      # 12. Simulador
      tabItem(tabName = "simulaciones", 
              mod_simulador_ui("simulador_1")),
      
      # 13. Comparativa Ganadas/Perdidas
      tabItem(tabName = "robados", 
              mod_comparativa_gp_ui("comparativa_gp_1")),
      
      # 14. Participación
      tabItem(tabName = "participacion", 
              mod_participacion_ui("participacion_1")),
      
      # 15. Lealtad
      tabItem(tabName = "lealtad", 
              mod_lealtad_ui("lealtad_1")),
      
      # 16. Colonias
      tabItem(tabName = "información_colonias", 
              mod_colonias_ui("colonias_1")),
      
      # 17. Gestión de Tiempo
      tabItem(tabName = "tiempoxsección", 
              mod_gestion_tiempo_ui("tiempo_1")),
      
      tabItem(tabName = "clustering_ia",
              mod_clustering_secciones_ui("cluster_1"))
      
    )
  )
)

# 4. Envoltura de Seguridad (Login) --------------------------------------------
ui <- secure_app(
  ui_code, 
  background  = APP_CONFIG$login_background,
  language = "es"
)