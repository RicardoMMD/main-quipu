# R/css_generator.R

generar_css_dinamico <- function(config) {
  
  # Usamos glue o paste para insertar las variables en el CSS
  # Nota: "skin-blue" es la clase base que vamos a sobrescribir
  
  HTML(paste0("
    /* --- HEADER (Barra Superior) --- */
    .skin-blue .main-header .navbar {
      background-color: ", config$header_bg_color, " !important;
    }
    
    /* Hover sobre los elementos del header (ej. botón de ocultar menú) */
    .skin-blue .main-header .navbar .sidebar-toggle:hover {
      background-color: ", config$header_hover_color, " !important;
    }
    
    /* --- LOGO AREA --- */
    .skin-blue .main-header .logo {
      background-color: ", config$logo_bg_color, " !important;
      color: ", config$logo_text_color, " !important;
    }
    .skin-blue .main-header .logo:hover {
      background-color: ", config$header_hover_color, " !important;
    }
    
    /* --- SIDEBAR (Barra Lateral) --- */
    .skin-blue .main-sidebar {
      background-color: ", config$sidebar_bg_color, " !important;
    }
    
    /* Item del menú activo (seleccionado) */
    .skin-blue .sidebar-menu > li.active > a,
    .skin-blue .sidebar-menu > li:hover > a {
      border-left-color: ", config$sidebar_active_border, " !important;
      background-color: ", config$sidebar_active_color, " !important;
      color: #fff !important;
    }
    
    /* Submenús */
    .skin-blue .sidebar-menu .treeview-menu {
      background-color: ", config$sidebar_bg_color, " !important;
      padding-left: 5px;
    }
  "))
}