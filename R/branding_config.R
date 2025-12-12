# R/branding_config.R

obtener_configuracion_marca <- function(marca = "QUIPU") {
  
  if (marca == "QUIPU") {
    list(
      # --- Identidad Básica ---
      app_title = "QUIPU Electoral",
      logo_path = "bienvenida_quipu_logo.png",
      browser_title = "QUIPU - Análisis",
      welcome_title = "Bienvenido a QUIPU Electoral",
      welcome_subtitle = "Tu centro de comando para el análisis geo-electoral.",
      
      # --- Colores de UI (Shinydashboard) ---
      # Colores para cajas (boxes) y botones estándar
      status_primary = "primary",   # Azul estándar
      status_info    = "info",      # Celeste
      
      # --- Colores PERSONALIZADOS (Hexadecimales) ---
      # Header (Barra superior)
      header_bg_color = "#0D3B66",      # Azul Profundo Quipu
      header_hover_color = "#1D4E80",   # Un poco más claro para hover
      
      # Logo (Esquina superior izquierda)
      logo_bg_color = "#082846",        # Azul más oscuro
      logo_text_color = "#FAF0CA",      # Dorado suave
      
      # Sidebar (Barra lateral)
      sidebar_bg_color = "#2C3E50",     # Gris azulado oscuro
      sidebar_hover_color = "#34495E",  
      sidebar_active_color = "#0D3B66", # El color de la pestaña activa
      sidebar_active_border = "#FAF0CA",# Borde izquierdo dorado
      
      # Login Background
      login_background = "linear-gradient(rgba(13, 59, 102, 0.9), rgba(250, 240, 202, 0.4)), url('https://aledomicom.files.wordpress.com/2024/03/bienvenida_quipu.png') center fixed;"
    )
    
  } else if (marca == "BOLETA") {
    list(
      # --- Identidad Básica ---
      app_title = "BOLETA Electoral",
      logo_path = "boleta_logo.png",
      browser_title = "BOLETA - Dashboard",
      welcome_title = "Bienvenidos a BOLETA Electoral",
      welcome_subtitle = "Una herramienta de análisis GeoElectoral interactiva.",
      
      # --- Colores de UI ---
      status_primary = "danger",    # Rojo estándar
      status_info    = "warning",   # Naranja
      
      # --- Colores PERSONALIZADOS (Hexadecimales) ---
      # Header
      header_bg_color = "#B41A1A",      # Rojo Morena/Institucional
      header_hover_color = "#D62424",   # Rojo más claro
      
      # Logo
      logo_bg_color = "#8F1515",        # Rojo oscuro
      logo_text_color = "#FFFFFF",      # Blanco
      
      # Sidebar
      sidebar_bg_color = "#222d32",     # Gris clásico oscuro
      sidebar_hover_color = "#4b646f",
      sidebar_active_color = "#B41A1A", # Rojo activo
      sidebar_active_border = "#FFFFFF",# Borde blanco
      
      # Login Background
      login_background = "linear-gradient(rgba(180, 26, 26, 0.8), rgba(255, 255, 255, 0.7)), url('https://aleadomi.wordpress.com/wp-content/uploads/2025/06/boletia.png') center fixed;"
    )
  }
}