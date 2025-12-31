# ==============================================================================
# SERVER.R - Controlador Principal
# ==============================================================================

function(input, output, session) {
  
  # ----------------------------------------------------------------------------
  # AGREGAR ESTO EN server.R (Sección 2 - Filtros Globales)
  # ----------------------------------------------------------------------------
  

  
  # 1. AUTENTICACIÓN Y GESTIÓN DE ROLES ----------------------------------------
  
  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  # Rol reactivo para pasar a los módulos si es necesario
  user_role <- reactive({
    req(res_auth$role)
    return(res_auth$role)
  })
  
  output$user_role <- renderText({
    paste("User Role:", user_role())
  })
  
  # Observador para mostrar/ocultar los selectores según el Nivel Geográfico
  observeEvent(input$tipo_filtro_inicial, {
    
    switch(input$tipo_filtro_inicial,
           "Municipio" = {
             shinyjs::show("municipio_inicial", anim = TRUE, animType = "slide")
             shinyjs::hide("federal_inicial",   anim = TRUE, animType = "slide")
             shinyjs::hide("local_inicial",     anim = TRUE, animType = "slide")
           },
           "DFederal" = {
             shinyjs::hide("municipio_inicial", anim = TRUE, animType = "slide")
             shinyjs::show("federal_inicial",   anim = TRUE, animType = "slide")
             shinyjs::hide("local_inicial",     anim = TRUE, animType = "slide")
           },
           "DLocal" = {
             shinyjs::hide("municipio_inicial", anim = TRUE, animType = "slide")
             shinyjs::hide("federal_inicial",   anim = TRUE, animType = "slide")
             shinyjs::show("local_inicial",     anim = TRUE, animType = "slide")
           },
           "Ninguno" = {
             shinyjs::hide("municipio_inicial", anim = TRUE, animType = "slide")
             shinyjs::hide("federal_inicial",   anim = TRUE, animType = "slide")
             shinyjs::hide("local_inicial",     anim = TRUE, animType = "slide")
           }
    )
  })
  
  
  # 2. LÓGICA DE FILTROS GLOBALES (SIDEBAR) ------------------------------------
  
  # Actualizar opciones del sidebar basado en el ROL del usuario
  observeEvent(user_role(), {
    role <- user_role()
    
    if (role == "usuario_monterrey") {
      choices_geograficos <- c("Municipio", "DFederal", "DLocal")
      
      # Datos específicos para MTY
      secciones_mty <- secciones_prev %>% filter(MUNICIPIO == 40)
      mun_rol           <- unique(secciones_mty$NOMBRE)
      distr_federal_rol <- unique(secciones_mty$DISTRITO)
      distr_loclal_rol  <- unique(secciones_mty$DISTRITO_L)
      
    } else if(role == "usuario_garcia"){
      choices_geograficos <- c("Municipio", "DLocal")
      
      # Datos específicos para Garcia
      dta_garcia <- secciones_prev %>% filter(NOMBRE == "GARCIA")
      mun_rol           <- "GARCIA"
      distr_federal_rol <- unique(dta_garcia$DISTRITO)
      distr_loclal_rol  <- unique(dta_garcia$DISTRITO_L)
      
    } else {
      # Usuario Nuevo León / Admin
      choices_geograficos <- c("Municipio", "DFederal", "DLocal", "Ninguno")
      mun_rol           <- unique(secciones_prev$NOMBRE)
      distr_federal_rol <- unique(secciones_prev$DISTRITO)
      distr_loclal_rol  <- unique(secciones_prev$DISTRITO_L)
    }
    
    # Actualizar inputs del UI
    updateSelectInput(session, "tipo_filtro_inicial", choices = choices_geograficos)
    updateSelectInput(session, "municipio_inicial", choices = mun_rol)
    updateSelectInput(session, "federal_inicial", choices = distr_federal_rol)
    updateSelectInput(session, "local_inicial", choices = distr_loclal_rol)
    
  }, ignoreNULL = TRUE)
  
  # 1. CREAR REACTIVO DE CONTEXTO GEOGRÁFICO
  # Este reactivo genera el texto que describe qué estamos viendo
  contexto_geografico_actual <- reactive({
    req(input$tipo_filtro_inicial)
    
    texto <- switch(input$tipo_filtro_inicial,
                    "Municipio" = paste("del Municipio de", input$municipio_inicial),
                    "DFederal"  = paste("del Distrito Federal", input$federal_inicial),
                    "DLocal"    = paste("del Distrito Local", input$local_inicial),
                    "Ninguno"   = "de todo el Estado de Nuevo León"
    )
    return(texto)
  })
  
  
  # 3. REACTIVOS MAESTROS (FUENTE DE VERDAD) -----------------------------------
  # Estos reactivos se calculan una vez aquí y se pasan a los módulos
  
  # A) Secciones Filtradas (Geometría Base)
  secciones_reactivas <- reactive({
    req(input$tipo_filtro_inicial)
    
    # Validaciones de requerimiento según el tipo de filtro
    if (input$tipo_filtro_inicial == "Municipio") req(input$municipio_inicial)
    if (input$tipo_filtro_inicial == "DFederal") req(input$federal_inicial)
    if (input$tipo_filtro_inicial == "DLocal") req(input$local_inicial)
    
    role <- user_role()
    
    # 1. Filtrado por ROL (Seguridad)
    secciones_filtradas_por_rol <- secciones_prev %>%
      #st_transform(crs = "+proj=longlat +datum=WGS84") %>%
      mutate(filter_rol = case_when(
        role == "usuario_nuevo_leon" ~ TRUE,
        role == "usuario_monterrey" ~ MUNICIPIO == 40,
        role == "usuario_garcia" ~ MUNICIPIO == 18,
        TRUE ~ FALSE 
      )) %>%
      filter(filter_rol) 
    
    # 2. Filtrado por UI (Sidebar)
    condicion_filtro_ui <- switch(input$tipo_filtro_inicial,
                                  "Municipio" = secciones_filtradas_por_rol$NOMBRE %in% input$municipio_inicial,
                                  "DFederal"  = secciones_filtradas_por_rol$DISTRITO %in% input$federal_inicial,
                                  "DLocal"    = secciones_filtradas_por_rol$DISTRITO_L %in% input$local_inicial,
                                  "Ninguno"   = TRUE
    )
    
    secciones_finales <- secciones_filtradas_por_rol %>% filter(condicion_filtro_ui)
    return(secciones_finales)
  })
  
  # B) Ranking de Votos (Cálculos Electorales)
  # Usado por: Ganadores, Sombra, Diferencias, Simulador
  rank_por_seccion_eleccion <- reactive({
    cant_votos_nl %>%
      group_by(seccion, eleccion_año_id) %>%
      arrange(desc(votos)) %>%
      mutate(
        votos_totales_seccion = sum(votos, na.rm = T),
        lugar = row_number()
      ) %>%
      select(seccion, eleccion_año_id, votos_totales_seccion, partido, votos, lugar) %>%
      pivot_wider(
        names_from = lugar,
        values_from = c(partido, votos),
        names_sep = "_"
      ) %>%
      ungroup()
  })
  
  # C) Base Ganadores Procesada
  # Usado por: Lealtad, Gestión de Tiempo
  base_ganadores_pr <- reactive({
    x <- secciones_reactivas()
    y <- st_drop_geometry(base_ganadores)
    z <- merge(x, y, by.x = "SECCION", by.y = "seccion", all.x = T)
    
    z <- z %>% 
      mutate(across(gober_15:pres_24, ~ replace_na(., "otro"))) %>% 
      mutate(across(gober_15:pres_24, ~ ifelse(. == "PVEM", "VERDE", .)))
    return(z)
  })
  
  
  # 4. LLAMADAS A MÓDULOS ------------------------------------------------------
  # Pasamos los reactivos SIN paréntesis () para pasar la expresión, no el valor
  
  # 1. Inicio
  mod_home_server("home_1", user_role)
  
  # 2. Datos Manzana (Requiere inputs globales del sidebar para lógica interna)
  mod_data_mza_server("data_mza_1", 
                      secciones_reactivas, 
                      inputs_globales = list(
                        tipo = reactive(input$tipo_filtro_inicial),
                        mun = reactive(input$municipio_inicial),
                        fed = reactive(input$federal_inicial),
                        loc = reactive(input$local_inicial)
                      ))
  
  # 3. Comparativo Histórico
  mod_ganadores_server("ganadores_1", secciones_reactivas, rank_por_seccion_eleccion)
  
  # 4. Voto Sombra
  mod_sombra_server("sombra_1", secciones_reactivas, rank_por_seccion_eleccion)
  
  # 5. Distancia 1ro vs 2do
  mod_diferencias_server("diferencias_1", secciones_reactivas) # Diferencias calcula su propia lógica interna basada en res_trab
  
  # 6. Demografía Espacial
  mod_demografia_server("demografia_1", secciones_reactivas)
  
  # 7. Archivo Electoral
  mod_archivo_electoral_server("archivo_1", 
                               secciones_reactivas, 
                               input_filtro_tipo = reactive(input$tipo_filtro_inicial))
  
  # 8. Rendimiento Histórico
  mod_rendimiento_server("rendimiento_1", secciones_reactivas)
  
  # 9. Evolución Partidista
  mod_evolucion_server("evolucion_1", secciones_reactivas)
  
  # 10. Fortaleza Electoral
  mod_fortaleza_server("fortaleza_1", secciones_reactivas)
  
  # 11. Datos Censales
  mod_censales_server("censales_1", secciones_reactivas)
  
  # 12. Simulador
  mod_simulador_server("simulador_1", secciones_reactivas)
  
  # 13. Comparativa Ganadas/Perdidas
  mod_comparativa_gp_server("comparativa_gp_1", secciones_reactivas, rank_por_seccion_eleccion)
  
  # 14. Participación
  mod_participacion_server("participacion_1", secciones_base = secciones_reactivas)
  
  
  
  
  # 15. Lealtad
  mod_lealtad_server("lealtad_1", base_ganadores_pr)
  
  # 16. Colonias
  mod_colonias_server("colonias_1", secciones_reactivas)
  
  # 17. Gestión de Tiempo
  mod_gestion_tiempo_server("tiempo_1", secciones_reactivas, base_ganadores_pr)
  
  mod_clustering_secciones_server("cluster_1", 
                                  secciones_reactivas, 
                                  contexto_geo = contexto_geografico_actual)
  
  
}