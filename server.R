
source("global.R")
credentials <- readxl::read_excel("www/data/contrasenas_nl.xlsx")


function(input, output, session) {

  # 2.1. AUTENTICACIÓN Y GESTIÓN DE USUARIO

  res_auth <- secure_server(
    check_credentials = check_credentials(credentials)
  )
  
  output$auth_output <- renderPrint({
    reactiveValuesToList(res_auth)
  })
  
  get_user_role <- reactive({
    req(res_auth$role)
    return(res_auth$role)
  })
  
  output$user_role <- renderText({
    paste("User Role:", get_user_role())
  })
  
  

  # 2.2. REACTIVOS DE DATOS PRIMARIOS (FILTRADO INICIAL)

  
  # Actualizar tipo_filtro_inicial en fincion del rol.
  observeEvent(get_user_role(), {
    
    role <- get_user_role()
    
    # Lógica para definir las opciones
    if (role == "usuario_monterrey") {
      choices_geograficos <- c("Municipio", "DFederal", "DLocal")
      
      ID_MONTERREY <- 40 
      secciones_mty <- secciones_prev %>% filter(MUNICIPIO == ID_MONTERREY)
      
      mun_rol           <- unique(secciones_mty$NOMBRE)
      distr_federal_rol <- unique(secciones_mty$DISTRITO)
      distr_loclal_rol  <- unique(secciones_mty$DISTRITO_L)
      
    } else if(role == "usuario_garcia"){
      choices_geograficos <- c("Municipio","DLocal")
      
      dta_garcia <- secciones_prev %>% 
        filter(
          NOMBRE == "GARCIA"
        )
      
      
      mun_rol <- "GARCIA"
      distr_federal_rol <- dta_garcia[["DISTRITO"]] %>% unique()
      distr_loclal_rol <- dta_garcia[["DISTRITO_L"]] %>% unique()
      
    }
    else {
      choices_geograficos <- c("Municipio", "DFederal", "DLocal", "Ninguno")
      
      mun_rol <- secciones_prev[["NOMBRE"]] %>% unique()
      distr_federal_rol <- secciones_prev[["DISTRITO"]] %>% unique()
      distr_loclal_rol <- secciones_prev[["DISTRITO_L"]] %>% unique()
    }
    
    # Actualiza el seleccion geográfica
    updateSelectInput(session, 
                      "tipo_filtro_inicial",
                      choices = choices_geograficos)
    
    updateSelectInput(
      session = session,
      inputId = "municipio_inicial",
      choices = mun_rol
      )
    updateSelectInput(
      session = session,
      inputId = "federal_inicial",
      choices = distr_federal_rol
    )
    updateSelectInput(
      session = session,
      inputId = "local_inicial",
      choices = distr_loclal_rol
    )
    
    
    
    
    
    
    
    
    
    
  }, ignoreNULL = TRUE) # ignoreNULL = TRUE evita que se active al inicio con un valor nulo.
  

  
  # Variables reactivas ----
  secciones <- reactive({
    

    req(input$tipo_filtro_inicial)
    
    if (input$tipo_filtro_inicial == "Municipio") {
      req(input$municipio_inicial)
    } else if (input$tipo_filtro_inicial == "DFederal") {
      req(input$federal_inicial)
    } else if (input$tipo_filtro_inicial == "DLocal") {
      req(input$local_inicial)
    }
    
    role <- get_user_role() 
    
    # 1. Filtrado por ROL de usuario
    secciones_filtradas_por_rol <- secciones_prev %>%
      st_transform(crs = "+proj=longlat +datum=WGS84") %>%
      mutate(filter_rol = case_when(
        role == "usuario_nuevo_leon" ~ TRUE,
        role == "usuario_monterrey" ~ MUNICIPIO == 40,
        role == "usuario_garcia" ~ MUNICIPIO == 18,

        
        TRUE ~ FALSE 
      )) %>%
      filter(filter_rol) 
    
    condicion_filtro_ui <- switch(input$tipo_filtro_inicial,
                                  "Municipio" = secciones_filtradas_por_rol$NOMBRE %in% input$municipio_inicial,
                                  "DFederal"  = secciones_filtradas_por_rol$DISTRITO %in% input$federal_inicial,
                                  "DLocal"    = secciones_filtradas_por_rol$DISTRITO_L %in% input$local_inicial,
                                  "Ninguno"   = TRUE # Si es "Ninguno", no se aplica filtro adicional.
    )
    
    # Se aplica el segundo filtro y se retorna el dataframe resultante.
    secciones_finales <- secciones_filtradas_por_rol %>% filter(condicion_filtro_ui)
    
    return(secciones_finales)
  })
  
  rank_por_seccion_eleccion <- reactive({
    x = cant_votos_nl %>%
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
    
    return(x)
  })
  
  base_ganadores_pr <- reactive({
    
    x <- secciones()
    y <- st_drop_geometry(base_ganadores)
    
    z <- merge(x,y, by.x = "SECCION", by.y = "seccion", all.x = T)
    
    
    z <- z %>% 
      mutate(across(gober_15:pres_24,~ replace_na(., "otro") )) %>% 
      mutate(across(gober_15:pres_24, ~ ifelse(. == "PVEM", "VERDE", .)))
    
    return(z)
    
    
  })
  
  # 2.4. RENDERIZACIÓN DE ELEMENTOS DE UI (USER INTERFACE)

  output$colonias_seleccionadas <- renderUI({
    
    secciones_actuales <- secciones()
    
    colonias_filtradas <- colonias %>%
      filter(SECCIÓN %in% secciones_actuales$SECCION)
    
    shiny::selectInput(
      "colonia_seleccionada", 
      "Selecciona la colonia de tu interés", 
      choices = sort(unique(colonias_filtradas$COLONIA))
    )
  })
  
  
  # Comparativo Resultados Históricos ----

  ### Primer Mapa
  base_ganador_1 <- reactive({

    req(input$eleccion_1)

    dta <- rank_por_seccion_eleccion()
    dta_filtrada <- dta %>%
      filter(
        eleccion_año_id == input$eleccion_1
      )
    
    return(dta_filtrada)
  })
  
  output$mapa_gana <- renderLeaflet({
    
    dta <- base_ganador_1()
    
    req(nrow(dta) > 0)
    
    dta_popup <- dta 

    validate(
      need(nrow(dta_popup) > 0, 
           "Con los permisos contratados, no tienes acceso a este municipio/distrito, cambia el filtro al inicio de la App. 
             Si consideras que esto es incorrecto o se trata de algún otro error, por favor contacta a tu proveedor.")
    )
    
    dta_popup$partido_1 <- factor(dta_popup$partido_1, levels = c("MC", "PAN", "PRI", "MORENA", "INDEPE", "VERDE", "PT", "otro", "PVEM"))
    
    
    pal <- colorFactor(c("orange","blue", "red", "brown", "purple", "#6CB655", "#E7DE08", "black", "#6CB655"), domain = dta_popup$partido_1)
    
    
    nombre_1a <- paste0("Resultados: ", input$eleccion_1)
    
    shp_dta_popup <- secciones() %>%
      left_join(dta_popup, by = c("SECCION" = "seccion")) %>%
      st_transform(crs = "+proj=longlat +datum=WGS84") %>%
      mutate(
        porc_votos_ganador = (votos_1 / votos_totales_seccion) * 100,
        
        distancia_votos_2do = votos_1 - votos_2,
        porc_distancia = (distancia_votos_2do / votos_totales_seccion) * 100,
        
        porc_participacion = (votos_totales_seccion/pobtotal)*100,
        
        popup_html = paste0(
          "<b>Sección: </b>", htmlEscape(SECCION), "<br>",
          "<b>Elección: </b>", htmlEscape(eleccion_año_id), "<hr style='margin:0.5rem;'>",
          
          "<b>Población: </b>",format(round(pobtotal,0), big.mark = ","),"<br>",
          "<b>Votos Totales: </b>", format(round(votos_totales_seccion,0), big.mark = ","), "<br>",
          "<b>% Participación: </b>", round(porc_participacion,2), "%<hr style='margin:0.5rem;'>",
          
          "<b>Ganador: </b>", htmlEscape(partido_1), "<br>",
          "<b>Votos Ganador: </b>",format(round(votos_1, 0), big.mark = ","), "<br>",
          "<b>% Voto Ganador: </b>", round(porc_votos_ganador, 2), "%<hr style='margin:0.5rem;'>",
          
          "<b>Ventaja sobre 2do lugar: </b>", format(distancia_votos_2do, big.mark = ","), " votos", " (",round(porc_distancia, 2),"%)"
        )
      )
    
    
    mapa <- leaflet() %>% 
      addProviderTiles(providers$CartoDB.Voyager) %>% 
      addLegend(
        position = "bottomright",
        pal = pal, 
        values =shp_dta_popup$partido_1, 
        labels = c("No", "Sí"), 
        title = "Ganador de sección") %>% 
      addPolygons(
        data = shp_dta_popup,
        color = "#596475", 
        stroke = T, weight= 1.3,
        fillColor = pal(shp_dta_popup$partido_1),
        fillOpacity = 0.7, 
        popup = ~ popup_html, 
        highlightOptions = highlightOptions(
          weight = 4,
          color = "#f72585",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )) %>% 
      addControl(nombre_1a, position = "bottomleft", className="map-title") 
    
    return(mapa)
    
  })
  
  
  output$downloadganador_elec1 <- downloadHandler(
    filename = function() {
      paste("my_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(st_drop_geometry(base_ganador_1()) , file)
    }
  )
  
  
  base_ganador_2 <- reactive({
    # input$eleccion_2
    
    req(input$eleccion_2)
    
    dta <- rank_por_seccion_eleccion()
    dta_filtrada <- dta %>%
      filter(
        eleccion_año_id == input$eleccion_2
      )
    
    return(dta_filtrada)
  })
  
  output$mapa_pierde <- renderLeaflet({
    
    dta <- base_ganador_2()
    
    req(nrow(dta) > 0)
    
    dta_popup <- dta 
    
    validate(
      need(nrow(dta_popup) > 0, 
           "Con los permisos contratados, no tienes acceso a este municipio/distrito, cambia el filtro al inicio de la App. 
             Si consideras que esto es incorrecto o se trata de algún otro error, por favor contacta a tu proveedor.")
    )
    
    dta_popup$partido_1 <- factor(dta_popup$partido_1, levels = c("MC", "PAN", "PRI", "MORENA", "INDEPE", "VERDE", "PT", "otro", "PVEM"))
    
    
    pal <- colorFactor(c("orange","blue", "red", "brown", "purple", "#6CB655", "#E7DE08", "black", "#6CB655"), domain = dta_popup$partido_1)
    
    
    nombre_1a <- paste0("Resultados: ", input$eleccion_1)
    
    shp_dta_popup <- secciones() %>%
      left_join(dta_popup, by = c("SECCION" = "seccion")) %>%
      st_transform(crs = "+proj=longlat +datum=WGS84") %>%
      mutate(
        porc_votos_ganador = (votos_1 / votos_totales_seccion) * 100,
        
        distancia_votos_2do = votos_1 - votos_2,
        porc_distancia = (distancia_votos_2do / votos_totales_seccion) * 100,
        
        porc_participacion = (votos_totales_seccion/pobtotal)*100,
        
        popup_html = paste0(
          "<b>Sección: </b>", htmlEscape(SECCION), "<br>",
          "<b>Elección: </b>", htmlEscape(eleccion_año_id), "<hr style='margin:0.5rem;'>",
          
          "<b>Población: </b>",format(round(pobtotal,0), big.mark = ","),"<br>",
          "<b>Votos Totales: </b>", format(round(votos_totales_seccion,0), big.mark = ","), "<br>",
          "<b>% Participación: </b>", round(porc_participacion,2), "%<hr style='margin:0.5rem;'>",
          
          "<b>Ganador: </b>", htmlEscape(partido_1), "<br>",
          "<b>Votos Ganador: </b>",format(round(votos_1, 0), big.mark = ","), "<br>",
          "<b>% Voto Ganador: </b>", round(porc_votos_ganador, 2), "%<hr style='margin:0.5rem;'>",
          
          "<b>Ventaja sobre 2do lugar: </b>", format(distancia_votos_2do, big.mark = ","), " votos", " (",round(porc_distancia, 2),"%)"
        )
      )
    
    mapa <- leaflet() %>% 
      addProviderTiles(providers$CartoDB.Voyager) %>% 
      addLegend(
        position = "bottomright",
        pal = pal, 
        values =shp_dta_popup$partido_1, 
        labels = c("No", "Sí"), 
        title = "Ganador de sección") %>% 
      addPolygons(
        data = shp_dta_popup,
        color = "#596475", 
        stroke = T, weight= 1.3,
        fillColor = pal(shp_dta_popup$partido_1),
        fillOpacity = 0.7, 
        popup = ~ popup_html, 
        highlightOptions = highlightOptions(
          weight = 4,
          color = "#f72585",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )) %>% 
      addControl(nombre_1a, position = "bottomleft", className="map-title") 
    
    return(mapa)
    
  })
  
  
  output$downloadganador_elec2 <- downloadHandler(
    filename = function() {
      paste("my_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(st_drop_geometry(base_ganador_2()) , file)
    }
  )
  
  
  
  
  
  
  
  
  
  
  
  
  # Simulación de escenarios ----
  
  base_sim_1 <- reactive({

    base_sim_1 <- res_trab %>% 
      filter(eleccion %in% input$eleccion_sim1) 
    
    eliminado_sim1 <- base_sim_1 %>% 
      filter(partido %in% input$dejar_afuera) %>% 
      transmute(seccion, partido = input$recibe_votos,
                proporcion_extra = proporcion) 
    
    base_sim_1 <- base_sim_1 %>% 
      filter(!partido %in% input$dejar_afuera) %>% 
      left_join(eliminado_sim1, by = c("seccion", "partido")) %>% 
      replace_na(list(proporcion_extra = 0)) %>% 
      mutate(proporcion = proporcion + proporcion_extra)%>% 
      select(seccion, partido, proporcion) %>% 
      pivot_wider(names_from = partido, values_from = proporcion) %>% 
      mutate(!!sym(input$dejar_afuera) := 0) %>% 
      mutate(ganador = case_when(
        pan > pri & pan > mc & pan > morena & pan > verde ~ "PAN",
        pri > pan & pri > mc & pri > morena& pri > verde ~ "PRI",
        morena > pri & morena > mc & morena > pan & morena > verde ~ "morena",
        mc > pri & mc > pan & mc > morena & mc > verde ~ "MC",
        verde > pri & verde > pan & verde > morena & verde > mc~ "VERDE",
        pt > pri & pt > pan & pt > morena & pt > mc & pt > verde ~ "pt",
        TRUE ~ "Empate"
      ))
    
    
    
    base_sim_1 <- secciones() %>% 
      left_join(base_sim_1, by = c("SECCION" = "seccion")) %>% 
      filter(!is.na(SECCION)) 
    
  })
  
  output$mapa_sim1 <- renderLeaflet({
    
    base_mapa_sim1 <- base_sim_1()
    
    
    validate(
      need(nrow(base_mapa_sim1) > 0, 
           "Con los permisos contratados, no tienes acceso a este municipio/distrito, cambia el filtro al inicio de la App. 
             Si consideras que esto es incorrecto o se trata de algún otro error, por favor contacta a tu proveedor.")
    )
    
    
    base_mapa_sim1$ganador <- factor(base_mapa_sim1$ganador, levels = c("MC", "PAN", "PRI", "morena", "VERDE", "pt"))
    
    pal <- colorFactor(c("orange","blue", "red", "maroon", "#6CB655","#E7DE08"), domain = base_mapa_sim1$ganador)
    
    
    base_mapa_sim1
    base_mapa_sim1 <- st_transform(base_mapa_sim1, crs = "+proj=longlat +datum=WGS84")
    
    popup_sim1 = paste0("Seccion: ", base_mapa_sim1$SECCION, 
                        ". <br/> Votos PRI: ", percent(base_mapa_sim1$pri),
                        ". <br/> Votos PAN: ", percent(base_mapa_sim1$pan),
                        ". <br/> Votos Morena: ", percent(base_mapa_sim1$morena),
                        ". <br/> Votos MC: ", percent(base_mapa_sim1$mc),
                        ". <br/> Votos VERDE: ", percent(base_mapa_sim1$verde),
                        ". <br/> Votos PT: ", percent(base_mapa_sim1$pt)
                        )
    
    
    mapa <- leaflet() %>% 
      addProviderTiles(providers$CartoDB.Voyager) %>% 
      addLegend(pal = pal, values = base_mapa_sim1$ganador,  title = "Quien ganaría la sección") %>% 
      
      addPolygons(
        data = base_mapa_sim1, 
        color = "#596475", fillColor = pal(base_mapa_sim1$ganador), 
        popup = popup_sim1, stroke = T, 
        fillOpacity = 0.7, weight = 1.5,
        highlightOptions = highlightOptions(
          weight = 4,
          color = "#f72585",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )) 
    
    
    return(mapa)
    
  })
  
  output$downloadganador_sim <- downloadHandler(
    filename = function() {
      paste("my_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(st_drop_geometry(base_sim_1()) , file)
    }
  )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Rendimiento Histórico ----
  
  dta_rendimiento_historico <- reactive({

    ganadores_votos_nl <- cant_votos_nl %>%
      group_by(seccion, eleccion_año_id) %>%  
      filter(votos == max(votos)) %>%         
      ungroup()
    
    
    slt_ganadores_votos_nl <- ganadores_votos_nl %>% 
      filter(
        eleccion_año_id %in% input$slt_candidatura_rh, 
        partido == input$partido_ganador
      )
    
    return(slt_ganadores_votos_nl)
  })
  
  output$mapa_rendimiento_historico <- renderLeaflet({
    
    input$municipio_inicial
    input$federal_inicial
    input$local_inicial
    
    
    dta_ganadores <- dta_rendimiento_historico()
    
    dta_ganadores_slt = dta_ganadores %>% 
      filter(
        eleccion_año_id %in% input$slt_candidatura_rh
      ) %>%
      group_by(seccion, partido) %>%
      summarise(
        eleccion_año_ids = paste(eleccion_año_id, collapse = "; "),
        votos = mean(votos)
      ) %>%
      ungroup()
    
    
    secciones_df <- secciones() %>%
      st_transform(crs = "+proj=longlat +datum=WGS84")
    
    
    
    
    shp_ganadores_secciones <- left_join(
      secciones_df,
      dta_ganadores_slt %>% 
        mutate(seccion = as.character(seccion)),
      by = c("SECCION" = "seccion")
    )
    
    shp_ganadores_secciones <- shp_ganadores_secciones %>%
      mutate(
        pobtotal_display = ifelse(is.na(pobtotal), "No disponible", as.character(pobtotal)),
        
        votos_display = ifelse(is.na(votos), "No disponible", as.character(votos)),
        
        proporcion_display = case_when(
          is.na(votos) | is.na(pobtotal) | pobtotal == 0 ~ "No disponible", 
          TRUE ~ paste0(round((votos / pobtotal) * 100, 2), "%") 
        ),
        
        popups_dinamicos = ifelse(
          is.na(partido), 

          # --- Caso 1: El partido seleccionado NO ganó en esta sección o no hay datos ---
          paste0(
            "<h4>Sección: ", SECCION, "</h4>", 
            "<strong>Municipio: </strong>", NOMBRE, "<br>",
            "<strong>Población (Total Sección): </strong>", pobtotal_display, "<hr>", 
            "<em>El partido '", input$partido_ganador, "' no obtuvo victoria en esta sección o no aplica para las elecciones filtradas.</em>"
          ),
          
          # --- Caso 2: El partido seleccionado SÍ ganó en esta sección ---
          paste0(
            "<h4>Sección: ", SECCION, "</h4>",
            "<strong>Municipio: </strong>", NOMBRE, "<br>",
            "<strong>Población (Total Sección): </strong>", pobtotal_display, "<hr>", 
            "<strong>Partido Ganador (filtrado): </strong>", partido, "<br>", 
            "<strong>Elección(es) Ganada(s): </strong>", 
            ifelse(is.na(eleccion_año_ids), "No especificado", eleccion_año_ids), "<br>", 
            "<strong>Votos obtenidos: </strong>", votos_display, "<br>",
            "<strong>Proporción votos/población: </strong>", proporcion_display
          )
        )
      )
    
    
    
    
    
    # --- 4. Crear el Mapa Leaflet ---
    leaflet(data = shp_ganadores_secciones) %>%
      addProviderTiles(providers$CartoDB.Voyager) %>%
      
      addPolygons(
        fillColor = ~ifelse(is.na(eleccion_año_ids), "#dad7cd", paleta_partidos[input$partido_ganador][[1]]),
        fillOpacity = 0.7,
        color = "#596475",
        weight = 1.5,
        stroke = TRUE,
        highlightOptions = highlightOptions(
          weight = 4,
          color = "#f72585",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        popup = ~popups_dinamicos,
        label = ~paste0("Sección: ", SECCION)
      ) %>%
      
      addLegend(
        position = "bottomright",
        colors = c(paleta_partidos[input$partido_ganador][[1]], "#dad7cd"),
        labels = c(paste("Victorias de:", input$partido_ganador), "Sin datos / Sin victoria"),
        title = "Referencia"
      )
  })
  
  
  output$dta_rendimiento_historico <- DT::renderDataTable({dta_rendimiento_historico()})
  
  paleta_partidos <- c(
    "PRI" = "#e41a1c",      # Rojo
    "PAN" = "#377eb8",      # Azul
    "MC" = "#ff7f00",       # Naranja
    "MORENA" = "#a65628",   # Marrón/Guinda
    "PT" = "#b30000",       # Rojo oscuro
    "VERDE" = "#4daf4a",    # Verde
    "INDEPE" = "#7678ed"     # Gris
  )
  
  output$download_quitar <- downloadHandler(
    filename = function() {
      paste("my_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(st_drop_geometry(dta_rendimiento_historico()) , file)
    }
  )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  # Evolución Partidista ----
  
  base_eleccion_1_a <- reactive({
    
    base <- res_trab %>% filter(eleccion %in% switch(
      input$eleccion1_a,
      "Diputado Local 2021" = "dl21",
      "Diputado Federal 2021" = "fed21",
      "Alcalde 2021" = "ayunt21",
      "Gobernador 2021" = "gob21",
      "Diputado Federal 2018" = "fed18",
      "Presidente 2018" = "pres18",
      "Diputado Local 2018" = "dipl18",
      "Senado 2018" = "sen18",
      "Alcalde 2018" = "ayunt18",
      "Diputado Federal 2015" = "fed15",
      "Alcalde 2015" = "ayunt15",
      "Diputado Local 2015" = "dipl15",
      "Presidente 2024" = "pres24", 
      "Senado 2024" = "sen24",
      "Diputado Federal 2024" = "fed24", 
      "Diputado Local 2024" = "dipl24", 
      "Alcalde 2024" = "ayunt24"
    )) %>% transmute(seccion, prop_1 = proporcion, partido)

    base
  })
  
  base_eleccion_2_a <- reactive({
    base <- res_trab %>% filter(eleccion %in% switch(
      input$eleccion2_a,
      "Diputado Local 2021" = "dl21",
      "Diputado Federal 2021" = "fed21",
      "Alcalde 2021" = "ayunt21",
      "Gobernador 2021" = "gob21",
      "Diputado Federal 2018" = "fed18",
      "Presidente 2018" = "pres18",
      "Diputado Local 2018" = "dipl18",
      "Senado 2018" = "sen18",
      "Alcalde 2018" = "ayunt18",
      "Diputado Federal 2015" = "fed15",
      "Alcalde 2015" = "ayunt15",
      "Diputado Local 2015" = "dipl15",
      "Presidente 2024" = "pres24",
      "Senado 2024" = "sen24",
      "Diputado Federal 2024" = "fed24", 
      "Diputado Local 2024" = "dipl24", 
      "Alcalde 2024" = "ayunt24"
    )) %>% transmute(seccion, prop_2 = proporcion, partido)
    
    base
  })
  
  
  
  base_mapa_gp_a <- reactive({
    
    partido_interes = input$partido_analisis
    
    
    base <- base_eleccion_1_a() %>% 
      full_join(base_eleccion_2_a(), by = c("seccion", "partido")) %>% 
      filter(partido %in% input$partido_analisis_a) %>% 
      mutate(cambio_s = (prop_2-prop_1)/prop_1)
    
    base_mapa_gp <- secciones()%>% 
      left_join(base, by = c("SECCION" = "seccion")) %>% 
      filter(!is.na(SECCION)) 
    
    
    base_mapa_gp
    base_mapa_gp <- st_transform(base_mapa_gp, crs = "+proj=longlat +datum=WGS84")
    
  })
  
  output$mapa_cambios_perc <- renderLeaflet({
    
    # Se obtiene el dataframe reactivo
    base_mapa_gp <- base_mapa_gp_a()
    
    # La validación está bien implementada, se mantiene igual
    validate(
      need(nrow(base_mapa_gp) > 0, 
           "Con los permisos contratados, no tienes acceso a este municipio/distrito, cambia el filtro al inicio de la App. 
         Si consideras que esto es incorrecto o se trata de algún otro error, por favor contacta a tu proveedor.")
    )
    
    # Se obtienen los valores de los inputs
    eleccion_n_1 <- input$eleccion1_a
    eleccion_n_2 <- input$eleccion2_a
    partido_n <- input$partido_analisis_a
    
    popup_text <- sprintf(
      "<strong>Sección: %s</strong><br/>
     Tasa de cambio para %s: <strong>%s</strong><br/>
     <hr>
     Resultado en %s: %s<br/>
     Resultado en %s: %s",
      base_mapa_gp$SECCION,
      partido_n,
      scales::percent(base_mapa_gp$cambio_s, accuracy = 0.1),
      eleccion_n_1,
      scales::percent(base_mapa_gp$prop_1, accuracy = 0.1),
      eleccion_n_2,
      scales::percent(base_mapa_gp$prop_2, accuracy = 0.1)
    ) %>% lapply(htmltools::HTML) # Convertir texto a HTML
    
    label_text <- sprintf(
      "Sección %s: %s",
      base_mapa_gp$SECCION,
      scales::percent(base_mapa_gp$cambio_s, accuracy = 0.1)
    )
    
    leaflet(base_mapa_gp) %>% 
      addProviderTiles(providers$CartoDB.Positron, group = "Mapa claro") %>%
      addProviderTiles(providers$CartoDB.DarkMatter, group = "Mapa oscuro") %>%
      
      addLegend(
        pal = paleta_cambio, 
        values = ~cambio_s,  
        title = paste("Tasa de cambio para", partido_n),
        position = "bottomright",
        opacity = 1
      ) %>%
      
      addPolygons(
        color = "#495057", 
        weight = 1.3,
        fillColor = ~paleta_cambio(cambio_s),
        fillOpacity = 0.7,
        
        label = label_text, 
        popup = popup_text, 
        
        highlightOptions = highlightOptions(
          weight = 3,
          color = "#e63946",
          bringToFront = TRUE,
          fillOpacity = 0.9
        )
      ) %>%
      
      # --- MEJORA 5: Control de capas para el usuario ---
      addLayersControl(
        baseGroups = c("Mapa claro", "Mapa oscuro"),
        options = layersControlOptions(collapsed = TRUE)
      )
  })
  
  output$download_tasas <- downloadHandler(
    filename = function() {
      paste("my_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(st_drop_geometry(base_mapa_gp_a()) , file)
    }
  )

  
  
  
  
  
  
  
  
  
  
  
  
  # Ganadores y perdedores (MÓDULO CORREGIDO) ----
  
  base_eleccion_1 <- reactive({
    req(input$eleccion1)
    
    dta <- rank_por_seccion_eleccion()
    dta_filtrada <- dta %>%
      filter(eleccion_año_id == input$eleccion1)
    
    return(dta_filtrada)
  })
  
  base_eleccion_2 <- reactive({
    req(input$eleccion2)
    
    dta <- rank_por_seccion_eleccion()
    dta_filtrada <- dta %>%
      filter(eleccion_año_id == input$eleccion2)
    
    return(dta_filtrada)
  })
  
  
  dta_ganadores_perdedores <- reactive({
    
    partido_interes = input$partido_analisis
    
    dta_eleccion_1 <- base_eleccion_1()
    dta_eleccion_2 <- base_eleccion_2()
    
    # Usamos full_join, pero debemos tener cuidado con NAs si las secciones cambiaron
    base <- full_join(
      dta_eleccion_1, dta_eleccion_2, by = "seccion"
    ) %>% 
      mutate(
        gana_o_pierde = case_when(
          # Caso 1: No lo tenía y ahora sí (Gana)
          partido_1.x != partido_interes & partido_1.y == partido_interes ~ "gana",
          # Caso 2: Lo tenía y ahora no (Pierde)
          partido_1.x == partido_interes & partido_1.y != partido_interes ~ "pierde",
          # Caso 3: No lo tenía y sigue sin tenerlo
          partido_1.x != partido_interes & partido_1.y != partido_interes ~ "igual (pierde)",
          # Caso 4: Lo tenía y lo mantuvo
          partido_1.x == partido_interes & partido_1.y == partido_interes ~ "igual (gana)",
          TRUE ~ "NA"
        )
      )
    
    return(base)
  })
  
  # MAPA GENERAL: GANA O PIERDE
  output$mapa_gana_o_pierde <- renderLeaflet({
    
    dta <- dta_ganadores_perdedores()
    
    validate(
      need(nrow(dta) > 0, "No hay datos para mostrar.")
    )
    
    dta$gana_o_pierde <- factor(dta$gana_o_pierde, levels = c("gana", "pierde", "igual (pierde)", "igual (gana)"))
    
    pal <- colorFactor(c("#06d6a0","#ef476f", "#e36414", "#219ebc"), domain = dta$gana_o_pierde)
    
    # Cruce espacial
    shp_dta <- secciones() %>% 
      st_transform(crs = "+proj=longlat +datum=WGS84") %>% 
      left_join(dta, by = c("SECCION" = "seccion")) %>% 
      filter(!is.na(gana_o_pierde)) %>% # Filtrar secciones sin datos
      mutate(
        popup_html = paste0("<b>Sección: </b>", SECCION, 
                            "<br><b>Estado: </b>", gana_o_pierde,
                            "<hr>",
                            "<b> ", input$eleccion1, ":</b> ", votos_1.x," (",partido_1.x,")",
                            "<br><b> ", input$eleccion2, ":</b> ", votos_1.y," (",partido_1.y,")"
        )
      )
    
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Voyager)  %>% 
      addLegend(pal = pal, values = shp_dta$gana_o_pierde,  title = "Balance") %>% 
      addPolygons(
        data = shp_dta, 
        color = "#596475", fillColor = ~ pal(gana_o_pierde),
        popup = ~ popup_html, 
        stroke = T, 
        fillOpacity = 0.7, 
        weight= 1.3,
        highlightOptions = highlightOptions(
          weight = 2,
          color = "#f72585",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )) 
  })
  
  
  # MAPA: ¿A QUIÉN LE GANÓ? (Donde gana_o_pierde == 'gana')
  output$mapa_gana <- renderLeaflet({
    
    # 1. Filtramos los datos
    dta <- dta_ganadores_perdedores() %>% 
      filter(gana_o_pierde == "gana")
    
    validate(need(nrow(dta) > 0, "No se registraron ganancias de secciones en esta comparación."))
    
    # 2. Unimos con shapefile (CORRECCIÓN IMPORTANTE)
    shp_dta <- secciones() %>%
      st_transform(crs = "+proj=longlat +datum=WGS84") %>%
      inner_join(dta, by = c("SECCION" = "seccion")) # inner_join para quedarnos solo con lo que se ganó
    
    # 3. Definimos paleta (CORRECCIÓN MAYÚSCULAS)
    # Usamos unique() para asegurar que cubrimos los partidos presentes
    partidos_previos <- unique(shp_dta$partido_1.x)
    
    # Usamos la paleta global definida en Global.R para consistencia
    # Si faltan colores, Leaflet los pondrá grises o usaremos una genérica
    pal <- colorFactor(palette = paleta_partidos, domain = partidos_previos, na.color = "#808080")
    
    leaflet() %>%  
      addProviderTiles(providers$CartoDB.Voyager)  %>%  
      addLegend(pal = pal, values = shp_dta$partido_1.x, title = "Se le ganó a:") %>%  
      addPolygons(
        data = shp_dta, 
        color = "#596475", weight= 1.3, stroke = T,
        fillColor = ~pal(partido_1.x), fillOpacity = 0.65,
        popup = ~paste0("<b>Sección:</b> ", SECCION, "<br><b>Antes gobernaba:</b> ", partido_1.x),
        highlightOptions = highlightOptions(
          weight = 4, color = "#f72585", fillOpacity = 0.9, bringToFront = TRUE
        )
      )
  })
  
  
  # MAPA: ¿QUIÉN NOS GANÓ? (Donde gana_o_pierde == 'pierde')
  output$mapa_pierde <- renderLeaflet({
    
    # 1. Filtramos datos
    dta <- dta_ganadores_perdedores() %>% 
      filter(gana_o_pierde == "pierde")
    
    validate(need(nrow(dta) > 0, "No se registraron pérdidas de secciones en esta comparación."))
    
    # 2. Unimos con shapefile (CORRECCIÓN IMPORTANTE)
    shp_dta <- secciones() %>%
      st_transform(crs = "+proj=longlat +datum=WGS84") %>%
      inner_join(dta, by = c("SECCION" = "seccion"))
    
    # 3. Paleta
    partidos_nuevos <- unique(shp_dta$partido_1.y)
    pal <- colorFactor(palette = paleta_partidos, domain = partidos_nuevos, na.color = "#808080")
    
    leaflet() %>% 
      addProviderTiles(providers$CartoDB.Voyager)%>% 
      addLegend(pal = pal, values = shp_dta$partido_1.y,  title = "Se perdió frente a:") %>% 
      addPolygons(
        data = shp_dta, 
        color = "#596475", fillColor = ~pal(partido_1.y), fillOpacity = 0.65,
        stroke = T,  weight= 1.3,
        popup = ~paste0("<b>Sección:</b> ", SECCION, "<br><b>Ahora gobierna:</b> ", partido_1.y),
        highlightOptions = highlightOptions(
          weight = 4, color = "#f72585", fillOpacity = 0.9, bringToFront = TRUE
        )
      )
  })
  
  # DOWNLOAD HANDLER CORREGIDO
  output$download_robados <- downloadHandler(
    filename = function() {
      paste("ganadores_perdedores-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      # Unimos con secciones() brevemente para obtener datos geográficos básicos si se desean,
      # o simplemente descargamos la tabla plana calculada.
      
      data_to_save <- dta_ganadores_perdedores() %>%
        # Si quieres Municipio, hay que unirlo
        left_join(st_drop_geometry(secciones()) %>% select(SECCION, NOMBRE, DISTRITO, DISTRITO_L), by = c("seccion" = "SECCION")) %>%
        select(Municipio = NOMBRE, Seccion = seccion, DFed = DISTRITO, DLoc = DISTRITO_L, 
               Estatus = gana_o_pierde, Ganador_Anterior = partido_1.x, Ganador_Nuevo = partido_1.y)
      
      write.csv(data_to_save, file, row.names = FALSE)
    }
  )
  


  
  
  # Demografía Espacial ----
  
  base_mapa_edad <- reactive({
    pattern_edades <- str_c(input$edades, collapse = "|") 
    pattern_genero <- str_c(input$genero, collapse = "|") 
    
    base_edad <- edades %>% 
      pivot_longer(hombres_18_24:mujeres_mayores, names_to = "categoria", values_to = "cantidad") %>% 
      filter(
        str_detect(categoria, pattern_edades),
        str_detect(categoria, pattern_genero)
      ) %>% 
      group_by(seccion)  %>% 
      summarise(cantidad = sum(cantidad), 
                total = mean(padron_total)) %>% 
      mutate(prop = cantidad/total)
    
    
    base_edad
    
    base_edad <- secciones() %>% 
      left_join(base_edad, by = c("SECCION" = "seccion")) %>% 
      filter(!is.na(SECCION)) 
    
    
  })
  
  output$mapa_generos_edad <- renderLeaflet({
    
    mapa_generos_edad <- base_mapa_edad()
    
    validate(
      need(nrow(mapa_generos_edad) > 0, 
           "Con los permisos contratados, no tienes acceso a este municipio/distrito, cambia el filtro al inicio de la App. 
             Si consideras que esto es incorrecto o se trata de algún otro error, por favor contacta a tu proveedor.")
    )

    pal <- colorNumeric(c("orange", "blue"), domain = mapa_generos_edad$prop)
    
    
    mapa_generos_edad
    mapa_generos_edad <- st_transform(mapa_generos_edad, crs = "+proj=longlat +datum=WGS84")
    
    popup_sim1 = paste0("Seccion: ", mapa_generos_edad$SECCION, 
                        ". <br/> Población con estas características: ", percent(mapa_generos_edad$prop, 3)
    )
    
    
    
    
    mapa_generos_edad <- leaflet() %>% 
      addProviderTiles(providers$CartoDB.Voyager) %>% 
      addLegend(pal = pal, values = mapa_generos_edad$prop,  title = "Proporción") %>% 
      addPolygons(
        data = mapa_generos_edad, 
        color = "#596475", fillColor = pal(mapa_generos_edad$prop), 
        popup = popup_sim1, stroke = T, fillOpacity = 0.7, weight= 1.3,
        highlightOptions = highlightOptions(
          weight = 4,
          color = "#f72585",
          fillOpacity = 0.9,
          bringToFront = TRUE
          )
        )
    
    return(mapa_generos_edad)
    
    
  })
  
  
  output$download_edades <- downloadHandler(
    filename = function() {
      paste("my_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(st_drop_geometry(base_mapa_edad()) , file)
    }
  )
  
  
  
  ## Creación de modelo por fortaleza en la elección
  
  base_modelo <- reactive({
    ## calentar con dip fed y PRI
    base <- base_ganadores %>% 
      rename(interes_elec = dip_fed_21 ) %>% 
      select(!ends_with("21" )) %>% 
      select(!c(geometry, distrito, poblacion))  %>% 
      mutate(resultado = case_when(
        interes_elec %in% input$partido_predecir ~ 1, 
        TRUE ~ 0
      )) %>% 
      mutate(across(contains("_"), as.factor))
    
    
    
    modelo <- glm(resultado ~ alcalde_15 + dip_local_15  + dip_local_18 + senado_18 +
                    alcalde_18 + dip_fed18 + pres_18, base, family = "binomial"
    )
    
    summary(modelo)
    
    # Add predicted probabilities to `base`
    base_predicciones <- cbind(base, Pred = predict(modelo, base)) %>% 
      arrange(desc(Pred))
    
    
    
    base_predicciones$grupo <- cut(base_predicciones$Pred, 
                                   breaks = quantile(base_predicciones$Pred, probs = c(0, 0.2, 0.4, 0.6, 0.8, 1), na.rm = TRUE), 
                                   include.lowest = TRUE,
                                   labels = F)
    
    
    base_modelo <- secciones() %>% 
      left_join(base_predicciones, by = c("SECCION" = "seccion")) %>% 
      filter(!is.na(SECCION)) 
    
    base_modelo
    
    
  })
  
  
  output$download_fortalezas <- downloadHandler(
    filename = function() {
      paste("my_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(st_drop_geometry(base_modelo()) , file)
    }
  )
  
  
  
  output$mapa_fortaleza <- renderLeaflet({
    
    base_mapa_gp <-base_modelo()
    
    base_mapa_gp <- st_transform(base_mapa_gp, crs = "+proj=longlat +datum=WGS84")
    
    
    validate(
      need(nrow(base_mapa_gp) > 0, 
           "Con los permisos contratados, no tienes acceso a este municipio/distrito, cambia el filtro al inicio de la App. 
             Si consideras que esto es incorrecto o se trata de algún otro error, por favor contacta a tu proveedor.")
    )
    
    base_mapa <- base_mapa_gp %>% 
      mutate(
        probabilidad = as.factor(case_when(
          grupo %in% 1 ~ "Muy baja fortaleza", 
          grupo %in% 2 ~ "Baja fortaleza",
          grupo %in% 3 ~ "Mediana fortaleza",
          grupo %in% 4 ~ "Alta fortaleza",
          grupo %in% 5 ~ "Muy alta fortaleza",
          
        ))
      )
    
    
    pal = colorFactor(palette = c("#caf0f8","#90e0ef", "#00b4d8", "#0077b6", "#03045e"),domain = base_mapa$probabilidad, 
                      levels = c("Muy baja fortaleza", "Baja fortaleza", "Mediana fortaleza", "Alta fortaleza", "Muy alta fortaleza")
    )

    
    pop_tiempo <-   paste0("Seccion: ", base_mapa$SECCION, 
                           ". <br/> Fortaleza de ganar ", base_mapa$probabilidad
    )
    
    
    mapa <- leaflet() %>% 
      addProviderTiles(providers$CartoDB.Voyager) %>% 
      addLegend(colors = c("#caf0f8", "#90e0ef", "#00b4d8", "#0077b6", "#03045e"),
                labels = c("Muy baja fortaleza", "Baja fortaleza", "Mediana fortaleza", "Alta fortaleza", "Muy alta fortaleza"), 
                title = "Fortaleza")  %>% 
      addPolygons(
        data = base_mapa, 
        color = "#596475", 
        fillColor = pal(base_mapa$probabilidad),
        popup = pop_tiempo, 
        stroke = T, 
        fillOpacity = 0.7, 
        weight= 0.5,
        highlightOptions = highlightOptions(
          weight = 4,
          color = "#f72585",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )) 
    
    return(mapa)
    
  })
  
  # Archivo electoral ----
  
  # pik_secc_archivo reactivo a geografia seleccionada 
  observeEvent(c(
    input$tipo_filtro_inicial,
    input$municipio_inicial,
    input$federal_inicial,
    input$local_inicial
  ),{
    
    # browser()
    # Extraemos seccionse segun geogrfia
    chs_secc_archivo <- switch (input$tipo_filtro_inicial,
                                "Municipio" = secciones_prev[secciones_prev$NOMBRE == input$municipio_inicial,]$SECCION, 
                                "DFederal"  = secciones_prev[secciones_prev$DISTRITO == input$federal_inicial,]$SECCION, 
                                "DLocal"    = secciones_prev[secciones_prev$DISTRITO_L == input$local_inicial,]$SECCION, 
                                "Ninguno"   = secciones_prev$SECCION
                                  )
      
      
      
    # Actualizamos pik_secc_archivo
    updatePickerInput(session = session,inputId = "pik_secc_archivo",choices = chs_secc_archivo,selected = chs_secc_archivo)
  })
  
  # Construimos datos desagregados
  query_archivo <- reactive({
    data_shp <- sf::st_drop_geometry(secciones()) %>% select(SECCION, DISTRITO, DISTRITO_L,NOMBRE)
    
    data_shp_votos <- merge(data_shp, cant_votos_nl, by.x = "SECCION",by.y = "seccion", all.x = T)
    
    data_shp_votos_filtrado <- data_shp_votos[data_shp_votos$partido %in% input$pik_partido_archivo,]
    data_shp_votos_filtrado <- data_shp_votos_filtrado[data_shp_votos_filtrado$eleccion %in% input$pik_elecc_archivo,]
    data_shp_votos_filtrado <- data_shp_votos_filtrado[data_shp_votos_filtrado$año %in% input$pik_año_archivo,]
    
    # filtramos secciones
    data_shp_votos_filtrado <- data_shp_votos_filtrado[data_shp_votos_filtrado$SECCION %in% input$pik_secc_archivo,]
    
    data_cols <- select(data_shp_votos_filtrado,
                        SECCION, DISTRITO, DISTRITO_L,NOMBRE,partido,eleccion,votos,año) 
    colnames(data_cols) <- c("Sección","Distrito Federal","Distrito Local","Municipio","Partido","Elección","Votos","Año")
    
    return(data_cols)
    
  })
  # Agragamos datos
  query_archivo_agg <- reactive({
    data_cols_group <- switch (input$tipo_filtro_inicial,
                               "Municipio" = query_archivo() %>% group_by(Municipio,Partido,Elección,Año) %>% summarise(`Votos totales` = round(sum(Votos,na.rm = T))), 
                               "DFederal"  = query_archivo() %>% group_by(`Distrito Federal`,Partido,Elección,Año) %>% summarise(`Votos totales` = round(sum(Votos,na.rm = T))),
                               "DLocal"    = query_archivo() %>% group_by(`Distrito Local`,Partido,Elección,Año) %>% summarise(`Votos totales` = round(sum(Votos,na.rm = T))), 
                               "Ninguno"   = query_archivo() %>% group_by(Partido,Elección,Año) %>% summarise(`Votos totales` = round(sum(Votos,na.rm = T))) 
    )
    return(data_cols_group)
  })
  
  output$dw_archivo_csv_archivo <- downloadHandler(
    filename = function() {
      paste0(format(Sys.Date(),"%d_%m_%Y"),"_archivo_electoral.csv")
    },
    content = function(file) {
      write.csv(query_archivo_agg(), file)
    }
  )

  
  output$dw_archivo_csv_2_archivo <- downloadHandler(
    filename = function() {
      paste0(format(Sys.Date(),"%d_%m_%Y"),"_archivo_electoral.csv")
    },
    content = function(file) {
      write.csv(query_archivo(), file)
    }
  )

  
  output$dt_agregada_archivo <- DT::renderDataTable({
    
    datatable(
      data = query_archivo_agg(),
      style = 'bootstrap5', # “default”, “bootstrap”, “bootstrap4”, “bootstrap5”, “bulma”, “foundation”, “jqueryui”, “semanticui”
      class = 'header stripe',
      extensions = c("Scroller"), #"Select","Buttons"
      selection = "single",
      options = list(
        searching = FALSE,
        deferRender = F,
        scrollY = 500,
        scroller = TRUE,
        autoWidth = TRUE,
        dom = 'Bfrtip',
        scrollX = TRUE,
        
        #scrolly = TRUE,
        rowCallback = DT::JS(
          "function(row, data) {",
          "$(row).on('click', function() {",
          "Shiny.setInputValue('fila_seleccionada', data[0]);",
          "});",
          "}"
        )
      )
    )
    

  })
  
  output$dt_desagregada_archivo <- DT::renderDataTable({
    
    datatable(
      data = query_archivo(),
      style = 'bootstrap5', 
      class = 'header stripe',
      extensions = c("Scroller"), 
      selection = "single",
      options = list(
        searching = FALSE,
        deferRender = F,
        scrollY = 500,
        scroller = TRUE,
        autoWidth = TRUE,
        dom = 'Bfrtip', 
        scrollX = TRUE,
        
        #scrolly = TRUE,
        rowCallback = DT::JS(
          "function(row, data) {",
          "$(row).on('click', function() {",
          "Shiny.setInputValue('fila_seleccionada', data[0]);",
          "});",
          "}"
        )
      )
    )
    

  })
  
  
  # Mapa censales ----
  
  query_censales <- reactive({
    # input$censo_interes

    dta_largo <- data_secc_cpv2020 %>%
      pivot_longer(
        cols = PHOG_IND:PROM_OCUP,
        names_to = "indicador",
        values_to = "valor"
      )
    
    dta_largo_filter = dta_largo %>% 
      filter(
        indicador == input$censo_interes
      )
    
    
    return(dta_largo_filter)
  })
  
  output$mapa_censales <- renderLeaflet({
    # input$censo_interes
    
    
    dta_largo <- query_censales()
    
    validate(
      need(nrow(dta_largo) > 0, 
           "Sin información")
    )
    
    
    # 2. Crea una etiqueta amigable a partir del nombre de la variable en el input
    label_base <- stringr::str_to_title(stringr::str_replace_all(input$censo_interes, "_", " "))
    
    # Puedes hacerla aún más descriptiva con un case_when si tienes pocas opciones
    # 2. Crea la etiqueta descriptiva final usando case_when
    label_interes <- case_when(
      # --- Población ---
      input$censo_interes == "POBTOT"      ~ "Población Total",
      input$censo_interes == "POBFEM"      ~ "Población Femenina",
      input$censo_interes == "POBMAS"      ~ "Población Masculina",
      
      # --- Grupos Específicos ---
      input$censo_interes == "PHOG_IND"    ~ "Población en Hogares Indígenas",
      input$censo_interes == "POB_AFRO"    ~ "Población Afrodescendiente",
      input$censo_interes == "PCON_DISC"   ~ "Población con Discapacidad",
      
      # --- Economía ---
      input$censo_interes == "POCUPADA"    ~ "Población Ocupada",
      input$censo_interes == "PDESOCUP"    ~ "Población Desocupada",
      
      # --- Vivienda ---
      input$censo_interes == "VIVPAR_HAB"  ~ "Viviendas Particulares Habitadas",
      input$censo_interes == "VIVPAR_DES"  ~ "Viviendas Particulares Deshabitadas",
      input$censo_interes == "PROM_OCUP"   ~ "Promedio de Ocupantes por Vivienda",
      
      # --- Educación ---
      input$censo_interes == "GRAPROES"    ~ "Grado Promedio de Escolaridad",
      
      # --- Caso por Defecto ---
      # Si input$censo_interes no coincide con ninguno de los anteriores,
      # usa la versión simple que creamos al principio.
      TRUE ~ label_base 
    )
    
    dta_wide <- pivot_wider(dta_largo, 
                            names_from = indicador, 
                            values_from = valor) %>%
      mutate(
        SECCION = as.character(SECCION),
        SECCION = str_pad(SECCION, width = 4, side = "left", pad = "0")
      )
    
    
    shp_dta <- secciones() %>% 
      st_transform(crs = "+proj=longlat +datum=WGS84") %>% 
      mutate(SECCION = str_pad(SECCION, width = 4, side = "left", pad = "0")) %>%
      left_join(dta_wide, by = "SECCION")
    
    vars_porcentaje <- c("POBFEM", "POBMAS", "PHOG_IND", "POB_AFRO", 
                         "PCON_DISC", "POCUPADA", "PDESOCUP")
    
    # 2. Construye el dataframe final en un solo paso
    shp_dta_con_popup <- shp_dta %>%
      mutate(
        # 3. Crea la columna 'valor_mapa' con la lógica correcta
        # Si la variable está en nuestra lista, calcula el porcentaje. Si no, usa el valor directo.
        valor_mapa = if (input$censo_interes %in% vars_porcentaje) {
          # Maneja la división por cero para evitar errores (NaN)
          ifelse(POBTOT > 0, round((.data[[input$censo_interes]] / POBTOT) * 100, 2), 0)
        } else {
          .data[[input$censo_interes]]
        },
        
        # 4. Define un formato dinámico para la leyenda y el popup (añade "%" si es necesario)
        formato_valor = if (input$censo_interes %in% vars_porcentaje) {
          paste0(format(valor_mapa, nsmall = 2, big.mark = ","), "%")
        } else {
          format(round(valor_mapa, 2), big.mark = ",")
        },
        
        # 5. Crea una línea extra solo si se analiza la escolaridad (código más limpio)
        linea_escolaridad = if (input$censo_interes == "GRAPROES") {
          paste0("<b>Nivel de escolaridad: </b>", GRAPROES_NIVEL, "<br>")
        } else {
          "" # De lo contrario, es un string vacío
        },
        
        # 6. Construye el popup una sola vez
        popup_html = paste0(
          "<b>Municipio: </b>", htmlEscape(NOMBRE), "<br>",
          "<b>Sección: </b>", htmlEscape(SECCION), "<hr style='margin:0.5rem;'>",
          "<b>Población Total: </b>", format(round(POBTOT, 0), big.mark = ","), "<br>",
          linea_escolaridad, # Se inserta la línea de escolaridad (o un vacío)
          "<b>", label_interes, ": </b>", formato_valor
        )
      )
    
    pal <- leaflet::colorNumeric(
      palette = "viridis",
      domain = shp_dta_con_popup[["valor_mapa"]]
    )
    legend_title <- if (input$censo_interes %in% vars_porcentaje) {
      paste0(label_interes, " (%)") # Añade (%) si es un porcentaje
    } else {
      label_interes # Usa la etiqueta normal si no lo es
    }

    fig_censales <- leaflet() %>% 
      addProviderTiles(providers$CartoDB.Voyager) %>% 
      addPolygons(
        data = shp_dta_con_popup,
        color = "#596475",
        fillColor = ~pal(valor_mapa), 
        popup = ~popup_html,
        stroke = TRUE,
        fillOpacity = 0.65,
        weight = 1.3,
        highlightOptions = highlightOptions(
          weight = 1.5,
          color = "#f72585",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        data = shp_dta_con_popup, 
        pal = pal, 
        values = ~valor_mapa, 
        title = legend_title, 
        position = "bottomright"
      )

    return(fig_censales)
    
  })
  
  output$dt_query_censales <- DT::renderDataTable({
    
    datatable(
      data = query_censales(),
      style = 'bootstrap5', # “default”, “bootstrap”, “bootstrap4”, “bootstrap5”, “bulma”, “foundation”, “jqueryui”, “semanticui”
      class = 'header stripe',
      extensions = c("Scroller"), #"Select","Buttons"
      selection = "single",
      options = list(
        searching = FALSE,
        deferRender = F,
        scrollY = 500,
        scroller = TRUE,
        autoWidth = TRUE,
        dom = 'Bfrtip', 
        scrollX = TRUE
      )
    )
    
  })
  
  output$dw_censal_csv_censales <- downloadHandler(
    filename = function() {
      paste0(format(Sys.Date(),"%d_%m_%Y"),"_censales.csv")
    },
    content = function(file) {
      write.csv(query_censales(), file)
    }
  )
  
  # Mapa manzanas ----
  base_mapa_mza <- reactive({ 
    base_mapa_mza <-  manzanas_nl %>% 
      mutate(variable_interes = as.numeric(eval(as.symbol(input$censo_interes_mza))))
    
    base_mapa_mza
    
  })
  
  output$mapa_manzanas <- renderLeaflet({
    
    
    base_mapa_mza = st_transform(base_mapa_mza(), st_crs(secciones()))
    
    
    base_mapa_manzanas <- secciones() %>% 
      st_join(base_mapa_mza) %>% 
      filter(!is.na(SECCION), 
             !is.na(variable_interes), 
             !CVEGEO %in% c("260300001280A001", "2603000011500005")
      ) 
    
    
    validate(
      need(nrow(base_mapa_manzanas) > 0, 
           "Con los permisos contratados, no tienes acceso a este municipio/distrito, cambia el filtro al inicio de la App. 
             Si consideras que esto es incorrecto o se trata de algún otro error, por favor contacta a tu proveedor.")
    )
    
    if(nrow(base_mapa_manzanas) == 0){
      mapa <- leaflet() %>%  
        setView(lng = -100.309731, lat = 25.672939, zoom = 5) %>%
        addProviderTiles(providers$CartoDB.Voyager)  %>%
        addPopups(-100.309731, 25.672939, "No tienes acceso a este municipio/distrito, cambia el filtro al inicio de la App. Si consideras que esto es incorrecto, por favor contacta a tu proveedor.")
      
      return(mapa)
    }
    pal <- colorNumeric(c("#e0e1dd", "#aec3b0", "#598392", "#124559", "#01161e"), domain = base_mapa_manzanas$variable_interes)
    
    
    base_mapa_manzanas <- st_transform(base_mapa_manzanas, crs = "+proj=longlat +datum=WGS84")
    
    popup_sim1 = paste0(". <br/> Población con estas características: ", comma(base_mapa_manzanas$variable_interes))
    
    
    
    
    mapa_manzanas <- leaflet() %>% 
      addProviderTiles(providers$CartoDB.Voyager) %>% 
      addLegend(pal = pal, values = base_mapa_manzanas$variable_interes,  title = "Población con estas características") %>% 
      addPolygons(data = base_mapa_manzanas, color = "#596475", fillColor = pal(base_mapa_manzanas$variable_interes), popup = popup_sim1, stroke = T, fillOpacity = 0.6, weight= 0.5)
    
    mapa_manzanas
    
  })

  # Voto sombra ----
  
  # rank_por_seccion_eleccion
  
  query_sombra <- reactive({
    req(input$eleccion_sombra)
    
    
    dta <- rank_por_seccion_eleccion()
    dta_filtrada <- dta %>%
      filter(
        eleccion_año_id == input$eleccion_sombra
      )
    
    return(dta_filtrada)
  })
  
  # Tabla
  output$dt_sombra_sombra <- DT::renderDT({
    # browser()
    
    data <- query_sombra()
    
    datatable(
      data = data,
      style = 'bootstrap5', # “default”, “bootstrap”, “bootstrap4”, “bootstrap5”, “bulma”, “foundation”, “jqueryui”, “semanticui”
      class = 'header stripe',
      extensions = c("Scroller"), #"Select",
      selection = "single",
      options = list(
        searching = FALSE,
        deferRender = TRUE,
        scrollY = 500,
        scroller = TRUE,
        autoWidth = TRUE,
        dom = 'Bfrtip',
        scrollX = TRUE
      )
    )
    
  })
  
  
  # Mapa
  output$mapa_sombra <- renderLeaflet({

      base_mapa <- query_sombra()
      
      shp <- secciones() %>% st_transform(crs = "+proj=longlat +datum=WGS84")
      
      shp_base_mapa <- left_join(
        shp,
        base_mapa,
        by = c("SECCION" = "seccion")
      ) %>%
        # Usar mutate para añadir las nuevas columnas de cálculo y el popup
        mutate(
          # --- Cálculos ---
          # Porcentaje de votos que obtuvo el ganador sobre el total de la sección
          porc_votos_ganador = (votos_1 / votos_totales_seccion) * 100,
          porc_votos_2do = (votos_2 / votos_totales_seccion) * 100,
          
          # Diferencia de votos entre el primer y segundo lugar
          distancia_votos_2do = votos_1 - votos_2,
          porc_distancia = (distancia_votos_2do / votos_totales_seccion) * 100,
          
          # Porcentaje de participación
          porc_participacion = (votos_totales_seccion/pobtotal)*100,
          
          # --- Creación del Popup con formato HTML ---
          popup_html = paste0(
            "<b>Sección: </b>", htmlEscape(SECCION), "<br>",
            "<b>Elección: </b>", htmlEscape(eleccion_año_id), "<hr style='margin:0.5rem;'>",
            
            "<b>Población: </b>",format(round(pobtotal,0), big.mark = ","),"<br>",
            "<b>Votos Totales: </b>", format(round(votos_totales_seccion,0), big.mark = ","), "<br>",
            "<b>% Participación: </b>", round(porc_participacion,2), "%<hr style='margin:0.5rem;'>",
            
            "<b>2do Lugar: </b>", htmlEscape(partido_2), "<br>",
            "<b>Votos 2do Lugar: </b>",format(round(votos_2, 0), big.mark = ","), "<br>",
            "<b>% Voto 2do Lugar: </b>", round(porc_votos_2do, 2), "%<hr style='margin:0.5rem;'>",
            
            "<b>Votos 1do Lugar: </b>",format(round(votos_1, 0), big.mark = ","), " (",partido_1,")", "<br>",
            "<b>Ventaja sobre 1do lugar: </b>", format(distancia_votos_2do, big.mark = ","), " votos", " (",round(porc_distancia, 2),"%)"
          )
        )

      validate(
        need(nrow(shp_base_mapa) > 0, 
             "Con los permisos contratados, no tienes acceso a este municipio/distrito, cambia el filtro al inicio de la App. 
             Si consideras que esto es incorrecto o se trata de algún otro error, por favor contacta a tu proveedor.")
      )
      
      paleta_partidos <- c(
        "PRI" = "#e41a1c",      # Rojo
        "PAN" = "#377eb8",      # Azul
        "MC" = "#ff7f00",       # Naranja
        "MORENA" = "#a65628",   # Marrón/Guinda
        "PT" = "#b30000",       # Rojo oscuro
        "VERDE" = "#4daf4a",    # Verde
        "INDEPE" = "#7678ed"     # Gris
      )
      
      # Obtener la lista de partidos únicos que están realmente en los datos del mapa (excluyendo los NA)
      partidos_en_mapa <- unique(na.omit(shp_base_mapa$partido_2))
      
      # Crear el vector de colores para la leyenda:
      # Primero los colores de los partidos presentes, luego el color para los NA.
      colores_leyenda <- c(paleta_partidos[partidos_en_mapa], "#808080")
      
      # Crear el vector de etiquetas para la leyenda, en el mismo orden que los colores
      etiquetas_leyenda <- c(partidos_en_mapa, "Sin datos / No aplica")
      
      
      mapa <- leaflet() %>% 
        addProviderTiles(providers$CartoDB.Voyager) %>% 
        addPolygons(
          data = shp_base_mapa,
          label = ~ SECCION,
          color = "#596475", 
          fillColor = ~ifelse(is.na(partido_2), 
                              "#dad7cd",  # Color gris para los valores NA
                              paleta_partidos[partido_2]), 
          popup = ~ popup_html, 
          stroke = T, 
          fillOpacity = 0.7, 
          weight= 1.3,
          highlightOptions = highlightOptions(
            weight = 1.5,
            color = "#f72585",
            fillOpacity = 0.9,
            bringToFront = TRUE
            )
          ) %>%
        addLegend(
          position = "bottomright",      # Posición de la leyenda en el mapa
          colors = colores_leyenda,      # El vector de colores que preparamos
          labels = etiquetas_leyenda,    # El vector de etiquetas que preparamos
          title = "Partido Ganador",     # Título para la leyenda
          opacity = 0.8                  # Opacidad del fondo de la leyenda
        )
      
      return(mapa)
      
    })
  
  output$dw_csv_sombra <- downloadHandler(
    filename = function() {
      paste0(format(Sys.Date(),"%d_%m_%Y"),"_voto_sombra.csv")
    },
    content = function(file) {
      write.csv(query_sombra(), file)
    }
  )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  

  # Distancia Primero-Segundo ----
  
  query_distancia <- reactive({
    
    x <- res_trab %>% 
      filter(eleccion %in% input$eleccion_diferencia) %>% 
      arrange(seccion, desc(proporcion)) %>% 
      group_by(seccion) %>%
      mutate(diff = (lead(proporcion) - proporcion)*-1,
             proporcion = proporcion
             ) %>%  
      filter(row_number() == 1)
    
    return(x)
  })

  reactive_diferencia <- reactiveValues(base = NULL)
  
  output$mapa_diferencia <- renderLeaflet({
    
    base <- query_distancia()
    shp <- secciones()
    
    shp_base <- shp %>% 
      left_join(base, by = c("SECCION" = "seccion")) %>% 
      st_transform(crs = "+proj=longlat +datum=WGS84")
    
    rescale_opacity <- function(x, min_opacity = 0.3, max_opacity = 0.9) {
      ((x - min(x)) / (max(x) - min(x))) * (max_opacity - min_opacity) + min_opacity
    }
    
    apply_opacity_threshold <- function(x, threshold = 0.42) {
      ((pmin(x, threshold) - min(x)) / (threshold - min(x)))
    }

    shp_base$opacidad <- rescale_opacity(shp_base$diff, min_opacity = 0.2)
    shp_base$opacidad <- apply_opacity_threshold(shp_base$diff, threshold = 0.43)

    validate(
      need(nrow(shp_base) > 0, 
           "Con los permisos contratados, no tienes acceso a este municipio/distrito, cambia el filtro al inicio de la App. 
             Si consideras que esto es incorrecto o se trata de algún otro error, por favor contacta a tu proveedor.")
    )
    
    shp_base$partido <- factor(shp_base$partido, levels = c("mc", "pan", "pri", "morena", "indep", "verde", "pt", "otro"))

    pal <- colorFactor(c("orange","blue", "red", "brown", "purple", "#6CB655", "#E7DE08", "black"), domain = shp_base$partido)
    
    shp_base <- shp_base %>% 
      mutate(
        popup_html = paste0(
          "<div style='font-family: sans-serif; padding: 5px; max-width: 250px;'>",
          "<h4 style='margin: 0 0 8px 0; color: #003366;'>Sección: ", SECCION, "</h4>",
          "<hr style='border: none; border-top: 1px solid #ddd; margin: 8px 0;'>",
          "<strong>Ganador:</strong> ", str_to_upper(partido), "<br>",
          "<strong>Votación:</strong> ", scales::percent(proporcion, accuracy = 0.01), "<br>",
          "<strong style='color: #E55934;'>Margen de Victoria:</strong> ", scales::percent(diff, accuracy = 0.01),
          "</div>"
        )
      )
    
    nombre_1a <- paste0("Resultados por sección en la elección de ", input$eleccion_diferencia)

    mapa <- leaflet() %>% 
      addProviderTiles(providers$CartoDB.Voyager) %>% 
      addLegend(pal = pal, values = shp_base$partido, title = "Ganador de la sección") %>% 
      addPolygons(
        data = shp_base,
        label = ~ SECCION,
        fillColor = ~pal(partido), 
        popup = ~ popup_html, 
        stroke = T, 
        fillOpacity = ~opacidad,
        
        color = "#596475",
        weight= 1.3,
        highlightOptions = highlightOptions(
          weight = 1.7,
          color = "#f72585",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )) %>% 
      addControl(nombre_1a, position = "bottomleft", className="map-title")
    
    reactive_diferencia$base <- shp_base %>% st_drop_geometry()
    
    return(mapa)
    
  })
    
  output$dt_diferencia_diferencia <- renderDT({
    # browser()
    
    data <- reactive_diferencia$base
    
    data_cols <- data %>% select(ENTIDAD,DISTRITO,DISTRITO_L,NOMBRE,SECCION,pobtotal,partido,eleccion,diff) 
    data_cols$diff <- round(data_cols$diff*100,2)
    colnames(data_cols) <- c("Entidad","Distrito Federal","Distrito Local","Municipio","Sección","Población total","Partido ganador","Elección","Diferencia")
    
    datatable(
      data = data_cols,
      style = 'bootstrap5', # “default”, “bootstrap”, “bootstrap4”, “bootstrap5”, “bulma”, “foundation”, “jqueryui”, “semanticui”
      class = 'header stripe',
      extensions = c("Scroller"), #"Select",
      selection = "single",
      options = list(
        searching = FALSE,
        deferRender = TRUE,
        scrollY = 500,
        scroller = TRUE,
        autoWidth = TRUE,
        dom = 'Bfrtip', 
        scrollX = TRUE
      )
    )
  })
  
  output$dw_csv_distancia <- downloadHandler(
    filename = function() {
      paste0(format(Sys.Date(),"%d_%m_%Y"),"_voto_sombra.csv")
    },
    content = function(file) {
      write.csv(reactive_diferencia$base, file)
    }
  )
  
  
  # Participación Electoral ----
  
  participacion_ <- reactive({
    
    
    x <- participacion %>% st_drop_geometry()
    
    cond <- switch(input$tipo_filtro_inicial,
                   "Municipio" = x$NOMBRE %in% input$municipio_inicial,
                   "DFederal" = x$DISTRITO %in% input$federal_inicial,
                   "DLocal" = x$DISTRITO_L %in% input$local_inicial,
                   "Ninguno" = TRUE
    )
    
    x <- x %>% filter(cond)
    
    
    return(x)
    
  })
  
  output$mapa_participacion <- renderLeaflet({
    
    variable <- case_when(
      input$participacion == "Lista Nominal" ~ "lista",
      input$participacion == "Tasa de participación" ~ "participacion",
      TRUE ~ "votos"
    )
    
    base_mapa <- participacion_() %>% 
      mutate(valor = .[[variable]]) %>%
      mutate(valor = ifelse(is.infinite(valor) | is.nan(valor), NA, valor)) 
    
    shp_base_mapa <- secciones() %>% 
      select(-ID,-ENTIDAD,-DISTRITO,-DISTRITO_L,-MUNICIPIO,-TIPO,-CONTROL,-NOMBRE) %>% 
      left_join(
        base_mapa, by = "SECCION"
      )
    
    # Bins: limitamos superior si es participación
    bins <- if (variable == "participacion") {
      vals <- shp_base_mapa$valor
      vals <- vals[!is.na(vals) & vals <= 1]  # ignorar >100% para leyenda
      unique(quantile(vals, probs = seq(0, 1, length.out = 10), na.rm = TRUE, names = FALSE))
    } else {
      unique(quantile(shp_base_mapa$valor, probs = seq(0, 1, length.out = 10), na.rm = TRUE, names = FALSE))
    }
    pal <- colorBin(
      palette = "YlOrRd",
      domain = shp_base_mapa$valor,
      bins = bins,
      na.color = "transparent"
    )
    
    lab_format <- switch(
      variable,
      "participacion" = labelFormat(suffix = "%", transform = function(x) x * 100, digits = 1),
      labelFormat(big.mark = ",", digits = 0)
    )
    
    popup_valor <- case_when(
      is.na(shp_base_mapa$valor) ~ "No disponible",
      variable == "participacion" & shp_base_mapa$valor > 1 ~ paste0(round(shp_base_mapa$valor * 100, 1), "% (supera el 100%)"),
      variable == "participacion" ~ paste0(round(shp_base_mapa$valor * 100, 1), "%"),
      TRUE ~ formatC(shp_base_mapa$valor, format = "d", big.mark = ",")
    )
    
    popup_sim1 <- paste0(
      "Sección: ", shp_base_mapa$SECCION,"<br/>",
      input$participacion,": ", popup_valor
    )
    
    
    leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addLegend(
        pal = pal,
        values = if (variable == "participacion") c(0, 1) else shp_base_mapa$valor,
        title = input$participacion,
        labFormat = lab_format,
        opacity = 1
      ) %>%
      addPolygons(
        data = shp_base_mapa,
        color = "black",
        fillColor = ~pal(valor),
        popup = popup_sim1,
        stroke = TRUE,
        fillOpacity = 0.7,
        weight = 0.5,
        highlightOptions = highlightOptions(
          weight = 4,
          color = "#f72585",
          fillOpacity = 0.8,
          bringToFront = TRUE
        )
      )
  })
  
  
  
  # Lealtad_ganador ----
  output$mapa_lealtad <- renderLeaflet({
    # Paso 1: Elegimos las columnas con resultados electorales
    cols_elecciones <- names(base_ganadores_pr())[grepl("gober|alcalde|dip|senado|pres", names(base_ganadores_pr()))]
    
    # Paso 2: Contamos para cada sección cuántas veces ganó cada partido
    lealtad_partidista <- base_ganadores_pr() %>%
      select(SECCION, all_of(cols_elecciones)) %>%
      st_drop_geometry() %>% 
      pivot_longer(-SECCION, names_to = "eleccion", values_to = "partido") %>%
      filter(!is.na(partido),
             !partido == "NULL",
             !partido == "0",
      ) %>%
      group_by(SECCION, partido) %>%
      summarise(victorias = n(), .groups = "drop") %>%
      arrange(SECCION, desc(victorias)) %>%
      group_by(SECCION) %>%
      slice_max(order_by = victorias, n = 1, with_ties = FALSE) %>%  # Ganador más frecuente
      ungroup()
    
    # Paso 3: Clasificamos por nivel de lealtad
    lealtad_partidista <- lealtad_partidista %>%
      mutate(lealtad = case_when(
        victorias <= 3 ~ "Baja lealtad",
        victorias == 4 ~ "Lealtad media",
        victorias == 5 ~ "Alta lealtad",
        victorias >= 6 ~ "Muy alta lealtad"
      )) %>% 
      left_join(select(base_ganadores_pr(), SECCION, geometry), by = "SECCION") %>% 
      st_as_sf() %>% 
      mutate(partido = as.character(partido))
    
    
    
    
    niveles_partido <- c("INDEPE", "MC", "morena", "PAN", "PRI", "PT", "PVEM", "NULL")
    
    pal_partido <- colorFactor(
      palette = c(
        "gray20", "orange", "#652123", "blue", "#F51F23", "purple", "#73A64E", "lightgray"
      ),
      levels = niveles_partido,
      na.color = "transparent"
    )
    
    
    lealtad_partidista <- lealtad_partidista %>% 
      st_transform(crs = "+proj=longlat +datum=WGS84") %>% 
      mutate(
        popup_html = paste0(
          "<b>Sección:</b> ", SECCION, 
          "<hr style='border: none; border-top: 1px solid #ddd; margin: 8px 0;'>",
          "<b>Partido dominante:</b> ", partido, "<br>",
          "<b>Victorias:</b> ", victorias, "/18", "<br>",
          "<b>Lealtad:</b> ", lealtad
        ) %>% lapply(HTML)
      )
    
    # Mapa leaflet
    mapa <- leaflet(lealtad_partidista) %>%
      #addProviderTiles(providers$CartoDB.Positron) %>%
      addProviderTiles(providers$CartoDB.Voyager) %>% 
      addPolygons(
        fillColor = pal_partido(lealtad_partidista$partido),
        opacity = 1,
        fillOpacity = 0.65,
        label = ~ SECCION,
        popup = ~ popup_html,
        color = "#596475",
        weight= 1.3,
        highlightOptions = highlightOptions(
          weight = 4,
          color = "#f72585",
          fillOpacity = 0.9,
          bringToFront = TRUE
        )
      ) %>%
      addLegend(
        title = "Partido con más victorias en la sección",
        pal = pal_partido,
        values = ~partido,
        opacity = 1
      )
    
    return(mapa)
    
  }) 
  
  
  
  # INFORMACIÓN DE COLONIAS ----
  
  output$colonias_seleccionadas <- renderUI({
    
    # Get the current values of secciones
    current_secciones <- secciones()
    
    # Filter colonias based on secciones
    filtered_colonias <- colonias %>%
      filter(SECCIÓN %in% current_secciones$SECCION)
    
    
    shiny::selectInput("colonia_seleccionada", "Selecciona la colonia de tu interés", choices =sort(unique(filtered_colonias$COLONIA)) )
    
  })
  
  base_colonias_sd <- reactive({
    
    colonias_2 <- colonias %>% 
      filter(COLONIA %in% input$colonia_seleccionada) %>% 
      select("SECCIÓN") %>% 
      as_vector()
    
    
    base_coloniass <- secciones_sd %>%
      filter(SECCION %in% colonias_2) %>%
      summarise(population = sum(POBTOT),
                graproes = sum(GRAPROES * POBTOT) / sum(POBTOT),
                pnacent = sum(PNACENT * POBTOT) / sum(POBTOT),
                analfabetismo = sum(P15YM_AN * POBTOT) / sum(POBTOT),
                graproes = sum(GRAPROES * POBTOT) / sum(POBTOT),
                catolica = sum(PCATOLICA * POBTOT) / sum(POBTOT),
                jovenprop = sum(p_joven * POBTOT) / sum(POBTOT),
                viejoprop = sum(p_vieja * POBTOT) / sum(POBTOT),
      ) %>% 
      mutate(across(everything(), as.numeric))
    
    base_coloniass
    
    
    
  })
  
  nombres_colonias <- reactive({
    colonias_2 <- colonias %>% 
      filter(COLONIA %in% input$colonia_seleccionada) %>% 
      select("SECCIÓN") 
    
    
  })
  
  votos_colonia <- reactive({
    colonias_2 <- colonias %>% 
      filter(COLONIA %in% input$colonia_seleccionada) %>% 
      select("SECCIÓN") %>% 
      as_vector()
    
    votos_colonia <- cant_votos %>% 
      filter(seccion %in% colonias_2) %>% 
      group_by(eleccion, partido) %>% 
      summarise(votos_total = sum(na.omit(votos))) %>% 
      group_by(eleccion) %>% 
      mutate(prop = votos_total/sum(votos_total)) %>% 
      filter(!is.na(partido)) %>% 
      filter(eleccion %in% input$eleccion_colonia)
    
    votos_colonia
    
  })
  
  
  votos_colonia_tiempo <- reactive({
    
    req(!is.null(input$colonia_seleccionada))
    
    colonias_2 <- colonias %>% 
      filter(COLONIA %in% input$colonia_seleccionada) %>% 
      select("SECCIÓN") %>% 
      as_vector()
    
    votos_colonia_tiempo <- cant_votos %>% 
      filter(seccion %in% colonias_2) %>% 
      group_by(eleccion, partido) %>% 
      summarise(votos_total = sum(na.omit(votos))) %>% 
      group_by(eleccion) %>% 
      mutate(prop = votos_total/sum(votos_total)) %>% 
      filter(partido %in% input$partido_colonia)
    
    votos_colonia_tiempo
    
    
    
  })
  
  
  output$texto_colonia <- renderText({
    
    base_colonias <-  base_colonias_sd()
    nombres_colonias <- nombres_colonias()
    
    paste0("La colonia escogida es ", input$colonia_seleccionada, ". Esta colonia pertenece a las secciones de '",  paste(nombres_colonias$SECCIÓN,collapse =  ","),
           "'. Estas secciones en conjunto tienen una población de ", comma(base_colonias$population), " personas.", " El grado promedio de escolaridad es de ", round(base_colonias$graproes,2),
           " años. El porcentaje de personas que nació en la entidad es de ", percent(base_colonias$pnacent, 2), ". Los jovenes ocupan al ", percent(base_colonias$jovenprop), 
           " de la población, mientras que la población mayor alcanza al ", percent(base_colonias$viejoprop), " de la población. "
    )
    
  })
  
  
  output$grafico_colonia <- renderPlot({
    
    
    ggplot(votos_colonia()) +
      geom_col(aes(x = partido, y = prop, fill = partido)) +
      labs(x = "Partido", y = "Proporción del voto obtenida por el partido", title = "Votos obtenidos por los partidos en la elección") +
      #theme_classic() +
      scale_fill_manual(values = c(
        "pri" = "red", "pan" = "blue", "morena" = "maroon", "mc" = "orange", "verde" = "#6CB655", "pt" = "#E7DE08"
      ))
  })
  
  output$grafico_colonia_tiempo <- renderPlot({
    
    
    titulo <- paste0("Votos obtenidos por el partido '", input$partido_colonia, "' en las elecciones.")
    ggplot(votos_colonia_tiempo()) +
      # Define las columnas para el gráfico. Se usan los niveles del factor para ordenar el eje X
      geom_col(aes(x = factor(eleccion, levels = c("ayunt15", "gob15", "dl15","fed15","ayunt18", "dipl18", "fed18", "sen18",
                                                   "pres18", "ayunt21", "gob21", "dl21", "fed21", "ayunt24", "dipl24", 
                                                   "fed24", "pres24", "sen24")), 
                   y = prop, fill = prop)) +
      
      # Asigna etiquetas personalizadas a cada elemento del eje X
      scale_x_discrete(labels = c(ayunt15 = "Alcaldía 2015", gob15 = "Gobernador 2015", dl15 = "Diputado Local 2015", fed15 = "Diputado Federal 2015",
                                  ayunt18 = "Alcaldía 2018", dipl18 = "Diputado local 2018", 
                                  fed18 = "Diputado Federal 2018", sen18 = "Senador 2018", pres18 = "Presidente 2018", 
                                  ayunt21 = "Alcaldía 2021", gob21 = "Gobernador 2021", dl21 = "Diputado Local 2021", fed21 = "Diputado Federal 2021",
                                  ayunt24 = "Alcaldía 2024", dipl24 = "Diputado local 2024", fed24 = "Diputado federal 2024", pres24 = "Presidente 2024", 
                                  sen24 = "Senador 2024"
      )) +
      
      # Rota las etiquetas del eje X para mejorar la legibilidad
      # angle = 90 rota el texto 90 grados.
      # hjust = 1 alinea el final del texto con la marca del eje.
      # vjust = 0.5 centra verticalmente el texto.
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)) +
      
      # Define los títulos del gráfico y los ejes
      labs(x = "Elección", 
           y = "Proporción del voto obtenida por el partido", 
           title = titulo,
           fill = "Proporción") # Añadido para dar título a la leyenda de color
    
    
    
    
  })
  
  output$download_votos_colonia_tiempo <- downloadHandler(
    filename = function() {
      paste("my_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(votos_colonia_tiempo() , file)
    }
  )
  
  output$download_votos_colonia <- downloadHandler(
    filename = function() {
      paste("my_data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(votos_colonia() , file)
    }
  )
  
  
  
  
  # Gestión de tiempo de campaña ----
  base_visitas_secciones <- reactive({
    
    z = (input$dias_campaña * input$equipos)/sum(na.omit(secciones()$pobtotal))
    
    basej <- base_ganadores %>% 
      mutate(pob18 = as.numeric(poblacion)) %>% 
      transmute(`seccion`, tiempoxseccion = round((z * pob18 * 8),2) ,
                PRIPAN = case_when(
                  dip_local_21 %in% "PRI"   ~ "PRI",
                  TRUE ~ "PAN"
                ), pob18#, distrito
      ) 
    
    base_final <- secciones() %>%
      left_join(
        # Modificamos basej 'al vuelo' justo antes de unirlo
        basej %>% mutate(seccion = as.character(seccion)),
        by = c("SECCION" = "seccion")
      ) %>%
      filter(!is.na(SECCION))
    
    return(base_final)
    
    
  })
  
  output$mapa_visitas_secciones <- renderLeaflet({
    
    base_mapa_gp <-base_visitas_secciones()
    
    base_mapa_gp <- st_transform(base_mapa_gp, crs = "+proj=longlat +datum=WGS84")
    
    
    
    validate(
      need(nrow(base_mapa_gp) > 0, 
           "Con los permisos contratados, no tienes acceso a este municipio/distrito, cambia el filtro al inicio de la App. 
             Si consideras que esto es incorrecto o se trata de algún otro error, por favor contacta a tu proveedor.")
    )
    
    
    
    base_mapa <- base_mapa_gp
    
    
    pal = colorNumeric(c("white", "red"), domain = base_mapa$tiempoxseccion)
    
    pop_tiempo <-   paste0("Seccion: ", base_mapa$SECCION, 
                           ". <br/> Población mayor a 18 años: ", comma(base_mapa$pob18),
                           ". <br/> Horas que se le deben dedicar a esta sección: ", base_mapa$tiempoxseccion
    )
    
    
    mapa <- leaflet() %>% 
      addProviderTiles(providers$CartoDB.Positron) %>% 
      addLegend(pal = pal, values = base_mapa$tiempoxseccion,  title = "Cuantas horas dedicar a la sección") %>% 
      addPolygons(data = base_mapa, color = "black", fillColor = pal(base_mapa$tiempoxseccion),popup = pop_tiempo, stroke = T, 
                  fillOpacity = 1, weight= 0.5) 
    
    mapa
    
  })
  
  output$download_basej <- downloadHandler(
    filename = "tiempo_por_seccion.csv",
    content = function(file) {
      basej <- base_visitas_secciones() %>% 
        rename(horas_a_dedicar = tiempoxseccion,
               presentarse_como = PRIPAN
        ) %>% 
        filter(!is.na(`seccion`)) %>% 
        st_drop_geometry()
      
      write.csv(basej, file, row.names = FALSE)
    }
  )
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  
  ### Fullscreen ----
  {
    observeEvent(input$mapFullscreen, {
      js$fullscreen()  # Call the JavaScript function
    })
    
    observeEvent(input$mapFortalezaFullscreen, {
      js$fullscreenFortaleza()  # Call the new JavaScript function
    })
    observeEvent(input$mapVisitasFullscreen, {
      js$fullscreenVisitas()  # Call the new JavaScript function
    })
    
    observeEvent(input$mapGanaOPierdeFullscreen, {
      js$fullscreenGanaOPierde()  # Call the new JavaScript function
    })
    
    observeEvent(input$mapGanaFullscreen, {
      js$fullscreenGana()  # Call the new JavaScript function
    })
    
    observeEvent(input$mapPierdeFullscreen, {
      js$fullscreenPierde()  # Call the new JavaScript function
    })
  }
  
  {
    observeEvent(input$mapGanador1Fullscreen, {
      js$fullscreenGanador1()
    })
    
    observeEvent(input$mapGanador2Fullscreen, {
      js$fullscreenGanador2()
    })
    
    observeEvent(input$mapGenerosEdadFullscreen, {
      js$fullscreenGenerosEdad()
    })
    
    observeEvent(input$mapPriFullscreen, {
      js$fullscreenPri()
    })
    
    observeEvent(input$mapSim1Fullscreen, {
      js$fullscreenSim1()
    })
    
    observeEvent(input$mapCambiosPercFullscreen, {
      js$fullscreenCambiosPerc()
    })
    
    
    observeEvent(input$mapManzanasFullscreen, {
      js$fullscreenManzanas() 
    })
    
    observeEvent(input$mapSombraFullscreen, {
      js$fullscreenSombra()  
    })
    
    
    observeEvent(input$mapDiferenciaFullscreen, {
      js$fullscreenDiferencia()  
    })
  }
  
  
  
  # Data Manzanas ----
  
  # Valores reactivos 
  foco_secc_select <- reactiveValues( secc_select = c() )
  
  # Mapa base
  output$map_mzas <- renderLeaflet({
    bounds <- secciones()%>% st_transform(4326) %>% sf::st_bbox() %>% as.character()
    leaflet() %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) 
  })
  
  # Ocultar|Mostrar y Actualizar selectores principales ----
  observeEvent(input$tipo_filtro_inicial,{
    #browser()
    switch (input$tipo_filtro_inicial,
            "Municipio" = {
              shinyjs::show("municipio_inicial",anim = T, animType = 'slide')
              shinyjs::hide("federal_inicial",anim = T, animType = 'slide')
              shinyjs::hide("local_inicial",anim = T, animType = 'slide')
            },
            "DFederal" = {
              shinyjs::hide("municipio_inicial",anim = T, animType = 'slide')
              shinyjs::show("federal_inicial",anim = T, animType = 'slide')
              shinyjs::hide("local_inicial",anim = T, animType = 'slide')
            },
            "DLocal" = {
              shinyjs::hide("municipio_inicial",anim = T, animType = 'slide')
              shinyjs::hide("federal_inicial",anim = T, animType = 'slide')
              shinyjs::show("local_inicial",anim = T, animType = 'slide')
            },
            "Ninguno" = {
              shinyjs::hide("municipio_inicial",anim = T, animType = 'slide')
              shinyjs::hide("federal_inicial",anim = T, animType = 'slide')
              shinyjs::hide("local_inicial",anim = T, animType = 'slide')
            }
    )
  })
  
  # Reactividad del mapa ----
  
  # Mapa reacciona a geografia seleccionada 
  observeEvent(c(
    input$tipo_filtro_inicial,
    input$municipio_inicial,
    input$federal_inicial,
    input$local_inicial
  ),{

    
    
    # Actualizamos piker de multiples secciones
    updatePickerInput(session  = session,
                      inputId  = "pik_secc_col_mza",
                      choices  = secciones()$SECCION,
                      selected = c()
                      )
    
    # Ocultamos btt para regresar y text de selección 
    shinyjs::hide("btt_return_secc_select",anim = T, animType = 'fade')
    shinyjs::hide("pik_elect_mza",         anim = T, animType = 'fade')
    shinyjs::hide("slid_año_mza",          anim = T, animType = 'fade')
    shinyjs::hide("pik_secc_col_mza",      anim = T, animType = 'fade')
    shinyjs::hide("pik_pob_mza",           anim = T, animType = 'fade')
    
    foco_secc_select$secc_select <- c()

    bounds <- secciones() %>% st_transform(4326) %>% st_bbox() %>% as.character()
    
    map_mzas <- leafletProxy("map_mzas") %>%
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      clearShapes() %>% clearControls() %>% 
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
      addPolygons(data = secciones() %>% st_transform(4326),
                  layerId = ~as.character(SECCION),
                  label = ~ SECCION,
                  fillOpacity = 0.6,
                  fillColor = '#168aad',
                  weight = 0.6, 
                  color = '#596475',
                  opacity = 1,
                  smoothFactor = 1,
                  highlightOptions = highlightOptions(color = '#f72585',
                                                      fillOpacity = 0.3,
                                                      weight = 2,
                                                      bringToFront = TRUE))
    
    return(map_mzas)
  
  })
  
  observeEvent(input$swch_colonias_mzas,{
    req(is.null(foco_secc_select$secc_select))
    if(input$swch_colonias_mzas){
      # browser()
      shp_cols_x_secc_selected <- switch (input$tipo_filtro_inicial,
                                          "Municipio" = shp_cols_x_secc[shp_cols_x_secc$MUNICIPIO %in% as.numeric(chs_mun_cols[[input$municipio_inicial]]),] , 
                                          "DFederal"  = shp_cols_x_secc[shp_cols_x_secc$DISTRITO_F %in% as.numeric(input$federal_inicial),] , 
                                          "DLocal"    = shp_cols_x_secc[shp_cols_x_secc$DISTRITO_L %in% as.numeric(input$local_inicial),], 
                                          "Ninguno"   = shp_cols_x_secc
      )
      
      
      # bounds <- secciones() %>% st_transform(4326) %>% st_bbox() %>% as.character()
      leafletProxy("map_mzas") %>%
        addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
        clearShapes() %>% clearControls() %>% 
        #fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
        addPolygons(data = shp_cols_x_secc_selected %>% st_transform(4326) , 
                    label = ~NOMBRE,
                    #layerId = ~as.character(SECCION),
                    fillOpacity = 0.6,
                    fillColor = '#f72585',
                    weight = 1, 
                    color = '#596475',
                    opacity = 1,
                    smoothFactor = 1,
                    highlightOptions = highlightOptions(color = '#f72585',
                                                        fillOpacity = 0.3,
                                                        weight = 2,
                                                        bringToFront = TRUE))
    }else{
      bounds <- secciones() %>% st_transform(4326) %>% st_bbox() %>% as.character()
      leafletProxy("map_mzas") %>%
        addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
        clearShapes() %>% clearControls() %>% 
        fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
        addPolygons(data = secciones() %>% st_transform(4326), 
                    layerId = ~as.character(SECCION),
                    label = ~paste("Seccion:",SECCION),
                    fillOpacity = 0.6,
                    fillColor = '#168aad',
                    weight = 0.6, 
                    color = '#596475',
                    opacity = 1,
                    smoothFactor = 1,
                    highlightOptions = highlightOptions(color = '#f72585',
                                                        fillOpacity = 0.3,
                                                        weight = 2,
                                                        bringToFront = TRUE))
    }
    
  })
  
  
  # Guardamos clic
  observeEvent(c(
    input$map_mzas_shape_click
  ),{
    
    # browser()
    
    req(!is.null(input$map_mzas_shape_click$id))
    
    if(input$map_mzas_shape_click$id %in% foco_secc_select$secc_select){
      # Encontrar la posición del valor a eliminar
      posicion <- which(foco_secc_select$secc_select == input$map_mzas_shape_click$id)
      
      # Eliminar el valor del vector
      foco_secc_select$secc_select <- foco_secc_select$secc_select[-posicion]
    }else{
      foco_secc_select$secc_select <- append(foco_secc_select$secc_select,as.character(input$map_mzas_shape_click$id))
    }
    
    updatePickerInput(session = session,
                      inputId = "pik_secc_col_mza",
                      # choices = foco_secc_select$secc_select,
                      selected = foco_secc_select$secc_select
                      )

  })
  
  # Regresamos a geografia anterior 
  observeEvent(input$btt_return_secc_select,{
    print("Se activa btt_return_secc_select")
    
    # Ocultamos btt para regresar y text de selección 
    shinyjs::hide("btt_return_secc_select",anim = T, animType = 'fade')
    shinyjs::hide("pik_elect_mza",anim = T, animType = 'fade')
    shinyjs::hide("slid_año_mza",anim = T, animType = 'fade')
    shinyjs::hide("pik_pob_mza",anim = T, animType = 'fade')
    shinyjs::hide("pik_secc_col_mza",anim = T, animType = 'fade')
    
    foco_secc_select$secc_select <- c()
    
    bounds <- secciones() %>% st_transform(4326)  %>% st_bbox() %>% as.character()
    
    leafletProxy("map_mzas") %>%
      clearShapes() %>% clearControls() %>% 
      addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
      fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
      addPolygons(data = secciones()%>% st_transform(4326), 
                  label = ~SECCION,
                  layerId = ~as.character(SECCION),
                  fillOpacity = 0.6,
                  fillColor = '#168aad',
                  weight = 0.7, 
                  color = '#596475',
                  opacity = 1,
                  smoothFactor = 1,
                  highlightOptions = highlightOptions(color = '#f72585',
                                                      weight = 2,
                                                      fillOpacity = 0.3,
                                                      bringToFront = TRUE)
      )
    
    output$py_ganadores <- renderPlotly({
      plot_ly(x=c(), y=c(), type = 'bar', orientation = 'h')
    })
    
  })
  
  # Mapa reacciona a al click (foco_secc_select)
  observeEvent(c(
    input$pik_secc_col_mza,
    input$pik_pob_mza,
    input$swch_colonias_mzas
    ),{
      # browser()
      
      req(!is.null(foco_secc_select$secc_select))
      req(!is.na(foco_secc_select$secc_select))
      
      
      
      shinyjs::show("btt_return_secc_select",anim = T, animType = 'fade')
      shinyjs::show("pik_elect_mza",         anim = T, animType = 'fade')
      shinyjs::show("slid_año_mza",          anim = T, animType = 'fade') 
      shinyjs::show("pik_pob_mza",           anim = T, animType = 'fade')
      shinyjs::show("pik_secc_col_mza",      anim = T, animType = 'fade')
      
      # Shapes de secciones complementarias a la seleccionada
      shp_secc_2024_mr = secciones() %>% st_transform(4326)
      shp_secc_2024_mr_secc_select <- shp_secc_2024_mr[!(shp_secc_2024_mr$SECCION %in% input$pik_secc_col_mza),]
      
      # Shapes de manzanas de sección seleccionada
      shp_mza_2023_secc <- shp_mza2023_secc2024_cpv2020[shp_mza2023_secc2024_cpv2020$SECCION %in% as.integer(input$pik_secc_col_mza),]
      bounds <- shp_mza_2023_secc %>% st_bbox() %>% as.character()
      
      # Construimos paleta de colores
      percentiles <- quantile(as.numeric(shp_mza_2023_secc[,input$pik_pob_mza][[1]]), 
                              probs = seq(0, 1, length.out = 6), 
                              na.rm = TRUE)
      pal <- colorBin("YlOrRd", 
                      domain = as.numeric(shp_mza_2023_secc[,input$pik_pob_mza][[1]])
      )
      
      
      
      # browser()
      
      shp_cols_x_secc_selected <- switch (input$tipo_filtro_inicial,
                                          "Municipio" = shp_cols_x_secc[shp_cols_x_secc$MUNICIPIO %in% as.numeric(chs_mun_cols[[input$municipio_inicial]]),] , 
                                          "DFederal"  = shp_cols_x_secc[shp_cols_x_secc$DISTRITO_F %in% as.numeric(input$federal_inicial),] , 
                                          "DLocal"    = shp_cols_x_secc[shp_cols_x_secc$DISTRITO_L %in% as.numeric(input$local_inicial),], 
                                          "Ninguno"   = shp_cols_x_secc
                                          )
        

      if( nrow(shp_secc_2024_mr_secc_select) > 0 ){
        
        map_mzas <- leafletProxy("map_mzas") %>%
          addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
          clearShapes() %>% clearControls() %>% 
          fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
          addPolygons(data = shp_secc_2024_mr_secc_select %>% st_transform(4326) , 
                      label = ~ SECCION,
                      layerId = ~as.character(SECCION),
                      fillOpacity = 0.6,
                      fillColor = '#adb5bd',
                      weight = 0.6, 
                      color = '#596475',
                      opacity = 1,
                      smoothFactor = 1,
                      highlightOptions = highlightOptions(color = '#f72585',
                                                          fillOpacity = 0.3,
                                                          weight = 2,
                                                          bringToFront = TRUE)) %>%
          addPolygons(data = shp_mza_2023_secc %>% st_transform(4326), 
                      label = lapply(1:nrow(shp_mza_2023_secc), function(i) {
                        x <- HTML(paste0(
                          "<table>
                        <tr>
                          <th colspan='2'>",
                        "<strong style = 'color:#0D3B66;'>", 
                        "Seccion: ",shp_mza_2023_secc$SECCION[i],
                        "</strong> 
                          </th>
                        </tr>
                        <tr>
                          <th colspan='2'>",
                        "<strong style = 'color:#0D3B66;'>", 
                        "Manzana: ",shp_mza_2023_secc$CVEGEO[i],
                        "</strong> 
                         </th>
                        </tr>
                        <tr>
                         <td>",input$pik_pob_mza,": ","</td>",
                        "<td>",
                        "<strong style = 'color:#596475;'>",
                        shp_mza_2023_secc[,input$pik_pob_mza][[1]][i],
                        "</strong>",
                        "</td>
                        </tr>
                        <tr>
                          <td>
                            Fuente: 
                          </td>
                          <td>
                          CPV 2020
                          </td>
                        </tr>
                       </table>"
                        ))
                        return(x)
                      }), 
                      fillOpacity = 0.6, 
                      fillColor = pal(shp_mza_2023_secc[,input$pik_pob_mza][[1]]),
                      weight = 2, 
                      color = '#596475',
                      opacity = 1.0, 
                      smoothFactor = 0.5, 
                      highlightOptions = highlightOptions(color = '#f72585',
                                                          weight = 2,
                                                          fillOpacity = 0.3, 
                                                          bringToFront = TRUE),
                      labelOptions = labelOptions(style = list('font-weight' = 'normal', 
                                                               padding = '3px 8px'),
                                                  textsize = '15px',
                                                  direction = 'auto')) %>%
          addLegend(colors = pal(as.vector(percentiles)),
                    labels = round(percentiles,0),
                    opacity = 0.7, 
                    title = "Densidad",
                    position = "bottomright") %>% 
          leaflet.extras2::addEasyprint(
            options = leaflet.extras2::easyprintOptions(exportOnly = TRUE,)
          ) 
      }
      
      if( nrow(shp_secc_2024_mr_secc_select) == 0 ){
        
        map_mzas <- leafletProxy("map_mzas") %>%
          addProviderTiles(providers$OpenStreetMap.Mapnik) %>%
          clearShapes() %>% clearControls() %>% 
          fitBounds(bounds[1], bounds[2], bounds[3], bounds[4]) %>%
          addPolygons(data = shp_mza_2023_secc %>% st_transform(4326), 
                      label = lapply(1:nrow(shp_mza_2023_secc), function(i) {
                        x <- HTML(paste0(
                          "<table>
                        <tr>
                          <th colspan='2'>",
                        "<strong style = 'color:#0D3B66;'>", 
                        "Seccion: ",shp_mza_2023_secc$SECCION[i],
                        "</strong> 
                          </th>
                        </tr>
                        <tr>
                          <th colspan='2'>",
                        "<strong style = 'color:#0D3B66;'>", 
                        "Manzana: ",shp_mza_2023_secc$CVEGEO[i],
                        "</strong> 
                         </th>
                        </tr>
                        <tr>
                         <td>",input$pik_pob_mza,": ","</td>",
                        "<td>",
                        "<strong style = 'color:#000;'>",
                        shp_mza_2023_secc[,input$pik_pob_mza][[1]][i],
                        "</strong>",
                        "</td>
                        </tr>
                        <tr>
                          <td>
                            Fuente: 
                          </td>
                          <td>
                          CPV 2020
                          </td>
                        </tr>
                       </table>"
                        ))
                        return(x)
                      }), 
                      fillOpacity = 0.6, 
                      fillColor = pal(shp_mza_2023_secc[,input$pik_pob_mza][[1]]),
                      weight = 2, 
                      color = '#596475',
                      opacity = 1.0, 
                      smoothFactor = 0.5, 
                      highlightOptions = highlightOptions(color = '#f72585',
                                                          weight = 2,
                                                          fillOpacity = 0.3, 
                                                          bringToFront = TRUE),
                      labelOptions = labelOptions(style = list('font-weight' = 'normal', 
                                                               padding = '3px 8px'),
                                                  textsize = '15px',
                                                  direction = 'auto')) %>%
          addLegend(colors = pal(as.vector(percentiles)),
                    labels = round(percentiles,0),
                    opacity = 0.7, 
                    title = "Densidad",
                    position = "bottomright") %>% 
          leaflet.extras2::addEasyprint(
            options = leaflet.extras2::easyprintOptions(exportOnly = TRUE,)
          ) 
        
      }
      
      
      if(input$swch_colonias_mzas){
        map_mzas <- map_mzas %>% 
          addPolygons(data = shp_cols_x_secc_selected %>% st_transform(4326) , 
                      label = ~ NOMBRE,
                      #layerId = ~as.character(SECCION),
                      fillOpacity = 0.6,
                      fillColor = '#f72585',
                      weight = 1, 
                      color = '#596475',
                      opacity = 1,
                      smoothFactor = 1,
                      highlightOptions = highlightOptions(color = '#f72585',
                                                          fillOpacity = 0.3,
                                                          weight = 2,
                                                          bringToFront = TRUE))
      }
      
      
      map_mzas
      
    })

  # Texto reactivo descriptor de selección
  observeEvent(c(
    foco_secc_select$secc_select,
    input$tipo_filtro_inicial,
    input$municipio_inicial,
    input$federal_inicial,
    input$local_inicial
  ),{
    
    # browser()
    
    req(!is.null(input$tipo_filtro_inicial))
    
    if(is.null(foco_secc_select$secc_select)){
      x_1 = "Seleccione una seccion"
    }else{
      x_1 = c("Secciones: ",foco_secc_select$secc_select)
    }
    
    x_2 = switch (input$tipo_filtro_inicial,
                  "Municipio" =  c("Municipio de ",input$municipio_inicial),
                  "DFederal"  =  c("DFederal "    ,input$federal_inicial),
                  "DLocal"    =  c("DLocal "      ,input$local_inicial),
                  "Ninguno"   =  c()
                  )
    
    x <- c(x_2,", ",x_1)
    
    req(!is.null(x))
    
    output$text_secc <- renderText({
      x
    })
    
  })
  
  
  # Plotly reactivo a selectores 
  observeEvent(c(
    foco_secc_select$secc_select,
    input$pik_elect_mza,
    input$slid_año_mza
  ),{
    output$py_ganadores <- renderPlotly({
      
      # browser()
      
      cant_votos_nl_secc <- cant_votos_nl[cant_votos_nl$seccion %in% foco_secc_select$secc_select,]
      
      cant_votos_select = cant_votos_nl_secc %>% filter(
        eleccion == input$pik_elect_mza,
        año == input$slid_año_mza
      )
      
      if(nrow(cant_votos_select)>0 ){
        cant_votos_group <- cant_votos_select %>% aggregate(votos ~ partido + eleccion + año,FUN = sum) %>% arrange(partido)
        color <- c()
        for(i in cant_votos_group$partido){
          color <- c(color,color_partido_2[[i]][1])
        }
        cant_votos_group$color <- color
      }else{
        cant_votos_group <- cant_votos_select %>% arrange(partido)
        cant_votos_group$color <- NA
      }
      
      
      
      plot_ly(data = cant_votos_group,  x = ~ votos, y = ~ partido, # color = ,
              type = 'bar', orientation = 'h',marker = list(color = ~ color)
      ) %>% 
        layout(title = "Cantidad de votos por sección",
               showlegend = FALSE)
      
    })
  })
  
  
  # Valore reactivos ----
  ### Valor: Población total
  observeEvent(c(
    foco_secc_select$secc_select,
    input$pik_pob_mza
  ),{
    #browser()
    
    data <- st_drop_geometry(shp_mza2023_secc2024_cpv2020)

    ifelse(
      is.null(foco_secc_select$secc_select),
      valor <- "-",
      ifelse(
        chs_poblaciones_manzanas_t[[input$pik_pob_mza]] %in% grep("^Población", names(chs_poblaciones_manzanas), value = TRUE),
        valor <- sum(data[data$SECCION %in% foco_secc_select$secc_select,input$pik_pob_mza],na.rm = T),
        valor <- round(mean(data[data$SECCION %in% foco_secc_select$secc_select,input$pik_pob_mza],na.rm = T),2)
      )
    )
    
    output$text_vb_pob_map <- renderText({valor})
    
    output$text_titvb_pob_map <- renderText({
      ifelse(
        is.null(input$pik_pob_mza),
        "Seleccione población",
        paste0(chs_poblaciones_manzanas_t[[input$pik_pob_mza]]," en sección seleccionada")
        )
    })
  })
  
  ### Valor: Votos totales de la sección
  observeEvent(c(
    foco_secc_select$secc_select,
    input$pik_elect_mza,
    input$slid_año_mza
  ),{
    
    #browser()
    
    ifelse(
      is.null(foco_secc_select$secc_select),
      valor <- "-",
      {
        data_secc <- cant_votos_nl[cant_votos_nl$seccion %in% foco_secc_select$secc_select,]
        
        data_filter <- data_secc %>% filter(
          eleccion == input$pik_elect_mza,
          año == input$slid_año_mza)
        
        valor <- sum(data_filter$votos, na.rm = T)
      }
      
    )
    
    output$text_vb_vots_map <- renderText({round(as.numeric(valor))})
    
    output$text_titvb_vots_map <- renderText({
      ifelse(
        is.null(input$pik_pob_mza),
        "Seleccione una elección",
        paste0("Votos totales en la elección de ",input$pik_elect_mza)
      )
    })
  })
  
  
  # Actualizamos selectores ----
  observeEvent(input$pik_elect_mza,{
    #browser()
    list_año <- sort(unique(cant_votos_nl[cant_votos_nl$eleccion == input$pik_elect_mza,]$año))
    updateSliderTextInput(session = session, inputId = "slid_año_mza",choices = c(list_año),selected = min(list_año))
  })
  
  
  
  
  
  
}






