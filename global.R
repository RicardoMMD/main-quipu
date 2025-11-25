# ==============================================================================
# GLOBAL.R - Configuración Inicial y Carga de Datos
# ==============================================================================

# 1. CARGA DE LIBRERÍAS --------------------------------------------------------
library(shiny)
library(shinydashboard)
library(shinymanager)
library(shinyWidgets)
library(shinyjs)
library(DT)
library(plotly)
library(leaflet)
library(sf)
library(tidyverse)
library(scales)
library(colorRamps)
library(readxl)
library(htmltools)
library(leaflet.extras2) # Necesario para easyprint en el módulo de manzanas

# 2. CARGA DE RECURSOS MODULARES Y FUNCIONES -----------------------------------
# Esto carga todas las funciones y módulos que creamos en la carpeta R/
# Así están disponibles inmediatamente en ui.R y server.R
list.files("R", full.names = TRUE, pattern = "\\.R$") %>% 
  purrr::walk(source)

# 3. CONSTANTES Y PALETAS DE COLORES -------------------------------------------

# Paletas
color_partido_2 <- c("MORENA" = "#B41A1A", "PAN" = "#0F58A8", "PRI" = "#FF0000",  
                     "MC" = "#FD7A13", "VERDE" = "#228B22", "PT" = "#FFF200",  
                     "INDEP" = "#A000E5")

paleta_partidos <- c("PRI" = "#e41a1c", "PAN" = "#377eb8", "MC" = "#ff7f00",
                     "MORENA" = "#a65628", "PT" = "#b30000", "VERDE" = "#4daf4a",
                     "INDEPE" = "#7678ed")

# Opciones Censales
chs_censales <- c(
  "Población Indígena" = "PHOG_IND",
  "Población Afromexicana" = "POB_AFRO",
  "Población con discapacidad" = "PCON_DISC",
  "Población ocupada (+12)" = "POCUPADA",
  "Población desocupada (+12)" = "PDESOCUP",
  "Viviendas part. habitadas" = "VIVPAR_HAB",
  "Viviendas part. deshabitadas" = "VIVPAR_DES",
  "Grado prom. escolaridad" = "GRAPROES",
  "Prom. ocupantes x vivienda" = "PROM_OCUP"
)

# Paleta de Cambio (Divergente)
bins_cambio <- c(-Inf, -0.8, -0.6, -0.4, -0.2, -0.1, -0.05, -0.01, 0.01, 
                 0.05, 0.1, 0.2, 0.4, 0.6, 0.8, Inf)

colores_cambio <- c(
  "#800f2f", "#a4133c", "#c9184a", "#ff4d6d", "#ff758f", "#ffb3c1", "#ffccd5", "#fff0f3", # Rojos
  "#f8f9fa", # Neutro
  "#d8f3dc", "#b7e4c7", "#95d5b2", "#74c69d", "#52b788", "#40916c", "#2d6a4f", "#1b4332", "#081c15" # Verdes
)

paleta_cambio <- colorBin(palette = colores_cambio, domain = NULL, bins = bins_cambio, na.color = "#ced4da")


lista_elecciones_nombres <- list(
  "Elecciones 2024" = c("Alcalde 2024", "Diputado Federal 2024", "Diputado Local 2024", "Presidencia 2024", "Senado 2024"),
  "Elecciones 2021" = c("Alcalde 2021", "Diputado Federal 2021", "Diputado Local 2021", "Gobernador 2021"),
  "Elecciones 2018" = c("Alcalde 2018", "Diputado Federal 2018", "Diputado Local 2018", "Presidencia 2018", "Senado 2018"),
  "Elecciones 2015" = c("Alcalde 2015", "Diputado Federal 2015", "Diputado Local 2015", "Gobernador 2015")
)

# 4. CARGA Y PROCESAMIENTO DE DATOS --------------------------------------------

# --- CREDENCIALES (Para shinymanager) ---
credentials <- readxl::read_excel("www/data/contrasenas_nl.xlsx")



# A) CARGA DE SHAPEFILES Y DATOS GEOGRÁFICOS
municipios_lista <- st_read("www/shps/MUNICIPIO.shp", quiet = TRUE) %>% 
  st_drop_geometry() %>% 
  select(MUNICIPIO, NOMBRE)

secciones_prev_shp <- st_read("www/shps/SECCION_2.shp", quiet = TRUE)
shp_cols_x_secc_shp <- st_read("www/shps/shp_cols_x_secc_simplify/shp_cols_x_secc.shp", quiet = TRUE)
shp_mza_2023_shp <- st_read('www/shps/19m.shp', quiet = TRUE)

# B) CARGA DE CSVs Y EXCELS
secciones_sd <- read_csv("www/data/datos_por_seccion_NUEVO_LEON.csv", show_col_types = FALSE)
cant_votos_nl <- read_csv("www/data/cant_votos_nl.csv", show_col_types = FALSE)
base_ganadores <- read_csv("www/data/datos_ganadores_NUEVO_LEON.csv", show_col_types = FALSE)
res_trab <- read_csv("www/data/resultados_trabajado_NUEVO_LEON.csv", show_col_types = FALSE)
cant_votos <- read_csv("www/data/resultados_trabajado_cant_votos_NUEVO_LEON.csv", show_col_types = FALSE)
colonias <- read_excel("www/data/colonias.xlsx")
edades <- read_csv("www/data/edades_arreglado.csv", show_col_types = FALSE)

participacion <- readRDS("www/data/participacion.rds") %>%
  left_join(municipios_lista, by = "MUNICIPIO")

# Datos Censales Manzanas (CPV 2020)
data_mza_urbana_cpv2020 <- read_csv("www/mzas/conjunto_de_datos_ageb_urbana_19_cpv2020.csv", show_col_types = FALSE)
data_nl_mza2023_secc2024 <- read_csv("www/mzas/data_nl_mza2023_secc2024.csv", show_col_types = FALSE)
select_data_cpv <- read.csv("www/mzas/diccionario_datos_ageb_urbana_19_cpv2020.csv")

data_secc_cpv2020 <- read_csv("www/data/INE_SECCION_2020.csv", show_col_types = FALSE) %>% 
  filter(ENTIDAD == 19) %>%
  select(ENTIDAD, DISTRITO, SECCION, POBTOT, POBFEM, POBMAS, P_18YMAS, PHOG_IND, 
         POB_AFRO, PCON_DISC, POCUPADA, PDESOCUP, PROM_OCUP, VIVPAR_HAB, 
         VIVPAR_DES, GRAPROES)

# C) PROCESAMIENTO Y UNIONES (ETL)

list_eleccion <- sort(unique(cant_votos_nl$eleccion))

# 1. Procesar Secciones (Unión con municipios y población)
secciones_prev <- secciones_prev_shp %>% 
  left_join(municipios_lista, by = "MUNICIPIO") %>% 
  left_join(select(secciones_sd, SECCION, pobtotal = POBTOT), by = "SECCION") %>% 
  mutate(SECCION = as.character(SECCION)) %>% 
  select(-GEOMETRY1_)

# 2. Transformaciones Espaciales (CRÍTICO: Hacerlo aquí para no repetir en server)
shp_cols_x_secc <- st_transform(shp_cols_x_secc_shp, 4326)
shp_mza_2023 <- st_transform(shp_mza_2023_shp, 4326)

# 3. Limpieza Votos
cant_votos_nl$partido <- toupper(cant_votos_nl$partido)
cant_votos <- cant_votos %>% 
  mutate(seccion = as.character(seccion)) %>% 
  left_join(select(st_drop_geometry(secciones_prev), seccion = SECCION, distrito = DISTRITO_L), by = "seccion")

edades <- edades %>% mutate(seccion = as.character(seccion))

# 4. Datos Educativos (Categorización)
data_secc_cpv2020$GRAPROES_NIVEL <- cut(
  data_secc_cpv2020$GRAPROES,
  breaks = c(-Inf, 0, 6, 9, 12, 17, Inf),
  labels = c("Sin Escolaridad", "Primaria", "Secundaria", "Preparatoria", "Licenciatura", "Posgrado"),
  right = FALSE, include.lowest = TRUE
)

# 5. Preparación de Manzanas (Unión Espacial + Censal)
# Unimos shapefile -> relación seccion -> datos censales
shp_mza2023_secc2024_cpv2020 <- shp_mza_2023 %>%
  left_join(select(data_nl_mza2023_secc2024, CVEGEO, SECCION, area_intersect), by = "CVEGEO") %>%
  left_join(select(data_mza_urbana_cpv2020, CVEGEO, NOM_ENT, NOM_MUN, NOM_LOC, POBTOT:VPH_SINTIC), by = "CVEGEO")

# 6. Creación de IDs para `cant_votos_nl` (Lookup Table Logic)
choices_rendimiento_historico = list(
  "Elecciones 2024" = c("Presidencia 2024" = "pres_24", "Senado 2024" = "sen_24", "Diputado federal 2024" = "dip_fed24", "Diputado local 2024" = "dip_local_24", "Alcalde 2024" = "alcalde_24"),
  "Elecciones 2021" = c("Gobernador 2021" = "gober_21", "Diputado federal 2021" = "dip_fed_21", "Diputado local 2021" = "dip_local_21", "Alcalde 2021" = "alcalde_21"),
  "Elecciones 2018" = c("Presidencia 2018" = "pres_18", "Senado 2018" = "senado_18", "Diputado federal 2018" = "dip_fed18", "Diputado local 2018" = "dip_local_18", "Alcalde 2018" = "alcalde_18"),
  "Elecciones 2015" = c("Gobernador 2015" = "gober_15", "Diputado federal 2015" = "dip_fed_15", "Diputado local 2015" = "dip_local_15", "Alcalde 2015" = "alcalde_15")
)

lookup_table <- tibble(
  display_name = unlist(lapply(choices_rendimiento_historico, names)),
  filter_value = unlist(choices_rendimiento_historico) 
) %>%
  mutate(
    año = as.numeric(str_extract(display_name, "\\d{4}$")), 
    eleccion_type = str_trim(str_remove(display_name, "\\s\\d{4}$")) 
  ) %>% select(eleccion_type, año, filter_value) 

cant_votos_nl <- cant_votos_nl %>%
  left_join(lookup_table, by = c("eleccion" = "eleccion_type", "año" = "año")) %>%
  rename(eleccion_año_id = filter_value)

# } # Fin del else (si implementas la carga condicional RDS)

# 5. DEFINICIÓN DE OPCIONES PARA UI (Listas dinámicas) -------------------------

# Opciones geográficas base
opciones_municipio <- sort(unique(secciones_prev$NOMBRE))
opciones_local <- sort(unique(secciones_prev$DISTRITO_L))
opciones_federal <- sort(unique(secciones_prev$DISTRITO))

# Mapeos para filtros de colonias
relacion_cve_mun_colonias <- secciones_prev %>%
  st_drop_geometry() %>% 
  filter(!is.na(NOMBRE)) %>%
  count(MUNICIPIO, NOMBRE)

chs_mun_cols <- setNames(relacion_cve_mun_colonias$MUNICIPIO, relacion_cve_mun_colonias$NOMBRE)

# Mapeos Manzanas
chs_poblaciones_manzanas <- setNames(select_data_cpv$Mnemónico, select_data_cpv$Indicador)
chs_poblaciones_manzanas_t <- setNames(select_data_cpv$Indicador, select_data_cpv$Mnemónico)

# Mapeos Elecciones Sombra
choices_elections_sombra <- list(
  "Elecciones 2024" = c("Presidente 2024" = "pres24", "Senado 2024" = "sen24", "Diputado Federal 2024" = "fed24", "Diputado Local 2024" = "dipl24", "Alcalde 2024" = "ayunt24"),
  "Elecciones 2021" = c("Gobernador 2021" = "gob21", "Diputado Federal 2021" = "fed21", "Diputado Local 2021" = "dl21", "Alcalde 2021" = "ayunt21"),
  "Elecciones 2018" = c("Presidente 2018" = "pres18", "Senado 2018" = "sen18", "Diputado Federal 2018" = "fed18", "Diputado Local 2018" = "dipl18", "Alcalde 2018" = "ayunt18"),
  "Elecciones 2015" = c("Diputado Federal 2015" = "fed15", "Diputado Local 2015" = "dipl15", "Alcalde 2015" = "ayunt15")
)