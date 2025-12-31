# ==============================================================================
# GLOBAL.R
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
library(htmltools)
library(leaflet.extras2)
library(shinycssloaders)
library(jsonlite)

library(promises)
library(future)
library(gemini.R)

plan(multisession)

source("R/branding_config.R")

# 2. CARGA DE MÓDULOS ----------------------------------------------------------
list.files("R", full.names = TRUE, pattern = "\\.R$") %>% 
  purrr::walk(source)

# --- GESTIÓN DE CREDENCIALES ---
creds_env <- Sys.getenv("APP_CREDENTIALS")

if (creds_env != "") {
  # Si existe la variable (estamos en el servidor configurado)
  credentials <- jsonlite::fromJSON(creds_env)
} else {
  # FALLBACK: Si estás en tu compu local y no configuraste el .Renviron,
  # intenta leer un archivo local (opcional) o usa unas credenciales de prueba.
  # Esto evita que la app truene si se te olvida configurar la variable localmente.
  
  if(file.exists("seguridad/credentials.rds")){
    credentials <- readRDS("seguridad/credentials.rds")
  } else {
    # Credenciales dummy de emergencia para desarrollo local
    credentials <- data.frame(
      user = c("admin"), 
      password = c("admin"), 
      role = c("admin"), 
      stringsAsFactors = FALSE
    )
  }
}

# CONFIGURACIÓN DE IDENTIDAD
# Usar "BOLETA" o "QUIPU" según lo que quieras desplegar
marca_actual <- Sys.getenv("MARCA_ACTIVA", unset = "QUIPU")

# Cargamos los parámetros visuales
APP_CONFIG <- obtener_configuracion_marca(marca_actual)

# 3. CONSTANTES ESTÁTICAS ------------------------------------------------------
# (Estas consumen poca memoria, se pueden dejar aquí)
color_partido_2 <- c("MORENA" = "#B41A1A", "PAN" = "#0F58A8", "PRI" = "#FF0000",  
                     "MC" = "#FD7A13", "VERDE" = "#228B22", "PT" = "#FFF200",  
                     "INDEP" = "#A000E5")

paleta_partidos <- c("PRI" = "#e41a1c", "PAN" = "#377eb8", "MC" = "#ff7f00",
                     "MORENA" = "#a65628", "PT" = "#b30000", "VERDE" = "#4daf4a",
                     "INDEPE" = "#7678ed", "INDEP" = "#7678ed")

chs_censales <- c(
  "Población Indígena" = "PHOG_IND", "Población Afromexicana" = "POB_AFRO",
  "Población con discapacidad" = "PCON_DISC", "Población ocupada (+12)" = "POCUPADA",
  "Población desocupada (+12)" = "PDESOCUP", "Viviendas part. habitadas" = "VIVPAR_HAB",
  "Viviendas part. deshabitadas" = "VIVPAR_DES", "Grado prom. escolaridad" = "GRAPROES",
  "Prom. ocupantes x vivienda" = "PROM_OCUP"
)

bins_cambio <- c(-Inf, -0.8, -0.6, -0.4, -0.2, -0.1, -0.05, -0.01, 0.01, 
                 0.05, 0.1, 0.2, 0.4, 0.6, 0.8, Inf)
colores_cambio <- c("#800f2f", "#a4133c", "#c9184a", "#ff4d6d", "#ff758f", "#ffb3c1", "#ffccd5", "#fff0f3", 
                    "#f8f9fa", "#d8f3dc", "#b7e4c7", "#95d5b2", "#74c69d", "#52b788", "#40916c", "#2d6a4f", "#1b4332", "#081c15")
paleta_cambio <- colorBin(palette = colores_cambio, domain = NULL, bins = bins_cambio, na.color = "#ced4da")


# 4. CARGA DE DATOS OPTIMIZADA (RDS) -------------------------------------------


# Listas Auxiliares (Cargamos el objeto y desglosamos)
listas_aux <- readRDS("data_optimizada/listas_auxiliares.rds")
choices_rendimiento_historico <- listas_aux$choices_rendimiento_historico
chs_mun_cols                  <- listas_aux$chs_mun_cols
chs_poblaciones_manzanas      <- listas_aux$chs_poblaciones_manzanas
chs_poblaciones_manzanas_t    <- listas_aux$chs_poblaciones_manzanas_t
lista_elecciones_nombres      <- listas_aux$lista_elecciones_nombres
choices_elections_sombra      <- listas_aux$choices_elections_sombra
rm(listas_aux) # Limpiar memoria

# Datos Espaciales (Ya vienen con proyección y joins)
secciones_prev <- readRDS("data_optimizada/secciones_mapa.rds")
shp_cols_x_secc <- readRDS("data_optimizada/colonias_mapa.rds")
shp_mza2023_secc2024_cpv2020 <- readRDS("data_optimizada/manzanas_censo_mapa.rds")
municipios_lista <- readRDS("data_optimizada/municipios_lista.rds")

# Datos Tabulares
cant_votos_nl <- readRDS("data_optimizada/cant_votos_nl_procesado.rds")
# cant_votos <- readRDS("data_optimizada/cant_votos_simple.rds")
base_ganadores <- readRDS("data_optimizada/base_ganadores.rds")
res_trab <- readRDS("data_optimizada/res_trab.rds")
edades <- readRDS("data_optimizada/edades.rds")
colonias <- readRDS("data_optimizada/colonias.rds") 
participacion <- readRDS("data_optimizada/participacion_full.rds")
data_secc_cpv2020 <- readRDS("data_optimizada/data_secc_cpv2020_procesado.rds")

# Variable simple necesaria para UI
list_eleccion <- sort(unique(cant_votos_nl$eleccion))


mapa_elecciones <- c(
  # 2015
  "ayunt15" = "alcalde_15",
  "gob15"   = "gober_15",
  "dl15"    = "dip_local_15",
  "fed15"   = "dip_fed_15",
  
  # 2018
  "ayunt18" = "alcalde_18",
  "dipl18"  = "dip_local_18",
  "fed18"   = "dip_fed18",      
  "sen18"   = "senado_18",
  "pres18"  = "pres_18",
  
  # 2021
  "ayunt21" = "alcalde_21",
  "gob21"   = "gober_21",
  "dl21"    = "dip_local_21",
  "fed21"   = "dip_fed_21",
  
  # 2024
  "ayunt24" = "alcalde_24",
  "dipl24"  = "dip_local_24",
  "fed24"   = "dip_fed24",
  "pres24"  = "pres_24",
  "sen24"   = "sen_24"
)


get_color_partido <- function(partido) {
  p <- tolower(partido)
  if (grepl("pan", p)) return("#0055bf")      # Azul PAN
  if (grepl("pri", p)) return("#00953b")      # Verde PRI
  if (grepl("morena", p)) return("#b1262d")   # Guinda Morena
  if (grepl("mc", p)) return("#ff8300")       # Naranja MC
  if (grepl("verde", p)) return("#50b747")    # Verde PVEM
  if (grepl("pt", p)) return("#d91d29")       # Rojo PT
  return("#666666")                           # Gris por defecto
}







