# ==============================================================================
# SCRIPT DE MANTENIMIENTO: TRANSFORMACIÓN DE GEOMETRÍAS A EPSG:4326
# ==============================================================================
# Objetivo: Estandarizar todas las capas espaciales para optimizar el renderizado
# en Leaflet (que usa EPSG:3857 pero requiere inputs en EPSG:4326).
# ==============================================================================

library(sf)
library(dplyr)

# 1. Configuración de Rutas ----------------------------------------------------
dir_data <- "data_optimizada"
dir_backup <- "data_optimizada/backup_crs_original"

# Mapa de archivos a procesar basado en tu global.R y metadatos
archivos_objetivo <- list(
  "secciones_mapa.rds"      = "secciones_prev",       # Estaba en UTM Zone 14N
  "participacion_full.rds"  = "participacion",        # Estaba en UTM Zone 14N
  "colonias_mapa.rds"       = "shp_cols_x_secc",      # Ya estaba en 4326, se asegura
  "manzanas_censo_mapa.rds" = "shp_mza_censo"         # Ya estaba en 4326, se asegura
)

# 2. Crear Respaldo de Seguridad -----------------------------------------------
if (!dir.exists(dir_backup)) {
  dir.create(dir_backup)
  message(sprintf("✅ Carpeta de respaldo creada: %s", dir_backup))
}

message("⏳ Iniciando respaldo de archivos originales...")

for (archivo in names(archivos_objetivo)) {
  path_origen <- file.path(dir_data, archivo)
  path_destino <- file.path(dir_backup, archivo)
  
  if (file.exists(path_origen)) {
    file.copy(path_origen, path_destino, overwrite = TRUE)
    message(sprintf("   -> Respaldado: %s", archivo))
  } else {
    warning(sprintf("   ⚠️ Archivo no encontrado: %s", archivo))
  }
}

# 3. Procesamiento y Transformación --------------------------------------------
message("\n🚀 Iniciando transformación de geometrías a EPSG:4326...")

for (archivo in names(archivos_objetivo)) {
  path_archivo <- file.path(dir_data, archivo)
  
  if (file.exists(path_archivo)) {
    
    # A) Cargar Datos
    message(sprintf("   ... Leyendo %s", archivo))
    geo_data <- readRDS(path_archivo)
    
    # B) Verificar si es objeto espacial (sf)
    if (inherits(geo_data, "sf")) {
      
      crs_actual <- st_crs(geo_data)$epsg
      
      # Solo transformar si no es 4326 o si el CRS es nulo/diferente
      if (is.null(crs_actual) || crs_actual != 4326) {
        
        message(sprintf("   🔄 Transformando de EPSG:%s a EPSG:4326...", crs_actual))
        geo_data_trans <- st_transform(geo_data, 4326)
        
        # C) Guardar archivo optimizado
        saveRDS(geo_data_trans, path_archivo)
        message(sprintf("   ✅ Guardado exitoso: %s", archivo))
        
      } else {
        message(sprintf("   👌 %s ya estaba en EPSG:4326. No se requieren cambios.", archivo))
      }
      
    } else {
      warning(sprintf("   ⚠️ El archivo %s no es un objeto 'sf'. Se omitió.", archivo))
    }
    
    # Limpieza de memoria
    rm(geo_data)
    gc()
    
  } 
}

message("\n✨ Proceso finalizado. Todas las capas están ahora en EPSG:4326.")