library(tidyverse)

# 1. Cargar los dos archivos
votos_new <- readRDS("data_optimizada/cant_votos_nl_procesado.rds")
votos_old <- readRDS("data_optimizada/cant_votos_simple.rds")

# 2. Obtener lista única de elecciones en cada uno
ids_new <- unique(votos_new$eleccion_año_id)
ids_old <- unique(votos_old$eleccion)

# 3. Imprimir comparación
cat("=== ELECCIONES EN EL ARCHIVO NUEVO (cant_votos_nl) ===\n")
print(sort(ids_new))

cat("\n=== ELECCIONES EN EL ARCHIVO VIEJO (cant_votos) ===\n")
print(sort(ids_old))

# 4. Ver si hay intersección (nombres iguales)
interseccion <- intersect(ids_new, ids_old)
cat("\n=== COINCIDENCIAS EXACTAS DE ID ===\n")
print(interseccion)