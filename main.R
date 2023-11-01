# 1. Librerías
# ------------------------------------
 
library(data.table)
library(dplyr)
library(stringr)


# 2. Cargar datos
# ------------------------------------

res_general <- fread("data/2023_resultados_electorales_generales.csv", header = TRUE, data.table = FALSE)
res_paso <- fread("data/2023_resultados_electorales_paso.csv", header = TRUE, data.table = FALSE)

ambitos_general <- fread("data/2023_ambitos_electorales_generales.csv", header = TRUE, data.table = FALSE)

colores <-  fread("data/2023_colores.csv", header = TRUE, data.table = FALSE)

## Definir distrito y sección objetivo 

distrito <- "Buenos Aires"
seccion  <- "Trenque Lauquen"

res_general_seccion <- res_general |> filter(distrito_nombre == distrito & seccion_nombre == seccion)
res_paso_seccion    <- res_paso |> filter(distrito_nombre == distrito & seccion_nombre == seccion)

ambitos_general_seccion <- ambitos_general |> filter(distrito_nombre == distrito & seccion_nombre == seccion)

colores_distrito <- colores |> filter(distrito_nombre == distrito)

rm(res_general, res_paso, ambitos_general, colores)
gc()



# 3. Transformación de datos
# ------------------------------------

var_obj <- c("eleccion_tipo", "circuito_id", "mesa_id", "mesa_tipo", "mesa_electores", "cargo_nombre", "agrupacion_nombre", "votos_tipo", "votos_cantidad")

general <- res_general_seccion |> select(any_of(var_obj)) |> mutate(lista_numero = NA)
paso <- res_paso_seccion |> select(any_of(c(var_obj, "lista_numero")))
resultados <- rbind(paso, general)

resultados <- resultados %>% 
  mutate(cargo_nombre = case_when(
    cargo_nombre %in% c("SENADOR NACIONAL", "SENADORES/AS NACIONALES") ~ "Senadores Nacionales",
    cargo_nombre %in% c("DIPUTADO NACIONAL", "DIPUTADOS/AS NACIONALES") ~ "Diputados Nacionales",
    cargo_nombre %in% c("DIPUTADO PROVINCIAL", "DIPUTADOS/AS PROVINCIALES") ~ "Diputados Provinciales",
    cargo_nombre %in% c("PRESIDENTE Y VICE", "PRESIDENTE/A") ~ "Presidente",
    cargo_nombre %in% c("GOBERNADOR Y VICE", "GOBERNADOR/A") ~ "Gobernador",
    cargo_nombre %in% c("INTENDENTE", "INTENDENTE/A") ~ "Intendente",
    TRUE ~ str_to_title(cargo_nombre)  # Mantener otros valores sin cambios
  ))

resultados <- resultados |> 
  mutate(mesa_tipo = str_to_title(mesa_tipo),
         eleccion_tipo = str_to_title(eleccion_tipo),
         agrupacion_nombre = str_to_title(agrupacion_nombre),
         mesa_tipo = str_to_title(mesa_tipo),
         votos_tipo = str_to_title(votos_tipo)
         )

circuitos_geo <- as.data.frame(matrix(nrow = 6, ncol = 2))
colnames(circuitos_geo) <- c("circuito", "descripcion")

circuitos_geo$circuito <- general |> select(circuito_id) |> distinct() |> arrange() |> pull()
circuitos_geo$descripcion <- c("TL-P.Junta-Lértora", "Beruti", "La Zanja", "30 de Agosto", "Trongé-Garré", "Girodías")



# 4. Grabar dataset depurado
# ------------------------------------

saveRDS(general, file = "data/general.rds")
saveRDS(paso, file = "data/paso.rds")
saveRDS(resultados, file = "data/resultados.rds")
saveRDS(colores_distrito, file = "data/colores.rds")
saveRDS(ambitos_general_seccion, file = "data/ambitos.rds")
saveRDS(circuitos_geo, file = "data/circuitos.rds")

rm(list = ls())
