# Se requiere abrir la base de datos de Nacimientos de INE 
# Disponible en https://www.ine.cl/estadisticas/sociales/demografia-y-vitales/proyecciones-de-poblacion


# Paquetes ----------------------------------------------------------------

library(tidyverse)
library(janitor)
library(sunburstR)

# Base de datos -----------------------------------------------------------

df_comuna_pob <- readRDS("inputs/23-poblacion.RDS")

# Gráfico -----------------------------------------------------------------

df_poblacion <- df_comuna_pob %>% 
  select(zona, nombre_region, nombre_comuna, poblacion)

df_poblacion$path = paste(df_poblacion$zona, df_poblacion$nombre_region, df_poblacion$nombre_comuna, sep="-")

dia23 <- sunburst(
  data.frame(xtabs(poblacion~path,df_poblacion)),
  sortFunction = htmlwidgets::JS())
dia23

