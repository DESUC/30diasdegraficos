# Disponible en https://www.ine.cl/estadisticas/sociales/demografia-y-vitales/proyecciones-de-poblacion


# Paquetes ----------------------------------------------------------------

library(tidyverse)
library(sunburstR)

# Base de datos -----------------------------------------------------------

df_comuna_pob <- readRDS("inputs/23-poblacion.rds")

# Gráfico -----------------------------------------------------------------

df_poblacion <- df_comuna_pob %>% 
  mutate(path = str_glue("{zona}-{nombre_region}-{nombre_comuna}")) %>% 
  select(path, Freq = poblacion)

p_sunburst <- sunburst(data = df_poblacion,
                       sortFunction = htmlwidgets::JS(),
                       legend = FALSE,
                       withD3 = FALSE)

p_sunburst

htmltools::save_html(p_sunburst, file = file.path("23-sunburst.html"))

library(webshot)
webshot('23-sunburst.html', "outputs/23-sunburst.png")
