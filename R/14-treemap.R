# Se requiere abrir la base de datos de Nacimientos de INE 
# Disponible en https://www.ine.cl/estadisticas/sociales/demografia-y-vitales/proyecciones-de-poblacion

library(tidyverse)
library(treemapify)

# Cambiar locale para que gráfico tenga meses en español
Sys.setlocale(category = "LC_ALL","es_ES.UTF-8")

# Gráfico de nacimientos por día

# Base de datos con la suma de necimientos por día desde 1985 a 2017.
df_comuna_pob <- readRDS('inputs/14-treemap-df_comuna_summary_30diasdegraficos_2020.rds')

# Ordenar base de datos de norte a sur.
df_comuna_pob <- df_comuna_pob %>% 
  arrange(region_orden)

# Crear factores segun orden geográfico
df_comuna_pob <- df_comuna_pob %>% 
  mutate_at(vars(region), as.character) %>% 
  mutate_at(vars(region:comuna), forcats::as_factor)

orden_region <- levels(df_comuna_pob$region)


levels(df_comuna_pob$nombre_region) <- levels(df_comuna_pob$nombre_region) %>% 
  str_remove_all('Libertador General Bernardo | del General Carlos Ibáñez del Campo| y de la Antártica Chilena| de Santiago')

# Variable de zona
df_comuna_pob <- df_comuna_pob %>% 
  mutate(zona = case_when(region %in% orden_region[1:5] ~ 'Norte',
                          region %in% orden_region[c(6, 8:10)] ~ 'Centro',
                          region %in% orden_region[11:16] ~ 'Sur',
                          region %in% orden_region[7] ~ 'RM'),
         zona = factor(zona, levels = c('Norte', 'Centro', 'Sur', 'RM')))

# Gráfico de árbol

ggplot(df_comuna_pob, 
       aes(area = poblacion, 
           fill = zona,
           label = nombre_comuna,
           subgroup = zona,
           subgroup2 = nombre_region)) +
  geom_treemap(aes(alpha = poblacion), 
               colour = 'white') +
  geom_treemap_subgroup2_border(colour = 'gray95') +
  geom_treemap_subgroup2_text(place = "topright", size = rel(12),
                              grow = FALSE, reflow = TRUE,
                              colour =  "black", fontface = "bold", 
                              min.size = 0) +
  see::scale_fill_flat_d(name = 'Zona', palette = 3) +
  geom_treemap_text(colour = "gray30",
                    place = "bottomleft",
                    reflow = TRUE) +
  theme_minimal() +
  theme(legend.position = 'top') +
  guides(alpha = 'none') +
  


ggsave('outputs/14-treemap.png',
       width = 10,
       height = 10,
       scale = 3,
       units = 'cm')
