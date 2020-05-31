#Encuesta de Convivencia Barrial e Interculturalidad 2019, realizada por DESUC para la 
#Subsecretaría de Prevención del Delito.

# Paquetes ----------------------------------------------------------------

library(haven)
library(janitor)
library(tidyverse)
library(desuctools)
library(igraph)
library(ggraph)
library(dplyr)
library(sjmisc)

# Base ----------------------------------------------------------------

base <- readRDS('inputs/20-redes-migrantes.rds')

# Generación de insumo ------------------------------------------------

gg <- desuctools::tabla_vars_segmentos(.data = base,
                                       .vars = vars(T_P32_1:T_P32_5,T_P32_7:T_P32_8),
                                       .segmentos = vars(nac_espec),
                                       .wt = rake_wbsp) %>% 
  filter(pregunta_cat == 'Sí') %>% 
  filter(segmento_cat != 'Otra') %>% 
  filter(segmento_cat != 'Argentina')

links <- gg %>% 
  select(pregunta_lab, segmento_cat, prop) %>% 
  mutate(prop = prop*100)

vertices <- links %>% 
  select(pregunta_lab) %>% 
  unique() %>% 
  mutate(label = pregunta_lab)

grafico <- graph_from_data_frame(d=links, vertices=vertices, directed=T) 

# Gráfico ------------------------------------------------

set.seed(123)
redes <- ggraph(grafico, layout="lgl") +
  geom_edge_arc(aes(color = prop, alpha = prop), width=1.1, curvature=0.3) +
  geom_node_point() + 
  geom_node_text(aes(label = label), size=4, color="black", repel=T) +
  scale_edge_color_gradient(low = "azure3", high = "cornflowerblue", name = '%') +
  scale_edge_alpha_continuous(guide = FALSE) +
  ggtitle("Porcentaje de personas con amistades de otras nacionalidades \nsegún nacionalidad")
  

ggsave('outputs/redes.png', 
       redes, 
       scale = 3,
       width = 5, height = 4, units = 'cm')  
