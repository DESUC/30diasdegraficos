# Se requiere abrir la base de datos de la Encuesta de Comportamiento Lector ECL 2014.

library(haven)
library(tidyverse)
library(dplyr)
library(tidyr)
library(janitor)
library(labelled)
library(ggplot2)

# Apertura de base

base <- readRDS('inputs/09-areasapiladas-comportamiento_lector2014_30diasdegraficos_2020.rds')

# Recodificaciones

base <- base %>% 
  mutate(a2_r = case_when(a2 == 2 ~ 0,
                          a2 == 1 ~ 1,
                          TRUE ~ NA_real_),
         a2_r = haven::labelled(a2_r,
                                labels = c('Sí' = 1,
                                           'No' = 0),
                                label = 'A2. ¿Ud. lee, aunque sea de vez en cuando y por más de 15 minutos seguidos, libros, revistas, diarios, textos digitales o algún otro tipo de material?'))

edades_cat = seq(16, 96, by = 5)

base$edad_cat <- cut(base$edad_encuestado, breaks = edades_cat)

# Cálcular datos y armar tabla para gráfico

base_chart <- base %>% 
  count(edad_cat, 
        sexo = as_factor(sexo_encuestado), 
        a2_r, wt = fexp, name = 'casos') %>% 
  group_by(edad_cat) %>% 
  mutate(prop = casos / sum(casos)) %>% 
  ungroup()

# Gráfico

gg <- base_chart %>% 
  filter(a2_r == 1, !is.na(edad_cat)) %>% 
  mutate(edad_cat = as.integer(edad_cat)) %>% 
  ggplot(aes(x = edad_cat, y = prop, fill = sexo)) + 
  geom_area() + 
  scale_x_continuous('Edad', 
                     labels = as.character(edades_cat),
                     breaks = seq_along(edades_cat)) +
  scale_y_continuous('% personas que leen 15 minutos seguidos \n o más', limits = c(0,1), labels = function(x) scales::percent(x, accuracy = 1)) +
  scale_fill_manual('Género', 
                    values = c("Hombre"="#0863B7", "Mujer"="#F3711D")) +
  geom_line(color = 'grey60', position = 'stack') +
  theme_minimal() +
  theme(plot.title = element_text(size = rel(1)),
        plot.subtitle = element_text(size = rel(0.8)),
        plot.caption = element_text(size = rel(0.7))) +
  labs(title = "Porcentaje de personas que \n leen 15 minutos seguidos o más, por edad y sexo",
       subtitle = "¿Ud. lee, aunque sea de vez en cuando y por más de 15 minutos seguidos, \n libros, revistas, diarios, textos digitales o algún otro tipo de material?",
       caption = "Segunda Encuesta de Comportamiento Lector\n Datos ponderados, n válido = 6.889 \n DESUC, 2014")

ggsave('outputs/09-areas_apiladas.png',
       width = 6,
       height = 5,
       scale = 3,
       units = 'cm')
