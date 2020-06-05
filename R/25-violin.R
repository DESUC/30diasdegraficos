

# Paquetes ----------------------------------------------------------------

library(sjlabelled)
library(tidyverse)
library(ggpubr)
library(haven)
library(janitor)
library(readxl)
library(scales)
library(hrbrthemes)


# Base de datos -----------------------------------------------------------


base <- read_excel(path = 'inputs/25-violin.xlsx',
                   skip = 1) %>% 
  clean_names()

names(base)

#creación de factores

base <- base %>%
  mutate_at(vars(codigo_region),
            ~ labelled(., labels = c('Metropolitana' = 13,
                                     'Biobío' = 8,
                                     'Valparaíso' = 5,
                                     'Tarapacá' = 1, 
                                     'Angofagasta' = 2)) %>% 
              as_factor(.))

base <- base %>%
  mutate(mayor_10 = if_else(percent_migrantes >= .1, TRUE, FALSE) %>% factor())

head(base)


# Tablas ------------------------------------------------------------------

#Porcentaje medio por comuna de población migrante.

region_percent_mean <- base %>% 
  filter(total_chile >= 50000) %>% 
  group_by(codigo_region) %>% 
  summarise(percent_migrantes_mean = mean(percent_migrantes))

region_percent_mean


# Gráficos ----------------------------------------------------------------

#Gráfico Proporción migrantes por región

set.seed(1000)

p_region_comuna <- base %>% 
  filter(total_chile >= 50000) %>% 
  ggplot(aes(x = codigo_region, y = percent_migrantes, color = mayor_10)) +
  geom_violin(aes(group = codigo_region), color = 'black') +
  geom_point(position = 'jitter') +
  scale_y_continuous(labels = function(.x) scales::percent(.x, accuracy = 1)) +
  scale_color_manual(values = c('grey69', '#5B9BD5')) +
  guides(color = 'none') +
  ggrepel::geom_text_repel(aes(label = if_else(mayor_10 == TRUE, nombre_comuna, NULL)),
                           size = 2, color = '#3A5B74',
                           position = 'jitter') +
  geom_point(data = region_percent_mean,
             aes(x = codigo_region, y = percent_migrantes_mean, color = NULL), 
             shape = 3) +
  geom_label(data = region_percent_mean,
             aes(x = codigo_region, y = percent_migrantes_mean, color = NULL,
                 label = percent(percent_migrantes_mean)),
             hjust = 1, nudge_x = -.05, nudge_y = -.011, color = '#209D89', 
             size = 2, alpha = .5) +
  labs(title = 'Comunas por región según porcentaje de población migrante',
       subtitle = 'Comunas de más de 50.000 habitantes.\nAzul se destacan comunas con 10% o más de población migrante.',
       caption = 'Fuente: Elaboración propia en base a Censo 2017',
       y = '% de población migrante',
       x = 'Regiones de interés') 

p_region_comuna

ggsave('outputs/25-violin.png',
       width = 8,
       height = 5,
       scale = 3,
       units = 'cm')




