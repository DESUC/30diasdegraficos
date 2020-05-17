# Se requiere abrir la base de datos de la Encuesta Bicentenario 2019. 
# Disponible en: https://encuestabicentenario.uc.cl

library(tidyverse)
library(haven)
library(desuctools)

# Apertura de base

df_bicen_19 <- readRDS('inputs/06-dona-df_bicen_19_30diasdegraficos_2020.rds')

# Tabla para gráfico

germenes <- tabla_vars_segmentos(.data = df_bicen_19,
                                  .vars = vars(t18_2),
                                  .segmentos = vars(edad_cat),
                                  .wt = pond_se) %>% 
  filter(!pregunta_cat=="No sabe [NO LEER]") %>% 
  filter(!pregunta_cat=="No responde {NO LEER]") %>% 
  mutate(prop = round(prop*100)) %>%
  group_by(segmento_cat) %>% 
  mutate(lab.ypos = cumsum(prop) - 0.5*prop) %>% 
  mutate(orden = 1:5)

# Gráfico


dona_germen <- ggplot(data = germenes, 
       aes(x = 2, y = prop, fill = reorder(factor(pregunta_cat), -orden)))+
  geom_bar(stat = "identity", color = "white")+
  coord_polar("y") +
  labs(title = "Sección Temores: Temor a los gérmenes, según edad",
       subtitle = "¿Cuánto temor le producen los gérmenes? (%) \n",
       caption = "n = 2047 \nSe omiten categorías No sabe y No responde \nEncuesta Bicentenario 2019, DESUC ") +
  geom_text(aes(y = lab.ypos, label = paste(prop)), col = "white", size = 3) +
  theme_void() +
  facet_wrap(~segmento_cat, ncol = 3) +
  scale_fill_manual(values = c('slategray1', 'skyblue2', 'skyblue3', 'skyblue4', 'slategray4')) +
  xlim(.5,2.5) +
  guides(fill = guide_legend(title = " "))
  

ggsave('outputs/06-dona_germen.png',
       width = 5,
       height = 5,
       scale = 3,
       units = 'cm')