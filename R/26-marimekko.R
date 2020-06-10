# Se requiere abrir la base de datos de la Encuesta Bicentenario 2019. 
# Disponible en: https://mma.gob.cl/encuestas-nacionales-del-medio-ambiente/

library(tidyverse)
library(ggplot2)
library(sjlabelled)
library(janitor)

base <- readRDS ('inputs/26-marimekko-df_encuesta_de_medio_ambiente_2016.rds')

# Datos a graficar
base2 <- base %>%
  filter(p21 != -999,
         p21 != -888,
         p27e != -999,
         p27e != -888) %>%
  count(p21, 
        p27e, wt = pond, name = 'casos') %>% 
  group_by(p21) %>% 
  mutate(prop = casos / sum(casos)) %>% 
  ungroup()

# Ancho de barras
base2$width <- with(base2, by(casos, p21, sum, na.rm = T)[p21])

# Etiquetas de p21 
base2$p21 <- factor(base2$p21, levels = c('1', '2', '3', '4'),
                        labels = c("Nada \n informado", "Poco \n informado", "Bastante \n informado", "Muy informado"))

# Gráfico

gg_marimekko <- ggplot(base2,
                    aes(x = p21, y = prop, width = width, fill = as.factor(p27e))) +
  geom_bar(stat = "identity", position = position_fill(reverse = TRUE), colour = "grey80") +
  # geom_text(aes(label = scales::percent(prop)), position = position_stack(vjust = 0.5), size = rel(3)) + # labels
  facet_grid(~p21, scales = "free_x", space = "free_x") +
  scale_fill_manual(' ', values=c("peachpuff4","olivedrab2","forestgreen","darkgreen"), 
                    labels = c("Nada \n importante", "Poco \n importante", "Bastante \n importante", "Muy \n importante")) +
  scale_y_continuous('', expand = c(0, 0), 
                     limits = c(0,1), 
                     labels = function(x) scales::percent(x, accuracy = 1)) +
  theme_dark() +
  theme(plot.title = element_text(size = rel(1.1)),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = rel(0.9), angle = 90),
        strip.text = element_blank(),
        legend.text = element_text(size = rel(0.7)),
        panel.spacing.x = unit(0, "npc")) +
  labs(title = 'Importancia del cambio climático para Ud. \n según cuán informado se siente sobre el cambio climático',
       caption = "Datos ponderados, n = 2.130, se excluyen NS/NR \nEncuesta Nacional del Medio Ambiente 2016, DESUC")

ggsave('outputs/26-marimekko.png',
       width = 5,
       height = 5,
       scale = 3,
       units = 'cm')
