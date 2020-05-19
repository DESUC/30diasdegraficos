# Se requiere abrir la base de datos de la Encuesta Bicentenario 2019. 
# Disponible en: https://encuestabicentenario.uc.cl

library(tidyverse)
library(haven)
library(sjmisc)
library(sjlabelled)
library(reshape2)

# Apertura de base

df_bicen19 <- haven::read_sav('inputs/190928 - 04-df_bicen_19_pond_org_difusion.sav')

# Revisión base

df_bicen19 <- df_bicen19 %>% 
  mutate(total = 1)

df_bicen19 <- df_bicen19 %>% 
  mutate(pobre = as.numeric(s03_1)) %>% 
  mutate(pobre = rec(pobre,
                     rec = '8=NA; 9=NA; else=copy'))

df_bicen19 <- df_bicen19 %>% 
  mutate(media = as.numeric(s03_2)) %>% 
  mutate(media = rec(media,
                     rec = '8=NA; 9=NA; else=copy'))

# Tabla para graficar

tabla_bicen <- melt(df_bicen19, id.vars = c('pobre', 'media'), measure.vars = 'pond_se')
names(tabla_bicen) <- c('pobre', 'media', 'variable', 'personas')

# Gráfico

gg_contorno <- ggplot(tabla_bicen, aes(x = pobre, y = media, z = personas)) +
  geom_contour(aes(colour = after_stat(level)), binwidth = 0.1, show.legend = FALSE) +
  labs(title = "Relación de expectativas",
       subtitle = "¿Cuál cree Ud. que es la probabilidad o chance que tiene en este país…? \n(1 es muy baja, y 5 muy alta)",
       x = "Un pobre de salir de la pobreza",
       y = "Una persona de clase media de llegar a tener \nuna muy buena situación económica",
       caption = "n = 2047 \nSe omiten categorías No sabe y No responde \nEncuesta Bicentenario 2019, DESU") +
  theme_minimal() +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=11),
        plot.caption = element_text(size = 10),
        plot.title = element_text(size = 13, face = "bold"),
        plot.subtitle = element_text(size = 12))


ggsave('outputs/08-contorno.png',
       width = 6,
       height = 5,
       scale = 3,
       units = 'cm')