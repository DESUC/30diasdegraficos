#Datos corresponden a Encuesta de Polivictimización en NNA 2017, realizada para Subsecretaría de Prevención del Delito.
#La base total está disponible públicamente en <http://cead.spd.gov.cl/estudios-y-encuestas/>


# Paquetes ----------------------------------------------------------------

library(tidyverse)
library(sjPlot)
library(sjmisc)
library(sjlabelled)

# Base de datos -----------------------------------------------------------

vict_vida_año_edad_cat6 <- readRDS("inputs/22-vict_vida_año_edad_cat6.RDS")

# Gráfico -----------------------------------------------------------------


grafico <- vict_vida_año_edad_cat6 %>% 
  filter(!is.na(edad_cat6),
         tipo == 'Último año')

etiqueta <- grafico %>% 
  filter(edad_cat6 == '17 años o más') %>% 
  arrange(desc(respuesta_pond)) %>% 
  head(5) %>% 
  bind_rows(.,
            grafico %>% filter(variable == 'PD_1', edad_cat6 == '17 años o más'))

etiqueta <- etiqueta %>% 
  transmute(x_pos = 6,
            y_pos = respuesta_pond,
            label = etiqueta)

gg_vict_año_edad_wgt_alu <- ggplot(grafico, aes(x = edad_cat6, y = respuesta_pond * 100)) +
  geom_line(aes(color = modulo, group = etiqueta)) +
  ggrepel::geom_text_repel(data = etiqueta, 
                           aes(x = x_pos, y = y_pos * 100, label = str_wrap(label, 25)),
                           nudge_x = 1.7, size = 3, force = 2, 
                           segment.size=.5,
                           point.padding = unit(.3, 'line'),
                           direction = 'both',
                           hjust = .5) +
  scale_x_discrete(labels = function(x) str_wrap(x, 8)) + 
  scale_y_continuous(limits = c(0, 60),
                     breaks = seq(0, 80, 20)) + 
  coord_cartesian(xlim = c(1.2 , 9)) +
  labs(title = 'Prevalencia victimizaciones últimos 12 meses por edad',
       x = 'Edad', y = 'Prevalencia victimización año',
       caption = 'Encuesta Polivictimización en NNA 2017\nDESUC-SPD') +
  scale_color_discrete(name = "Módulo", 
                       labels = c("Delitos comunes", 
                                  "Cuidadores", 
                                  "Pares", "Sexuales", 
                                  "Indirectas", 
                                  'Digitales'))

ggsave(here::here('03-Polivictimizacion_-_Descriptivo_output', 'gg_vict_año_edad_wgt_alu.png'), 
       plot = gg_vict_año_edad_wgt_alu,
       height = 5)

gg_vict_año_edad_wgt_alu


ggsave('outputs/22-anotaciones.png',
       width = 6,
       height = 5,
       scale = 3,
       units = 'cm')