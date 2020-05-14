# Para este script se usan los datos de la Encuesta Nacional de Polivictimización en Niños, Niñas y Adolescentes (2017), 
# disponible en: <http://cead.spd.gov.cl/estudios-y-encuestas/>

library(haven)
library(tidyverse)

# Base de datos

if(!exists('base_val')){
  temp_sav <- tempfile(fileext = '.zip')
  
  download.file(url = 'http://cead.spd.gov.cl/?wpdmpro=base-de-datos-1ra-encuesta-nacional-sobre-polivictimizacion-en-ninas-ninos-y-adolescentes-2017&wpdmdl=2507',
                destfile = temp_sav)
  
  base_val <- haven::read_sav(temp_sav)
}

## Victimizacion

### Prevalencia vida y año

vict_vida <- base_val %>% 
  select(matches('^P[A-F]_\\d$'), wgt_alu) %>%
  gather('variable', 'respuesta', -wgt_alu) %>%
  group_by(variable) %>% 
  summarise(respuesta_pond = 
              sum(if_else(respuesta == 1, wgt_alu, 0), na.rm = TRUE)/
              sum(if_else(!is.na(respuesta), wgt_alu, 0), na.rm = TRUE),
            respuesta      = 
              sum(if_else(respuesta == 1,       1, 0), na.rm = TRUE)/
              length(na.omit(respuesta)),
            tipo = 'Vida')

vict_año <- base_val %>% 
  select(matches('^P[A-F]_\\d_1$'), wgt_alu) %>%
  gather('variable', 'respuesta', -wgt_alu) %>%
  group_by(variable) %>% 
  summarise(respuesta_pond = 
              sum(if_else(respuesta == 1, wgt_alu, 0), na.rm = TRUE)/
              sum(if_else(!is.na(respuesta), wgt_alu, 0), na.rm = TRUE),
            respuesta      = 
              sum(if_else(respuesta == 1,       1, 0), na.rm = TRUE)/
              length(na.omit(respuesta)),
            tipo = 'Último año') %>% 
  ungroup() %>% 
  mutate(variable = str_extract(variable, pattern = 'P[A-F]_\\d'))

# Unir información de prevalencia vida y año.
vict_vida_año <- bind_rows(vict_vida, 
                           vict_año)

# Agregar etiquetas para graficos

source('R/03-puntos_polivictimizacion_-_etiquetas.R', 
       encoding = 'UTF8')

vict_label <- b_etiquetas_corto$label
names(vict_label) <- b_etiquetas_corto$variable

vict_modulo <- b_etiquetas_corto$modulo
names(vict_modulo) <- b_etiquetas_corto$variable


vict_vida_año <- vict_vida_año %>% 
  mutate(etiqueta = vict_label[variable],
         modulo   = vict_modulo[variable])

# Oreden de victimiaciones según prevalencia
vict_vida_año <- vict_vida_año %>% 
  mutate(etiqueta = fct_reorder(etiqueta, respuesta_pond))

#### Gráfico 

grafico_modulo <- function(datos, 
                           filtro_variables, 
                           color_grafico){
  
  datos %>% 
    filter(str_detect(variable, filtro_variables)) %>% 
    mutate(etiqueta = fct_relabel(etiqueta, str_wrap, 40)) %>% 
    ggplot(aes(x = etiqueta, 
               y = respuesta_pond * 100, 
               shape = tipo)) +
    geom_point(size = 5,
               color = color_grafico, 
               fill = color_grafico) +
    geom_linerange(aes(ymin = rep(0, length(after_stat(x))), 
                       ymax = after_stat(y)),
                   color = color_grafico, 
                   size = 3, alpha = .4) +
    geom_text(aes(label = round(after_stat(y))), 
              color = 'black', 
              fontface = 'plain',
              size = 4) +
    coord_flip() + 
    scale_y_continuous(limits = c(0,60), breaks = seq(0, 100, by = 10)) +
    labs(title = 'Prevalencia victimizaciones vida y últimos 12 meses',
         x = '', 
         y = '% respuesta Sí',
         caption = 'Fuente: Encuesta Polivictimización 2017') +
    theme_minimal() + 
    theme(legend.position = "top",
          axis.text.y = element_text(size = rel(1.25)),
          plot.title.position = 'plot') +
    guides(color = 'none')
}

#### Modulo A: Delitos comunes

gg_vict_vida_año_A_wgt_alu <- grafico_modulo(vict_vida_año, 'PA.*', '#F8766D') +
  labs(x = 'Delitos comunes')

gg_vict_vida_año_A_wgt_alu

ggsave('outputs/03-puntos_modulo_A_wgt_alu.png',
       width = 23,
       height = 13.7,
       units = 'cm')

