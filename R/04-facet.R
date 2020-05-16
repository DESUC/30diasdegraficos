
# Se requiere abrir la base de datos de la Encuesta de Convivencia Barrial e Interculturalidad 2019. 
# Disponible en: http://cead.spd.gov.cl/estudios-y-encuestas/

library(tidyverse)

# devtools::install_github('caayala/desuctools')
library(desuctools)

if(!exists('df_migrantes')){
  temp_sav <- tempfile(fileext = '.zip')
  
  download.file(url = 'http://cead.spd.gov.cl/?wpdmpro=base-de-datos-encuesta-de-convivencia-barrial-e-interculturalidad-2019&wpdmdl=3033',
                destfile = temp_sav)
  
  df_migrantes <- haven::read_sav(temp_sav)
}

### Satisfacción con la policía

#### Tabla general

policias <- c('I_6_P75', 'I_7_P75', 'I_8_P75', 'I_9_P75', 'I_10_P75', 'I_11_P75')

# Corrección de etiqueta truncada en la pregunta `I_8_P75`
attr(df_migrantes[['I_8_P75']], 
     'label') <- '(La policía hace un buen trabajo respondiendo a quienes han sido víctimas de del delito en este barrio)'

preg <- desuctools::tabla_vars_segmentos(df_migrantes,
                                         .vars = vars(policias),
                                         .segmentos = vars(nacionalidad_seleccionado),
                                         .wt = rake_wbsp)

tabla <- preg %>% 
  mutate(pregunta_cat = fct_collapse(pregunta_cat,
                                     'De acuerdo' = c('De acuerdo', 'Muy de acuerdo'),
                                     'En desacuerdo' = c('En desacuerdo', 'Muy en desacuerdo'))) %>% 
  filter(pregunta_cat %in% c('De acuerdo', 'En desacuerdo')) %>% 
  group_by_at(vars(segmento_var:pregunta_cat)) %>% 
  summarise_at(vars(casos, prop), sum) %>% 
  ungroup() %>% 
  mutate(pregunta_lab = fct_reorder2(pregunta_lab, .x = prop, .y = pregunta_cat, 
                                     .fun = function(.x, .y) sum(if_else(.y == 'De acuerdo', .x, 0)),
                                     .desc = FALSE),
         pregunta_lab = fct_relabel(pregunta_lab, desuctools::str_entre_parentesis),
         pregunta_lab = fct_recode(pregunta_lab, 
                                   'La policía en Chile a veces abusan de su poder*' = 'La policía en Chile a veces abusan de su poder',
                                   'La policía no es capaz de mantener el orden en las calles y veredas del barrio*' = 'La policía no es capaz de mantener el orden en las calles y veredas del barrio'),
         prop = if_else(pregunta_cat == 'En desacuerdo', -prop, prop))

tabla_total <- tabla %>% 
  count(segmento_cat, pregunta_lab, pregunta_cat, wt = prop)


#### Gráfico general

gg_barras_general <- function(.data,
                              fill = segmento_cat,
                              lab_text = TRUE,
                              ylim = c(0, 1.1)){
  ggplot(.data, 
         aes(x = pregunta_lab, y = prop, fill = {{ fill }})) +
    geom_col(width = .6) + 
    coord_flip() +
    facet_grid(cols = vars(segmento_cat)) +
    theme_desuc +
    theme(axis.text.y  = element_text(size = rel(1)),
          axis.text.x  = element_text(size = rel(0.3))) + 
    scale_x_discrete(labels = label_wrap_gen(60)) +
    scale_y_continuous(limits = ylim,
                       labels = function(x) scales::percent(x, accuracy = 1))
}

theme_desuc <- list(geom_hline(yintercept = 0),
                    theme_minimal(base_size = 12),
                    theme(plot.caption = element_text(size = rel(.5)),
                          axis.text.x  = element_text(size = rel(1.0)),
                          axis.text.y  = element_text(size = rel(.5)),
                          axis.title.y = element_text(size = rel(.75)),
                          strip.text.x = element_text(size = rel(1.5)), 
                          strip.text.y = element_text(size = rel(1.25), 
                                                      angle = 0, hjust = 0,
                                                      color = 'gray11')))

tabla %>% 
  gg_barras_general(fill = pregunta_cat,
                    ylim = c(-1, 1)) +
  geom_text(data = tabla_total,
            aes(y = n + sign(n) * .025, 
                hjust = if_else(sign(n) == 1, 0, 1),
                label = round(abs(n) * 100, 0), 
                fill = NULL)) +
  facet_grid(cols = vars(segmento_cat)) +
  scale_fill_brewer('Grado de acuerdo', palette = 'Set1', direction = 1) +
  labs(title = 'Satisfacción con policias',
       subtitle = 'Qué tan de acuerdo está con las siguientes afirmaciones\n* señala atributos negativos respecto de las policias',
       y = 'Porcentaje de respuesta',
       caption = 'Encuesta de Convivencia Barrial 2019, DESUC') + 
  theme(legend.position = 'top',
        plot.title.position = 'plot')

ggsave('outputs/04-facet_gg_policias.png',
       width = 23,
       height = 13.7,
       units = 'cm')
