# Se requiere abrir la base de datos de la Encuesta de Comportamiento Lector ECL 2014.
# http://plandelectura.gob.cl/recursos/encuesta-de-comportamiento-lector-2014/

library(haven)
library(janitor)
library(tidyverse)
library(waffle)
library(desuctools)

base <- readRDS('inputs/16-waffle-comportamiento_lector2014_30diasdegraficos_2020.rds')

base$total=1

# Recodificaciones

base <- base %>%              
  mutate(b1a_r = case_when(b1a == 1 ~ 1,
                           b1a == 2 ~ 0,
                          TRUE ~ NA_real_),
         b1a_r = haven::labelled(b1a_r,
                                labels = c('Sí - impreso' = 1,
                                           'No' = 0),
                                label = 'En los últimos 12 meses, ¿ha leído libros…? (impreso)'),
         b1b_r = case_when(b1b == 1 ~ 10,
                           b1b == 2 ~ 0,
                           TRUE ~ NA_real_),
         b1b_r = haven::labelled(b1b_r,
                                 labels = c('Sí  - digital' = 10,
                                            'No' = 0),
                                 label = 'En los últimos 12 meses, ¿ha leído libros…? (digital)'),
         c1_r = case_when(c1 == 1 ~ 1,
                          c1 == 2 ~ 0,
                           TRUE ~ NA_real_),
         c1_r = haven::labelled(c1_r,
                                 labels = c('Sí - impreso' = 1,
                                            'No' = 0),
                                 label = 'En los últimos 12 meses, ¿ha leído diarios y/o periódicos…? (impreso)'),
         d1_r = case_when(d1 == 1 ~ 10,
                          d1 == 2 ~ 0,
                           TRUE ~ NA_real_),
         d1_r = haven::labelled(d1_r,
                                 labels = c('Sí  - digital' = 10,
                                            'No' = 0),
                                 label = 'En los últimos 12 meses, ¿ha leído diarios y/o periódicos…? (digital)'),
         e1a_r = case_when(e1a == 1 ~ 1,
                           e1a == 2 ~ 0,
                           TRUE ~ NA_real_),
         e1a_r = haven::labelled(e1a_r,
                                 labels = c('Sí - impreso' = 1,
                                            'No' = 0),
                                 label = 'En los últimos 12 meses, ¿ha leído revistas…? (impreso)'),
         e1b_r = case_when(e1b == 1 ~ 10,
                           e1b == 2 ~ 0,
                           TRUE ~ NA_real_),
         e1b_r = haven::labelled(e1b_r,
                                 labels = c('Sí  - digital' = 10,
                                            'No' = 0),
                                 label = 'En los últimos 12 meses, ¿ha leído revistas…? (digital)'),
         f1a_r = case_when(f1a == 1 ~ 1,
                           f1a == 2 ~ 0,
                           TRUE ~ NA_real_),
         f1a_r = haven::labelled(f1a_r,
                                 labels = c('Sí - impreso' = 1,
                                            'No' = 0),
                                 label = 'En los últimos 12 meses, ¿ha leído Historietas y/o cómics…? (impreso)'),
         f1b_r = case_when(f1b == 1 ~ 10,
                           f1b == 2 ~ 0,
                           TRUE ~ NA_real_),
         f1b_r = haven::labelled(f1b_r,
                                 labels = c('Sí - digital' = 10,
                                            'No' = 0),
                                 label = 'En los últimos 12 meses, ¿ha leído Historietas y/o cómics…? (digital)'))

# Sumar lectura en formato impreso + digital
base <- base %>% 
  mutate(libros = b1a_r + b1b_r,
         diarios = c1_r + d1_r,
         revistas = e1a_r + e1b_r,
         comic = f1a_r + f1b_r)

# Obtener proporciones ponderadas  

tab <- base %>% 
  tabla_vars_segmentos(.vars = vars(libros, diarios, revistas, comic),
                       .segmentos = vars(total),
                       miss = NA, 0,
                       .wt = fexp)

  # Generar vectores con las proporciones obtenidas en la tabla
  libros_prop = (c('Sí, solo impreso (44%)' = 44, 
                   'Sí, impreso y/o digital (27%)' = 27, 
                   'No (29%)' = 29))
  
  diarios_prop = (c('Sí, solo impreso (43%)' = 43, 
                    'Sí, impreso y/o digital (36%)' = 36, 
                    'No (21%)' = 21))
  
  revistas_prop = (c('Sí, solo impreso (38%)' = 38, 
                     'Sí, impresa y/o digital (17%)' = 17, 
                     'No (45%)' = 45))
  
  comic_prop = (c('Sí, solo impreso (21%)' = 21, 
                  'Sí, impreso y/o digital (10%)' = 10, 
                  'No (69%)' = 69))
  
  # Gráficos
  gg_waffle1 <- waffle(libros_prop, rows=5, colors = c("#0863B7", "steelblue2", "grey60"),
                      title = 'En los últimos 12 meses, ¿ha leído...?') +
    labs(subtitle = 'Libros') +
    theme(plot.title = element_text(size = rel(1), face = "bold"),
          plot.subtitle = element_text(size = rel(0.8)))
    
  gg_waffle2 <- waffle(diarios_prop, rows=5, colors = c("#0863B7", "steelblue2", "grey60")) +
    labs(subtitle = 'Diarios') +
    theme(plot.title = element_text(size = rel(0.9)),
          plot.subtitle = element_text(size = rel(0.8)))
  
  gg_waffle3 <- waffle(revistas_prop, rows=5, colors = c("#0863B7", "steelblue2", "grey60")) +
    labs(subtitle = 'Revistas') +
    theme(plot.title = element_text(size = rel(0.9)),
          plot.subtitle = element_text(size = rel(0.8)))
  
  gg_waffle4 <- waffle(comic_prop, rows=5, colors = c("#0863B7", "steelblue2", "grey60")) +
    labs(subtitle = 'Cómics',
         caption = 'Segunda Encuesta de Comportamiento Lector\n Datos ponderados, n válido = 6.990 \n DESUC, 2014') +
    theme(plot.title = element_text(size = rel(0.9)),
          plot.subtitle = element_text(size = rel(0.8)),
          plot.caption= element_text(size = rel(0.6)))
  
  # Unir y guardar
  png("outputs/16-waffle.png")
  
  iron(gg_waffle1,
       gg_waffle2,
       gg_waffle3,
       gg_waffle4)
  invisible(dev.off())
  
