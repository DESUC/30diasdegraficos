# Se requiere abrir la base de datos de la Encuesta Bicentenario 2019. 
# Disponible en: https://encuestabicentenario.uc.cl

library(haven)
library(tidyverse)
library(janitor)
library(desuctools)
library(scales)

# Función

f_df_respuestas_3 <- function(.data, 
                              .vars,
                              .segmentos,
                              labels_3,
                              reducir_5a3 = TRUE,
                              missing = 'Ns/Nr'){
  
  # Genera data.frame con respuestas recodificadas en tres niveles.
  
  if(reducir_5a3){
    .data <- .data %>% 
      mutate_at(vars({{ .vars }}), desuctools::rec_cat_5a3, labels = labels_3)
  }
  
  df <- .data %>% 
    desuctools::tabla_vars_segmentos(.vars = vars({{ .vars }}), 
                                     .segmentos = .segmentos, 
                                     .wt = pond_se,
                                     miss = missing)
  
  # Orden de factor de variables para que queden ordenadas de lo más positivo hacia abajo.
  df %>%
    mutate_at(vars(starts_with('prop')),
              ~if_else(pregunta_cat == names(labels_3[1]), -., .)) %>%
    mutate(pregunta_lab = fct_reorder_cat(pregunta_lab, .cat = pregunta_cat, .val = prop, 
                                          cat_orden = names(labels_3[3]),
                                          .desc = FALSE))
}

## Labels
labels_temor   <- c('Poco y nada' = 1, 'Algo' = 2, 'Mucho y bastante' = 3)

# Apertura de base

base <- readRDS ('inputs/12-lollipop-df_bicen_19_30diasdegraficos_2020.rds')

# Armar base de datos
## Invertir categorías: más temor = número.

base$	t01_1	<- rev_niveles(base$		t01_1	, rango_inv = c(1, 5))
base$	t01_2	<- rev_niveles(base$		t01_2	, rango_inv = c(1, 5))
base$	t01_3	<- rev_niveles(base$		t01_3	, rango_inv = c(1, 5))
base$	t02_1	<- rev_niveles(base$		t02_1	, rango_inv = c(1, 5))
base$	t02_2	<- rev_niveles(base$		t02_2	, rango_inv = c(1, 5))
base$	t03_1	<- rev_niveles(base$		t03_1	, rango_inv = c(1, 5))
base$	t03_2	<- rev_niveles(base$		t03_2	, rango_inv = c(1, 5))
base$	t03_3	<- rev_niveles(base$		t03_3	, rango_inv = c(1, 5))
base$	t03_4	<- rev_niveles(base$		t03_4	, rango_inv = c(1, 5))
base$	t04_1	<- rev_niveles(base$		t04_1	, rango_inv = c(1, 5))
base$	t04_2	<- rev_niveles(base$		t04_2	, rango_inv = c(1, 5))
base$	t04_3	<- rev_niveles(base$		t04_3	, rango_inv = c(1, 5))

## Pasar 5 categorías a 3
df_temores <- f_df_respuestas_3(base,
                                  .vars = 't01_1':'t04_3',
                                  .segmentos = vars(d07),
                                  labels_3 = labels_temor)

## Seleccionar categoría de interés
df_temores_mucho <- df_temores %>% 
  filter(pregunta_cat %in% c('Mucho y bastante')) %>% 
  group_by_at(vars(segmento_cat:pregunta_lab)) %>% 
  summarise_if(is.numeric, sum)

## Limpiar la etiqueta de la pregunta
df_temores_mucho$pregunta_lab <- fct_relabel(df_temores_mucho$pregunta_lab, str_entre_parentesis)

# Pivotear base
df_temores_mucho_pivot <- df_temores_mucho %>%
   pivot_wider(id_cols = c(segmento_cat:pregunta_lab), names_from = segmento_cat, values_from = prop_val)

# Gráfico
gg_temores <-   ggplot(data = df_temores_mucho_pivot) +
  geom_segment(aes(x=Hombre, xend=Mujer, y=pregunta_lab, yend=pregunta_lab), color = 'grey50', size = 1.2) +
  geom_point(aes(x=Mujer, y=pregunta_lab, color = 'Mujer'), size = 4) +
  geom_point(aes(x=Hombre, y=pregunta_lab, color = 'Hombre'), size = 4) + 
  scale_x_continuous('', limits = c(0,1), labels = function(x) scales::percent(x, accuracy = 1)) +
  scale_y_discrete(labels = wrap_format(30)) +
  scale_shape_manual(values=c(19,20)) +
  scale_colour_manual(values=c("#0863B7", "#F3711D")) +
  theme_minimal() +
  theme(legend.title = element_blank(),
        plot.title = element_text(size = rel(1)),
        plot.subtitle = element_text(size = rel(0.8))) +
  labs(title = "Sección Temores: Según género",
       subtitle = "¿Cuánto temor le producen las siguientes situaciones? \n (% Mucho y bastante)",
       caption = "Datos ponderados, n = 2047 \nEncuesta Bicentenario 2019, DESUC ",
       y ='')
  
ggsave('outputs/12-lollipop_temores.png',
       width = 5,
       height = 5,
       scale = 3,
       units = 'cm')

