# Se requiere abrir la base de datos de la Encuesta de Satisfacción de usuarios del Ministerio de Hacienda.
# Disponible en: https://satisfaccion.gob.cl/datos-abiertos

library(tidyverse)
library(haven)
library(desuctools)
library(sjmisc)
library(ggridges)
library(sjlabelled)
library(viridis)


# Apertura de base

df_hacienda <- read.csv2('inputs/satisfaccion_20200518_1589774768.csv', header = TRUE, sep = ";")

# Revisión de datos

df_hacienda %>% 
  group_by(AÃ.o) %>% 
  frq(Institucion)

df_hacienda <- df_hacienda %>% 
  mutate(p1 = case_when(
    P1 == "1.0"	~	1L,
    P1 == "2.0"	~	2L,
    P1 == "3.0"	~	3L,
    P1 == "4.0"	~	4L,
    P1 == "5.0"	~	5L,
    P1 == "6.0"	~	6L,
    P1 == "7.0"	~	7L,
    P1 == "8.0"	~	8L,
    P1 == "9.0"	~	9L,
    TRUE ~ as.integer(NA)
  )) %>% 
  var_labels(p1 = 'Con una escala de 1 a 7, donde 1 es pésimo y 7 es excelente, ¿cómo evalúa en general a la institución?') %>% 
  set_labels(., p1, labels = c('Pésimo', '2', '3', '4', '5', '6',  'Excelente', 'No sabe', 'No responde')) 

df_hacienda <- df_hacienda %>% 
  mutate(pond = as.numeric(PONDERADOR))

df_hacienda <- df_hacienda %>% 
  mutate(inst = case_when(
    Institucion == "AN" ~	1L,
    Institucion == "CHILEATIENDE_PRESENCIAL" ~	2L,
    Institucion == "COMPIN" ~	3L,
    Institucion == "DT" ~	4L,
    Institucion == "FONASA" ~	5L,
    Institucion == "SII" ~	6L,
    Institucion == "SRCeI" ~	7L,
    Institucion == "SUSESO" ~	8L,
    Institucion == "TGR" ~	9L,
    TRUE ~ as.integer(NA)
  )) %>% 
  var_labels(inst = 'Institución') %>% 
  set_labels(., inst, labels = c('Archivo nacional', 'Chileatiende presencial', 'Compin', 'Dirección del trabajo', 'Fonasa', 'Servicio Impuestos Internos', 'Registro civil', 'Superintendencia de Seguridad Social', 'Tesorería general de la republica'))


# Tabla

df_hacienda <- df_hacienda %>% 
  filter(AÃ.o=="2018.0")

satisfaccion <- tabla_vars_segmentos(.data = df_hacienda,
                                     .vars = vars(p1),
                                     .segmentos = vars(inst),
                                     .wt = pond) %>% 
  filter(!pregunta_cat=="No sabe") %>% 
  filter(!pregunta_cat=="No responde") %>% 
  mutate(prop = round(prop*100))

# Gráfico

gg_rid <- ggplot(satisfaccion, aes(x = prop, y = pregunta_cat, fill = ..x..)) + 
  geom_density_ridges_gradient(alpha = 0.5, scale = 1, show.legend = FALSE, color = "grey") +
  scale_fill_viridis() +
  labs(title = "Distribución de la escala de satisfacción \nMedición de la Satisfacción Usuaria 2018 Ministerio de Hacienda",
       subtitle = "Con una escala de 1 a 7, donde 1 es pésimo y 7 es excelente, \n¿cómo evalúa en general a la institución?",
       x = "Proporción",
       y = "Categoría de respuesta",
       caption = "n válido = 26.838\nIncluye todas las instituciones evaluadas ese año\nDESUC") +
  theme_minimal()


ggsave('outputs/07-ridgeline.png',
       width = 6,
       height = 7,
       scale = 3,
       units = 'cm')