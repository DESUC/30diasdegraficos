#Casen 2017.

# Paquetes ----------------------------------------------------------------

library(janitor)
library(sjlabelled)
library(sjmisc)
library(ggplot2)
library(desuctools)

# base <- haven::read_sav('Casen 2017.sav') %>%
#   clean_names() %>%
#   select(folio, expr, v13, v39f, qaut, dau)
# 
# base %>%
#   saveRDS(file.path('inputs/30-nightingale-casen2017.rds'))

base <- readRDS ('inputs/30-nightingale-casen2017.rds')

base$total=1

# Recodificaciones  -------------------------------------------------------
base <- base %>% 
  mutate(v13_r = case_when(v13 == 1 ~ 1,
                           v13 == 3 ~ 1,
                           v13 == 2 ~ 2,
                           v13 == 4 ~ 2,
                           v13 == 5 ~ 3,
                           v13 == 6 ~ 4,
                           v13 == 7 ~ 5,
                           v13 == 8 ~ 5,
                           v13 == 9 ~ 6,
                           v13 == 10 ~ 6,
                           v13 == 11 ~ 6,
                          TRUE ~ NA_real_),
         v13_r = haven::labelled(v13_r,
                                labels = c('Propia o compartida \npagada' = 1,
                                           'Propia o compartida \npagándose' = 2,
                                           'Arrendada con contrato' = 3,
                                           'Arrendada sin contrato' = 4,
                                           'Cedida \n(por trabajo o por familiar)' = 5,
                                           'Usufructo/\nOcupación o Posesión irregular' = 6),
                                label = 'Situación bajo la cual su hogar ocupa la vivienda'))

# Gráfico -------------------------------------------------------

tab <- base %>% 
  tabla_vars_segmentos(.vars = vars(dau),
                       .segmentos = vars(v13_r),
                       miss = NA,
                       .wt = expr) %>% 
  filter(segmento_cat != 'No sabe/no responde',
         !is.na(segmento_cat),
         !is.na(pregunta_cat))

gg_polar <- ggplot(tab, aes(x = pregunta_cat, y = prop,  fill = as.factor(segmento_cat))) + 
  geom_bar(stat = "identity", position = "identity", width = 1) +
  coord_polar() +
  # geom_text(aes(label = scales::percent(prop)), position = position_stack(vjust = 0.5)) +
  scale_fill_brewer(palette = "Spectral") +
  theme_minimal() +
  theme(plot.title = element_text(size = rel(1.1)),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        legend.title = element_blank()) +
  labs(title = 'Situación bajo la cual su hogar ocupa la vivienda, según decil',
       caption = "Casen 2017")

gg_polar
