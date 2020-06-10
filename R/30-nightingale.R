#Casen 2017.

# Paquetes ----------------------------------------------------------------

library(janitor)
library(sjlabelled)
library(sjmisc)
library(ggplot2)
library(desuctools)
library(reshape2)

# base <- haven::read_sav('Casen 2017.sav') %>%
#   clean_names() %>% 
#   select(folio, expr, v13, dau)
# 
# base %>%
#   saveRDS(file.path('inputs/30-nightingale-casen2017.rds'))

base <- readRDS ('inputs/30-nightingale-casen2017.rds')

tab <- base %>% 
  tabla_vars_segmentos(.vars = vars(v13),
                       .segmentos = vars(dau),
                       miss = NA, 0,
                       .wt = expr)

gg_polar <- ggplot(tab, aes(x = pregunta_cat, y = prop,  fill = as.factor(segmento_cat))) + 
  geom_bar(stat = "identity", position = "identity", width = 1) +
  coord_polar()
gg_polar
