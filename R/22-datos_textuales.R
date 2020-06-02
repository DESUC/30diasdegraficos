#Encuesta Nacional de Cultura Científica 2015.

# Paquetes ----------------------------------------------------------------

library(haven)
library(janitor)
library(tidyverse)
library(desuctools)
library(dplyr)
library(sjmisc)
library(wordcloud2)
library(RColorBrewer)
library(webshot)


# Base ----------------------------------------------------------------

base <- readRDS('inputs/22-texto-conicyt.rds')

# Generación de insumo ------------------------------------------------

gg <- desuctools::tabla_vars_segmentos(.data = base,
                                       .vars = vars(P7_1_R),
                                       .segmentos = vars(R_EDAD),
                                       .wt = FEXP) %>% 
  select(segmento_cat, pregunta_cat, casos)

gg_15_29 <- gg %>% 
  filter(segmento_cat == '15 a 29 años') %>% 
  select(-(segmento_cat))

gg_30_44 <- gg %>% 
  filter(segmento_cat == '30 a 44 años') %>% 
  select(-(segmento_cat))

gg_45_59 <- gg %>% 
  filter(segmento_cat == '45 a 59 años') %>% 
  select(-(segmento_cat))

gg_60_mas <- gg %>% 
  filter(segmento_cat == '60 y más') %>% 
  select(-(segmento_cat))

paleta_colores <- 'RdBu'
colores_graficos <- RColorBrewer::brewer.pal(9, paleta_colores)

# Gráficos ------------------------------------------------

set.seed(123)
gg_15_29 <- wordcloud2(data=gg_15_29, size = 0.7, color=colores_graficos, shape = 'circle', 
                       fontFamily = 'sans', backgroundColor = "white",
                       minRotation = 1, maxRotation = 0.1, rotateRatio = 0.1)

saveWidget(gg_15_29,"1.html",selfcontained = F)
webshot::webshot("1.html","22-texto-15_29.png",vwidth = 400, vheight = 323, delay =10)

set.seed(123)
gg_30_44 <- wordcloud2(data=gg_30_44, size = 0.7, color=colores_graficos, shape = 'circle', 
                       fontFamily = 'sans', backgroundColor = "white",
                       minRotation = 1, maxRotation = 0.1, rotateRatio = 0.1)

saveWidget(gg_30_44,"1.html",selfcontained = F)
webshot::webshot("1.html","22-texto-30_44.png",vwidth = 400, vheight = 323, delay =10)

set.seed(123)
gg_45_59 <- wordcloud2(data=gg_45_59, size = 0.7, color=colores_graficos, shape = 'circle', 
                       fontFamily = 'sans', backgroundColor = "white",
                       minRotation = 1, maxRotation = 0.1, rotateRatio = 0.1)

saveWidget(gg_45_59,"1.html",selfcontained = F)
webshot::webshot("1.html","22-texto-45_59.png",vwidth = 400, vheight = 323, delay =10)

set.seed(123)
gg_60_mas <- wordcloud2(data=gg_60_mas, size = 0.7, color=colores_graficos, shape = 'circle', 
                       fontFamily = 'sans', backgroundColor = "white",
                       minRotation = 1, maxRotation = 0.1, rotateRatio = 0.1)

saveWidget(gg_60_mas,"1.html",selfcontained = F)
webshot::webshot("1.html","22-texto-60_mas.png",vwidth = 400, vheight = 323, delay =10)
