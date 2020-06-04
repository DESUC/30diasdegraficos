#Censo de Población y Vivienda 2017.

# Paquetes ----------------------------------------------------------------

library(janitor)
library(sjlabelled)
library(sjmisc)
library(desuctools)
library(tidyverse, warn.conflicts = FALSE)
library(circlize)

# Base ----------------------------------------------------------------

base <- readRDS('inputs/28-censo.rds')

# Generación de insumo ------------------------------------------------

censo <- censo %>% 
  mutate(nacimiento_aux1 = ifelse(P12 == 1, COMUNA, NA),#Variables auxiliares para determinación de comunas de residencia y nacimiento 
         nacimiento_aux2 = ifelse(P12 == 2, P12COMUNA, NA),
         nacimiento_aux3 = ifelse(P12 %in% c(3:8), 17000, NA), #Otros países quedaron momentáneamente con este código
         comuna_nacimiento = coalesce(nacimiento_aux1,nacimiento_aux2,nacimiento_aux3),
         residencia_aux1 = ifelse(P10 %in% (1:2), COMUNA, NA),
         residencia_aux2 = ifelse(P10 == 3, P10COMUNA, NA),
         residencia_aux3 = ifelse(P10 == 4, 17000, NA),
         comuna_residencia = coalesce(residencia_aux1,residencia_aux2,residencia_aux3))

censo <- censo %>% 
  select(REGION18, comuna_nacimiento, comuna_residencia) %>% 
  transmute(region_nacimiento = floor(comuna_nacimiento/1000),#Para obtener las regiones según comunas
            region_residencia = floor(comuna_residencia/1000),
            prov_nacimiento = floor(comuna_nacimiento/100),#Esto se hace para obtener provincias para poder deducir comunas de la región 16
            prov_residencia = floor(comuna_residencia/100),
            region_nacimiento = ifelse(prov_nacimiento == 84, 16, region_nacimiento),
            region_residencia = ifelse(prov_residencia == 84, 16, region_residencia)) %>% 
  filter(region_nacimiento != 0) %>% #Se filtran casos que no tienen información válida
  filter(region_residencia != 0)

gg <- desuctools::tabla_vars_segmentos(.data = censo,
                                       .vars = vars(region_residencia),
                                       .segmentos = vars(region_nacimiento)) %>% 
  select(segmento_cat, pregunta_cat, casos) %>% 
  mutate(segmento_cat = paste('R', segmento_cat, sep = " "),
         pregunta_cat = paste('R', pregunta_cat, sep = " "),
         segmento_cat = ifelse(segmento_cat == 'R 17', 'Otro país', segmento_cat),
         pregunta_cat = ifelse(pregunta_cat == 'R 17', 'Otro país', pregunta_cat))

coul <- RColorBrewer::brewer.pal(11, "RdBu") 
coul <- colorRampPalette(coul)(17)

# Gráfico ------------------------------------------------

png("output/28-cuerdas-censo.png") 
set.seed(123)
chordDiagram(gg, big.gap = 30, grid.col = coul, col = coul, annotationTrack = "grid", preAllocateTracks = 1, directional = 1,  direction.type = "arrows",
             link.arr.type = "big.arrow")
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  circos.text(mean(xlim), ylim[1] + .1, sector.name, facing = "clockwise", niceFacing = TRUE, adj = c(0, 0.5), col = "darkslategrey")
}, bg.border = NA)
title('Migración entre regiones según Censo 2017')
dev.off()
