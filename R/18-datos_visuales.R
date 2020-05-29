#Se utilizan las bases de datos del Censo de 2017. 
#Este proceso formó parte de la construcción de la muestra de la 
#Encuesta de Convivencia Barrial e Interculturalidad 2019, realizada por DESUC para la 
#Subsecretaría de Prevención del Delito.

# Paquetes ----------------------------------------------------------------

library(haven)
library(readxl)
library(scales)
library(sf)
library(sampling)

library(sjlabelled)
library(sjmisc)
library(janitor)

library(ggpubr)
library(knitr)

library(tidyverse)

set.seed(1000)
umbral_zona_migrantes <- .1

# Funciones ---------------------------------------------------------------

#para graficar mapas

gg_manz_zonas <- function(.data, zonas, 
                          title = NULL, 
                          subtitle = NULL,
                          id = folio_man){
  id_quo <- enquo(id)
  
  has_id <- match(rlang::as_name(id_quo), names(.data), nomatch = FALSE)
  
  label_com <- str_to_title(zonas$nom_comuna[[1]])
  label_pos_x <- st_bbox(zonas$geometry)$xmin + 0.01
  label_pos_y <- st_bbox(zonas$geometry)$ymin + 0.01
  
  g <- ggplot() +
    geom_sf(data = zonas, alpha = 0, size = .75) +
    geom_sf(data = filter(.data, tasa_inmigrantes < umbral_zona_migrantes),
            fill = 'white',
            alpha = .5,
            size = 0.1) +
    geom_sf(data = filter(.data, tasa_inmigrantes >= umbral_zona_migrantes),
            aes(fill = barrio),
            alpha = .7,
            size = 0.1) +
    geom_sf_label(data = (zonas %>%
                            group_by(nom_comuna) %>%
                            summarise(geometry = mean(geometry))),
                  aes(label = nom_comuna),
                  size = 2.5) +
    ggspatial::annotation_north_arrow(location = "bl",
                                      which_north = "true",
                                      height = unit(0.5, 'cm'), width = unit(0.5, 'cm')) +
    labs(title = title,
         subtitle = subtitle) +
    scale_fill_discrete(guide = 'none') +
    theme(axis.title = element_blank(),
          axis.text = element_blank()) +
    theme_minimal()
  
  if(has_id){
    g <- g +
      geom_sf_text(data = .data,
                   aes(label = !!id_quo),
                   size = 0.3)
  }
  return(g)
}

gg_print <- function(gg_map, com_codigo, com_nombre, tipo = NULL, umbral = NULL, dir = 'outputs') {
  pdf(here::here(dir, str_glue('gg_{com_codigo}_{com_nombre}_{tipo}_{umbral %||% "sin_umbral"}.pdf')), 
      height = 5)
  print(gg_map)
  dev.off()
}


# Base de datos -----------------------------------------------------------

#base del censo 2017 con información de comunas, zonas y manzanas de interés y seleccionadas
c17_comuna_interes <- readRDS('inputs/18-censo_marco_interes_spd_migrantes.rds')


# Tablas ------------------------------------------------------------------

names(c17_comuna_interes)

  c17_comuna_interes %>% 
    select(manzanas_sel, zonas_a_seleccionar, manzanas_por_zona) %>% 
    reduce(rbind)

  c17_comuna_interes <- c17_comuna_interes %>% 
    mutate(gg_zonas = pmap(list(zonas, zonas, nom_comuna), 
                           ~ gg_manz_zonas(..1, ..2, 
                                           title = str_glue("{str_to_title(..3)} zonas con tasa sobre {umbral_zona_migrantes*100}%"))))

  c17_comuna_interes %>% {
    pwalk(list(.$gg_zonas, .$comuna, .$nom_comuna), 
          ~ gg_print(..1, ..2, ..3, 'zonas', umbral = umbral_zona_migrantes))
  }

# Mapa --------------------------------------------------------------------

  c17_santiago_interes <- c17_comuna_interes %>% 
    filter(region == 13) %>% 
    select(manzanas, zonas) %>% 
    summarise_all(~ list(reduce(., rbind)))
  
 
  c17_santiago_interes <- c17_santiago_interes %>% 
    mutate(gg_zonas = pmap(list(zonas, zonas, 'Zona Metropolitana'), 
                           ~ gg_manz_zonas(..1, ..2, 
                                           title = str_glue("{str_to_title(..3)} zonas con tasa sobre {umbral_zona_migrantes*100}%"))))
  
  ggsave(here::here('outputs', str_glue('gg_13100_zona_metropolitana_zonas_{umbral_zona_migrantes}.png')), 
         plot = c17_santiago_interes$gg_zonas[[1]],
         device = 'png',
         height = 5)