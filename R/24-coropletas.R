# Se requiere abrir la base de datos de Nacimientos de INE 
# Disponible en sitio: https://www.ine.cl, además de los marcos muestrales de INE a partir del Censo de 2017. No se disponibilizan por precaución.

# Paquetes ----------------------------------------------------------------

library(sf)
library(tidyverse)
library(gganimate)
library(desuctools)

# Base de datos -----------------------------------------------------------

# Cartografía comunal del Gran Santiago
sf_comunas_gs <- readRDS('inputs/24-sf_comunas_gs.rds')

# Embarazo adolescente

# Base de datos con información por comuna y año respecto de la cantidad de nacimientos registrados
# según la edad de la madre `ea_madre`.
ea_anos_comuna <- readRDS('inputs/24-nacimientos_comunas_anos.rds')


# Cantidad de nacimientos por comuna y por años según si son Embarazo Adolecente (EA) o no
ea_anos_comuna <- ea_anos_comuna %>% 
  add_count(comuna, ano_nacim, wt = n, name = 'total') %>%
  mutate(prop_ea = 1 - n/total) %>% 
  filter(ea_madre == 'no EA') %>% 
  rename(noEA = n)

# Unir cartografía con datos de embarazo adolescente.
ea_anos_comuna_gs <- inner_join(ea_anos_comuna,
                                sf_comunas_gs,
                                by = 'comuna')

# Transformar data.frame a sf
ea_anos_comuna_gs <- sf::st_as_sf(ea_anos_comuna_gs)

ea_anos_comuna_gs <- ea_anos_comuna_gs %>% 
  mutate(nom_comuna = str_wrap(str_to_lower(nom_comuna), width = 8))

st_bbox(ea_anos_comuna_gs)

gg_ea_gran_santiago <- ea_anos_comuna_gs %>% 
  ggplot(aes(fill = prop_ea)) +
  geom_sf() +
  geom_sf_text(aes(label = nom_comuna), 
               size = rel(2.5),
               alpha = .9, colour = 'gray90') + 
  coord_sf(xlim = c(-70.8, -70.5), 
           ylim = c(-33.65, -33.32),
           expand = FALSE) + 
  scale_fill_viridis_c(name = 'Proporción\nembarazo\nadolescente',
                       direction = -1,
                       labels = function(x) scales::percent(x, accuracy = 2)) +
  labs(subtitle = 'Nacimientos de menores de 19 años partido por nacimientos totales',
       caption = 'DESUC, a partir de datos INE') +
  theme_minimal() + 
  theme(axis.text = element_blank(),
        axis.title = element_blank())

gg_ea_gran_santiago

gg_ea_gran_santiago_anim <- gg_ea_gran_santiago +
  transition_states(ano_nacim,
                    transition_length = 1,
                    state_length = 3) +
  labs(title = 'Proporción de embarazo adolescente según año. \nAño {closest_state} Gran Santiago')

animate(
  gg_ea_gran_santiago_anim,
  start_pause = 2,
  end_pause = 10
)

anim_save('outputs/24-coropletas.mp4')
