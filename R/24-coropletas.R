
# Paquetes ----------------------------------------------------------------

library(sf)
library(desuctools)

# Base de datos -----------------------------------------------------------

df_comunas <- read_sf('inputs/COMUNA_C17.shp') %>% 
  mutate_at(vars(region, comuna, provincia), as.integer)

nacimientos <- readRDS('inputs/01-df_nacimientos.rds')


# Embarazo adolescente


emb_adolecente <- function(edad){
  
  niveles <- c('14 años o menos', '15 o 16 años', '17 o 19 años', 'no EA')
  
  case_when(edad %in%  1:14          ~ '14 años o menos',
            edad %in% 15:16          ~ '15 o 16 años',
            edad %in% 17:19          ~ '17 o 19 años',
            is.na(edad) | edad == 99 ~ NA_character_,
            TRUE                     ~ 'no EA') %>% 
    as_factor() %>% 
    fct_relevel(niveles)
}

nacimientos <- nacimientos %>% 
  mutate(edad_padre = coalesce(edad_padre, edad_p),
         edad_madre = coalesce(edad_madre, edad_m),
         ea_padre = emb_adolecente(edad_padre),
         ea_madre = emb_adolecente(edad_madre))

nacimientos %>% 
  skimr::skim(edad_padre, edad_madre, ea_padre, ea_madre)

#eliminar missing
sum(nacimientos$edad_madre > 60, na.rm = TRUE)



nacimientos <- nacimientos %>% 
  filter(edad_madre < 60)

# Gráfico -----------------------------------------------------------------

ea_anos_comuna <- nacimientos %>% 
  filter(ano_nacim > 2000 & comuna != 0) %>% 
  count(ano_nacim, ea_madre, comuna) %>% 
  group_by(comuna, ano_nacim) %>% 
  mutate(total = sum(n),
         prop_ea = 1-n/total) %>% 
  filter(ea_madre == 'no EA') %>% 
  select(-ea_madre) %>% 
  rename(noEA = n) %>% 
  arrange(comuna)

comunas_gran_santiago <- regiones_y_comunas %>% 
  filter(gran_santiago) %>% 
  pull(comuna)

ea_anos_comuna_gs <- ea_anos_comuna %>% 
  filter(comuna %in% comunas_gran_santiago) %>% 
  left_join(df_comunas, 
            by = 'comuna')

gg_ea_gran_santiago <- ea_anos_comuna_gs %>% 
  # filter(ano_nacim == 2017) %>%
  ggplot(aes(fill = prop_ea)) +
  geom_sf() +
  scale_fill_viridis_c(name = 'Proporción\nembarazo\nadolescente',
                       labels = function(x) scales::percent(x, accuracy = 2)) +
  labs(subtitle = 'Nacimientos de menores de 19 años partido por nacimientos totales',
       caption = 'DESUC, a partir de datos INE')

gg_ea_gran_santiago_anim <- gg_ea_gran_santiago +
  transition_states(ano_nacim,
                    transition_length = 1,
                    state_length = 3) +
  labs(title = 'Proporción de embarazo adolescente según año. \nAño {closest_state}')

animate(
  gg_ea_gran_santiago_anim
)
