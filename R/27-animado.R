#Base de datos previamente preparada desde datos disponibles públicamente en INE.


# Paquetes ----------------------------------------------------------------

library(lubridate)
library(readxl)
library(janitor)
library(knitr)
library(gganimate)
library(tidyverse)

# Base de datos ------------------------------------------------------------

nacimientos <- readRDS('inputs/01-df_nacimientos.rds')


# Tabla de datos ----------------------------------------------------------

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

#eliminar datos missing
sum(nacimientos$edad_madre > 60, na.rm = TRUE)

nacimientos <- nacimientos %>% 
  filter(edad_madre < 60)


# Gráfico -----------------------------------------------------------------


edad_madre_mean <- nacimientos %>% 
  filter(!is.na(ano_nacim)) %>% 
  group_by(ano_nacim) %>% 
  summarise(edad_mean = mean(edad_madre))

gg_edad_madre <- nacimientos %>% 
  filter(!is.na(ano_nacim)) %>% 
  ggplot(aes(x = edad_madre, fill = ea_madre)) + 
  geom_bar() +
  geom_vline(data = edad_madre_mean, aes(xintercept = edad_mean,
                                         group = ano_nacim), 
             alpha = .5) +
  geom_text(data = edad_madre_mean, aes(x = edad_mean, 
                                        y = 1000, 
                                        fill = NULL,
                                        label = round(edad_mean, 1))) +
  scale_fill_viridis_d(name = 'Edad de la madre', end = .75) +
  scale_y_continuous(limits = c(0, NA),
                     labels = function(x) scales::comma(x, decimal.mark = ".", scale = .001)) +
  labs(subtitle = 'Nacimientos en Chile',
       x = 'Edad de la madre',
       y = 'Nacimientos (miles)', 
       caption = 'DESUC, a partir de datos INE')

gg_edad_madre_anim <- gg_edad_madre +
  transition_states(ano_nacim,
                    transition_length = 1,
                    state_length = 3) +
  labs(title = 'Distribución de edad de la madre según año. \nAño {closest_state}')

animate(
  gg_edad_madre_anim,
)

anim_save('27-gg_edad_madre_anim.mp4')

