#La base de datos corresponde a los nacimientos por año en Chile disponible públicamente en INE.
#Este análisis forma parte de un estudio asociado a embarazo adolescente, pronto a finalizar y publicarse.

# Paquetes ----------------------------------------------------------------

library(lubridate)
library(janitor)
library(tidyverse)
library(ggTimeSeries)

# Base de datos -----------------------------------------------------------

nacimientos <- readRDS('inputs/01-df_nacimientos.rds')

# Tabla -------------------------------------------------------------------


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

#eliminar missing.
sum(nacimientos$edad_madre > 60, na.rm = TRUE)

nacimientos <- nacimientos %>% 
  filter(edad_madre < 60)

#tabla madres adolescentes por año.
ea_anos <- bind_rows(nacimientos %>% 
                       count(ano_nacim, ea_madre) %>% 
                       filter(ea_madre != "no EA") %>% 
                       mutate(variable = 'EA'),
                     nacimientos %>% 
                       count(ano_nacim) %>% 
                       mutate(ea_madre = 'Total',
                              variable = 'Total')) %>% 
  filter(!(is.na(ano_nacim) | is.na(ea_madre))) %>% 
  mutate(ea_madre = fct_explicit_na(ea_madre, na_level = 'Total'))

ea_anos <- ea_anos %>% 
  filter(!ea_madre == "Total")


# Gráfico -----------------------------------------------------------------

sg_nac  <- ggplot(ea_anos,
         aes(x = ano_nacim,
             y = n,
             group = ea_madre,
             fill = ea_madre)) +
  stat_steamgraph() +
  scale_y_continuous(breaks = c(-20000,0,20000),labels = c(0,20000,40000))+
  theme_minimal() +
  labs(title="Nacimientos de madres adolescentes en Chile 1984 a 2017", 
       subtitle = "Según tramo de edades de adolescentes",
       y="Cantidad de nacimientos", 
       x="Año", 
       caption="Elaboración propia desde datos INE")+
  scale_fill_manual(values = c('slategray1', 'skyblue2', 'skyblue3')) +
    guides(fill = guide_legend(title = "Tramos de edad"))

ggsave('outputs/19-steamgraph.png',
       width = 8,
       height = 5,
       scale = 3,
       units = 'cm')