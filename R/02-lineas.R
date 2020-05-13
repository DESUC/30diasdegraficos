library(readxl)
library(tidyverse)

base <- readxl::read_xlsx('inputs/02-imad_2019.xlsx')

gg_plazas <- ggplot(data = base, aes(x = imad, y = prop_m, colour = cargo)) +
    geom_line(size = 2) +
    geom_point(colour = 'white', size = rel(7)) +
    geom_text(aes(label = round(prop_m, 0)), 
              size = rel(4), fontface = 'bold',
              show.legend = FALSE) + 
    labs(title = 'Total empresas IMAD',
         subtitle = 'Proporción de cargos ocupados por mujeres (%)') +
    scale_x_continuous('Años', breaks = c(2017:2019)) +
    scale_y_continuous('Porcentaje', limits = c(0,100)) +
    scale_colour_manual('Cargos', values = c('trabajadores' = 'gray20', 
                                             'ejecutivos' = '#D03339', 
                                             'directorio' = '#416EA8'),
                        labels = c('Total fuerza laboral', 
                                   'Línea ejecutiva principal', 
                                   'Directorio')) + 
    theme_minimal() +
    theme(legend.position = 'top',
          plot.title = element_text(face = 'bold'),
          plot.subtitle = element_text(face = 'italic')) 


ggsave('outputs/imad_2019.png', 
       gg_plazas, 
       scale = 3,
       width = 5, height = 4, units = 'cm')