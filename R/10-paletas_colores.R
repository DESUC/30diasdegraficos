# Se requiere abrir la base de datos de la Encuesta Bicentenario 2019. 
# Disponible en: https://encuestabicentenario.uc.cl

library(haven)
library(tidyverse)
library(janitor)
library(patchwork)
library(see)

# Apertura de base

df_bicen_19 <- readRDS('inputs/10-paleta_colores-df_bicen_19_30diasdegraficos_2020.rds')

label_conductas <- df_bicen_19 %>% 
  select(-folio, -cvar1, -edad_cat, -pond_se) %>% 
  map_chr(~attr(., 'label'))

# Etiquetas para variables de conductas.
label_conductas <- structure(paste0(str_extract(label_conductas, '(?<=\\().*(?= por razones)'), '?'),
                              names = names(label_conductas))

df_bicen_19_long <- df_bicen_19 %>% 
  mutate_at(vars(everything(), -folio, -pond_se), as_factor) %>% 
  pivot_longer(cols = c(-folio, -cvar1, -edad_cat, -pond_se),
               names_to = 'conducta_var',
               values_to = 'Respuesta') %>% 
  mutate(conducta_lab = label_conductas[conducta_var]) %>% 
  select(-conducta_var)

# Calculo de porcentajes de respuesta por pregunta por tipo de respuesta

df_resp <- df_bicen_19_long %>% 
  count(conducta_lab, Respuesta, wt = pond_se, name = 'casos') %>% 
  group_by(conducta_lab) %>% 
  mutate(prop = casos / sum(casos)) %>% 
  ungroup()

# Orden de preguntas
conducta_orden <- df_resp %>% 
  filter(Respuesta %in% c('Siempre', 'Casi siempre')) %>% 
  count(conducta_lab, wt = prop) %>% 
  arrange(n) %>% 
  pull(1)

# Eliminar categorías no sabe y no responde.
df_resp <- df_resp %>% 
  filter(!(Respuesta %in% c('No aplica [No leer]', 'NS [No leer]', 'NR [No leer]'))) %>% 
  mutate(conducta_lab = fct_relevel(conducta_lab, conducta_orden), # ajustar orden de preguntas
         conducta_lab = fct_relabel(conducta_lab, str_wrap, 30)) # limitar largo de lineas de texto

palettes <- c("ice", "full", "rainbow", "complement", "contrast")

plot_palette <- function(palette){
  p <- ggplot(df_resp, aes(y = conducta_lab,
                      x = prop, 
                      fill = Respuesta)) +
    geom_col() +
    scale_x_continuous(labels = scales::percent) + 
    see::theme_modern(base_size = 6) +
    labs(title = str_glue('see::scale_fill_flat_d(palette = "{palette}")'),
         x = NULL,
         y = NULL)
   
  if(palette == 'ice'){
   p + 
      scale_fill_flat_d(palette = palette)
  } else {
    p + 
      scale_fill_flat_d(palette = palette, guide = 'none')
  }
}

# Generar gráficos según los nombres de paletas disponibles.
gg <- map(palettes, plot_palette)

# Unir los gráficos en un solo grob.
gg <- wrap_plots(gg, guides = 'collect') + guide_area() &
  theme(plot.title.position = 'plot',
        plot.title = element_text(size = rel(2)),
        axis.text.x = element_text(size = rel(0.5)),
        axis.text.y = element_text(size = rel(0.75)))
gg + 
  plot_annotation(title = 'Paletas de colores disponibles en see::scale_fill_flat_d',
                  subtitle = 'Declaración respecto de conductas ambientales. ¿Qué tan a menudo Ud.…?.',
                  caption = 'Encuesta Bicentenario 2019, DESUC') 

ggsave('outputs/10-paletas_colores_gg_conductas.png',
       width = 10,
       height = 7,
       scale = 3,
       units = 'cm')
