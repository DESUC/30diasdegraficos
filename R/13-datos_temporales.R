# Se requiere abrir la base de datos de Nacimientos de INE 
# Disponible en alguna parte del sitio: https://www.ine.cl

library(tidyverse)
library(ggrepel)
library(patchwork)

# Cambiar locale para que gráfico tenga meses en español
Sys.setlocale(category = "LC_ALL","es_ES.UTF-8")

# Base de datos ####
# Base de datos con la suma de necimientos por día desde 1985 a 2017.
df_nacimientos_dia <- readRDS('inputs/13-datos_temporañes_df_nacimientos_dia_30diasdegraficos_2020.rds')

# Año, mes y día de la semana
df_nacimientos_dia <- df_nacimientos_dia |> 
  mutate(mes = month(fecha_mes_dia,
                     label = TRUE,
                     abbr = FALSE),
         dia_semana = wday(fecha_mes_dia, 
                           label = TRUE,
                           abbr = FALSE,
                           week_start = 1))

# 11 días con mayores nacimientos.
fecha_mes_dia_min <- df_nacimientos_dia %>% 
  arrange(nacimientos) %>% 
  head(11) %>% 
  pull(fecha_mes_dia)

# Base de datos para notar los dias con mauor nímero de nacimientos.
df_fecha_mes_dia_min <- tibble(fechas = fecha_mes_dia_min,
                               texto = format(fecha_mes_dia_min, '%e-%m'))

# Número total de nacimientos considerados en el análisis.
nacimientos_total <- sum(df_nacimientos_dia$nacimientos)
chr_nacimientos_total <- format(nacimientos_total, big.mark = '.', decimal.mark = ',')


# Gráfico nacimientos días anaual ####

theme_set(
  theme_minimal() +
    theme(legend.position = 'top')
)

f_scales <- function(y_breaks){
  list(
    scale_y_continuous("Nacimientos (miles)",
                       limits = c(0, NA),
                       breaks = y_breaks,
                       labels = function(x) scales::comma(x, 
                                                          decimal.mark = ",", 
                                                          big.mark = '.',
                                                          scale = .001, accuracy = 1),
                       expand = expansion(mult = c(0, .05))),
    labs(
      subtitle = str_glue('Total entre 1985 y 2017, para un total de {chr_nacimientos_total} nacimientos.'),
      caption = 'A partir de datos INE, DESUC')
  )
}

df_nacimientos_dia %>% 
  ggplot(aes(x = fecha_mes_dia, y = nacimientos)) +
  geom_smooth(method = 'loess', formula = y ~ x, se = FALSE) +
  geom_vline(xintercept = df_fecha_mes_dia_min$fechas, 
             color = 'red', 
             alpha = .5) +
  ggrepel::geom_label_repel(data = df_fecha_mes_dia_min, 
                            aes(x = fechas, label = texto),
                            y = 7500, 
                            size = 3) +
  geom_line(alpha = 1) +
  scale_x_date("día de nacimiento", 
               date_labels = '%b', 
               date_breaks = 'month') +
  scale_color_gradient(low = 'red', high = 'blue') +
  f_scales(y_breaks = seq(0, 100000, 5000)) + 
  labs(title = 'Nacimientos en Chile por día a lo largo de un año')


ggsave('outputs/13-datos_temporales.png',
       width = 10,
       height = 5,
       scale = 3,
       units = 'cm')

# Gráfico nacimientos mes ####

gg_mes <- df_nacimientos_dia |> 
  count(mes, 
        wt = nacimientos, 
        name = 'nacimientos') |> 
  ggplot(aes(x = mes, y = nacimientos)) +
  geom_linerange(aes(ymin = 0, ymax = after_stat(y)),
                 colour = 'gray70',
                 linewidth = 2) +
  # geom_point() +
  geom_label(aes(label = scales::number(after_stat(y),
                                        scale = 0.001,
                                        decimal.mark = ",", 
                                        big.mark = '.',
                                        accuracy = 1)),
             label.size = 0,
             fill = '#a4b4c6') + 
  f_scales(y_breaks = seq(0, 1000000, 100000)) +
  guides(x = ggplot2::guide_axis(n.dodge = 2)) +
  labs(title = 'Por mes',
       subtitle = NULL,
       caption = NULL,
       x = 'Mes') +
  theme(axis.text.x = element_text(size = rel(1.25)))

gg_mes

ggsave('outputs/13.2-datos_temporales_mes.png',
       width = 5,
       height = 5,
       scale = 3,
       units = 'cm')

# Gráfico nacimientos día semana ####

gg_dia <- df_nacimientos_dia |> 
  count(dia_semana, 
        wt = nacimientos, 
        name = 'nacimientos') |> 
  ggplot(aes(x = dia_semana, y = nacimientos)) +
  geom_linerange(aes(ymin = 0, ymax = after_stat(y)),
                 colour = 'gray60',
                 linewidth = 2) +
  # geom_point() +
  geom_label(aes(label = scales::number(after_stat(y),
                                        scale = 0.001,
                                        decimal.mark = ",", 
                                        big.mark = '.',
                                        accuracy = 1)),
             label.size = 0,
             fill = '#a4b4c6') + 
  f_scales(y_breaks = seq(0, 1500000, 150000)) +
  guides(x = ggplot2::guide_axis(n.dodge = 2)) +
  labs(title = 'Por día de la semana',
       subtitle = NULL,
       caption = NULL,
       x = 'Día de la semana') +
  theme(axis.text.x = element_text(size = rel(1.4)))

gg_dia

ggsave('outputs/13.3-datos_temporales_dia_semana.png',
       width = 5,
       height = 5,
       scale = 3,
       units = 'cm')


## Gráfico conjunto día y mes

gg_dia + gg_mes +
  patchwork::plot_layout(widths = c(1, 1.5)) + 
  plot_annotation(title = 'Nacimientos en Chile',
                  subtitle = str_glue('Total entre 1985 y 2017, para un total de {chr_nacimientos_total} nacimientos.'),
                  caption = 'A partir de datos INE, DESUC')

ggsave('outputs/13-datos_temporales_dia_mes.png',
       width = 10,
       height = 5,
       scale = 3,
       units = 'cm')
