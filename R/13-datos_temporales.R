# Se requiere abrir la base de datos de Nacimientos de INE 
# Disponible en alguna parte del sitio: https://www.ine.cl

library(tidyverse)

# Cambiar locale para que gráfico tenga meses en español
Sys.setlocale(category = "LC_ALL","es_ES.UTF-8")

# Gráfico de nacimientos por día

# Base de datos con la suma de necimientos por día desde 1985 a 2017.
df_nacimientos_dia <- readRDS('inputs/13-datos_temporañes_df_nacimientos_dia_30diasdegraficos_2020.rds')

# 11 días con mayores nacimientoes.
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


# Creación del gréfico.
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
  scale_y_continuous("Nacimientos (miles por días)",
                     limits = c(0, NA),
                     breaks = seq(0, 100000, 5000),
                     labels = function(x) scales::comma(x, decimal.mark = ".", scale = .001, accuracy = 1)) + 
  scale_color_gradient(low = 'red', high = 'blue') +
  labs(title = 'Nacimientos en Chile por día a lo largo de un año',
       subtitle = str_glue('Total entre 1985 y 2017, para un total de {chr_nacimientos_total} nacimientos'),
       caption = 'A partir de datos INE, DESUC') +
  theme(legend.position = 'top') +
  theme_minimal()


ggsave('outputs/13-datos_temporales.png',
       width = 10,
       height = 5,
       scale = 3,
       units = 'cm')
