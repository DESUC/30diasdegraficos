#Para este script se usan los datos de la Encuesta Nacional de Polivictimización en Niños, Niñas y Adolescentes (2017), disponible en:http://cead.spd.gov.cl/estudios-y-encuestas/ 


## Victimizacion

### Prevalencia vida y año


vict_vida <- base_val %>% 
  select(matches('^P[A-F]_\\d$'), wgt_alu) %>%
  gather('variable', 'respuesta', -wgt_alu) %>%
  group_by(variable) %>% 
  summarise(respuesta_pond = 
              sum(if_else(respuesta == 1, wgt_alu, 0), na.rm = TRUE)/
              sum(if_else(!is.na(respuesta), wgt_alu, 0), na.rm = TRUE),
            respuesta      = 
              sum(if_else(respuesta == 1,       1, 0), na.rm = TRUE)/
              length(na.omit(respuesta)),
            tipo = 'Vida') 

vict_año <- base_val %>% 
  select(matches('^P[A-F]_\\d_1$'), wgt_alu) %>%
  gather('variable', 'respuesta', -wgt_alu) %>%
  group_by(variable) %>% 
  summarise(respuesta_pond = 
              sum(if_else(respuesta == 1, wgt_alu, 0), na.rm = TRUE)/
              sum(if_else(!is.na(respuesta), wgt_alu, 0), na.rm = TRUE),
            respuesta      = 
              sum(if_else(respuesta == 1,       1, 0), na.rm = TRUE)/
              length(na.omit(respuesta)),
            tipo = 'Último año') %>% 
  ungroup() %>% 
  mutate(variable = str_extract(variable, pattern = 'P[A-F]_\\d'))

vict_vida_año <- bind_rows(vict_vida, vict_año)

vict_vida_año <- vict_vida_año %>% 
  mutate(etiqueta = vict_label[variable],
         modulo   = vict_modulo[variable])

# Orden de etiquetas según victimizaciones vida
vict_orden_vida_etiqueta <- vict_vida_año %>% 
  filter(tipo == 'Vida') %>% 
  arrange(modulo, desc(respuesta_pond)) %>% 
  pull(etiqueta)

# Orden de etiquetas según victimizaciones Último año
vict_orden_año_etiqueta <- vict_vida_año %>% 
  filter(tipo == 'Último año') %>% 
  arrange(modulo, desc(respuesta_pond)) %>% 
  pull(etiqueta)

vict_vida_año %>% 
  arrange(variable) %>% head()


#### Gráfico 

grafico_modulo <- function(datos, filtro_variables, color_grafico){
  
  grafico <- datos %>% 
    filter(str_detect(variable, filtro_variables)) %>% 
    mutate(etiqueta = fct_relabel(etiqueta, str_wrap, 40)) %>% 
    ggplot(aes(x = etiqueta, 
               y = respuesta_pond * 100, shape = tipo)) +
    geom_point(size = 5,
               color = color_grafico, 
               fill = color_grafico) +
    geom_linerange(aes(ymin = rep(0, length(..x..)), ymax = ..y..),
                   color = color_grafico, 
                   size = 3, alpha = .4) +
    geom_text(aes(label = round(..y..)), color = 'black', size = 3) +
    coord_flip() + 
    scale_y_continuous(limits = c(0,58), breaks = seq(0, 100, by = 10)) +
    labs(title = 'Prevalencia victimizaciones \nvida y últimos 12 meses',
         x = '', 
         y = '% respuesta Sí') +
    theme(legend.position = "top") +
    guides(color = 'none')
  
  return(grafico)
}

#### Modulo A: Delitos comunes

gg_vict_vida_año_A_wgt_alu <- grafico_modulo(vict_vida_año, 'PA.*', '#F8766D') +
  labs(x = 'Delitos comunes')

gg_vict_vida_año_A_wgt_alu
