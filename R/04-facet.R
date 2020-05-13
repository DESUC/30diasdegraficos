
#Se requiere abrir la base de datos de la Encuesta de Convivencia Barrial e Interculturalidad 2019. Disponible en: http://cead.spd.gov.cl/estudios-y-encuestas/

### Satisfacción con la policía

#### Tabla general

get_label(df_migrantes, I_6_P75)

policias <- c('I_6_P75', 'I_7_P75', 'I_8_P75', 'I_9_P75', 'I_10_P75', 'I_11_P75')

preg <- tabla_vars_segmentos(df_migrantes,
                             .vars = vars(policias),
                             .segmentos = vars(nacionalidad),
                             .wt = rake_wbsp)

tabla <- preg %>% 
  mutate(pregunta_cat = fct_collapse(pregunta_cat,
                                     'De acuerdo' = c('De acuerdo', 'Muy de acuerdo'),
                                     'En desacuerdo' = c('En desacuerdo', 'Muy en desacuerdo'))) %>% 
  filter(pregunta_cat %in% c('De acuerdo', 'En desacuerdo')) %>% 
  group_by_at(vars(segmento_var:pregunta_cat)) %>% 
  summarise_at(vars(casos, prop), sum) %>% 
  ungroup() %>% 
  mutate(pregunta_lab = fct_reorder2(pregunta_lab, .x = prop, .y = pregunta_cat, 
                                     .fun = function(.x, .y) sum(if_else(.y == 'De acuerdo', .x, 0)),
                                     .desc = FALSE),
         pregunta_lab = fct_relabel(pregunta_lab, str_entre_parentesis),
         pregunta_lab = fct_recode(pregunta_lab, 
                                   'La policía en Chile a veces abusan de su poder*' = 'La policía en Chile a veces abusan de su poder',
                                   'La policía no es capaz de mantener el orden en las calles y veredas del barrio*' = 'La policía no es capaz de mantener el orden en las calles y veredas del barrio'),
         prop = if_else(pregunta_cat == 'En desacuerdo', -prop, prop))

tabla_total <- tabla %>% 
  count(segmento_cat, pregunta_lab, pregunta_cat, wt = prop)

tabla %>% 
  select(-segmento_var, -pregunta_var, -casos) %>% 
  mutate(prop = scales::percent(abs(prop))) %>% 
  spread(segmento_cat, prop) %>% 
  arrange(desc(pregunta_lab)) %>% 
  kable()


#### Gráfico general

gg_policias <- tabla %>% 
  gg_barras_general(fill = pregunta_cat,
                    ylim = c(-1, 1),
                    lab_text = FALSE) +
  geom_text(data = tabla_total,
            aes(y = n + sign(n) * .025, 
                hjust = if_else(sign(n) == 1, 0, 1),
                label = round(abs(n) * 100, 0), 
                fill = NULL)) +
  facet_grid(cols = vars(segmento_cat)) +
  scale_fill_brewer('Grado de acuerdo', palette = 'Set1', direction = 1) +
  labs(title = 'Satisfacción con policias',
       subtitle = 'Qué tan de acuerdo está con las siguientes afirmaciones\n* señala atributos negativos respecto de las policias',
       y = 'Porcentaje de respuesta') + 
  theme(legend.position = 'top')

gg_policias

gg_policias %>% 
  gg_save_desuc('gg_policias.png',
                height = 12)

