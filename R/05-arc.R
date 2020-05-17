
# Se requiere abrir la base de datos de la Encuesta Bicentenario 2019. 
# Disponible en: https://encuestabicentenario.uc.cl

library(tidyverse)
library(ggraph)
library(tidygraph)

df_bicen_19 <- readRDS('inputs/05-arc-df_bicen_19_30diasdegraficos_2020_confianza.rds')

label_conflictos <- df_bicen_19 %>% 
  select(-folio, -cvar1, -edad_cat, -pond_se) %>% 
  map_chr(~attr(., 'label'))
  
# Etiquetas para variables de conflicto.
label_conflictos <- structure(str_extract(label_conflictos, '(?<=\\().*(?=\\))'),
                              names = names(label_conflictos))

df_bicen_19_long <- df_bicen_19 %>% 
  mutate_at(vars(everything(), -folio, -pond_se), as_factor) %>% 
  pivot_longer(cols = c(-folio, -cvar1, -edad_cat, -pond_se),
               names_to = 'conflicto_var',
               values_to = 'tipo') %>% 
  mutate(conflicto_lab = label_conflictos[conflicto_var]) %>% 
  select(-conflicto_var)

gen_pares <- function(vec) {
  vec <- unlist(vec)
  
  df <- combn(vec, 
              m = 2) %>% 
    t() %>% 
    as.data.frame()
  
  colnames(df) <- c('from', 'to')
  
  as_tibble(df)
}

# Consideramos respuestas de Mucha o Bastante confianza.
df_bicen_19_nest <- df_bicen_19_long %>% 
  filter(tipo %in% c('Mucho', 'Bastante')) %>%
  select(-tipo) %>% 
  group_by_at(vars(folio:pond_se)) %>% 
  group_nest() 

# Eliminar casos sin relaciones. nrow(data) < 2.
df_bicen_19_nest <- df_bicen_19_nest[map_int(df_bicen_19_nest$data, nrow) > 1, ]
  
df_bicen_19_graph <- df_bicen_19_nest %>% 
  mutate(combinacion = map(data, gen_pares)) %>% 
  select(-data) %>% 
  unnest(combinacion)

# simplificar tabla y caluclar el peso de cada relación
df_bicen_19_graph_edad <- df_bicen_19_graph %>% 
  count(from, to, edad_cat, wt = pond_se, name = 'weight')

df_bicen_19_graph_edad <- df_bicen_19_graph_edad %>% 
  as_tbl_graph(directed = FALSE)

df_bicen_19_graph_edad

df_bicen_19_graph_edad <- df_bicen_19_graph_edad %>% 
  mutate(Menciones = centrality_degree(mode = 'in', weights = weight)) %>% 
  arrange(desc(Menciones)) %>% 
  activate(edges) %>% 
  mutate(edad_cat = as.factor(edad_cat))

p <- ggraph(df_bicen_19_graph_edad, 
            layout = 'linear') + 
  geom_edge_arc(aes(colour = edad_cat, 
                    edge_width = weight),
                edge_alpha = .3) + 
  geom_edge_point(aes(size = weight, 
                      colour = edad_cat)) +
  geom_node_text(aes(label = str_wrap(name, 12)),
                 size = rel(2.5),
                 vjust = 0.5) +
  scale_y_continuous(NULL, breaks = NULL) +  
  scale_x_continuous(NULL, breaks = NULL) +
  coord_cartesian(clip = 'off')

p <- p + 
  facet_edges(facets = ~edad_cat, 
              ncol = 1, 
              strip.position = 'left') +
  scale_edge_color_brewer(palette = 'Set1', guide = 'none') +
  theme_minimal(base_size = 8) +
  guides(edge_size = 'none', edge_width = 'none') +
  theme(panel.spacing = unit(0.5, 'cm'),
        plot.caption = element_text(size = rel(.5)))

p +
  labs(title = 'Relación de confianza entre instituciones según edad',
       subtitle = 'Se consideran respuestas "mucho" y "bastante"\nY en general, ¿cuánto confía en las instituciones que le nombraré?',
       caption = 'Encuesta Bicentenario 2019, DESUC')

ggsave('outputs/05-arc_gg_confianza.png',
       width = 12,
       height = 10,
       scale = 1,
       units = 'cm')
