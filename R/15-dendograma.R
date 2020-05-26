
#Se realiza un cluster como ejercicio para el desafío, no es una solución adecuada.
#para una solución adecuada, se requiere de más pruebas y otras variables.

# Librería ----------------------------------------------------------------

library(cluster)
library(factoextra)
library(tidyverse)
library(sjmisc)
library(sjlabelled)
library(desuctools)

# Base de datos -----------------------------------------------------------

#La base de datos completa está disponible públicamente en:
#https://encuestabicentenario.uc.cl/

data <- readRDS(file = 'inputs/15-bicen_dendo.RDS')

# Cluster -----------------------------------------------------------------

data_cluster <- data %>% 
  select(-gse_gfk)

#conversion a factor.
var <- names(data_cluster)
data_cluster[,var] <- lapply(data_cluster[,var] , factor)

#calculo distancia
distancia <- daisy(data_cluster[ ,3:11], metric = c("gower"))
class(distancia)

#calculo cluster jerárquico
cluster <- hclust(distancia, method = "complete")
plot(cluster)

prueba1 <- eclust(distancia, "hclust", k = 3, graph = FALSE) 
prueba2 <- eclust(distancia, "hclust", k = 2, graph = FALSE) 
prueba3 <- eclust(distancia, "hclust", k = 4, graph = FALSE) 

#prueba de silueta

fviz_silhouette(prueba1)
fviz_silhouette(prueba2)
fviz_silhouette(prueba3) #ninguno presenta una medida satisfactoria.

# Gráfico -----------------------------------------------------------------

chart_1 <- fviz_dend(prueba2, rect = TRUE, show_labels = FALSE)

colors <- c('skyblue4', 'skyblue2', 'grey50', 'grey')

dendograma <- chart_1 +
  scale_colour_manual(values = colors) +
  labs(title = "Dendograma de cluster", 
       subtitle = "Tipologías según percepciones de movilidad social, \nedad, sexo y NSE",
       x = "Casos", y = "Altura",
       caption = "n=2047 datos no ponderados\nEncuesta Bicentenario 2019, DESUC") +
  theme(axis.text.y = element_text(size = rel(1.0)),
        plot.title = element_text(hjust = 0, vjust = 0, size = 10, face = "bold"),
        plot.subtitle = element_text(hjust = 0, vjust = 0, size = 9),
        legend.text = element_text(size = rel(0.8)))

ggsave('outputs/15-dendograma.png',
       width = 6,
       height = 5,
       scale = 3,
       units = 'cm')

# Caracterización cluster -------------------------------------------------

#creacion variable
data_cluster <- data_cluster %>% 
  mutate(tipo = cutree(prueba2, k = 2))
frq(data_cluster$tipo)

tabla <- tabla_vars_segmentos(.data = data_cluster,
                              .vars = vars(s03_1, s03_2, s03_4, s03_5, s03_6, s03_7, d07, gse_calc),
                              .segmentos = vars(tipo)) %>% 
  mutate(prop=round(prop*100)) %>% 
  filter(!pregunta_cat=="8") %>% 
  filter(!pregunta_cat=="9") 

