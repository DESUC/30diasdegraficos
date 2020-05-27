
# Paquetes ----------------------------------------------------------------

library(sjmisc)
library(sjlabelled)
library(janitor)
library(kableExtra)
library(desuctools)
library(tidyverse)
library(RColorBrewer)
library(ggalluvial)

# Base de datos -----------------------------------------------------------

# Se requiere abrir la base de datos de la Encuesta Bicentenario 2019. 
# Disponible en: https://encuestabicentenario.uc.cl

df_bc_2018<- readRDS("inputs/16-bicen_2018.rds") %>% 
  clean_names()


# Preparación de datos ----------------------------------------------------

flat_table(df_bc_2018, gse_calc, d06)
flat_table(df_bc_2018, gse_calc, gse_gfk)

df_bc_2018_nse <- df_bc_2018 %>% 
  transmute_at(vars(gse_calc, gse_gfk, d06), to_label) %>% 
  arrange_all() %>% 
  mutate(id = n():1)

df_bc_2018_nse_long <- df_bc_2018_nse %>% 
  gather('Variable_NSE', 'Nivel', -id, factor_key = TRUE)

nse_categorias <- c('AB', 'ABC1', 'C1a', 'C1b', 'C2', 'C3', 'D', 'E')

df_bc_2018_nse_long <- df_bc_2018_nse_long %>% 
  mutate(Nivel = fct_relevel(Nivel, nse_categorias))

df_bc_2018_nse_long %>% 
  count(Nivel, Variable_NSE) %>% 
  ggplot(aes(x = Variable_NSE, y = n, fill = Nivel)) +
  geom_col()


df_bc_2018_nse_wide <- df_bc_2018_nse %>% 
  count(gse_calc, gse_gfk, d06) %>% 
  mutate_at(vars(everything(), -n), ~fct_explicit_na(., na_level = 'Sin info'))

df_bc_2018_nse_long <- to_lodes_form(df_bc_2018_nse_wide,
                                     axes = vars(gse_calc, gse_gfk, d06))

df_bc_2018_nse_long$stratum <- fct_relevel(df_bc_2018_nse_long$stratum, nse_categorias)

frq(df_bc_2018_nse_long$stratum)


# Gráfico -----------------------------------------------------------------

mypal <- colorRampPalette(brewer.pal( 11 , "Spectral" ) )
paleta <- mypal(15)

colores <- c(paleta[which(nse_categorias == "AB"):which(nse_categorias == "ABC1")], 
             paleta[which(nse_categorias == "C1a"):which(nse_categorias == "C3") + 2],
             paleta[which(nse_categorias == "D"):which(nse_categorias == "E") + 4],
             'gray')

sankey <- ggplot(df_bc_2018_nse_long,
       aes(x = x, 
           stratum = stratum, 
           alluvium = alluvium,
           y = n,
           label = stratum,
           fill = stratum)) +
  ggalluvial::geom_alluvium() + 
  ggalluvial::geom_stratum(width = 1/3, reverse = TRUE, colour = 'grey') +
  geom_text(stat = "stratum", label.strata = TRUE, reverse = TRUE, size = 3) +
  scale_x_discrete(labels = c("NSE Calculado", "NSE AIM", 'NSE Observado')) +
  scale_fill_manual(guide = FALSE,
                    values = colores) +
  labs(title = 'Relación entre distintos tipos de cálculo de Nivel Socio Eeconómico (NSE)',
       subtitle = 'Calculo NSE Bicentenario, Cálculo NSE método AIM, NSE observado por encuestador',
       x = 'Variables de NSE',
       y = 'Casos',
       caption = 'Encuesta Bicentenario 2018\nAnálisis de DESUC')+
  theme_minimal() +
  theme(axis.text=element_text(size=10),
        axis.title=element_text(size=11),
        plot.caption = element_text(size = 10),
        plot.title = element_text(size = 13, face = "bold"),
        plot.subtitle = element_text(size = 12))

ggsave('outputs/17-sankey.png',
       width = 6,
       height = 5,
       scale = 3,
       units = 'cm')