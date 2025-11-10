#### Dualidade de Hucthinson ####
# Representando pontos no espaço geográfico e no espaço ambiental

# Artigos legais sobre o assunto:
browseURL("https://www.pnas.org/doi/pdf/10.1073/pnas.0901650106")
browseURL("https://www.nature.com/articles/s41586-023-06577-5.pdf")
# Climatic conditions that cover more extensive land areas (and times) are believed to support more individuals, leading to larger populations. Larger populations, in turn, are associated with increased rates of speciation and reduced rates of extinction

# Carregar pacotes
library(terra)
library(mapview)
library(dplyr)
library(tidyr)
library(ggplot2)
library(scales)
library(data.table)
library(tidyterra)
library(patchwork)
library(ggpubr)

#### Ecorregioes: espaço geográfico vs espaço ambiental ####

# Importar shapefile de ecorregioes
eco <- vect("Shapefiles//Ecoregions2017.gpkg")
plot(eco)

# Cortar para o Parana
pr <- vect("Shapefiles//Parana.gpkg")
pr_eco <- crop(eco, pr)
plot(pr_eco, col = c("red", "forestgreen", "orange", "blue", "yellow"))

# Importar variaveis ambientais
variaveis <- rast("Variaveis_Neotropico/Presente/Variaveis.tiff")
plot(variaveis$bio_1)

# Cortar para Parana
v_pr <- crop(variaveis, pr, mask = TRUE)
#Selecionar apenas variaveis de temperatura e precipitação anual
# bio_1 e bio_6
v <- v_pr[[c("bio_1", "bio_12")]]

# Como ficam as ecorregiões no espaço geográfico?
plot(v$bio_1)
plot(pr_eco, add = TRUE)

# Como ficam as ecorregiões no espaço ambiental?
# Extrair temperatura e precipitação para cada ecorregião
eco <- terra::extract(v, pr_eco, #Forçar usar extract de terra
                      xy = TRUE) # Para retornar coordenadas
head(eco)
# Cria uma coluna ID que corresponde à ordem das geometrias
pr_eco_df <- as.data.frame(pr_eco) %>%
  mutate(ID = 1:n())
pr_eco_df
# Unir dados
eco <- right_join(pr_eco_df, eco, by = "ID")
View(eco)

# Definir cores manualmente
unique(eco$ECO_NAME)

cores <- c("#D55E00", "#009E73", "#F0E442", "#0072B2", "#CC79A7")
cores <- setNames(cores, nm = unique(eco$ECO_NAME))

g_env <- ggplot() +
  geom_point(data = eco, aes(x = bio_1, y = bio_12,
                             fill = ECO_NAME),
             pch = 21, alpha = 0.6, cex = 2) +
  scale_fill_manual(values = cores, name = "Ecoregion") +
  xlab("Annual Mean Temperature") +
  ylab("Annual Precipitation") +
  ggtitle("Environmental space") +
  ggpubr::theme_pubclean() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"))
g_env

# Vamos plotar o mapa
eco_map <- ggplot() +
  geom_spatvector(data = pr_eco, aes(fill = ECO_NAME)) +
  scale_fill_manual(values = cores, name = "Ecoregion") +
  ggtitle("Geographic space") +
  theme_light() +
  theme(legend.position = "bottom",
        plot.title = element_text(hjust = 0.5, face = "bold"))
eco_map

# Vamos unir os plots
geo_env <- (eco_map + g_env) & theme(legend.position = "none")

# Obter legenda
legenda <- ggpubr::get_legend(eco_map)
geo_env2 <- geo_env / legenda +
  plot_layout(heights = c(1, 0.15))
geo_env2