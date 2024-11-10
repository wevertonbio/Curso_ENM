#### OBJETOS ESPACIAIS NO R ####

#Carregar pacotes
library(terra) #Manipular dados espaciais
library(geobr) #Obter mapas do Brasil
library(mapview) #Plotar mapa interativo
library(florabr) #Pacote para acessar Flora do Brasil
library(dplyr) #Manipulação de dataframes e uso do %>% 
library(geodata) #Obter dados do worldclim
library(ggplot2) #Plotar mapas
library(tidyterra) #Plotar mapas
library(rnaturalearth) #Obter mapas do mundo
library(metR) #Configurações adicionais de plots
library(ggspatial) #Configurações adicionais de plots

#### 1 - Objetos Raster ####
#Um pouco de confusão...
# Raster: objeto compostos por uma matriz de píxels/células, cada uma contendo um valor que representa as condições da área que esta célula cobre.
# raster é também o nome de um famoso pacote para importar e manipular objetos espaciais no R, mas o autor do pacote se aposentou (e o pacote também).
# O pacote 'raster' foi substituido pelo pacote 'terra'.

#Importar arquivo raster
f <- system.file("ex/elev.tif", package="terra") #Obter caminho do arquivo de exemplo
f
r <- rast(f) #Importar arquivo
class(r) #Classe do objeto
plot(r) #Plot estático
plet(r) #Plot interativo do terra (não muito bom)
mapview(r) #Plot interativo do mapview (melhor)

#Podemos transformar um raster em um dataframe
d <- as.data.frame(r, xy = TRUE, cells = TRUE)
#E podemos transformar um dataframe com coordenadas em um raster
r_from_d <- rasterize(x = as.matrix(d[,c("x", "y")]), #Matriz de coordenadas
                      y = r, #Raster base
                      values = d$elevation) #Valores
plot(r_from_d)

#Salvar rasters
# Criar pasta para salvar arquivos temporários
dir.create("temp")
writeRaster(r, "temp/Raster.tif") #TIF é melhor formato para dados rasters
#Importar dados novamente
r2 <- rast("temp/Raster.tif")

#### 2 - Vetores espaciais ####
# Vetores espaciais podem ser polígonos (ex: limites de estado ou país), pontos (ex: registros de ocorrência) ou linhas.
f <- system.file("ex/lux.shp", package="terra")  #Obter caminho do arquivo de exemplo
f
v <- vect(f) #Importar arquivo
v
class(v) #Classe do objeto
plot(v) #Plot estático
plet(v) #Plot interativo do terra (não muito bom)
mapview(v) #Plot interativo do mapview (melhor)

# Também podemos transformar um vetor espacial em um data.frame
d2 <- as.data.frame(v, geom = "WKT")
# E podemos transformar um dataframe em um vetor espacial
v_from_d2 <- vect(d2, geom = "geometry")
plot(v_from_d2)

# Também podemos transformar coordenadas long-lat em vetores espaciais
data("occurrences", package = "florabr")
#Selecionar ocorrencias de Araucaria angustifolia
occ <- occurrences %>% filter(species == "Araucaria angustifolia")
#Transformar dataframe em dados espaciais
pts <- vect(occ, geom = c(x = "x", y = "y"), crs = "+init=epsg:4326")
plot(pts)
mapview(pts)
#Salvar vetores
writeVector(pts, "temp/Pontos.shp") #Formato shapefile (mais comum) - Ver tamanho
writeVector(pts, "temp/Pontos.gpkg") #Formato shapefile (mais comum) - Ver tamanho
#Importar dados novamente
pts2 <- vect("temp/Pontos.gpkg")

#Transformando spatvector de pontos para dataframe
pts_df <- as.data.frame(pts, geom = "XY")

# Fazendo subseleção de pontos com polígonos
# Imagina que você quer selecionar apenas os pontos dentro do estado do Paraná
# Vamos importar um mapa do Estado do Paraná do pacote geobr
pr <- read_state(code_state = "PR")
plot(pr)
class(pr) #Classe do objeto
#Converter para spatvector
pr <- vect(pr)
class(pr)
mapview(pr) + mapview(pts)
#Selecionar apenas pontos dentro do Paraná
pts_in <- is.related(pts, pr, "intersects") #Quais pontos intersectam com o PR?
pts_in
sum(pts_in) #Quantos pontos estão dentro do PR?
pts_pr <- pts[pts_in, ]
mapview(pts_pr)

#Como a sequencia dos pontos é a mesma, podemos usar o mesmo vetor para selecionar no data.frame
occ_pr <- occ[pts_in, ]

# Cortando rasters com polígonos
#Podemos baixar variáveis direto do worldclim com o pacote geodata
# worldclim_global(var = "bio", res = 10, path = "WorldClim10")
#Importar variaveis
lf <- list.files("Variaveis_brutas/wc2.1_5m/", full.names = TRUE)
lf
wc <- rast(lf)
wc
plot(wc$wc2.1_10m_bio_1)
#Cuidado ao abrir rasters grandes com mapview
# mapview(wc$wc2.1_10m_bio_1)
#Renomear variaveis
names(wc)
names(wc) <- gsub("wc2.1_10m_", "", names(wc)) #Remover wc2.1_10m_
names(wc) #Novos nomes, sem wc2.1_10m_

#Cortar variáveis para Paraná
wc_pr_ext <- crop(wc, pr)
plot(wc_pr_ext)
wc_pr <- crop(wc, pr, mask = TRUE)
plot(wc_pr)

#Converter rasters (vários) para dataframe
wc_df <- as.data.frame(wc_pr, xy = TRUE, cells = TRUE)

#Extraindo valores de rasters nos pontos
wc_pts <- extract(x = wc_pr, y = occ_pr[,c("x", "y")], xy = TRUE)

#### Plot de mapa com ggplot ####
#Obter alguns mapas
br <- read_state() %>% vect() #Brasil
plot(br)
w <- ne_countries() %>% vect() #Mundo
plot(w)
sa <- w[w$continent == "South America", ] #Subset South America
plot(sa)

#Obter limites de longitude do PR para dar zoom
bb_pr <- ext(wc_pr$bio_1)

#Comandos básicos
# geom_spatvector: plotar spatvector
# geom_spatraster: plotar spatraster
# fill: cor de preenchimento
# colour: cor da linha de contorno
# size ou linewidth: espessura da linha de contorno
# scale_fill_whitebox_c: escala de cores do raster (veja mais opções em:
# https://dieghernan.github.io/tidyterra/articles/palettes.html)
# geom_point: plotar coordenadas de dataframe - Colocar nomes das colunas com long-lat dentro de aes()
# coord_sf: definir caixa de zoom
# xlab e ylab: nomes dos eixos x e y
# scale_x_longitude e scale_y_latitude: intervalo entre as marcas de coordenadas.
# annotation_scale: adiciona rosa dos ventos barra de escala
# annotation_north_arrow: adiciona rosa dos ventos
# theme: configurações internas de cores de fundo do mapa e da legenda.

m <- ggplot() + #Chamar ggplot
  geom_spatvector(data = sa, fill = "grey77", size = 0.1, colour = "white") +
  geom_spatvector(data = br, fill = "grey80", size = 0.1, colour = "grey40") +
  geom_spatraster(data = wc_pr$bio_1) + 
  scale_fill_whitebox_c(palette = "bl_yl_rd", name = "Temperatura") +
  geom_point(data = occ_pr, aes(x, y), size  = 2, 
             shape = 21, colour = "black", fill = "darkgreen") +
  geom_spatvector(data = br, fill = NA, size = 0.1, colour = "grey40") +
  coord_sf(xlim = c(bb_pr[1] - 0.5, xmax=bb_pr[2] + 0.5),
           ylim = c(bb_pr[3] - 0.5, ymax=bb_pr[4] + 0.5),
           expand = T) +
  xlab("Longitude") + ylab("Latitude") +
  metR::scale_x_longitude(ticks = 1) + metR::scale_y_latitude(ticks = 1) +
  ggspatial::annotation_scale(pad_x = unit(8, "cm")) +
  ggspatial::annotation_north_arrow() +
  theme(legend.background = element_rect(fill = "white", linewidth = 0.5, colour = "black"),
        panel.background = element_rect(fill = 'aliceblue', colour = NA),
        panel.border = element_rect(colour = "black", size = 2, fill = NA)) 
m #Plot

#Salvar em alta resolução
ggsave("temp/Meu primeiro mapa.png",
       m,
       dpi = 600, units = "px",
       width = 1600,
       height = 1100, scale = 4)