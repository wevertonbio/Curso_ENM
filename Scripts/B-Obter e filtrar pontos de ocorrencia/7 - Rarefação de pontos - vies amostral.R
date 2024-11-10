#### RAREFAÇÃO DOS PONTOS PARA REDUZIR VIÉS AMOSTRAL ####

# Sugestões de artigos sobre maneiras de reduzir viés amostral:
# Rarefação no espaço geográfico
# https://nsojournals.onlinelibrary.wiley.com/doi/epdf/10.1111/ecog.01132
# https://scar.github.io/EGABIcourse19/course_material/literature/Boria%202014.pdf
# https://onlinelibrary.wiley.com/doi/abs/10.1111/jbi.14854

# Rarefação no espaço ambiental
# https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13142
# https://nsojournals.onlinelibrary.wiley.com/doi/abs/10.1111/j.1600-0587.2013.00441.x

# Target-group: usar como background pixels mais bem amostrados
# https://onlinelibrary.wiley.com/doi/10.1111/ddi.13442

# Extrair pontos aleatórios no espaço ambiental
# https://www.sciencedirect.com/science/article/pii/S030438002400142X

# Carregar pacotes
library(spThin) #Para filtrar pontos no espaço geográfico
library(flexsdm) #Para filtrar pontos no espaço ambiental
library(moranfast) #Para calcular autocorrelação espacial
library(data.table) #Importar e salvar tabelas
library(dplyr) #Manipulação de dataframes e uso do %>%
library(mapview) #Para visualizar mapa interativo
library(terra) #Manipulação de dados espaciais
library(ggplot2) #Plotar gráficos
source("Scripts/helpers/filter_geo_moran.R") #Função para testar várias distancias

#### Araucaria angustifolia ####
#Como sempre, começamos atribuindo o nome da espécie e o diretório a objetos
sp <- "Araucaria angustifolia"
sp_dir <- file.path("Ocorrencias/", sp)
sp_dir

# Importar registros 
occ <- fread(file.path("Ocorrencias/", sp, "Check_points/C - Occ_filtrados.gz"),
             data.table = FALSE)
#Espacializar pontos
pts <- vect(occ, geom = c(x = "decimalLongitude", #Converte pontos para spatvector
                          y = "decimalLatitude"), crs = "+init=epsg:4326")
mapview(pts, #Converte pontos para spatvector
        zcol = "data_source", #Coluna usada para colorir
        burst = TRUE) #Filtrar por valor da coluna

# Vamos importar uma variável de temperatura para ver a distribuição dos pontos
# nessa dimensão
v <- rast("Variaveis_Neotropico/Presente/Variaveis.tiff")
occ_temp <- extract(v$bio_1, pts, ID = FALSE)[[1]]
boxplot(occ_temp)

#### Filtrar usando uma distância fixa ####
#Agora, usamos a função thin do pacote spthin para filtrar seguindo uma 
# distância mínima que um ponto deve ter do outro
# Aqui, vamos usar 10km
occ_10 <- thin(loc.data = occ,
               lat.col = "decimalLatitude", long.col = "decimalLongitude",
               spec.col = "scientificName",
               thin.par = 10, #Distância em km
               reps = 5, #Número de réplicas
               locs.thinned.list.return = TRUE,
               write.files = FALSE, write.log.file = FALSE, verbose = FALSE)
#Vamos manter a replica que mantem maior numero de registros
n_occ <- sapply(occ_10, nrow) #Ver numero de linhas em cada dataframe da lista
n_occ
occ_10 <- occ_10[[which(max(n_occ) == max(n_occ))[1]]]
# Criar coluna com nome da espécie e mudar nomes de colunas de longitude e latitude
occ_10 <- occ_10 %>% dplyr::select(x = Longitude, y = Latitude) %>% 
  mutate(species = sp, .before = x)
#Vamos ver os pontos removidos
#Espacializar pontos
pts_10 <- vect(occ_10, geom = c(x = "x", #Converte pontos para spatvector
                          y = "y"), crs = "+init=epsg:4326")
mapview(pts, col.regions = "black") + mapview(pts_10, col.regions = "yellow")

#Vamos testar uma distancia maior, de 50 km
occ_50 <- thin(loc.data = occ,
               lat.col = "decimalLatitude", long.col = "decimalLongitude",
               spec.col = "scientificName",
               thin.par = 50, #Distância em km
               reps = 5, #Número de réplicas
               locs.thinned.list.return = TRUE,
               write.files = FALSE, write.log.file = FALSE, verbose = FALSE)
#Vamos manter a replica que mantem maior numero de registros
n_occ <- sapply(occ_50, nrow) #Ver numero de linhas em cada dataframe da lista
n_occ
occ_50 <- occ_50[[which(max(n_occ) == max(n_occ))[1]]]
# Criar coluna com nome da espécie e mudar nomes de colunas de longitude e latitude
occ_50 <- occ_50 %>% dplyr::select(x = Longitude, y = Latitude) %>% 
  mutate(species = sp, .before = x)
#Vamos ver os pontos removidos
#Espacializar pontos
pts_50 <- vect(occ_50, geom = c(x = "x", #Converte pontos para spatvector
                                y = "y"), crs = "+init=epsg:4326")
mapview(pts, col.regions = "black") + mapview(pts_10, col.regions = "yellow") +
  mapview(pts_50,  col.regions = "red")

# Vamos ver como isso muda a "temperatura ótima" para a espécie
# Extrair informações e salvar em dataframe
occ_temp <- extract(v$bio_1, pts, ID = FALSE)[[1]]
occ_temp <- data.frame("Distancia" = 0, "Temperatura" = occ_temp)
occ_temp_10 <- extract(v$bio_1, pts_10, ID = FALSE)[[1]]
occ_temp_10 <- data.frame("Distancia" = 10, "Temperatura" = occ_temp_10)
occ_temp_50 <- extract(v$bio_1, pts_50, ID = FALSE)[[1]]
occ_temp_50 <- data.frame("Distancia" = 50, "Temperatura" = occ_temp_50)
#Unir todas as informações
occ_temp_all <- rbind(occ_temp, occ_temp_10, occ_temp_50)
#Converte coluna de distancia para fator - para plot de boxplot
occ_temp_all$Distancia <- as.factor(occ_temp_all$Distancia)
# Plotar boxplot
ggplot(data = occ_temp_all) + 
  geom_boxplot(aes(Distancia, Temperatura, fill = Distancia))
#Diferentes distâncias de filtragem irão dar diferentes informações para o modelo

# Qual a melhor distância?
# Não existe melhor distância:
# https://onlinelibrary.wiley.com/doi/abs/10.1111/jbi.14854

#### Filtrar testando várias distâncias e autocorrelação espacial ####

# Podemos testar várias distâncias e ver qual delas apresenta um balanço entre:
# Diminuir autocorrelação espacial vs manter o máximo de pontos
# Para isso, vamos usar a função customizada filter_geo_moran()
# Autocorrelação é baseada nas variaveis ambientais, e não pode considerar
# variáveis categoricas
soiltype_id <- which(names(v) == "soilType") #Qual variavel é soilType
v_cont <- v[[-soiltype_id]] #Remover soiltype
names(v_cont) #Conferir variaveis
# Testar várias distancias
occ_geo_moran <- filter_geo_moran(occ = occ, #Ocorrencias
                                  species = "scientificName", #Coluna com nome científico 
                                  long = "decimalLongitude", #Coluna com nome longitude
                                  lat = "decimalLatitude", #Coluna com nome latitude
                                  d = c(0, 2, 5, 7.5, 10, 12.5, 15, 17.5, 20, #Distancias
                                        22.5, 25, 30),
                                  variables = v_cont) #Variaveis continuas
# Qual a distancia selecionada?
occ_geo_moran$Distance
#Ver resultados do indice de Moran
View(occ_geo_moran$imoran)
#Plotar pontos selecionados e comparar com distancia de 10 e 50 usadas anteriormente
occ_moran <- occ_geo_moran$occ
#Espacializar pontos
pts_moran <- vect(occ_moran, geom = c(x = "x", #Converte pontos para spatvector
                                y = "y"), crs = "+init=epsg:4326")
mapview(pts, col.regions = "black", cex = 5) + 
  mapview(pts_10, col.regions = "yellow", cex = 5) +
  mapview(pts_50,  col.regions = "red", cex = 5) + 
  mapview(pts_moran, col.regions = "green", cex = 5)

# Vamos ver como isso muda a "temperatura ótima" para a espécie e comparar com
# as distancias anteriores
occ_temp_moran <- extract(v$bio_1, pts_moran, ID = FALSE)[[1]]
occ_temp_moran <- data.frame("Distancia" = "Moran", "Temperatura" = occ_temp_moran)
#Unir todas as informações
occ_temp_all <- rbind(occ_temp_all, occ_temp_moran)
#Converte coluna de distancia para fator - para plot de boxplot
occ_temp_all$Distancia <- as.factor(occ_temp_all$Distancia)
# Plotar boxplot
ggplot(data = occ_temp_all) + geom_boxplot(aes(Distancia, Temperatura, 
                                               fill = Distancia))

#### Filtrar no espaço ambiental ####
?flexsdm::occfilt_env
# it is recommended to use a small number of bins between 2-5 if more than ten 
# variables are used.
# while the greater the number of bins, the greater records retained
occ_filt_env <- occfilt_env(data = occ, 
                            x = "decimalLongitude", y = "decimalLatitude", 
                            id = "index", env_layer = v_cont, nbins = 5)
# Criar coluna com nome da espécie e mudar nomes de colunas de longitude e latitude
occ_filt_env <- occ_filt_env %>% 
  dplyr::select(x = decimalLongitude, y = decimalLatitude) %>% 
  mutate(species = sp, .before = x)

#Comparar número de registros
nrow(occ)
nrow(occ_filt_env)
#Espacializar pontos
pts_filt_env <- vect(occ_filt_env, geom = c(x = "x", #Converte pontos para spatvector
                                      y = "y"), crs = "+init=epsg:4326")
mapview(pts, col.regions = "black", cex = 5) + 
  mapview(pts_moran, col.regions = "green", cex = 5) +
  mapview(pts_filt_env, col.regions = "red", cex = 5)
  
# Vamos ver como isso muda a "temperatura ótima" para a espécie e comparar com
# as distancias anteriores
occ_temp_env <- extract(v$bio_1, pts_filt_env, ID = FALSE)[[1]]
occ_temp_env <- data.frame("Distancia" = "Ambiental", "Temperatura" = occ_temp_env)
#Unir todas as informações
occ_temp_all <- rbind(occ_temp_all, occ_temp_env)
#Converte coluna de distancia para fator - para plot de boxplot
occ_temp_all$Distancia <- as.factor(occ_temp_all$Distancia)
# Plotar boxplot
ggplot(data = occ_temp_all) + geom_boxplot(aes(Distancia, Temperatura, 
                                               fill = Distancia))

#Escolha um dos pontos filtrados para usar no modelo
# Opções disponíveis:
nrow(occ_10) #Distância fixa de 10km
nrow(occ_50) #Distância fixa de 50km
nrow(occ_moran) #Distância que diminui autocorrelação espacial
nrow(occ_filt_env) #Distância fixa no espaço ambiental

#SALVAR CHECKPOINT
fwrite(occ_moran, 
       file.path(sp_dir, "Check_points/D - Pontos_finais.gz"),
       compress = "gzip", row.names = FALSE)

# Repita o procedimento acima com as espécies Boana albomarginata e depois com Hemitriccus kaempferi
# Copie o código acima e cole aqui embaixo (ou em outro script) para repetir
