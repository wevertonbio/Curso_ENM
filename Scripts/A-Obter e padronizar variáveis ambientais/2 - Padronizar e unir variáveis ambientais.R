#### PADRONIZAÇÃO DE VARIÁVEIS AMBIENTAIS ####

# MAIOR PROBLEMA: variáveis de diferentes fontes tem diferentes resoluções e extensões

# Resolução é o tamanho do pixel:
# - 30arc-sec = 0.00833333º = ~ 1km x 1km
# - 2.5arc-min = 0.0416667º = ~ 4.6km x 4.6km
# - 5arc-min = 0.0833333º = ~ 9km x 9km
# - 10arc-min = 0.166667º = ~ 18.5km x 18.5km

# O R costuma mostrar resolução em graus.

# Extensão corresponde aos limites máximos e mínimo de longitude e latitude

# Variáveis precisam estar EXATAMENTE na mesma resolução e extensão

# Funções do terra para padronizar variáveis:
# - crop(mask = TRUE) para cortar variáveis
# - aggregate() para agrupar pixels e diminuir resolução (pixel maior)
# - resample() para diminuir resolução e garantir que variáveis tenham mesma extensão
# - project() para garantir que variaveis tenham mesmo sistema de coordenadas.

# Carregar pacotes
library(dplyr) #Manipulação de dataframes e uso do %>%
library(mapview) #Para visualizar mapa interativo
library(terra) #Manipulação de dados espaciais
library(sf) #Manipulação de dados espaciais. Será usada função gdal_utils

# Primeiros, vamos cortar as variáveis para uma região do mundo que com certeza 
# engloba o M de todas as espécies
#Nesse caso, vamos usar a região neotropical como inicial
neot <- vect("Shapefiles/Neotropical.gpkg")
mapview(neot)
#Para garantir, vamos criar um buffer de 10 km ao redor dos neotropicos
neot <- buffer(neot, width = 10 * 1000)
mapview(neot)

#Agora, vamos padronizando as variaveis de cada fonte, começando pelas do 
# Worldclim, que servirão de base para padronizar todas as outras

#### Worldclim ####
wc_files <- list.files("Variaveis_brutas/wc2.1_5m/", full.names = TRUE) #Listar arquivos na pasta
wc_files
wc <- rast(wc_files) #Importar arquivos
wc
#Ver nomes das variáveis
names(wc)
#Renomear variáveis
# IMPORTANTE: AO PROJETAR PARA OUTROS CENÁRIOS/TEMPOS, VARIÁVEIS DEVEM TER O MESMO NOME!
names(wc) <- gsub("wc2.1_5m_", "", names(wc))
names(wc) #Novos nomes

#Cortar variáveis para o Neotropico
wc_neot <- crop(wc, neot, mask = TRUE)
plot(wc_neot$bio_1)
plot(wc_neot$bio_12)

#### SoilGrids ####
#Soil grids temos dois tipos de variáveis:
# Variáveis tif (clay e sand)
# Variavel vrt (most probable soil type)
# Vamos trabalhar com elas separadamente
soil <- list.files("Variaveis_brutas/SoilGrids/", full.names = TRUE,
                   pattern = ".tif") #Apenas variáveis .tiff
soil #Ver arquivos
#Remover arquivos aux.xml da lista
soil <- soil[!grepl("aux.xml|SoilType", soil)]
soil
soil <- rast(soil)
# Tentar unir dados de solo com worldclim
wc_soil <- c(soil, wc_neot) #Erro esperado

#Compare a projeção de soil com do worldclim
crs(wc_neot) # WGS84 :)
crs(soil) # Homolosine :(
res(wc_neot) #0.083 graus :)
res(soil) #5000 metros :(
#Vamos reprojetar o raster de solo para wgs84, usando o wc_neot como base
soil <- project(soil, wc_neot$bio_1, 
                method = "bilinear")
soil
#Ver projeção e resolução
crs(soil) # WGS84 :)
res(soil) #0.083 graus :)
plot(soil)

#Vamos renomear as variáveis
names(soil)
names(soil) <- c("clay", "sand") #Novos nomes

#Perceba que as variáveis de solo possuem alguns "buracos" (NA)
mapview(soil$clay)
#Podemos preencher esses buracos usando informações de pixels ao redor
new_soil <- focal(soil,
                  w = 3, #Tamanho da janela de preenchimento
                  fun = "mean", #Função para preencher NA
                  na.policy="only") #Preencher somente NA
mapview(new_soil$clay) #Melhorou...
#Agora, vamos cortar o raster
soil_neot <- crop(new_soil, neot, mask = TRUE)
plot(soil_neot)
#Tentar unir dados de solo e do worldclim
wc_soil <- c(wc_neot, soil_neot)
names(wc_soil) #Ver variáveis

#Agora, vamos padronizar a variável de tipo de solo
soiltype <- rast("Variaveis_brutas/SoilGrids/MostProbable.vrt")
soiltype
plot(soiltype) #Tentar plotar...
# Vai dar erro!

# Variaveis vrt são variáveis virtuais: os arquivos não estão no seu computador,
# mas sim, na internet
# Vamos usar a função gdal_utils para construir essa raster virtual no seu 
# computador
# Convertendo o VRT para TIFF
gdal_utils(
  util = "translate", #
  source = "Variaveis_brutas/SoilGrids/MostProbable.vrt", #Caminho do arquivo vrt
  destination = "Variaveis_brutas/SoilGrids/SoilType.tif", #Caminho do arquivo de saída
  options = c(
    "-of", "GTiff",  # Define o formato de saída como GeoTIFF
    "-tr", "0.08333333", "0.08333333",  # Define a resolução para 0.08333333 graus
    "-r", "near", # Define o método de resampling como nearest neighbor
    "-co", "COMPRESS=LZW" #Define o método de compressão
  ))
#Agora, vamos importar o arquivo correto gerado
soiltype <- rast("Variaveis_brutas/SoilGrids/SoilType.tif")
plot(soiltype)
#Ver projeção e resolução
crs(soiltype) # WGS84 :)
res(soiltype) #0.083 graus :)

#Renomear variável
names(soiltype) <- "soilType"

#Cortar variáveil para neotropico
soiltype_neot <- crop(soiltype, neot, mask = TRUE)
mapview(soiltype_neot) #ver mapa

#Preencher buracos usando informações de pixels ao redor
new_soiltype <- focal(soiltype_neot,
                  w = 3, #Tamanho da janela de preenchimento
                  fun = "modal", #Função para preencher NA - Modal é melhor para categoricos
                  na.policy="only") #Preencher somente NA
mapview(new_soiltype) #Melhorou
#Agora, vamos cortar o raster
soiltype_neot <- crop(new_soiltype, neot, mask = TRUE)
plot(soiltype_neot)
#Tentar unir com dados anteriores
wc_soil_type <- c(wc_soil, soiltype_neot) #Vai dar erro de novo!
#Erro porque variáveis possuem diferentes extents
ext(wc_soil) == ext(soiltype_neot) #São iguais?
ext(wc_soil)
ext(soiltype_neot)

#Vamos usar a função resample para garantir que ambos tem o mesmo ext
soiltype_res <- resample(soiltype_neot, wc_soil,
                         method = "mode") #Moda porque é categórico
plot(soiltype_res)
#Arrumar nome
names(soiltype_res)
names(soiltype_res) <- "soilType"
#Tentar unir com dados anteriores
wc_soil_type <- c(wc_soil,
                  soiltype_res) #Agora, é soiltype resampled
names(wc_soil_type)

#### Topografia - EARTHENV ####
slope <- rast("Variaveis_brutas/Earthenv_topo/slope_5KMmn_GMTEDmd.tif")
#Ver projeção e resolução
crs(slope) # WGS84 :)
res(slope) #0.041 graus :(  Precisamos fazer um resample
# Resample (já corta para mesma area do wc_soil)
slope_res <- resample(slope, wc_soil,
                      method = "average") #Média porque é continua
plot(slope_res)
#Renomear
names(slope_res)
names(slope_res) <- "slope"
#Tentar unir
var_final <- c(wc_soil_type, slope_res)
names(var_final)

#Salvar variáveis finais
#Criar diretorio para salvar variaveis processadas
dir.create("Variaveis_Neotropico/Presente", recursive = TRUE)
writeRaster(var_final, "Variaveis_Neotropico/Presente/Variaveis.tiff",
            overwrite = T)

#Testar se deu certo
# rm(list = ls()) #Limpar objetos
v <- rast("Variaveis_Neotropico/Presente/Variaveis.tiff")
v
names(v) #Nomes das variaveis
