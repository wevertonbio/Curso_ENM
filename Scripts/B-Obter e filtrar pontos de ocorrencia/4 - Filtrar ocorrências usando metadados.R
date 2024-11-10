#### USANDO METADADOS PARA FILTRAR OCORRÊNCIAS ####

#Artigos sobre importância e estratégias de limpeza de dados:
# https://peerj.com/articles/9916/
# https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13152

#Carregar pacotes
library(data.table) #Importar e salvar tabelas
library(dplyr) #Manipulação de dataframes e uso do %>%
library(mapview) #Para visualizar mapa interativo
library(terra) #Manipulação de dados espaciais
library(stringr) #Manipulação de texto
library(rnaturalearthdata) #Obter mapas do mundo
library(geobr) #Obter mapa de estados do Brasil
source("Scripts/helpers/standardize_country_state.R") #Função para padronizar nomes de países/estados
source("Scripts/helpers/check_country_state.R") #Função para checar pontos em países/estados


# Metadados são as informações associadas as ocorrências (país, ano de coleta, tipo de observação, etc)
# Podemos usar essas informações para remover pontos duvidosos.
# Podemos checar essas informações manualmente, ou automatizar alguns filtros...

#### Araucaria angustifolia ####
#Como sempre, começamos atribuindo o nome da espécie e o diretório a objetos
sp <- "Araucaria angustifolia"
sp_dir <- file.path("Ocorrencias/", sp)
sp_dir

# Vamos criar uma pasta chamada "Check_points", para salvar o resultado de cada etapa...
dir.create(file.path(sp_dir, "Check_points/Removidos"),
           recursive = TRUE)

# Importar registros unidos
occ <- fread(file.path("Ocorrencias/", sp, "Ocorrencias_unidas.gz"))

#Espacializar pontos
pts <- vect(occ, geom = c(x = "decimalLongitude", #Converte pontos para spatvector
                          y = "decimalLatitude"), crs = "+init=epsg:4326")
mapview(pts, #Converte pontos para spatvector
        zcol = "data_source", #Coluna usada para colorir
        burst = TRUE) #Filtrar por valor da coluna

### Remover fósseis
#Tipos de observações
table(occ$basisOfRecord)
pts$basisOfRecord[pts$basisOfRecord == ""] <- NA
#Plotar mapa
mapview(pts, #Converte pontos para spatvector
        zcol = "basisOfRecord", #Coluna usada para colorir
        burst = TRUE) #Filtrar por valor da coluna
#Selecionar fosseis
fossil_id <- which(occ$basisOfRecord %in% c("FOSSIL_SPECIMEN", "FossilSpecimen"))
fossil <- occ[fossil_id, ]
#Salvar
fwrite(fossil,
       file.path(sp_dir, "Check_points/Removidos/Fossil.gz"),
       compress = "gzip", row.names = FALSE)
#Atualizar occ
occ <- occ[-fossil_id, ]

### Remover registros do iNaturalist sem research-grade (não confiáveis)
inat_id <- which(grepl("iNaturalist", occ$datasetName) & #Retorna ID de Inaturalist...
                          !grepl("research-grade", occ$datasetName)) #Sem research grade
# Todos sem research-grade :)
occ %>% filter(grepl("iNaturalist", datasetName)) %>% 
  select(datasetName) %>% View()
#Se precisasse remover...
# inat <- occ[inat_id, ]
# #Salvar
# fwrite(inat,
#        file.path(sp_dir, "Check_points/Removidos/iNaturalist.gz"),
#        compress = "gzip", row.names = FALSE)
# #Atualizar occ
# occ <- occ[-inat_id, ]

### Remover duplicatas completas
# Aqui, vamos considerar scientificName, coordinates, year e country.
#Criar coluna com index para identificar e remover duplicatas
occ$index <- row.names(occ)

#Vamos escolher a ordem de preferência das bases de dados
# Ex: mesmo registro no gbif e specieslink, manter do gbif
unique(occ$data_source)
preferred_order <- c("gbif", "splink")
####Filtrar duplicatas
occ_unique <- occ %>%
  group_by(scientificName, decimalLongitude, decimalLatitude, year, country) %>% #Identificar valores duplicados
  arrange(match(data_source, preferred_order)) %>% #Definir ordem de preferencia de data_source
  filter(row_number() == 1) %>% #Remover duplicados
  ungroup()
#Obter valores duplicados removidos
dup_ind <- setdiff(occ$index, occ_unique$index)
dup_ind
occ_dup <- occ %>% filter(index %in% dup_ind)
pts_dup <- vect(occ_dup, geom = c(x = "decimalLongitude", #Converte pontos para spatvector
                              y = "decimalLatitude"), crs = "+init=epsg:4326")
#Checar pontos
mapview(pts) + mapview(pts_dup, #Converte pontos para spatvector
        zcol = "data_source", #Coluna usada para colorir
        col.regions = c("red", "forestgreen", "yellow"), #Cores
        burst = TRUE) #Filtrar por valor da coluna

#Salvar
fwrite(occ_dup,
       file.path(sp_dir, "Check_points/Removidos/Duplicados.gz"),
       compress = "gzip", row.names = FALSE)
# Atualizar occ
occ <- occ_unique

####Remover pontos de acordo com o ano ####
#Qual ano? Depende dos objetivos, numero de pontos, variáveis ambientais...
#Ex: Dados climáticos do Worldclim 2.1 correspodem aos anos de 1970-2000.
#Ex: Dados climáticos do CHELSA correspodem aos anos de 1981-2010.
#Aqui, vamos usar o ano de 1970 como valor de corte
# E também vamos manter os registros sem ano de coleta especificado
year_ind <- which(occ$year < 1970)
occ_year <- occ[year_ind, ]
nrow(occ_year) #Número de pontos removidos por ano

#Plotar mapa de pontos removidos
pts_year <- vect(occ_year, geom = c(x = "decimalLongitude", #Converte pontos para spatvector
                                    y = "decimalLatitude"), crs = "+init=epsg:4326")
mapview(pts_year, #Converte pontos para spatvector
        zcol = "year") + mapview(pts)

#Salvar
fwrite(occ_year,
       file.path(sp_dir, "Check_points/Removidos/Anteriores_a_1970.gz"),
       compress = "gzip", row.names = FALSE)
# Atualizar occ
occ <- occ[-year_ind, ]

#### Remover registros de individuos cultivados ####
#Usar mesmos termos usados por plantR
cult <- plantR:::cultivated #Cultivados
cult
pastecult <- paste(cult, collapse = "|") #Unir dados em unico character

notcult <- plantR:::notCultivated #Identificadores de não cultivados
notcult
#Adicionar mais identificadores de não cultivados
notcult <- c(notcult, "no plantada", "no cultivada", "not cultivated", "not planted")
pastenotcult <- paste(notcult, collapse = "|") #Unir dados em unico character

#Identificar cultivados
cult_id <- which(grepl(pastecult, occ$occurrenceRemarks, perl = TRUE, #Padrões cult encontrados em occurrenceRemarks
                ignore.case = TRUE) |
                 grepl(pastecult, occ$habitat, perl = TRUE, #Padrões cult encontrados em habitat
                                           ignore.case = TRUE)) %>% 
  unique() #Manter valores unicos
#Identificar não cultivados
notcult_id <- which(grepl(pastenotcult, occ$occurrenceRemarks, perl = TRUE, #Padrões notcult encontrados em occurrenceRemarks
                          ignore.case = TRUE) |
                      grepl(pastenotcult, occ$habitat, perl = TRUE, #Padrões notcult encontrados em habitat
                            ignore.case = TRUE)) %>% 
  unique() #Manter valores unicos
#Remover notcult_id de cult_id
cult_id <- setdiff(cult_id, notcult_id)
length(cult_id) #Numero de individuos provavelmente cultivados
#Subset de individuos cultivados
occ_cult <- occ[cult_id,]
#Checar
occ_cult %>% select(occurrenceRemarks, habitat) %>% View()
#Plotar mapa de pontos removidos
pts_cult <- vect(occ_cult, geom = c(x = "decimalLongitude", #Converte pontos para spatvector
                                    y = "decimalLatitude"), crs = "+init=epsg:4326")
mapview(pts_cult)

#Salvar
fwrite(occ_cult,
       file.path(sp_dir, "Check_points/Removidos/Cultivados.gz"),
       compress = "gzip", row.names = FALSE)
# Atualizar occ
occ <- occ[-cult_id , ]

#### Filtrar registros que caem em países diferentes dos descritos nos metadados
table(occ$country)
# Vamos usar a função standardize_country para padronizar os nomes dos países de 
# acordo com o mapa do mundo do rnaturalearthdata::countries50
w <- vect(rnaturalearthdata::countries50) #Obter mapa do mundo 
#Espacialiar pontos
pts <- vect(occ, geom = c(x = "decimalLongitude", #Converte pontos para spatvector
                          y = "decimalLatitude"), crs = "+init=epsg:4326")
mapview(w, zcol = "admin") + mapview(pts)
# Veja que pontos na Ilha de Guadalupe caem fora da ilha...
# Caso não considere um erro muito grande, podemos usar um raster ao invés de um polígono do mundo
#Importar raster base (de preferencia um raster usado no modelo com preditor)
r <- rast("WorldClim10/wc2.1_10m/wc2.1_10m_bio_1.tif")
plot(r)
w_r <- rasterize(x = w, y = r, field = "admin", touches = TRUE) #rasterizar
plot(w_r)
mapview(w_r) + mapview(w, alpha.regions = 0) + mapview(pts)
# Na função abaixo, ocorrerá o seguinte
# 1 - Serão identificados países com match imperfeito entre os metadados e os países do mapa
# 2 - Função tentará corrigir usando os códigos postais (BR = Brazil, AR = Argentina, etc)
# 3 - Para pontos sem matchs perfeitos e sem códigos postais, função irá usar
# fuzzy match para tentar encontrar país (Brasil = Brazil, Paraguai = Paraguay, etc)
# 4 - Pontos sem fuzzy match terão países extraídos das coordenadas no mapa

#Padronizar dados
occ_country <- standardize_country_state(data = occ, #Tabela com ocorrencias
                                   country_state = "country", #Nome da coluna em occ com os países
                                   new_column = "new_country", #Nome da nova coluna com país corrigido
                                   obs_column = "country_obs", #Nome da coluna com observações
                                   x = "decimalLongitude", #Nome da coluna com longitude
                                   y = "decimalLatitude", #Nome da coluna com latitude
                                   map = w, #Spatvector com mapa do mundo 
                                   map_column = "admin", #Nome da coluna em map com nome do país a ser usado
                                   postal_column = "iso_a2", #Nome da coluna em map com códigos postais
                                   use_raster = TRUE, #Rasterizar polígono do mundo usando raster? (Mais rápido e ajuda a evitar pontos no mar)
                                   r = r, #Raster usado para rasterizar poligono do mapa
                                   verbose = TRUE) #Mostrar mensagens?
table(occ_country$country_obs)
table(occ_country$new_country)

#Checar se correções estão corretas
occ_country %>% select(country, new_country, country_obs) %>% #Seleciona colunas
  distinct() %>% #Valores unicos
  View() #Abrir dataframe

# Por enquanto, apenas corrigimos os países com base nos NOMES dos países
# Veja o país do ponto de Araucaria na Antarctica
pts_country <- vect(occ_country,
                    geom = c(x = "decimalLongitude", #Converte pontos para spatvector
                             y = "decimalLatitude"), crs = "+init=epsg:4326")
mapview(pts_country, zcol = "new_country")

# Agora, vamos verificar se os pontos realmente caem nos países descritos nos metadados
occ_check_country <- check_country_state(data = occ_country, #Tabela com ocorrências
                                   long = "decimalLongitude", #Nome da coluna com longitude
                                   lat = "decimalLatitude", #Nome da coluna com latitude
                                   country = "new_country", #Nome da coluna em occ com os países
                                   new_column = "correct_country", #Nome da nova coluna com resultados do teste
                                   distance = 25, #Distância (em km) de tolerância do ponto ao país
                                   map = w, #Spatvector com mapa do mundo
                                   column_map = "admin") #Nome da coluna em world_map com nome do país a ser usado
table(occ_check_country$correct_country) #Ver países corretos e incorretos
#Plotar pontos
pts_check_country <- vect(occ_check_country,
                    geom = c(x = "decimalLongitude", #Converte pontos para spatvector
                             y = "decimalLatitude"), crs = "+init=epsg:4326")
mapview(pts_check_country, zcol = "correct_country", burst = TRUE)

#Remover pontos de países errados
occ_wrong_country <- occ_check_country %>% filter(!correct_country)
nrow(occ_wrong_country) #Numero de pontos em países incorretos
#Salvar
fwrite(occ_wrong_country,
       file.path(sp_dir, "Check_points/Removidos/País_errado.gz"),
       compress = "gzip", row.names = FALSE)
#Atualizar occ
occ <- occ_check_country %>% filter(correct_country | 
                                      is.na(correct_country))

#Agora, vamos aplicar o mesmo filtro, mas agora usando a coluna com informações
# dos Estados do Brasil
table(occ$stateProvince)
# Importar mapa do brasil
br <- geobr::read_state() %>% 
  vect() #Transformar em SpatVector
br

#Vamos dividir os dados em pontos que caem no Brasil e pontos fora do Brasil
occ_br <- occ %>% filter(new_country == "Brazil")
occ_out_br <- occ %>% filter(new_country != "Brazil")

#Padronizar dados de Estados que caem no Brasil
occ_state <- standardize_country_state(data = occ_br, #Tabela com ocorrencias no Brasil
                                         country_state = "stateProvince", #Nome da coluna em occ com os estados
                                         new_column = "new_state", #Nome da nova coluna com estado corrigido
                                         obs_column = "state_obs", #Nome da coluna com observações
                                         x = "decimalLongitude", #Nome da coluna com longitude
                                         y = "decimalLatitude", #Nome da coluna com latitude
                                         map = br, #Spatvector com mapa dos estados
                                         map_column = "name_state", #Nome da coluna em map com nome do país a ser usado
                                         postal_column = "abbrev_state", #Nome da coluna em map com códigos postais
                                         use_raster = TRUE, #Rasterizar polígono do mundo usando raster? (Mais rápido e ajuda a evitar pontos no mar)
                                         r = r, #Raster usado para rasterizar poligono do mapa
                                         verbose = TRUE) #Mostrar mensagens?
table(occ_state$state_obs)
table(occ_state$new_state)

#Checar se correções estão corretas
occ_state %>% select(stateProvince, new_state, state_obs) %>% #Seleciona colunas
  distinct() %>% #Valores unicos
  View() #Abrir dataframe

# Agora, vamos verificar se os pontos realmente caem nos estados descritos nos metadados
occ_check_state <- check_country_state(data = occ_state, #Tabela com ocorrências
                                         long = "decimalLongitude", #Nome da coluna com longitude
                                         lat = "decimalLatitude", #Nome da coluna com latitude
                                         country = "new_state", #Nome da coluna em occ com os estados
                                         new_column = "correct_state", #Nome da nova coluna com resultados do teste
                                         distance = 20, #Distância (em km) de tolerância do ponto ao estado
                                         map = br, #Spatvector com mapa dos estados
                                         column_map = "name_state") #Nome da coluna em world_map com nome do estado a ser usado

table(occ_check_state$correct_state, useNA = "always") #Ver países corretos e incorretos
#NA são pontos que caem fora do Brasil portanto não sabemos se estão corretos ou incorretos

#Remover pontos de estados errados
occ_wrong_state <- occ_check_state %>% filter(!correct_state)

#Plotar pontos errados
pts_wrong_state <- vect(occ_wrong_state,
                        geom = c(x = "decimalLongitude", #Converte pontos para spatvector
                                 y = "decimalLatitude"), crs = "+init=epsg:4326")

mapview(pts_wrong_state, zcol = "new_state")

# #Salvar
# fwrite(occ_wrong_state,
#        file.path(sp_dir, "Check_points/Removidos/Estado_errado.gz"),
#        compress = "gzip", row.names = FALSE)

#Atualizar occ
occ_ok <- occ_check_state %>% filter(correct_state | is.na(correct_state))

#NÃO ESQUEÇA DE UNIR COM DADOS DE FORA DO BRASIL
occ <- bind_rows(occ_ok, occ_out_br)

#Salvar checkpoint
fwrite(occ,
       file.path(sp_dir, "Check_points/A - Occ_metadados_ok.gz"),
       compress = "gzip", row.names = FALSE)

#Espacializar pontos
pts <- vect(occ, geom = c(x = "decimalLongitude", #Converte pontos para spatvector
                          y = "decimalLatitude"), crs = "+init=epsg:4326")
mapview(pts, #Converte pontos para spatvector
        zcol = "data_source", #Coluna usada para colorir
        burst = TRUE) #Filtrar por valor da coluna

# Repita o procedimento acima com as espécies Boana albomarginata e depois com Hemitriccus kaempferi
# Copie o código acima e cole aqui embaixo para repetir