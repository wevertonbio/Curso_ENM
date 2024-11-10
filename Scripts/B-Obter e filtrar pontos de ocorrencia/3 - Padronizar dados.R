#### PADRONIZAR E UNIR DADOS DE DIFERENTES FONTES ####
# Artigo mostrando importância de usar várias fontes:
# https://link.springer.com/article/10.1007/s10531-022-02458-x

library(data.table) #Importar e salvar tabelas
library(dplyr) #Manipulação de dataframes e uso do %>%
library(mapview) #Para visualizar mapa interativo
library(terra) #Manipulação de dados espaciais
source("Scripts/helpers/fix_columns.R") #Corrigir e padronizar colunas

# Para unir dados de diferentes fontes (ex, GBIF + SpeciesLink + Datapaper), precisamos que os dataframes tenham as mesmas colunas e as mesmas classes.
# Problema comum com colunas com datas, que podem ser numeric, character ou Date.

#Identificar colunas numericas
nc <- c("decimalLongitude", "decimalLatitude",
        "coordinateUncertaintyInMeters", "elevation", "year")
#Identificar colunas para checar encoding (problemas com caracteres especiais)
c_encod <- c("collectionCode", "catalogNumber",
             "country", "stateProvince", "municipality",
             "locality", "eventDate", "recordedBy", "identifiedBy",
             "basisOfRecord", "datasetName")

#### Araucaria angustifolia ####

# Unir dados
sp <- "Araucaria angustifolia"
sp_dir <- file.path("Ocorrencias/", sp)
sp_dir

#### PADRONIZAR GBIF
gbif_occ <- fread(file.path(sp_dir, "GBIF/Ocorrencias.gz"))
colnames(gbif_occ)

# Criar dataframe com metadados identificando as colunas
gbif_metadata <- data.frame(scientificName = "acceptedScientificName", #Nome científico
                            occurrenceID = "gbifID", #ID da ocorrência
                            collectionCode = "collectionCode",
                            catalogNumber = "catalogNumber",
                            decimalLongitude = "decimalLongitude", #Longitude
                            decimalLatitude = "decimalLatitude", #Latitude
                            coordinateUncertaintyInMeters = "coordinateUncertaintyInMeters", #Incerteza
                            elevation = "elevation", #Altitude
                            country = "countryCode", #País
                            stateProvince = "stateProvince", #Estado
                            municipality = "municipality", #Município
                            locality = "locality", #Local
                            year = "year", #Ano
                            eventDate = "eventDate", #Data de coleta
                            recordedBy = "recordedBy", #Coletor
                            identifiedBy = "identifiedBy", #Identificador
                            basisOfRecord = "basisOfRecord", #Tipo de registro (Preserved specimen, human observation, etc)
                            occurrenceRemarks = "occurrenceRemarks", #Observações sobre ocorrencia
                            habitat = "habitat", #Habitat onde foi encontrado
                            datasetName = "datasetName", #Origem dos dados (ex: herbário X)
                            datasetKey ="datasetKey", #Chave da origem dos dados
                            key = "speciesKey") #Chave da espécie no GBIF
#Corrigir colunas
gbif_fixed <- fix_columns(data = gbif_occ, #Tabela com ocorrencias
                          numeric_columns = nc, #Colunas que devem ser numericas
                          check_encoding = c_encod, #Colunas pare checar encoding
                          metadata = gbif_metadata, #Metadados com colunas
                          data_source = "gbif") #Fonte dos dados
#### Padronizar speciesLink
splink_occ <- fread(file.path(sp_dir, "SpeciesLink/Ocorrencias.gz"))
colnames(splink_occ)
#Criar coluna com occurrenceID
splink_occ$ocurrenceID <- 1:nrow(splink_occ)

# Criar dataframe com metadados identificando as colunas
splink_metadata <- data.frame(scientificName = "scientificname", #Nome científico
                              occurrenceID = "ocurrenceID", #ID da ocorrência
                              collectionCode = "collectioncode",
                              catalogNumber = "catalognumber",
                              decimalLongitude = "decimallongitude", #Longitude
                              decimalLatitude = "decimallatitude", #Latitude
                              coordinateUncertaintyInMeters = NA, #Incerteza da coordenada
                              elevation = "maximumelevationinmeters", #Altitude
                              country = "country", #País
                              stateProvince = "stateprovince", #Estado
                              municipality = "county", #Município
                              locality = "locality", #Local
                              year = "yearcollected", #Ano
                              eventDate = NA, #Data de coleta
                              recordedBy = "recordedby", #Coletor
                              identifiedBy = "identifiedby", #Identificador
                              basisOfRecord = "basisofrecord", #Tipo de registro (Preserved specimen, human observation, etc)
                              occurrenceRemarks = "occurrenceremarks", #Observações sobre ocorrencia
                              habitat = NA, #Habitat onde foi encontrado
                              datasetName = NA, #Origem dos dados (ex: herbário X)
                              datasetKey = NA, #Chave da origem dos dados
                              key = NA) #Chave da espécie no GBIF

#Corrigir colunas
splink_fixed <- fix_columns(data = splink_occ, #Tabela com ocorrencias
                          numeric_columns = nc, #Colunas que devem ser numericas
                          check_encoding = c_encod, #Colunas pare checar encoding
                          metadata = splink_metadata, #Metadados com colunas
                          data_source = "splink") #Fonte dos dados

#### Extraindo dados de datapapers
#Neotropictree: https://data.mendeley.com/datasets/jmv8bn8fwc/2
nt <- fread("DataPapers/NeotropicTree.gz", encoding = "Latin-1")
#Extrair apenas Araucaria angustifolia
nt <- nt %>% filter(species == sp)
#Criar colunas
nt$occurrenceID <- 1:nrow(nt)
nt$coordinateUncertaintyInMeters <- 5000 #Neotropictree is a checklist of species in each site, with each site being a circular area with a radius of 5 km
nt$year <- 2017
# Criar dataframe com metadados identificando as colunas
nt_metadata <- data.frame(scientificName = "species",
                          occurrenceID = "occurrenceID",
                          collectionCode = NA,
                          catalogNumber = NA,
                          decimalLongitude = "decimalLongitude",
                          decimalLatitude = "decimalLatitude",
                          coordinateUncertaintyInMeters = "coordinateUncertaintyInMeters",
                          elevation = "altitude(m)",
                          country = "country",
                          stateProvince = "state",
                          municipality = NA,
                          locality = "sitename",
                          year = "year",
                          eventDate = NA,
                          recordedBy = NA,
                          identifiedBy = NA,
                          basisOfRecord = NA,
                          occurrenceRemarks = NA, #Observações sobre ocorrencia
                          habitat = NA, #Habitat onde foi encontrado
                          datasetName = NA,
                          datasetKey = NA,
                          key = NA)
#Corrigir colunas
nt_fixed <- fix_columns(data = nt, #Tabela com ocorrencias
                        numeric_columns = nc, #Colunas que devem ser numericas
                        check_encoding = c_encod, #Colunas pare checar encoding
                        metadata = nt_metadata, #Metadados com colunas
                        data_source = "neotropicTree") #Fonte dos dados
#Unir informações
all_occ <- list(gbif_fixed, splink_fixed, nt_fixed) #Colocar dataframes em lista
all_occ <- rbindlist(all_occ) #Unir dataframes
table(all_occ$data_source) #Numero de registros por base
table(all_occ$scientificName) #Ver espécies
#Obter nome binomial
all_occ$scientificName <- florabr::get_binomial(all_occ$scientificName,
                                                include_subspecies = FALSE,
                                                include_variety = FALSE)
table(all_occ$scientificName) #Ver espécies
#Garantir que tem apenas espécies que estamos interessados
all_occ <- all_occ %>% filter(scientificName == sp)
table(all_occ$scientificName) #Ver espécies


#Salvar registros
fwrite(all_occ,
       file.path(sp_dir, "Ocorrencias_unidas.gz"),
       compress = "gzip", row.names = FALSE,
       encoding = "UTF-8") #Garantir encoding correto

#Plotar registros
pts <- vect(all_occ, geom = c(x = "decimalLongitude", #Converte pontos para spatvector
                              y = "decimalLatitude"), crs = "+init=epsg:4326")
mapview(pts,
        zcol = "data_source", #Coluna usada para colorir
        burst = TRUE) #Filtrar por valor da coluna

#Data papers da Mata Atlântica:
# https://esajournals.onlinelibrary.wiley.com/doi/toc/10.1002/(ISSN)1939-9170.AtlanticPapers


# Repita o procedimento acima com as espécies Boana albomarginata e depois com Hemitriccus kaempferi
# Copie o código acima e cole aqui embaixo para repetir
# Essas espécies não tem datapaper, apenas gbif e specieslink



#### Boana albomarginata ####

# Unir dados
sp <- "Boana albomarginata"
sp_dir <- file.path("Ocorrencias/", sp)
sp_dir

#### PADRONIZAR GBIF
gbif_occ <- fread(file.path(sp_dir, "GBIF/Ocorrencias.gz"))
colnames(gbif_occ)

# Criar dataframe com metadados identificando as colunas
gbif_metadata <- data.frame(scientificName = "acceptedScientificName", #Nome científico
                            occurrenceID = "gbifID", #ID da ocorrência
                            collectionCode = "collectionCode",
                            catalogNumber = "catalogNumber",
                            decimalLongitude = "decimalLongitude", #Longitude
                            decimalLatitude = "decimalLatitude", #Latitude
                            coordinateUncertaintyInMeters = "coordinateUncertaintyInMeters", #Incerteza
                            elevation = "elevation", #Altitude
                            country = "countryCode", #País
                            stateProvince = "stateProvince", #Estado
                            municipality = "municipality", #Município
                            locality = "locality", #Local
                            year = "year", #Ano
                            eventDate = "eventDate", #Data de coleta
                            recordedBy = "recordedBy", #Coletor
                            identifiedBy = "identifiedBy", #Identificador
                            basisOfRecord = "basisOfRecord", #Tipo de registro (Preserved specimen, human observation, etc)
                            occurrenceRemarks = "occurrenceRemarks", #Observações sobre ocorrencia
                            habitat = "habitat", #Habitat onde foi encontrado
                            datasetName = "datasetName", #Origem dos dados (ex: herbário X)
                            datasetKey ="datasetKey", #Chave da origem dos dados
                            key = "speciesKey") #Chave da espécie no GBIF
#Corrigir colunas
gbif_fixed <- fix_columns(data = gbif_occ, #Tabela com ocorrencias
                          numeric_columns = nc, #Colunas que devem ser numericas
                          check_encoding = c_encod, #Colunas pare checar encoding
                          metadata = gbif_metadata, #Metadados com colunas
                          data_source = "gbif") #Fonte dos dados

#### Padronizar speciesLink
splink_occ <- fread(file.path(sp_dir, "SpeciesLink/Ocorrencias.gz"))
colnames(splink_occ)
#Criar coluna com occurrenceID
splink_occ$ocurrenceID <- 1:nrow(splink_occ)

# Criar dataframe com metadados identificando as colunas
splink_metadata <- data.frame(scientificName = "scientificname", #Nome científico
                              occurrenceID = "ocurrenceID", #ID da ocorrência
                              collectionCode = "collectioncode",
                              catalogNumber = "catalognumber",
                              decimalLongitude = "decimallongitude", #Longitude
                              decimalLatitude = "decimallatitude", #Latitude
                              coordinateUncertaintyInMeters = NA, #Incerteza da coordenada
                              elevation = "maximumelevationinmeters", #Altitude
                              country = "country", #País
                              stateProvince = "stateprovince", #Estado
                              municipality = "county", #Município
                              locality = "locality", #Local
                              year = "yearcollected", #Ano
                              eventDate = NA, #Data de coleta
                              recordedBy = "recordedby", #Coletor
                              identifiedBy = "identifiedby", #Identificador
                              basisOfRecord = "basisofrecord", #Tipo de registro (Preserved specimen, human observation, etc)
                              occurrenceRemarks = "occurrenceremarks", #Observações sobre ocorrencia
                              habitat = NA, #Habitat onde foi encontrado
                              datasetName = NA, #Origem dos dados (ex: herbário X)
                              datasetKey = NA, #Chave da origem dos dados
                              key = NA) #Chave da espécie no GBIF

#Corrigir colunas
splink_fixed <- fix_columns(data = splink_occ, #Tabela com ocorrencias
                            numeric_columns = nc, #Colunas que devem ser numericas
                            check_encoding = c_encod, #Colunas pare checar encoding
                            metadata = splink_metadata, #Metadados com colunas
                            data_source = "splink") #Fonte dos dados

#Unir informações
all_occ <- list(gbif_fixed, splink_fixed) #Colocar dataframes em lista
all_occ <- rbindlist(all_occ) #Unir dataframes
table(all_occ$data_source) #Numero de registros por base
table(all_occ$scientificName) #Ver espécies
#Obter nome binomial
all_occ$scientificName <- florabr::get_binomial(all_occ$scientificName,
                                                include_subspecies = FALSE,
                                                include_variety = FALSE)
table(all_occ$scientificName) #Ver espécies
#Garantir que tem apenas espécies que estamos interessados
all_occ <- all_occ %>% filter(scientificName == sp)
table(all_occ$scientificName) #Ver espécies

#Salvar registros
fwrite(all_occ,
       file.path(sp_dir, "Ocorrencias_unidas.gz"),
       compress = "gzip", row.names = FALSE,
       encoding = "UTF-8") #Garantir encoding correto

#Plotar registros
pts <- vect(all_occ, geom = c(x = "decimalLongitude", #Converte pontos para spatvector
                              y = "decimalLatitude"), crs = "+init=epsg:4326")
mapview(pts,
        zcol = "data_source", #Coluna usada para colorir
        burst = TRUE) #Filtrar por valor da coluna

#### Hemitriccus kaempferi ####

# Unir dados
sp <- "Hemitriccus kaempferi"
sp_dir <- file.path("Ocorrencias/", sp)
sp_dir

#### PADRONIZAR GBIF
gbif_occ <- fread(file.path(sp_dir, "GBIF/Ocorrencias.gz"))
colnames(gbif_occ)

# Criar dataframe com metadados identificando as colunas
gbif_metadata <- data.frame(scientificName = "acceptedScientificName", #Nome científico
                            occurrenceID = "gbifID", #ID da ocorrência
                            collectionCode = "collectionCode",
                            catalogNumber = "catalogNumber",
                            decimalLongitude = "decimalLongitude", #Longitude
                            decimalLatitude = "decimalLatitude", #Latitude
                            coordinateUncertaintyInMeters = "coordinateUncertaintyInMeters", #Incerteza
                            elevation = "elevation", #Altitude
                            country = "countryCode", #País
                            stateProvince = "stateProvince", #Estado
                            municipality = "municipality", #Município
                            locality = "locality", #Local
                            year = "year", #Ano
                            eventDate = "eventDate", #Data de coleta
                            recordedBy = "recordedBy", #Coletor
                            identifiedBy = "identifiedBy", #Identificador
                            basisOfRecord = "basisOfRecord", #Tipo de registro (Preserved specimen, human observation, etc)
                            occurrenceRemarks = "occurrenceRemarks", #Observações sobre ocorrencia
                            habitat = "habitat", #Habitat onde foi encontrado
                            datasetName = "datasetName", #Origem dos dados (ex: herbário X)
                            datasetKey ="datasetKey", #Chave da origem dos dados
                            key = "speciesKey") #Chave da espécie no GBIF
#Corrigir colunas
gbif_fixed <- fix_columns(data = gbif_occ, #Tabela com ocorrencias
                          numeric_columns = nc, #Colunas que devem ser numericas
                          check_encoding = c_encod, #Colunas pare checar encoding
                          metadata = gbif_metadata, #Metadados com colunas
                          data_source = "gbif") #Fonte dos dados

table(gbif_fixed$scientificName) #Ver espécies
#Obter nome binomial
gbif_fixed$scientificName <- florabr::get_binomial(gbif_fixed$scientificName,
                                                include_subspecies = FALSE,
                                                include_variety = FALSE)
table(gbif_fixed$scientificName) #Ver espécies
#Garantir que tem apenas espécies que estamos interessados
gbif_fixed <- gbif_fixed %>% filter(scientificName == sp)
table(gbif_fixed$scientificName) #Ver espécies

#Salvar registros
fwrite(gbif_fixed,
       file.path(sp_dir, "Ocorrencias_unidas.gz"),
       compress = "gzip", row.names = FALSE,
       encoding = "UTF-8") #Garantir encoding correto

#Plotar registros
pts <- vect(gbif_fixed, geom = c(x = "decimalLongitude", #Converte pontos para spatvector
                              y = "decimalLatitude"), crs = "+init=epsg:4326")
mapview(pts, #Converte pontos para spatvector
        zcol = "data_source", #Coluna usada para colorir
        burst = TRUE) #Filtrar por valor da coluna
