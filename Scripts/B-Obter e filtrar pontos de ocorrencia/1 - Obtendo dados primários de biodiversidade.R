# Aquisição e preparo de dados primários de biodiversidade ####

#Carregar pacotes
library(RuHere) 
library(dplyr)
library(data.table)
library(terra)
library(mapview)

#### SALVANDO CHAVES DE ACESSO ####

# Tanto o GBIF quanto o specieslink pedem chaves de acesso para baixar registros
# Vamos começar pelo GBIF
# Acesse (ou crie) sua conta no GBIF: https://www.gbif.org/
# Acesse: https://www.gbif.org/user/profile para ver seu email e nome de usuário

# Vamos salvar permanentemente a chave do GBIF direto no nosso ambiente do R
# Substitua as informações abaixo pelas suas informações
set_gbif_credentials(gbif_username = "my_username",
                     gbif_email = "my_email@example.com",
                     gbif_password = "my_password")
# Veja se salvou no seu ambiente do R
usethis::edit_r_environ()
# A modificação é permanente, você só precisa fazer isso uma vez :)

# Agora, vamos obter a chave do specieslink
#Crie uma conta no SpeciesLink: https://specieslink.net/
#Acesse sua chave: https://specieslink.net/aut/profile/apikeys
# Vamos salvar permanentemente a chave do specieslink direto no nosso ambiente do R
# Substitua as informações abaixo pelas suas informações
set_specieslink_credentials(specieslink_key = "my_key")
# Veja se salvou no seu ambiente do R
usethis::edit_r_environ()
# A modificação é permanente, você só precisa fazer isso uma vez :)

#### DOWNLOAD DE REGISTROS DE OCORRÊNCIA ####

#### Download de registros do GBIF ####

#Podemos baixar ocorrencias direto do gbif:
# Acesse: https://www.gbif.org/species/2684940


# Vamos baixar pelo R

# DICA: CRIE UM OBJETO COM O NOME DA ESPÉCIE!
# Esse objeto sera usado para criar pastas e baixar as ocorrências
sp <- "Araucaria angustifolia"
#Criar diretório para salvar ocorrencias
sp_dir <- file.path("Ocorrencias", sp)
sp_dir
dir.create(sp_dir, recursive = TRUE)

# Criar diretorio para salvar dados do GBIF
dir.create(file.path(sp_dir, "GBIF"))

# Primeiro, precisamos checar chave (key) da espécie no gbif
gbif_prepared <- prepare_gbif_download(species = "Araucaria angustifolia")
# Abra o arquivo e veja se as informações estão corretas

# Agora, vamos fazer o pedido para o GBIF 🥺🙏
gbif_request <- request_gbif(gbif_info = gbif_prepared, #Output da função anterior
                             hasCoordinate = TRUE, #Apenas com coordenadas
                             hasGeospatialIssue = FALSE) #Sem problemas geoespaciais
gbif_request #Chave do pedido
# Podemos acompanhar o pedido usando:
rgbif::occ_download_wait(gbif_request)
# Precisamos aguardar o pedido ser atendido (uns 15minutos)
# Enquanto isso, vamos baixar do specieslink
# Clique em stop ou tecle ESC para interromper a função anterior
# Vamos salvar as informações e a chave do pedido
fwrite(gbif_prepared, 
       file.path(sp_dir, "GBIF", "gbif_prepared.gz"))
saveRDS(gbif_request, 
        file.path(sp_dir, "GBIF/gbif_request.rds"))

# Voltamos aqui em alguns instantes...
# Se necessário, importe a chave do pedido novamente
gbif_request <- readRDS(file.path(sp_dir, "GBIF/gbif_request.rds"))

# Acompanhar pedido novamente
rgbif::occ_download_wait(gbif_request)

# Após succeeded, podemos importar pro R
# Vamos importar o data.frame completo, com todas as 226 colunas
occ_gbif_completo <- import_gbif(gbif_request, # Output de request_gbif
                                 select_columns = FALSE) # Retornar todas as colunas
colnames(occ_gbif_completo)

# Vamos importar apenas as colunas úteis para filtrar os registros
occ_gbif <- import_gbif(gbif_request, # Output de request_gbif
                        select_columns = TRUE) # Retornar apenas algumas colunas
colnames(occ_gbif)

# Salvar registros
fwrite(occ_gbif,
       file.path(sp_dir, "GBIF/occ_gbif.gz"))

#### Download de registros do speciesLink ####
# Download direto pelo site
# https://specieslink.net/search/

#Carregar pacotes

#Criar diretório para salvar ocorrencias do specieslink
sp_dir
dir.create(file.path(sp_dir, "SpeciesLink"), recursive = TRUE)

#Download de ocorrencias do specieslink
occ_splink <- get_specieslink(species = sp, 
                              limit = 10000) #Numero máximo de registros para retornar

#Salvar registros
# Podemos salvar em dois formatos:
# CSV (separado por virgula) - Abre com Excel
write.csv(occ_splink,
          file.path(sp_dir, "SpeciesLink/occ_splink.csv"))

# Ou podemos salvar num formato comprimido, muito mais leve (mas que só abre no R)
fwrite(occ_splink,
       file.path(sp_dir, "SpeciesLink/occ_splink.gz"),
       compress = "gzip", row.names = FALSE)
# Veja a diferença de tamanho dos arquivos na pasta (em kb)
file.info(file.path(sp_dir, "SpeciesLink/occ_splink.csv"))$size /1000
file.info(file.path(sp_dir, "SpeciesLink/occ_splink.gz"))$size /1000

# Importar novamente o arquivo gz
occ_splink <- fread(file.path(sp_dir, "SpeciesLink/occ_splink.gz"))

# Vamos voltar para o GBIF...

#### Espacializar e comparar GBIF com SpeciesLink ####
# GBIF
pts_gbif <- spatialize(occ_gbif) #Espacializar
plot(pts_gbif) #Plot estático
plet(pts_gbif) #Plot interativo do terra
mapview(pts_gbif) #Plot interativo do mapview

# SpeciesLink
#Espacializar
pts_splink <- spatialize(occ_splink, 
                         long = "decimallongitude",lat = "decimallatitude")
mapview(pts_splink)
# Ver os dois
mapview(pts_gbif, col.regions = "red", layer.name = "GBIF") +
  mapview(pts_splink, col.regions = "blue", layer.name = "SpeciesLink")

#### Download de registros do BIEN ####
# Botanical Information and Ecology Network Database
# https://bien.nceas.ucsb.edu/bien/
occ_bien <- get_bien(species = sp) #Demora um pouco...
# Porém, veja informações de local e coordenadas...

# Apenas como exemplo, veja esse exemplo com outra espécie
occ_paubrasil <- get_bien(species = "Paubrasilia echinata")
# Veja que quase todos os registros são do GBIF

# Caso queira salvar...
dir.create(file.path(sp_dir, "BIEN"))
fwrite(occ_bien, 
       file.path(sp_dir, "BIEN/occ_bien.gz"))


#### Download de registros do iDigBio ####
# https://www.idigbio.org/
occ_idig <- get_idigbio(species = sp)
occ_idig <- occ_idig %>%  #(PRECISO ARRUMAR NO PACOTE)
  select("scientificname", "collectioncode", 
         "catalognumber", "lon", "lat", "coordinateuncertainty", 
         "country", "stateprovince", "municipality", "locality", "maxelevation",  
         "datecollected", "collector", "basisofrecord", "datasetid")

# Salvar
dir.create(file.path(sp_dir, "iDigBio"))
fwrite(occ_idig, 
       file.path(sp_dir, "iDigBio/occ_idig.gz"))


# Vamos plotar as ocorrencias do IdigBio
pts_idig <- spatialize(occ_idig, long = "lon", lat = "lat")
mapview(pts_gbif, col.regions = "red", layer.name = "GBIF") +
  mapview(pts_splink, col.regions = "blue", layer.name = "SpeciesLink") +
  mapview(pts_idig, col.regions = "orange", layer.name = "IdigBio")

# Quase todos os do IdigBio são do GBIF

#### Dados de outras fontes - Exemplo: NeotropTree ####
# http://www.neotroptree.info/

# Importar dados
neotrop <- fread("DataPapers/NeotropicTree.gz", 
                 encoding = "Latin-1")
# Subset da espécie
occ_neotrop <- neotrop %>% filter(species == sp)

# Vamos salvar
dir.create(file.path(sp_dir, "NeotropTree"))
fwrite(occ_neotrop, 
       file.path(sp_dir, "NeotropTree/occ_neotrop.gz"))


# Vamos plotar as ocorrencias do neotropic tree
#Espacializar
pts_neotrop <- spatialize(occ_neotrop, 
                          long = "decimalLongitude", lat = "decimalLatitude") 
mapview(pts_gbif, col.regions = "red", layer.name = "GBIF") +
  mapview(pts_splink, col.regions = "blue", layer.name = "SpeciesLink") +
  mapview(pts_idig, col.regions = "orange", layer.name = "IdigBio") +
  mapview(pts_neotrop, col.regions = "forestgreen", layer.name = "NeotropTree")

