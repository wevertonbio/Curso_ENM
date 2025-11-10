#### Integrar ocorrências de diferentes bases de dados ####

# Para unir dados de diferentes fontes (ex, GBIF + SpeciesLink + IDigBio + outra bases)
# precisamos que os dataframes tenham as mesmas colunas e as mesmas classes.
# Problema comum com colunas com datas, que podem ser numeric, character ou Date.
# Colunas com informações de local podem ter problemas com caracteres especiais (ç, ã, ó...)

# Remover todos os objetos #
rm(list = ls())

#Carregar pacotes
library(RuHere) 
library(dplyr)
library(data.table)
library(terra)
library(mapview)

# Sempre começamos atribuindo o nome da espécie a um objeto
# Isso facilita construir sempre os mesmos caminhos, mudando apenas o nome da espécie
sp <- "Araucaria angustifolia"
# Diretorio com ocorrencias
sp_dir <- file.path("Ocorrencias/", sp)

# Vamos importar os dados novamente
occ_gbif <- fread(file.path(sp_dir, "GBIF/occ_gbif.gz"))
occ_splink <- fread(file.path(sp_dir, "SpeciesLink/occ_splink.gz"))
occ_idig <- fread(file.path(sp_dir, "iDigBio/occ_idig.gz"))
occ_neotrop <- fread(file.path(sp_dir, "NeotropTree/occ_neotrop.gz"), 
                     encoding = "Latin-1") #Precisa arrumar encoding

#Tentar unir dados
all_occ <- bind_here(occ_gbif, occ_splink, occ_idig, occ_neotrop) #Vai dar erro


# Primeiro, precisamos padronizar as colunas
# Vamos usar a função format_columns do pacote RuHere
# Função funciona automaticamente com gbif, specieslink, bien e idigbio
gbif_standard <- format_columns(occ = occ_gbif,
                                metadata = "gbif")
# Veja a diferença das colunas
colnames(occ_gbif)
colnames(gbif_standard)

# Vamos padronizar os outros data.frames
# SpeciesLink
splink_standard <- format_columns(occ = occ_splink,
                                  metadata = "specieslink")
# Idigbio
idig_standard <- format_columns(occ = occ_idig,
                                metadata = "idigbio")
colnames(occ_idig)
colnames(idig_standard)


# Para o neotropic tree, não temos metadados prontos no pacote
# Os metadados precisam ter esse formato:
# Nomes das colunas com nomes padronizados
# Primeira linha com nomes correspondentes na tabela de ocorrências
View(RuHere::prepared_metadata$gbif)
# Essas são as colunas que devemos encontrar correspondentes na nossa tabela de 
# ocorrencias
colnames(RuHere::prepared_metadata$gbif)
# E essaa são as colunas da nossa tabela
colnames(occ_neotrop)

# Vamos usar a função create_metadata para criar essa tabela de referencia
neot_metadata <- create_metadata(scientificName = "species", 
                                 decimalLongitude = "decimalLongitude",
                                 decimalLatitude = "decimalLatitude",
                                 elevation = "altitude(m)", 
                                 country = "country", stateProvince = "state", 
                                 locality = "sitename", 
                                 habitat = "vegetation type")
View(neot_metadata)
# Vamos padronizar nossos dados do neotropictree
neotrop_standard <- format_columns(occ =occ_neotrop,
                                     metadata = neot_metadata, 
                                     binomial_from = "species",
                                     data_source = "NeotropTree")
# Agora, podemos unir
all_occ <- bind_here(gbif_standard, splink_standard, idig_standard, 
                     neotrop_standard)
# Ver numero de registros por base de dados
table(all_occ$data_source)


# Salvar dados
fwrite(all_occ, file.path(sp_dir, "1-Ocorrencias_unidas.gz"), 
       encoding = "UTF-8")

# Espacializar
pts_unidos <- spatialize(all_occ)

# Plot interativo
mapview(pts_unidos,
        zcol = "data_source", # Uma cor para cada data_source
        burst = TRUE) #Tratar cada zcol como uma camada individual

