#### DOWNLOAD DE REGISTROS DE OCORRÊNCIA DO SpeciesLink ####
#Carregar pacotes
library(plantR) #pacote para baixar dados do specieslink
#Use a versão atualizada do pacote: remotes::install_github("jaum20/plantR")
library(data.table) #Importar e salvar tabelas

#Crie uma conta no SpeciesLink: https://specieslink.net/
#Acesse sua chave: https://specieslink.net/aut/profile/apikeys
my_key <- ""

# DICA: CRIE UM OBJETO COM O NOME DA ESPÉCIE!
# Esse objeto sera usado para criar pastas e baixar as ocorrências
sp <- "Araucaria angustifolia"

#Criar diretório para salvar ocorrencias do specieslink
sp_dir <- file.path("Ocorrencias", sp)
sp_dir
dir.create(file.path(sp_dir, "SpeciesLink"), recursive = TRUE)

#Download de ocorrencias do specieslink
occ_sp <- rspeciesLink(species = sp,
             key = my_key,
             Coordinates = "Original",
             Scope = "a", #p for Plants, a for animals
             Synonyms = "no synonyms",
             limit = 5000)
colnames(occ_sp)

#Salvar registros
fwrite(occ_sp,
       file.path(sp_dir, "SpeciesLink/Ocorrencias.gz"),
       compress = "gzip", row.names = FALSE)

# Outras fontes de dados para baixar direto pelo R:
# BIEN_occurrence_species (pacote BIEN) - Para Plantas em Botanical Information and Ecology Network
# idig_search_records (pacote ridigbio) - iDigBio (Integrated Digitized Biocollections)

# Repita o procedimento acima com as espécies Boana albomarginata e depois com Hemitriccus kaempferi
# Copie o código acima e cole aqui embaixo para repetir





