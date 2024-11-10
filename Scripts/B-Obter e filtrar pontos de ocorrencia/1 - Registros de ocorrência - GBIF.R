#### DOWNLOAD DE REGISTROS DE OCORRÊNCIA ####
#Carregar pacotes
library(rgbif) #pacote para baixar dados do gbif
library(data.table) #Importar e salvar tabelas

#Crie uma conta no GBIF: https://www.gbif.org/

#Podemos baixar ocorrencias direto do gbif:
# Acesse: https://www.gbif.org/species/2684940


#Para baixar pelo R, precisamos da chave da espécie.

# DICA: CRIE UM OBJETO COM O NOME DA ESPÉCIE!
# Esse objeto sera usado para criar pastas e baixar as ocorrências
sp <- "Hemitriccus kaempferi"

# Checar chave (key) da espécie no gbif
sp_info <- name_backbone(name = sp,
                         kingdom = "Animal") #Veja outras opções na função
#Obter chave
k <- sp_info$usageKey
k
#Obter numero de ocorrências
n_occ <- occ_count(taxonKey = k)
n_occ
n_occ_with_coordinates <- rgbif::occ_count(taxonKey = k,
                                           hasCoordinate = TRUE)
n_occ_with_coordinates

#Fazer download com GBIF
user = "wevertonf1993" #Username de registro no gbif
pwd <- "nene1993" #Senha
email = "wevertonf1993@hotmail.com" #Email

occ_request <- occ_download(
  pred_in("taxonKey", k),
  pred("hasCoordinate", TRUE),
  pred("hasGeospatialIssue", FALSE),
  format = "DWCA",
  user = user, pwd = pwd, email = email)

#Checar status de download
occ_request
occ_download_wait(occ_request[1])
#Aguarde cerca de 5-15 minutos
#Após status mudar para SUCCEEDED, podemos baixar diretamente do site ou pelo R
#Criar diretório para salvar ocorrencias
sp_dir <- file.path("Ocorrencias", sp)
sp_dir
dir.create(file.path(sp_dir, "GBIF"), recursive = TRUE)
occ_d <- occ_download_get(key = occ_request, #Cole a chave aqui ou o objeto occ_request
                          path = file.path(sp_dir, "GBIF"),
                          overwrite=TRUE)
#Importar para R
occ <- occ_download_import(occ_d)
colnames(occ) #Ver colunas

#Salvar tabela no formato csv (Para importar, use read.csv)
write.csv(occ,
          file.path(sp_dir, "GBIF/Ocorrencias.csv"), #Paste e nome do arquivo
          row.names = FALSE)
#Salvar tabela no formato gz (comprimido) (Para importar, use fread do pacote data.table)
fwrite(occ,
       file.path(sp_dir, "GBIF/Ocorrencias.gz"), #Paste e nome do arquivo
       compress = "gzip", row.names = FALSE)
#Importar
occ2 <- fread(file.path(sp_dir, "GBIF/Ocorrencias.gz"))

# Repita o procedimento acima com as espécies Boana albomarginata e depois com Hemitriccus kaempferi
# Copie o código acima e cole aqui embaixo para repetir