#### Finalizar filtragem com CoordinateCleaner ####
# Vamos finalizar a filtragem dos pontos usando o pacote CoordinateCleaner:
# https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13152

# "CoordinateCleaner compares the coordinates of occurrence records to reference 
# databases of country and province centroids, country capitals, urban areas, 
# known natural ranges and tests for plain zeros, equal longitude/latitude, 
# coordinates at sea, country borders and outliers in collection year"


#Carregar pacotes
library(pbapply) #Mostrar barra de progresso em algumas funções customizadas
library(data.table) #Importar e salvar tabelas
library(dplyr) #Manipulação de dataframes e uso do %>%
library(mapview) #Para visualizar mapa interativo
library(terra) #Manipulação de dados espaciais
library(CoordinateCleaner) #Filtrar pontos

#### Araucaria angustifolia ####
#Como sempre, começamos atribuindo o nome da espécie e o diretório a objetos
sp <- "Araucaria angustifolia"
sp_dir <- file.path("Ocorrencias/", sp)
sp_dir

# Importar registros 
occ <- fread(file.path("Ocorrencias/", sp, "Check_points/B - Occ_dist_natural.gz"),
             data.table = FALSE)
#Espacializar pontos
pts <- vect(occ, geom = c(x = "decimalLongitude", #Converte pontos para spatvector
                          y = "decimalLatitude"), crs = "+init=epsg:4326")
mapview(pts, #Converte pontos para spatvector
        zcol = "data_source", #Coluna usada para colorir
        burst = TRUE) #Filtrar por valor da coluna

# Pontos em centróides de países
cap <- cc_cap(occ, lon = "decimalLongitude", lat = "decimalLatitude",
              species = "scientificName", buffer = 1000, #Buffer do centroide em metros
              value = "flagged")
sum(!cap) #Numero de registros sinalizados
#Função para salvar registros se houver sinalizações
if(sum(!cap) > 0){ #Se algum registro for sinalizado...
  #Remover registros e salvar registros removidos
  occ_cap_removed <- occ[!cap,]
  fwrite(occ_cap_removed,
         file.path(sp_dir, "Check_points/Removidos/Capitais_de_países.gz"),
         compress = "gzip", row.names = FALSE)
  #Manter apenas ocorrências que passaram no teste
  occ <- occ[cap,]
    }

# Pontos em centróides de Estados do Brasil
br <- geobr::read_state() %>% vect() #Obter mapa do brasil
br_cen <- terra::centroids(br) #Obter centroides de estados
br_cen_b <- buffer(br_cen, width = 1 * 1000) #Adicionar buffer de 1 km
mapview(br_cen_b)
#Checar se algum ponto cai dentro do centróide
#Espacializar pontos
pts <- vect(occ, geom = c(x = "decimalLongitude", #Converte pontos para spatvector
                          y = "decimalLatitude"), crs = "+init=epsg:4326")
cen <- !is.related(pts, br_cen_b, "intersects") #Se não cai dentro, é TRUE (passou no teste)
sum(!cen) #Numero de registros sinalizados
#Função para salvar registros se houver sinalizações
if(sum(!cen) > 0){ #Se algum registro for sinalizado...
  #Remover registros e salvar registros removidos
  occ_cen_removed <- occ[!cen,]
  fwrite(occ_cen_removed,
         file.path(sp_dir, "Check_points/Removidos/Capitais_de_estados.gz"),
         compress = "gzip", row.names = FALSE)
  #Manter apenas ocorrências que passaram no teste
  occ <- occ[cen,]
}

#Registros com longitude e latitude identicos
equ <- cc_equ(occ, lon = "decimalLongitude",
              lat = "decimalLatitude",
              test = "absolute",
              value = "flagged")
sum(!equ) #Numero de registros sinalizados
#Função para salvar registros se houver sinalizações
if(sum(!equ) > 0){ #Se algum registro for sinalizado...
  #Remover registros e salvar registros removidos
  occ_equ_removed <- occ[!equ,]
  fwrite(occ_equ_removed,
         file.path(sp_dir, "Check_points/Removidos/Lat_long_identicos.gz"),
         compress = "gzip", row.names = FALSE)
  #Manter apenas ocorrências que passaram no teste
  occ <- occ[equ,]
}

#Registros fora do sistema de coordenadas
# lat >90, lat <-90, lon > 180 and lon < -180
val <- cc_val(occ, lon = "decimalLongitude",
              lat = "decimalLatitude",
              value = "flagged")
sum(!val) #Numero de registros sinalizados


#Registros em coordenadas 0-0 (cairia no mar)
zero <- cc_zero(occ, lon = "decimalLongitude",
                lat = "decimalLatitude",
                buffer = 0.1, #Buffer in degrees
                value = "flagged")
sum(!zero) #Numero de registros sinalizados

#Pontos na sede do GBIF em Copenhagen
gbif <- cc_gbif(occ, lon = "decimalLongitude",
                lat = "decimalLatitude",
                species = "scientificName",
                buffer = 1000, #Buffer em metros
                value = "flagged")
sum(!gbif) #Numero de registros sinalizados
#Função para salvar registros se houver sinalizações
if(sum(!gbif) > 0){ #Se algum registro for sinalizado...
  #Remover registros e salvar registros removidos
  occ_gbif_removed <- occ[!gbif,]
  fwrite(occ_gbif_removed,
         file.path(sp_dir, "Check_points/Removidos/Sede_do_GBIF.gz"),
         compress = "gzip", row.names = FALSE)
  #Manter apenas ocorrências que passaram no teste
  occ <- occ[gbif,]
}

#Pontos em instituições de biodiversidade (jardins botanicos, herbarios, museus, etc)
#Antes, vamos ver a localização dessas instituições
bio_inst <- vect(institutions[!is.na(institutions$decimalLongitude) & #Mapa de referencia do CoordinateCleaner
                                !is.na(institutions$decimalLatitude), ], 
                 geom = c(x = "decimalLongitude", y = "decimalLatitude"),
                 crs = "+proj=longlat +datum=WGS84 +no_defs")
mapview(bio_inst)
#Testar se pontos caem nas instituições         
inst <- cc_inst(occ, lon = "decimalLongitude",
                lat = "decimalLatitude",
                species = "scientificName",
                buffer = 100, #Buffer in meters
                value = "flagged")
sum(!inst) #Numero de registros sinalizados
#Função para salvar registros se houver sinalizações
if(sum(!inst) > 0){ #Se algum registro for sinalizado...
  #Remover registros e salvar registros removidos
  occ_inst_removed <- occ[!inst,]
  fwrite(occ_inst_removed,
         file.path(sp_dir, "Check_points/Removidos/Instituicoes_biodiversidade.gz"),
         compress = "gzip", row.names = FALSE)
  #Ver mapa
  pts_inst_removed <- vect(occ_inst_removed, #Converte pontos para spatvector
                           geom = c(x = "decimalLongitude", 
                                    y = "decimalLatitude"), 
                           crs = "+init=epsg:4326")
  mapview(bio_inst) + mapview(pts_inst_removed, col.regions = "yellow")
  #Manter apenas ocorrências que passaram no teste
  occ <- occ[inst,]
}

### Remover duplicatas (apenas coordenadas)
# Se coluna index ainda não foi cirada, criar para identificar e remover duplicatas
# COmo criamos anteriormente, não precisamos criar de novo
# occ$index <- row.names(occ)

#Vamos escolher a ordem de preferência das bases de dados
# Ex: mesmo registro no gbif e specieslink, manter do gbif
unique(occ$data_source)
preferred_order <- c("gbif", "splink", "neotropicTree")
####Filtrar duplicatas
occ_unique <- occ %>%
  group_by(scientificName, decimalLongitude, decimalLatitude) %>% #Identificar valores duplicados
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
       file.path(sp_dir, "Check_points/Removidos/Duplicados_v2.gz"),
       compress = "gzip", row.names = FALSE)
# Atualizar occ
occ <- occ_unique

# Filtrar por outliers geográficos
# Cuidado: outliers são mesmo outliers? Ou é apenas uma distribuição disjunta?
# If “quantile”: a boxplot method is used and records are flagged as outliers 
# if their mean distance to all other records of the same species is larger than
# mltpl * the interquartile range of the mean distance of all records of this species. I
?cc_outl
outl <- cc_outl(x = occ,
  lon = "decimalLongitude",
  lat = "decimalLatitude",
  species = "scientificName",
  method = "quantile", #Vamos usar o método de quantil
  mltpl = 5, #Multiplicador do método quartil
  value = "flagged",
  verbose = TRUE,
  min_occs = 7)
sum(!outl)
#Ver pontos marcados como outliers
pts_outl <- vect(occ, #Converte pontos para spatvector
                 geom = c(x = "decimalLongitude", 
                          y = "decimalLatitude"), 
                         crs = "+init=epsg:4326")
#Adicionar info de outliers
pts_outl$outlier <- outl
mapview(pts_outl, zcol = "outlier")

#Remover registros e salvar registros removidos
occ_outl <- occ[!outl,]
fwrite(occ_outl,
       file.path(sp_dir, "Check_points/Removidos/Outliers.gz"),
       compress = "gzip", row.names = FALSE)

#Manter apenas ocorrências que passaram no teste
occ <- occ[outl,]

#SALVAR CHECKPOINT
fwrite(occ,
       file.path(sp_dir, "Check_points/C - Occ_filtrados.gz"),
       compress = "gzip", row.names = FALSE)


#Ver mapa final
#Espacializar pontos
pts <- vect(occ, geom = c(x = "decimalLongitude", #Converte pontos para spatvector
                          y = "decimalLatitude"), crs = "+init=epsg:4326")
pts$basisOfRecord[pts$basisOfRecord == ""] <- NA #Substituir valores vazios por NA
mapview(pts, #Converte pontos para spatvector
        zcol = "basisOfRecord", #Coluna usada para colorir
        burst = TRUE) #Filtrar por valor da coluna

# Repita o procedimento acima com as espécies Boana albomarginata e depois com Hemitriccus kaempferi
# Copie o código acima e cole aqui embaixo (ou em outro script) para repetir