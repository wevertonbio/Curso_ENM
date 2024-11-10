#### USANDO INFORMAÇÕES DE ESPECIALISTAS PARA FILTRAR PONTOS ####
#Aqui, vamos usar informações de 3 fontes diferentes:
# - Flora e Funga do Brasil (para plantas)
# - World Checklist of Vascular Plants (para plantas)
# - Catálogo Taxonômico da Fauna do Brasil (para animais)
# - Polígonos da IUCN (para plantas e animais)

#Carregar pacotes
library(pbapply) #Mostrar barra de progresso em algumas funções customizadas
library(data.table) #Importar e salvar tabelas
library(dplyr) #Manipulação de dataframes e uso do %>%
library(mapview) #Para visualizar mapa interativo
library(terra) #Manipulação de dados espaciais
library(florabr) #Acesso a Flora do Brasil
library(faunabr) #Acesso a Fauna do Brasil
library(rWCVPdata) #Acesso a base do WCVP (World Checklist of Vascular Plants)
source("Scripts/helpers/filter_wcvp.R") #Filtrar usando informações de wcvp

#### Araucaria angustifolia ####
#Como sempre, começamos atribuindo o nome da espécie e o diretório a objetos
sp <- "Araucaria angustifolia"
sp_dir <- file.path("Ocorrencias/", sp)
sp_dir

# Importar registros unidos
occ <- fread(file.path("Ocorrencias/", sp, "Check_points/A - Occ_metadados_ok.gz"),
             data.table = FALSE)
#Espacializar pontos
pts <- vect(occ, geom = c(x = "decimalLongitude", #Converte pontos para spatvector
                          y = "decimalLatitude"), crs = "+init=epsg:4326")
mapview(pts, #Converte pontos para spatvector
        zcol = "data_source", #Coluna usada para colorir
        burst = TRUE) #Filtrar por valor da coluna


#Para Araucaria, vamos usar informações disponíveis na Flora do Brasil
# Para isso, vamos baixar os dados mais recentes
dir.create("florabr")
#RODE ESSE SCRIPT ABAIXO APENAS UMA VEZ!
# get_florabr(output_dir = "florabr", #Pasta criada para salvar dados
#             solve_discrepancy = TRUE, #Resolver discrepancias species-subspecies?
#             overwrite = TRUE) #Sobrescrever arquivo?

#Após baixar os dados, vamos carregá-lo:
fbr <- load_florabr(data_dir = "florabr")
head(fbr)

#O pacote possui uma função para filtrar pontos de ocorrência por Estado, Bioma 
# e endemismo no Brasil
?filter_florabr
occ_florabr <- filter_florabr(data = fbr,
                              occ = occ,
                              species = "scientificName", 
                              long = "decimalLongitude",
                              lat = "decimalLatitude",
                              by_state = TRUE, buffer_state = 20, by_biome = TRUE,
                              buffer_biome = 20, by_endemism = TRUE,
                              buffer_brazil = 20, value = "flag&clean",
                              keep_columns = TRUE,
                              verbose = TRUE)
pts_florabr <- vect(occ_florabr$flagged,
                    geom = c(x = "decimalLongitude", #Converte pontos para spatvector
                             y = "decimalLatitude"), crs = "+init=epsg:4326")
#Checar ponts
mapview(pts_florabr, zcol = "inside_state") + 
  mapview(pts_florabr, zcol = "inside_biome")

#Salvar registros fora de estados/biomas
occ_flagged <- occ_florabr$flagged %>% filter(!filters_ok)
#Salvar
fwrite(occ_flagged,
       file.path(sp_dir, "Check_points/Removidos/Fora_de_estados_ou_biomas.gz"),
       compress = "gzip", row.names = FALSE)
# Atualizar occ
occ <- occ_florabr$cleaned
#Espacializar pontos
pts <- vect(occ, geom = c(x = "decimalLongitude", #Converte pontos para spatvector
                          y = "decimalLatitude"), crs = "+init=epsg:4326")
mapview(pts, #Converte pontos para spatvector
        zcol = "data_source", #Coluna usada para colorir
        burst = TRUE) #Filtrar por valor da coluna

# Podemos ver uma melhora nos pontos dentro do Brasil...
# Mas ainda existem pontos fora do Brasil bastante duvidosos.
# Araucaria na Europa e África???
# Vamos agora usar uma base de dados internacional (WCVP) para ajudar a filtrar esses pontos

#Primeiro, vamos unir os dados dos nomes das espécies e distribuição em um unico dataframe
# Para isso, vamos usar a função customizada prepare_wcvp_data()
wcvp_names <- rWCVPdata::wcvp_names #Nome das espécies
wcvp_dist <- rWCVPdata::wcvp_distributions #Distribuição
wcvp_map <- rWCVPdata::wgsrpd3 #Mapa referente a distribuição
wcvp_data <- prepare_wcvp_data(wcp_names = wcvp_names,
                               wcp_dist = wcp_dist,
                               wcp_map = wcvp_map)
#A função retorna uma lista com dois elementos
head(wcvp_data$wcp_data) #Regiões onde espécies ocorre de forma nativa
mapview(wcvp_data$wcp_map, zcol = "LEVEL3_NAM") #Mapa com as regiões
#Vamos ver em quais regiões a Araucaria ocorre
wcvp_data$wcp_data %>% filter(species == "Araucaria angustifolia")

#Vamos usar a função customizada filter_wcvp() para filtrar pontos fora dessas regiões:
occ_wcvp <- filter_wcvp(occ = occ, #Tabela com ocorrencias
                        species = "scientificName", #Nome da coluna com espécies
                        x = "decimalLongitude", #Nome da coluna com longitude
                        y = "decimalLatitude", #Nome da coluna com latitude
                        wcvp_data = wcvp_data, #Lista resultante de prepare_wcvp_data()
                        buffer = 20, #Buffer de tolerância ao redor das regiões (em km)
                        return_map = TRUE) #Retornar mapa de distribuição de cada espécie?
#Checar pontos sinalizados
pts_wcvp <- vect(occ_wcvp[[sp]]$flagged, 
                 geom = c(x = "decimalLongitude", #Converte pontos para spatvector
                          y = "decimalLatitude"), crs = "+init=epsg:4326")
mapview(pts_wcvp, zcol = "natural_range_wcvp") + 
  mapview(occ_wcvp[[sp]]$map)
#Salvar registros fora de distribuição natural
occ_out_wcvp <- occ_wcvp[[sp]]$flagged %>% 
  filter(!natural_range_wcvp)
#Salvar
fwrite(occ_out_wcvp,
       file.path(sp_dir, "Check_points/Removidos/Fora_dist_natural_wcvp.gz"),
       compress = "gzip", row.names = FALSE)
# Atualizar occ
occ <- occ_wcvp[[sp]]$flagged %>% 
  filter(natural_range_wcvp)
#Espacializar pontos
pts <- vect(occ, geom = c(x = "decimalLongitude", #Converte pontos para spatvector
                          y = "decimalLatitude"), crs = "+init=epsg:4326")
mapview(pts, #Converte pontos para spatvector
        zcol = "data_source", #Coluna usada para colorir
        burst = TRUE) #Filtrar por valor da coluna

#Salvar checkpoint B
fwrite(occ,
       file.path(sp_dir, "Check_points/B - Occ_dist_natural.gz"),
       compress = "gzip", row.names = FALSE)

#### Boana albomarginata ####
rm(list=ls()) #Limpar console
#Importar funções customizadas de novo
source("Scripts/helpers/filter_wcvp.R") #Filtrar usando informações de wcvp

#Como sempre, começamos atribuindo o nome da espécie e o diretório a objetos
sp <- "Boana albomarginata"
sp_dir <- file.path("Ocorrencias/", sp)
sp_dir

# Importar registros unidos
occ <- fread(file.path("Ocorrencias/", sp, "Check_points/A - Occ_metadados_ok.gz"),
             data.table = FALSE)
#Espacializar pontos
pts <- vect(occ, geom = c(x = "decimalLongitude", #Converte pontos para spatvector
                          y = "decimalLatitude"), crs = "+init=epsg:4326")
mapview(pts, #Converte pontos para spatvector
        zcol = "data_source", #Coluna usada para colorir
        burst = TRUE) #Filtrar por valor da coluna

#Para Boana, vamos usar informações disponíveis na Fauna do Brasil
# Para isso, vamos baixar os dados mais recentes
dir.create("faunabr")
# # RODE ESSE SCRIPT ABAIXO APENAS UMA VEZ!
get_faunabr(output_dir = "faunabr", #Pasta criada para salvar dados
            solve_discrepancies = FALSE, #Resolver discrepancias species-subspecies?
            overwrite = TRUE) #Sobrescrever arquivo?

#Após baixar os dados, vamos carregá-lo:
fbr <- load_faunabr(data_dir = "faunabr")
head(fbr)


#O pacote possui uma função para filtrar pontos de ocorrência por Estado, Bioma 
# e endemismo no Brasil
?filter_faunabr
occ_faunabr <- filter_faunabr(data = fbr,
                              occ = occ,
                              species = "scientificName", 
                              long = "decimalLongitude",
                              lat = "decimalLatitude",
                              by_state = TRUE, buffer_state = 30,
                              by_country = TRUE, buffer_country = 30,
                              value = "flag&clean",
                              keep_columns = TRUE,
                              verbose = TRUE)
pts_faunabr <- vect(occ_faunabr$flagged,
                    geom = c(x = "decimalLongitude", #Converte pontos para spatvector
                             y = "decimalLatitude"), crs = "+init=epsg:4326")
#Checar ponts
mapview(pts_faunabr, zcol = "inside_state") + 
  mapview(pts_faunabr, zcol = "inside_country")

#Salvar registros fora de estados/países
occ_flagged <- occ_faunabr$flagged %>% filter(!filters_ok)
#Salvar
fwrite(occ_flagged,
       file.path(sp_dir, "Check_points/Removidos/Fora_de_estados_ou_países.gz"),
       compress = "gzip", row.names = FALSE)
# Atualizar occ
occ <- occ_faunabr$cleaned
#Espacializar pontos
pts <- vect(occ, geom = c(x = "decimalLongitude", #Converte pontos para spatvector
                          y = "decimalLatitude"), crs = "+init=epsg:4326")
mapview(pts, #Converte pontos para spatvector
        zcol = "data_source", #Coluna usada para colorir
        burst = TRUE) #Filtrar por valor da coluna

#Também podemos usar um shapefile da IUCN para filtrar ainda mais os pontos
# https://www.iucnredlist.org/species/55376/85898219
# O shapefile baixado está na pasta IUCN
sp_iucn <- file.path("IUCN_ranges/", sp, "data_0.shp") #Obter caminho
sp_iucn <- vect(sp_iucn)
mapview(sp_iucn) + mapview(pts)

#Para filtrar os pontos que caem fora, vamos usar a função is.related() do Terra
#Mas antes, vamos definir um buffer de tolerância
sp_iucn_b <- buffer(sp_iucn, width = 25 * 1000) #Buffer de 25 km (25 mil metros)
mapview(sp_iucn_b) + mapview(pts)
#Criar coluna com resultados de teste
occ$inside_iucn <- is.related(pts, sp_iucn_b, "intersects")
table(occ$inside_iucn) #Contagem de resultados
#Checar
#Espacializar pontos novamente com nova coluna
pts <- vect(occ, geom = c(x = "decimalLongitude", #Converte pontos para spatvector
                          y = "decimalLatitude"), crs = "+init=epsg:4326")
mapview(sp_iucn_b) + mapview(pts, #Converte pontos para spatvector
        zcol = "inside_iucn", #Coluna usada para colorir
        burst = TRUE) #Filtrar por valor da coluna 

#Salvar registros fora de range da IUCN
occ_out_iucn <- occ %>% filter(!inside_iucn)
#Salvar
fwrite(occ_out_iucn,
       file.path(sp_dir, "Check_points/Removidos/Fora_de_range_iucn.gz"),
       compress = "gzip", row.names = FALSE)
# Atualizar occ
occ <- occ %>% filter(inside_iucn)
#Salvar checkpoint B
fwrite(occ,
       file.path(sp_dir, "Check_points/B - Occ_dist_natural.gz"),
       compress = "gzip", row.names = FALSE)

#### Agora, faça a mesma coisa com a espécie Hemitriccus kaempferi

#### Hemitriccus kaempferi ####
rm(list=ls()) #Limpar console
#Importar funções customizadas de novo
source("Scripts/helpers/filter_wcvp.R") #Filtrar usando informações de wcvp

#Como sempre, começamos atribuindo o nome da espécie e o diretório a objetos
sp <- "Hemitriccus kaempferi"
sp_dir <- file.path("Ocorrencias/", sp)
sp_dir

# Importar registros unidos
occ <- fread(file.path("Ocorrencias/", sp, "Check_points/A - Occ_metadados_ok.gz"),
             data.table = FALSE)
#Espacializar pontos
pts <- vect(occ, geom = c(x = "decimalLongitude", #Converte pontos para spatvector
                          y = "decimalLatitude"), crs = "+init=epsg:4326")
mapview(pts, #Converte pontos para spatvector
        zcol = "data_source", #Coluna usada para colorir
        burst = TRUE) #Filtrar por valor da coluna

#Para Boana, vamos usar informações disponíveis na Fauna do Brasil
# Para isso, vamos baixar os dados mais recentes
dir.create("faunabr")
# # RODE ESSE SCRIPT ABAIXO APENAS UMA VEZ!
# get_faunabr(output_dir = "faunabr", #Pasta criada para salvar dados
#             solve_discrepancies = FALSE, #Resolver discrepancias species-subspecies?
#             overwrite = TRUE) #Sobrescrever arquivo?

#Após baixar os dados, vamos carregá-lo:
fbr <- load_faunabr(data_dir = "faunabr")
head(fbr)


#O pacote possui uma função para filtrar pontos de ocorrência por Estado, Bioma 
# e endemismo no Brasil
?filter_faunabr
occ_faunabr <- filter_faunabr(data = fbr,
                              occ = occ,
                              species = "scientificName", 
                              long = "decimalLongitude",
                              lat = "decimalLatitude",
                              by_state = TRUE, buffer_state = 30,
                              by_country = TRUE, buffer_country = 30,
                              value = "flag&clean",
                              keep_columns = TRUE,
                              verbose = TRUE)
pts_faunabr <- vect(occ_faunabr$flagged,
                    geom = c(x = "decimalLongitude", #Converte pontos para spatvector
                             y = "decimalLatitude"), crs = "+init=epsg:4326")
#Checar ponts
mapview(pts_faunabr, zcol = "inside_state") + 
  mapview(pts_faunabr, zcol = "inside_country")

#Salvar registros fora de estados/países
occ_flagged <- occ_faunabr$flagged %>% filter(!filters_ok)
#Salvar
fwrite(occ_flagged,
       file.path(sp_dir, "Check_points/Removidos/Fora_de_estados_ou_países.gz"),
       compress = "gzip", row.names = FALSE)
# Atualizar occ
occ <- occ_faunabr$cleaned
#Espacializar pontos
pts <- vect(occ, geom = c(x = "decimalLongitude", #Converte pontos para spatvector
                          y = "decimalLatitude"), crs = "+init=epsg:4326")
mapview(pts, #Converte pontos para spatvector
        zcol = "data_source", #Coluna usada para colorir
        burst = TRUE) #Filtrar por valor da coluna

#Também podemos usar um shapefile da IUCN para filtrar ainda mais os pontos
# https://www.iucnredlist.org/species/22698957/118775733
# O shapefile baixado está na pasta IUCN
sp_iucn <- file.path("IUCN_ranges/", sp, "data_0.shp") #Obter caminho
sp_iucn <- vect(sp_iucn)
mapview(sp_iucn) + mapview(pts)

#Para filtrar os pontos que caem fora, vamos usar a função is.related() do Terra
#Mas antes, vamos definir um buffer de tolerância
sp_iucn_b <- buffer(sp_iucn, width = 25 * 1000) #Buffer de 25 km (25 mil metros)
mapview(sp_iucn_b) + mapview(pts)
#Criar coluna com resultados de teste
occ$inside_iucn <- is.related(pts, sp_iucn_b, "intersects")
table(occ$inside_iucn) #Contagem de resultados
#Checar
#Espacializar pontos novamente com nova coluna
pts <- vect(occ, geom = c(x = "decimalLongitude", #Converte pontos para spatvector
                          y = "decimalLatitude"), crs = "+init=epsg:4326")
mapview(sp_iucn_b) + mapview(pts, #Converte pontos para spatvector
                             zcol = "inside_iucn", #Coluna usada para colorir
                             burst = TRUE) #Filtrar por valor da coluna 

#Salvar registros fora de range da IUCN
occ_out_iucn <- occ %>% filter(!inside_iucn)
#Salvar
fwrite(occ_out_iucn,
       file.path(sp_dir, "Check_points/Removidos/Fora_de_range_iucn.gz"),
       compress = "gzip", row.names = FALSE)
# Atualizar occ
occ <- occ %>% filter(inside_iucn)
#Salvar checkpoint B
fwrite(occ,
       file.path(sp_dir, "Check_points/B - Occ_dist_natural.gz"),
       compress = "gzip", row.names = FALSE)
