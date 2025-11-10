#### Filtrar pontos de ocorrência usando informações de especialistas ####

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

#' Várias bases de dados trazem informações (mesmo que grosseiras) sobre a
#' distribuição nativa da espécie
#' Ex: https://wevertonbio.github.io/florabr/articles/Spatialize_florabr.html
#' Ex: https://wevertonbio.github.io/faunabr/articles/Spatialize_faunabr.html
#'
#' No RuHere, implementados funções semelhantes para sinalizar registros fora
#' da distribuição natural da espécie segundo essas bases de dados:
#' - Flora do Brasil (via florabr) - Apenas plantas
#' - Fauna do Brasil (via faunabr) - Apenas animais
#' - World Checklist of Vascular Plants (WCVP) - Apenas plantas
#' - IUCN - Plantas e animais
#' - BIEN - Apenas plantas
#' - Fishbase - Apenas peixes
#' 
#' Conhecem mais alguma base de dados???
#' 

#### Baixando informações de especialistas ####
# Antes de começar, precisamos baixar essas informações

# Vamos criar um diretório para salvar esses dados
d <- file.path("Ocorrencias/", "Dados_especialistas")
dir.create(d)

# Flora e Funga do Brasil
# Não precisa definir espécie, baixa toda base de dados
florabr_here(data_dir = d)

# Catalogo Taxonomico da Fauna do Brasil
# Não precisa definir espécie, baixa toda base de dados
faunabr_here(data_dir = d)

# World Checklist of Vascular Plants (WCVP)
# Não precisa definir espécie, baixa toda base de dados
wcvp_here(data_dir = d)

# IUCN
# Não são os poligonos 🥲
# São informações mais grosseiras, a nivel de país e estados do mapa da WGSRPD
# Precisa definis espécie
iucn_here(data_dir = d, species = sp)

# BIEN
# Aqui são polígonos 😁
# Precisa definis espécie
bien_here(data_dir = d, species = sp)
# Não tem pra Araucaria 😭
# Apenas para teste com ipe amarelo
bien_here(data_dir = d, species = "Handroanthus albus")
ipe_dist <- vect(file.path(d, "BIEN/Handroanthus albus.gpkg"))
mapview(ipe_dist)

# Fishbase
# Não precisa definir espécie, baixa toda base de dados
# fishbase_here(data_dir = d)

# Vamos checar quais bases de dados possuem informações sobre nossa espécie
dados <- available_datasets(data_dir = d, species = sp)
View(dados)

# Podemos também ver os ranges da espécie definidos por essas bases
dados_com_dist <- available_datasets(data_dir = d, species = sp, 
                                     return_distribution = TRUE)
names(dados_com_dist)
# Plotar
# ATENCAO: QUANDO FOR PLOTAR COM SUA ESPÉCIE, UTILIZE A TECLA TAB PARA EXPLORAR
# AS OPÇÕES DISPONÍVEIS DENTRO DO OBJETO dados_com_dist
mapview(dados_com_dist$florabr$states_biomes, layer.name = "florabr", 
        col.regions = "green") +
  mapview(dados_com_dist$iucn, layer.name = "iucn", col.regions = "red") +
  mapview(dados_com_dist$wcvp, layer.name = "wcvp", col.regions = "blue")


# Vamos plotar os registros
occ <- fread(file.path(sp_dir, "3-Ocorrencias_metadados_checados.gz"))
pts <- spatialize(occ)
# Plotar
mapview(dados_com_dist$florabr$states_biomes, layer.name = "florabr", 
        col.regions = "green") +
  mapview(dados_com_dist$iucn, layer.name = "iucn", col.regions = "red") +
  mapview(dados_com_dist$wcvp, layer.name = "wcvp", col.regions = "blue") +
  mapview(pts, layer.name = "Ocorrencias", cex = 2, col.regions = "yellow")


#### SINALIZAR REGISTROS POTENCIALMENTE ERRADOS ####

# Vamos usar as funções do RuHere para identificar os registros fora desses poligonos
# Testar com wcvp
occ_florabr <- flag_florabr(dir = d, occ = occ, 
                            buffer_state = 25, 
                            buffer_biome = 25)

# Quantos passaram no teste?
table(occ_florabr$florabr_flag)
# Lembrando que flora do brasil só testa registros no Brasil!!

# Testar com WCVP
occ_wcvp <- flag_wcvp(data_dir = d, occ = occ_florabr, buffer = 25)
# Quantos passaram no teste?
table(occ_wcvp$wcvp_flag)

# Testar com IUCN
occ_iucn <- flag_iucn(data_dir = d, occ = occ_wcvp, buffer = 25)
# Quantos passaram no teste?
table(occ_iucn$iucn_flag)

# Plotar pontos
mapa_flags <- plot_here(occ = occ_iucn, 
                        flags = "all", 
                        cex = 4, #Tamanho do ponto
                        label = "record_id")
# Plotar mapa
mapa_flags

# Plotar mapa com range natural estimado por bases de dados
mapa_flags +
  mapview(dados_com_dist$florabr$states_biomes, layer.name = "florabr", 
           col.regions = "green") +
  mapview(dados_com_dist$iucn, layer.name = "iucn", col.regions = "red") +
  mapview(dados_com_dist$wcvp, layer.name = "wcvp", col.regions = "blue")

# REMOVER PONTOS SINALIZADOS COMO POTENCIALMENTE ERRADOS #
# Vamos salvar os registros removidos
# Vai que o Revisor 2 pede...
pasta_removidos <- file.path(sp_dir, "Removidos")
dir.exists(pasta_removidos) #Verificar se pasta existe

# Podemos escolher pontos sinalizados para manter (as exceções)
# Como exemplo, vamos escolher 2 pontos, armazenando o ID dos registros
excecoes <- c("gbif_1958", #Uruguai
              "gbif_1967") #Espírito Santo

occ_filtrado <- remove_flagged(occ = occ_iucn, 
                               flags = "all", 
                               exceptions = excecoes, 
                               column_exception = "record_id", 
                               save_flagged = TRUE,
                               output_dir = pasta_removidos)
# Veja a pasta Removidos
fs::dir_tree(pasta_removidos)

# Quantos registros sobraram?
nrow(occ_iucn) #Quantos tinham?
nrow(occ_filtrado) #Quantos sobraram?
nrow(occ_iucn) - nrow(occ_filtrado) #Quantos removidos?

# Plotar apenas mantidos
plot_here(occ = occ_filtrado)

# Salvar registros
fwrite(occ_filtrado, 
       file.path(sp_dir, "4-Ocorrencias_filtradas_especialistas.gz"))
