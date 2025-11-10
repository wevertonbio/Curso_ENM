#### Remover duplicatas e finalizar filtragem com CoordinateCleaner ####

# Remover todos os objetos #
rm(list = ls())

#Carregar pacotes
library(RuHere)
library(dplyr)
library(data.table)
library(terra)
library(mapview)
library(CoordinateCleaner)

# Sempre começamos atribuindo o nome da espécie a um objeto
# Isso facilita construir sempre os mesmos caminhos, mudando apenas o nome da espécie
sp <- "Araucaria angustifolia"
# Diretorio com ocorrencias
sp_dir <- file.path("Ocorrencias/", sp)

# Importar ocorrências
occ <- fread(file.path(sp_dir, "4-Ocorrencias_filtradas_especialistas.gz"))
# Plotar ocorrências
plot_here(occ)


#### REMOVER DUPLICADOS ####
# Primeiro, vamos remover os pontos duplicados
# Com mesma longitude e mesma latitude
# Aqui, podemos escolher a ordem de "prioridade" baseado em uma variável continua
# Por exemplo, entre os pontos duplicados, podemos escolher manter o mais recente
# Também podemos escolher a ordem de "prioridade" baseado em uma variável categórica
# Por exemplo, sempre manter pontos do GBIF entre as duplicatas
# Vamos fazer isso
# Verificar bases de dados disponíveis
unique(occ$data_source)
# Criar ordem de prioridade
prioridade <- c("gbif", "specieslink", "NeotropTree", "idigbio")
# Identificar duplicatas
# Por padrão, função usa coluns de especies e de coordenadas para definir duplicados
# Pode adicionar mais colunas no argumento additional_groups.
occ_dup <- flag_duplicates(occ = occ,  
                           continuous_variable = "year", #Ordenar por ano
                           decreasing = TRUE, #Manter mais recentes
                           categorical_variable = "data_source", #Ordenar por fonte
                           priority_categories = prioridade, #Manter nessa ordem
                           additional_groups = NULL, #Usar colunas padrão
                           by_cell = FALSE) #Usar coordenadas
#Quantos duplicados?
table(occ_dup$duplicated_flag)
# Plotar
plot_here(occ_dup, flags = "duplicated_flag", cex = 4)

# Remover duplicados
# Definir pasta para salvar removidos
pasta_removidos <- file.path(sp_dir, "Removidos")
# Remover
occ_unicos <- remove_flagged(occ = occ_dup, flags = "duplicated",
                             save_flagged = TRUE, output_dir = pasta_removidos)
# Quantos sobraram e quantos foram removidos?
nrow(occ) #Quantos tinham?
nrow(occ_unicos) #Quantos sobraram?
nrow(occ) - nrow(occ_unicos) #Quantos removidos?
# Veja a pasta Removidos
fs::dir_tree(pasta_removidos) #Agora tem duplicated

#### FINALIZAR FILTRAGEM COM COORDINATE CLEANER ####
browseURL("https://besjournals.onlinelibrary.wiley.com/doi/10.1111/2041-210X.13152")

# "CoordinateCleaner compares the coordinates of occurrence records to reference 
# databases of country and province centroids, country capitals, urban areas, 
# known natural ranges and tests for plain zeros, equal longitude/latitude, 
# coordinates at sea, country borders and outliers in collection year"

# Funções de plot e de remoção de flags do RuHere é compatível com CoordinateCleaner
# Ver filtros disponíveis
?CoordinateCleaner::clean_coordinates()
# Invalid lat/long, Equal lat/long, Zero lat/long, Capital centroid, 
# Country/Province centroid, Open sea, Urban area, Outlier, GBIF headquarters, 
# Biodiversity Institution, Artificial Hotspot Occurrence

# Vamos definir os testes usados:
testes <- c("capitals", "centroids", "equal", "gbif", "institutions", 
            "outliers", "zeros")

# Centroids não é muito bom, estamos trabalhando numa função melhor
# Mas por enquanto, vamos usar elas

# Aplicar testes
occ_cc <- clean_coordinates(x = occ_unicos, tests = testes)
# Plotar
plot_here(occ_cc)

# Remover ou não pontos em instituições de biodiversidade?
# Trabalhando em uma função que verifica o quando registros sinalizados mudam
# o "nicho" da espécie (ex: ranges e médias de tempertatura)

# Aqui, vamos remover esses pontos
# Não vamos esquecer das excecoes
excecoes <- c("gbif_1958", #Uruguai
              "gbif_1967") #Espírito Santo
occ_final <- remove_flagged(occ = occ_cc, 
                            flags = "all", 
                            exceptions = excecoes, 
                            column_exception = "record_id", 
                            save_flagged = TRUE,
                            output_dir = pasta_removidos)
# Veja a pasta Removidos
fs::dir_tree(pasta_removidos) #Agora tem Biodiversity Institution

# Vamos salvar os registros únicos e filtrados com CoordinateCleaner
fwrite(occ_final,
       file.path(sp_dir, "5-Ocorrencias_unicas_CoordinateCleaner.gz"))
