#### Filtrar pontos de ocorrência usando metadados ####

# Remover todos os objetos #
rm(list = ls())

#Carregar pacotes
library(RuHere) 
library(dplyr)
library(data.table)
library(terra)
library(mapview)

# Dados primários de biodiversidade costumam trazer informações detalhadas do
# país e do Estado onde a espécie foi encontrada

# Essas informações podem ser úteis para ver se as coordenadas estão corretas
# Por exemplo: registro diz ter sido feito no Brasil, mas coordenadas caem na
# Europa.

# Antes de checar se os registros estão corretor, vamos remover os registros
# com coordenadas invalidas:
# Coordenadas vazias ou NA
# Coordenadas fora do planeta Terra: latitude > 90 ou < -90 e longitude >180 ou < -180

#### Importar dados ####

# Sempre começamos atribuindo o nome da espécie a um objeto
# Isso facilita construir sempre os mesmos caminhos, mudando apenas o nome da espécie
sp <- "Araucaria angustifolia"
# Diretorio com ocorrencias
sp_dir <- file.path("Ocorrencias/", sp)

# Importar registros unidos
occ <- fread(file.path(sp_dir, "1-Ocorrencias_unidas.gz"))

#### Remover registros com coordenadas invalidas ####
occ_coordenadas <- remove_invalid_coordinates(occ = occ)
# Função retorna lista com registros validos e invalidos
View(occ_coordenadas$invalid)
View(occ_coordenadas$valid)

# Vamos criar uma pasta para ir salvando os registros removidos
dir.create(file.path(sp_dir, "Removidos"))
fwrite(occ_coordenadas$invalid,
       file.path(sp_dir, "Removidos/Coordenadas_invalidas.gz"))

# Vamos trabalhar só com as coordenadas válidas
occ <- occ_coordenadas$valid

#### Padronizar os nomes dos países ####
# Vamos dar uma olhada na coluna com os países
occ %>% distinct(country) %>% View()

# O pacote RuHere possui um "dicionário" para padronizar os nomes em diversas linguas
View(RuHere::country_dictionary$country_name)

# Vamos usar lookup_na_country = TRUE para que registros sem informações de paises
# sejam preenchidos de acordo com as coordenadas
occ_country <- standardize_countries(occ = occ, 
                                     long = "decimalLongitude",
                                     lat = "decimalLatitude",
                                     lookup_na_country = TRUE)
# Ver dicionario usado para padronizar dados
View(occ_country$report)
# Ver ocorrencias
View(occ_country$occ)

# Vamos agora padronizar os dados de Estados

#### Padronizar os nomes dos estados ####
# Vamos dar uma olhada na coluna com os estados
occ %>% distinct(stateProvince) %>% View()
# O pacote RuHere possui um "dicionário" para padronizar os nomes em diversas linguas
View(RuHere::states_dictionary$states_name)
# Países disponveis
RuHere::states_dictionary$states_name$country %>% unique()

# Vamos usar lookup_na_country = TRUE para que registros sem informações de paises
# sejam preenchidos de acordo com as coordenadas
occ_states <- standardize_states(occ = occ_country$occ, 
                                 long = "decimalLongitude",
                                 lat = "decimalLatitude",
                                 lookup_na_state = TRUE)
# Ver dicionario usado para padronizar dados
View(occ_states$report)
# Ver ocorrencias
View(occ_states$occ)

# Estou tentando incluir provincias de mais países da América do Sul
# Por ex, Misiones na Argentina

# Com nomes padronizados, podemos ver se registros caem mesmo nesses países

# Vamos começar pelos países
occ_coutry_checked <- check_countries(occ = occ_states$occ,
                                      country_column = "country_suggested",
                                      try_to_fix = TRUE)

# Espacializar pontos para ver
pts_country_checked <- spatialize(occ_coutry_checked)
mapview(pts_country_checked,
        zcol = "country_issues",
        burst = TRUE)


# Checar se registros caem nos Estados definidos nos metadados
occ_states_checked <- check_states(occ = occ_coutry_checked,
                                   state_column = "state_suggested",
                                   try_to_fix = TRUE)

# Espacializar pontos para ver
pts_states_checked <- spatialize(occ_states_checked)
mapview(pts_states_checked,
        zcol = "state_issues",
        burst = TRUE)

# Quantos registros passaram nos testes?
table(occ_states_checked$correct_country) #Teste de países
table(occ_states_checked$correct_state) #Teste de estados
table(occ_states_checked$correct_country & occ_states_checked$correct_state) #Nos dois testes

# Por enquanto, vamos manter esses registros POTENCIALMENTE errados
# Mas vamos salvar o data.frame
fwrite(occ_states_checked,
       file.path(sp_dir, "2-Ocorrencias_paises_estados_checados.gz"))

#### Usar metadados para identificar outros registros potencialmente problemáticos ####
# Vamos importar os registros novamente
occ_states_checked <- fread(file.path(sp_dir, 
                                      "2-Ocorrencias_paises_estados_checados.gz"))

# É comum que alguns registros de plantas sejam de individuos cultivados
# Em alguns casos, é recomendável remover esses registros
# Vamos identificá-los usando os termos que comumemente são utilizados para descrever
# esses individuos
RuHere::cultivated$cultivated #Termos usados

# Identificar registros cultivados
occ_cultivated <- flag_cultivated(occ = occ_states_checked)
# Quantos são cultivados (FALSE)
table(occ_cultivated$cultivated_flag)
# Vamos dar uma olhada nesses registros
occ_cultivated %>% 
  filter(!cultivated_flag) %>% 
  select(occurrenceRemarks, locality, habitat) %>% 
  View()
# Vamos plotar esses pontos
pts_cultivated <- spatialize(occ_cultivated)
mapview(pts_cultivated,
        zcol = "cultivated_flag", burst = TRUE)

# Identificar registros fósseis
# Em alguns casos, bases de dados retornam registros fósseis
occ_fossil <- flag_fossil(occ = occ_cultivated)
# Quantos são fósseis (FALSE)
table(occ_fossil$fossil_flag)

# Identificar registro do iNaturalist (sem selo de research-grade)
occ_inaturalist <- flag_inaturalist(occ = occ_fossil, 
                                    research_grade = FALSE) #Não sinalizar registros com selo research grade
# Quantos são do inaturalist sem research-grade?
table(occ_inaturalist$inaturalist_flag)

# E se sinalizarmos todos do inaturalist, inclusive os com research grade?
occ_inaturalist_todos <- flag_inaturalist(occ = occ_fossil, 
                                          research_grade = TRUE) #Sinalizar registros com selo research grade
# Quantos são do inaturalist sem research-grade? (FALSE)
table(occ_inaturalist_todos$inaturalist_flag)
# Espacializar para plotar
pts_inaturalist_todos <- spatialize(occ_inaturalist_todos)
mapview(pts_inaturalist_todos, zcol = "inaturalist_flag", burst = TRUE)

# Vamos confiar nos registros do iNaturalist com research-grade

# Salvar registros
fwrite(occ_inaturalist,
       file.path(sp_dir, "3-Ocorrencias_metadados_checados.gz"))
