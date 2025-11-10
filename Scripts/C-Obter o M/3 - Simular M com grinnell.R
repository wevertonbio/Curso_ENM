#### Simular M com grinnell ####

# Versão modificada (usada aqui) em:
# https://github.com/wevertonbio/grinnell
# remotes::install_github("wevertonbio/grinnell")

# Resumo de como o grinnell funciona:
# 1 - Determina nicho da espécie usando elipsoide.
# 2 - Projeta nicho da espécie no presente e em layers do passado
# 3 - Pixels adequados representam populações aptas a se dispersar.
#     Quando mais adequado é o pixel, mais chances de dispersar.
# 4 - Simulação começa no presente em direção a cenários do passado.
# 5 - Capacidade de dispersão da espécie é determinado principalmente por
#     dispersal_events e kernel_spread. Dispersal events é o número de "pulsos"
#     de dispersão entre um cenário e outro, e kernel spread é o quão longe a
#     população consegue dispersar.
# 6 - No fim, obtemos um raster e polígono mostrando áreas que a espécie 
#    colonizou ou tentou colonizar. Essas áreas representam o M da espécie.

# Novidades da versão modificada:
# While the original version constructs glacial-interglacial climate conditions 
# based on interpolations between current and LGM climate conditions, this 
# alternative version allows for the use of climatic variables representing 
# glacial-interglacial climate conditions provided externally by other sources, 
# such as PaleoClim and Oscillayers.

# Nova versão também permite testar várias combinações de dispersal_events e 
# kernel_spread, remover polígonos disjuntos sem ocorrências, adicionar um buffer
# extra ao redor do M final para tentar abranger todos os pontos.


# Remover todos os objetos #
rm(list = ls())

# Carregar pacotes
library(RuHere)
library(grinnell) # Para simular M
library(data.table) #Importar e salvar tabelas
library(mapview) #Para visualizar mapa interativo
library(terra) #Manipulação de dados espaciais
library(dplyr)

#### Carregar variáveis ####
#Variáveis do presente
current_variables <- rast("Variaveis_brutas/paleoclim_10/cur.tif")
# Sequencia de períodos do passado, do mais recente pro mais antigo
periods <- c("lh", "mh", "eh", "yds", "ba", "hs1", "lgm", "30kya", "40kya",
             "50kya", "60kya", "70kya", "80kya", "90kya",
             "100kya", "110kya", "120kya","lig")
# Variáveis climáticas a serem usadas
# Excluir variáveis 08, 09, 18 e 19 por apresentarem variações bruscas
# Essas variações não são naturais, mas artefatos estatísticos pela maneira como
# são calculadas
# https://onlinelibrary.wiley.com/doi/abs/10.1111/aec.13234
variables <- c("bio_1", "bio_2", "bio_3", "bio_4", "bio_5", "bio_6", "bio_7",
               "bio_10", "bio_11", "bio_12", "bio_13", "bio_14", "bio_15",
               "bio_16", "bio_17")

# Determinar pasta onde estão variáveis do passado
projection_dir <- "Variaveis_brutas/paleoclim_10"


#### Araucaria angustifolia ####
#Como sempre, começamos atribuindo o nome da espécie e o diretório a objetos
sp <- "Araucaria angustifolia"
sp_dir <- file.path("Ocorrencias/", sp)
sp_dir

#Criar diretório para salvar M
m_dir <- file.path("M_poligonos/", sp)
m_dir
dir.create(m_dir)

# Importar registros 
occ <- fread(file.path(sp_dir, "6-Pontos_rarefeitos.gz"),
             data.table = FALSE)
#Espacializar pontos
pts <- spatialize(occ)
mapview(pts)

# Criar grid de combinações de dispersal events and kernel_spread
# Quanto maior o kernel_spread, mais "longe" a espécie vai
# Distancia depende da resolução do raster de entrada
# Dispersal events: quantas vezes a espécie vai se dispersar a cada tempo
# Vamos testar várias combinações
comb_grid <- data.frame(dispersal_events = c(1, 5, 10, 15, 20, 25, 30, 35),
                        kernel_spread = c(2, 2, 2, 2, 2, 3, 4, 6))
tibble(comb_grid)

#Run M simulation
m <- m_simulations(data = occ, #Dataframe com longitude and latitude 
                   #Nomes das colunas com longitude e latitude
                   long = "decimalLongitude", lat = "decimalLatitude",
                   current_variables = current_variables, # Variaveis do presente
                   variables = variables, #Nome das variaveis para incluir
                   scale = TRUE, #Escalar variáveis ao fazer PCA?
                   center = TRUE, #Centralizar variáveis ao fazer PCA?
                   projection_dir = projection_dir, #Pasta com variaveis do passado
                   periods = periods, #Períodos, em ordem começando pelo mais recente
                   pattern = ".tif", #Formato dos arquivos na pasta projection_dir
                   initial_m_buffer = 1000, #Buffer inicial em torno dos pontos
                   suitability_threshold = 5, 
                   starting_proportion = 0.75, #Proporção de "populações" para começar simulação
                   proportion_to_disperse = 1, #Proporção de "populações" para começar simulação
                   sampling_rule = "random",
                   dispersal_kernel = "normal",
                   kernel_spread = 2,
                   max_dispersers = 4,
                   dispersal_events = 25,
                   comb_grid = comb_grid, #Grid de combinações de dispersal_events e kernel_spread
                   replicates = 3, #Número re réplicas
                   threshold = 5,
                   set_seed = 42,
                   skip_extinction = TRUE, #Pular cenários em que toda área fica inadequada
                   results_by_event = TRUE, #Retornar rasters resultantes por evento
                   results_by_scenario = TRUE, #Retornar rasters resultantes por cenário
                   remove_m_without_records = TRUE, #Remover M disjunto sem registros
                   extra_buffer = 50, #Buffer ao redor do m final (em km)
                   progress_bar = TRUE, #Mostrar barra de progresso
                   verbose = TRUE) #Mostrar mensagens

#Resumo dos resultados
# surplus_m é o número de poligonos disjuntos. Valores maiores que 0 indicam problemas
# Espécie pode ter surgido em vários lugares disjuntos? Talvez em áreas invadidas?
# occ_outside é o número de registros que ficaram fora do M. 
# Valores maiores que 0 indicam problemas
# Simulação também ajuda a identificar pontos problemáticos
# Se ponto está fora do M, isso indica que dificilmente o individuo chegou lá
# por si só naturalmente

# Ver resultados
m$summary %>% View()

#Vamos plotar alguns Ms
mapview(m$Combination_8$m_final, col.regions = "green", layer.name = "C8") +
  mapview(m$Combination_5$m_final, col.regions = "red", layer.name = "C5") +
  mapview(m$Combination_3$m_final, col.regions = "yellow", layer.name = "C3") +
  mapview(m$Combination_2$m_final, col.regions = "pink", layer.name = "C2") +
  mapview(m$Combination_1$m_final, col.regions = "lightblue", layer.name = "C1") +
  mapview(pts, layer.name = "pts")

# Ver resultados de simulação por cenário
plot(m$Combination_2$m_by_scen)

# Vamos gerar um GIF mostrando a simulação de algum M
gif_dispersion(rasters = m$Combination_2$m_by_event, #Escolher simulação por evento de uma combinação
               gif_file = file.path("M_poligonos/", sp, 
                                    "/Dispersion_by_event.gif"), #Pasta para salvar
               width = 2200,
               height = 1500,
               res = 300,
               delay = 0.15, #Controla tempo do gif
               loop = FALSE,
               progress_bar = TRUE,
               verbose = TRUE)

# Escolha uma das combinações para salvar como M
m_final <- m$Combination_2$m_final
writeVector(m_final,
            file.path("M_poligonos/", sp, "m_grinnell.gpkg"),
            overwrite = TRUE)
# Ver opções disponíveis
fs::dir_tree("M_poligonos/")
