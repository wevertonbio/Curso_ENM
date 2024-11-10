#### Preparar dados para modelos com flexsdm ####
# Site bastante didática do pacote:
# https://sjevelazco.github.io/flexsdm/index.html
# Artigo: https://besjournals.onlinelibrary.wiley.com/doi/pdfdirect/10.1111/2041-210X.13874

# Pacote possui várias funções de pré-processamento, modelagem e pós processamento

# Os arquivos básicos de entrada são semelhantes aos usados no kuenm2:
# Spatraster com variáveis preditoras
# Ocorrências com colunas espécie, x (longitude) e y (latitude)
# SpatVector representando M da espécie

# Carregar pacotes
library(terra)
library(data.table)
library(dplyr)
library(mapview)
library(flexsdm)

#### Importar variáveis ####
v <- rast("Variaveis_Neotropico/Presente/Variaveis.tiff")

# Excluir variáveis 08, 09, 18 e 19 por apresentarem variações bruscas
# Essas variações não são naturais, mas artefatos estatísticos pela maneira como
# são calculadas
# https://onlinelibrary.wiley.com/doi/abs/10.1111/aec.13234
# Identificar posição das variáveis
id_remove <- which(names(v) %in% c("bio_8", "bio_9", "bio_18", "bio_19"))
v <- v[[-id_remove]]
names(v)

#Se for selecionar variaveis, siga esses passos
#var_to_keep <- c("Nomes das variaveis selecionadas")
#v <- v[[var_to_keep]]

#### Araucaria angustifolia ####

#Como sempre, começamos atribuindo o nome da espécie e o diretório a objetos
sp <- "Araucaria angustifolia"
sp_dir <- file.path("Ocorrencias/", sp)
sp_dir

#Criar diretório para salvar modelos
sp_model <- file.path("Models/flexsdm/", sp)
dir.create(sp_model, recursive = TRUE)

# Importar registros 
occ <- fread(file.path("Ocorrencias/", sp, "Check_points/D - Pontos_finais.gz"),
             data.table = FALSE)
#Importar M
# Opções
list.files(file.path("M_poligonos/", sp), pattern = ".gpkg")
#Escolha uma das opções
m_sp <- vect(file.path("M_poligonos/", sp, "m_grinnell.gpkg"))

#Espacializar pontos
pts <- vect(occ, geom = c(x = "x", #Converte pontos para spatvector
                          y = "y"), crs = "+init=epsg:4326")
mapview(m_sp) +
  mapview(pts, #Converte pontos para spatvector
          burst = TRUE) #Filtrar por valor da coluna

#### Reduzir colinearidade com PCA ####
# Cortar variáveis para M da espécie
v_m <- crop(v, m_sp, mask = TRUE)
#Separar variável categórica de tipo de solo
soil <- v_m$soilType
v_m2 <- v_m[[names(v_m) != "soilType"]]

#Rodar PCA e selecionar eixos que retem 95% da explicação
pca_var <- correct_colinvar(v_m2, method = c("pca"))
plot(pca_var$env_layer)
#Adicionar solo as variáveis
pca_var$env_layer <- c(pca_var$env_layer, soil)
plot(pca_var$env_layer)

#Aqui, escolha se quer usar PCA ou apenas algumas variáveis
# my_var <- pca_var$env_layer
# my_var <- v_m[["Nomes das variáveis"]]

# Se for usar PCA, salve variáveis
# Isso porque flexsdm não faz PCA internamente
dir.create(file.path(sp_model, "pca_var"))
writeRaster(my_var, 
            file.path(sp_model, "pca_var", "Variables.tif"),
            overwrite = T)

#### Particionar pontos de treino e de teste ####
# Nas ocorrências, precisa criar uma coluna indicando que são pontos de presença
occ$pr_ab <- 1

# Separar aleatoriamente
sp_part_random <- part_random(
  data = occ,
  pr_ab = "pr_ab",
  method = c(method = "kfold", folds = 4)
)
head(sp_part_random) #Função cria coluna com partição

# Separar em bandas/faixas de longitude ou latitude
sp_part_band <- part_sband(
  env_layer = my_var,
  data = occ,
  x = "x",
  y = "y",
  pr_ab = "pr_ab",
  type = "lat", # specify bands across different degrees of longitude 'lon' or latitude 'lat'.
  min_bands = 2, # minimum number of spatial bands to be tested
  max_bands = 20, # maximum number of spatial bands to be tested
  n_part = 4, #Numero de partições
  prop = 0.5)
plot(sp_part_band$grid, col = gray.colors(20))
points(sp_part_band$part[c("x", "y")],
       col = rainbow(8)[sp_part_band$part$.part],
       cex = 0.9,
       pch = c(1, 19)[sp_part_band$part$pr_ab + 1])

#Separar em blocos (tabuleiro de xadrez)
sp_part_block <- part_sblock(
  env_layer = my_var,
  data = occ,
  x = "x",
  y = "y",
  pr_ab = "pr_ab",
  min_res_mult = 10, # Minimum value used for multiplying raster resolution and define the finest resolution to be tested
  max_res_mult = 100, # Maximum value used for multiplying raster resolution and define the coarsest resolution to be tested
  num_grids = 30, # Number of grid to be tested between min_res_mult X (raster resolution) and max_res_mult X (raster resolution)
  n_part = 4, # Number of partitions
  prop = 0.5 # Proportion of points used for testing autocorrelation between groupds (0-1)
)
plot(sp_part_block$grid)
points(sp_part_block$part[c("x", "y")],
       col = rainbow(8)[sp_part_block$part$.part],
       cex = 0.5,
       pch = 19
)
# Cortar blocos usando o M da espécie
grid_env <- get_block(env_layer = my_var, 
                      best_grid = sp_part_block$grid)
plot(grid_env) # this is a block layer with the same layer properties as environmental variables.
points(sp_part_block$part[c("x", "y")],
       col = rainbow(8)[sp_part_block$part$.part],
       cex = 0.5,
       pch = 19
)

# Separar usando cross-validação espacial
# Separa pontos em k-grupos de acordo com autocorrelação espacial
# Pontos com maior autocorrelação tendem a ficar no mesmo grupo
sp_part_cross <- part_senv(
  env_layer = my_var,
  data = occ,
  x = "x",
  y = "y",
  pr_ab = "pr_ab",
  min_n_groups = 2, # Minimum number of groups to be tested
  max_n_groups = 10, # Maximum number of groups to be tested
  prop = 0.5 # Proportion of points used for testing autocorrelation between groups (0-1)
)
plot(m_sp)
points(sp_part_cross$part[c("x", "y")],
       col = hcl.colors(length(unique(sp_part_cross$part)))[sp_part_cross$part$.part],
       cex = 1,
       pch = 19
)

#Escolha uma das alternativas de partição para prosseguir:
# sp_part_random, sp_part_band, sp_part_block ou sp_part_cross
sp_part <- sp_part_block
p_data <- sp_part$part

# #Se método de partição for band ou block, gere um grid
grid_env <- get_block(env_layer = my_var,
                      best_grid = sp_part$grid)
plot(grid_env)
points(sp_part$part[c("x", "y")],
       col = rainbow(8)[sp_part$part$.part],
       cex = 0.5,
       pch = 19)

#### Selecionar pontos de background ####
set.seed(42) #Para gerar sempre mesmos resultados
# Definir numero de pontos de background por partição
nbg <- floor(10000/4) #Arredondar para cima se for decimal
nbg
#Ver partições
part <- unique(sp_part$part$.part)
part

#### Amostrar background ####
bg <- lapply(part, function(x) {
  b_x <- sample_background(
    data = p_data, #Tabela com partição
    x = "x",
    y = "y",
    n = nbg, # number of background points to be sampled
    method = "random",
    rlayer = grid_env, # Se partição for band ou block, use grid_env. Se não, my_var
    maskval = x, #Se partição for band ou block, use essa opção. Se não, use NULL
    calibarea = NULL) # A SpatVector which delimit the calibration area used for a given species
  b_x$.part <- x
  return(b_x)
}) %>%
  bind_rows() %>%
  mutate(pr_ab = 0)
#Plotar background
plot(grid_env, main = "Background points")
points(bg, cex = .1, pch = 19)

# Amostrar 'pseudoausências'
# Define áreas inadequadas para espécie usando bioclim (envelope climático)
# Amostragem de pontos de pseudoausência acontece apenas nas áreas inadequadas
set.seed(42) #Para gerar sempre mesmos resultados
psa <- lapply(part, function(x) {
  psa_x <- sample_pseudoabs(
    data = p_data,
    x = "x",
    y = "y",
    n = nbg, # number of pseudo-absence points to be sampled
    method = c("env_const", env = my_var),
    rlayer = grid_env, # Se partição for band ou block, use grid_env. Se não, my_var
    maskval = x, #Se partição for band ou block, use essa opção. Se não, use NULL
    calibarea = NULL) # A SpatVector which delimit the calibration area used for a given species
  psa_x$.part <- x
  return(psa_x)
}) %>%
  bind_rows() %>%
  mutate(pr_ab = 0)

#Comparar background e pseudo-ausencia
par(mfrow = c(1, 2)) #Gerar layout
plot(grid_env, main = "Background points")
points(bg, cex = .1, pch = 19)
plot(grid_env, main = "Pseudo-absences")
points(psa, cex = .1, pch = 19)
par(mfrow = c(1, 1)) #Retornar layout original

#Vamos trabalhar com as duas opções para ver a diferença

#### Extrair valores de variáveis ambientais ####
# Unid dados de presença com background/pseudoausencia
p_bg <- bind_rows(p_data, bg)
p_psa <- bind_rows(p_data, psa)

#Extrair dados
#Usando pontos de background
calib_bg <- sdm_extract(
  data = p_bg,
  x = "x",
  y = "y",
  env_layer = my_var, # Raster with environmental variables
  variables = NULL, # Vector with the variable names of predictor
  # variables Usage variables. = c("aet", "cwd", "tmin"). If no variable is specified, function will return data for all layers.
  filter_na = TRUE
)
View(calib_bg)

#Usando pontos de pseudoausencia
calib_psa <- sdm_extract(
  data = p_psa,
  x = "x",
  y = "y",
  env_layer = my_var, # Raster with environmental variables
  variables = NULL, # Vector with the variable names of predictor
  # variables Usage variables. = c("aet", "cwd", "tmin"). If no variable is specified, function will return data for all layers.
  filter_na = TRUE
)
View(calib_bg)

#Salvar dados de calibração
fwrite(calib_bg,
       file.path(sp_model, "calib_background.gz"),
       row.names = FALSE)
fwrite(calib_psa,
       file.path(sp_model, "calib_psa.gz"),
       row.names = FALSE)
