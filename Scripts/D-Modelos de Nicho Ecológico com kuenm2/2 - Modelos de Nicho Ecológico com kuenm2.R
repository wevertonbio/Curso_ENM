#### Modelos de Nicho Ecológico com kuenm2 ####
# kuenm2 é uma atualização do kuenm:
# https://peerj.com/articles/6281.pdf

#' Ideia principal do kuenm:
#' Para cada espécie, cria diversos modelos candidatos.
#' Cada modelo candidato é uma combinação única de:
#'   - Variáveis ambientais
#'   - Feature: linear, quadrático, produto, theshold e hinge
#'   - Regularization multiplier: quanto menor o valor, mais complexo e ajstado 
#'                                é o modelo
#'                                
#' Dentre os modelos candidatos, são selecionados os melhores modelos baseado em:
#'  - Valor de pROC: seleciona modelos com valores de pROC significativamente
#'    maiores que de um modelo nulo.
#'  - Taxa de omissão: seleciona modelos com taxa de omissão menor que o threshold
#'    especificado.
#'  - AIC: dos modelos que sobraram, é calculado o valor de Delta AIC e mantido os
#'    modelos com Delta AIC menor que 2 (ou outro valor especificado).
#' 
#' O modelo candidato que passa por todos esses filtros é considerado o melhor 
#' modelo.
#' Se mais de um modelo candidato for selecionado, é possível obter um 
#' consenso/ensemble dos modelos: média ou mediana.
#' 



#' Novidades do kuenm2:
#' - Usa maxnet ao invés de maxent (mais rápido e menos arquivos gerados).
#' - Também ajusta modelos com GLM (necessita de presença/ausência)
#' - Modelos finais baseados em réplicas.
#' - Ajuste de modelos em paralelo.
#' - Possibilidade de testar e remover modelos com curvas côncavas (maior 
#' adequabilidade nos extremos)
#' Preparação dos dados e projeções muito mais fácil e rápida.
#' Curvas de resposta e importância de variáveis (baseado em deviance)
#' Análise de consensos entre diferentes GCMs: quantos GCMs predizem ganho ou 
#' perda de área adequada?
#' Análise de risco de extrapolação de projeções.
#' Funções para explorar dados que alimentam os modelos.
#' Possibilidade de transformar variáveis brutas em PCA-variáveis de duas
#' maneiras: externamente ou internamente.

# Remover todos os objetos #
rm(list = ls())

# Carregar pacotes
library(RuHere)
library(kuenm2)
library(terra)
library(data.table)
library(dplyr)
library(mapview)

#### Importar variáveis ####
v <- rast("Variaveis_Neotropico/Presente/Variaveis.tiff")

# Selecionar variáveis
minhas_variaveis <- readRDS("Variaveis_Neotropico/minhas_variaveis.rds")
minhas_variaveis

# Selecionar variaveis
v <- v[[minhas_variaveis]]
plot(v)

# Remover soilType
# Precisamos corrigir no kuenm2...
v <- v[[setdiff(names(v), "soilType")]]
names(v)

#### Araucaria angustifolia ####

#Como sempre, começamos atribuindo o nome da espécie e o diretório a objetos
sp <- "Araucaria angustifolia"
sp_dir <- file.path("Ocorrencias/", sp)
sp_dir

#Criar diretório para salvar modelos
sp_model <- file.path("Models/kuenm/", sp)
dir.create(sp_model, recursive = TRUE)

# Importar registros 
occ <- fread(file.path(sp_dir, "6-Pontos_rarefeitos.gz"),
             data.table = FALSE)

#Importar M
# Opções
fs::dir_tree("M_poligonos/")

#Escolha uma das opções
m_sp <- vect(file.path("M_poligonos/", sp, "m_grinnell.gpkg"))

#Espacializar pontos
pts <- spatialize(occ)
mapview(m_sp) + mapview(pts)


#### Preparar dados ####
?prepare_data #Ajuda da função

#Quantos pixels eu tenho na área acessível
v_m <- crop(v, m_sp, mask = TRUE) #Cortar variáveis para o M da espécie
global(v_m[[1]], fun="notNA") #Ver quantidade de pixels que não são NAs
# Geralmente, um background definido como 10% a 20% dos pontos é suficiente
nbg <- ceiling(global(v_m$bio_6, fun="notNA") * 0.1) %>% 
  as.numeric()
nbg

# Preparar dados
sp_swd <- prepare_data(algorithm = "maxnet", #Maxnet (maxent) ou GLM
                       occ = occ, #Tabela com ocorrências
                       species = sp, #Nome da espécie (opcional)
                       x = "decimalLongitude", #Coluna em occ com longitude
                       y = "decimalLatitude", #Coluna em occ com latitude
                       raster_variables = v, #Variáveis raster
                       mask = m_sp, #Area acessivel para cortar raster (opcional)
                       categorical_variables = NULL, #Alguma variável categorica?
                       do_pca = FALSE, #Fazer PCA das variáveis?
                       n_background = nbg, #Numero de pontos de background
                       partition_method = "kfolds", #Método de partição (treino e teste)
                       n_partitions = 4, #Numero de partições
                       min_number = 4, #Numero minimo de variáveis em cada modelo candidato
                       features = c("l", "lq", "lqp"), #Tipo de resposta
                       r_multiplier = c(0.1, 1, 5), #Regularizadores (geralmente usamos mais)
                       seed = 42) #Seed para definir partição dos dados
sp_swd$formula_grid %>% nrow() #Ver numero de modelos candidatos
View(sp_swd$formula_grid) #Ver formulas de cada modelo candidato
View(sp_swd$calibration_data) #Ver dados de calibração

#Salvar dados de preparação
saveRDS(sp_swd, file.path(sp_model, "calibration_data.rds"))


#### Explorar dados de calibração ####

# Cortar variáveis usando M
v_m <- crop(v, m_sp, mask = TRUE) #Cortar variáveis para o M da espécie
# Remover variavel categorica
# v_m <- v_m[[setdiff(names(v_m), "soilType")]]
# names(v_m)

# Histogramas dos dados de presença e background
calib_hist <- explore_calibration_hist(data = sp_swd,
                                       magnify_occurrences = 2,
                                       include_m = FALSE)
# Plot dos histogramas
plot_explore_calibration(explore_calibration = calib_hist,)

# Ver distribuição espacial de dados de treino e de teste
part_geo <- explore_partition_geo(data = sp_swd, raster_variables = v_m)
plot(part_geo$Presence)
mapview(part_geo$Presence)

# Ver distribuição das partições no espaço ambiental
part_env <- explore_partition_env(data = sp_swd, 
                                  raster_variables = v_m,
                                  use_pca = FALSE,
                                  variables = c("bio_6", "bio_12"))


# Ver se há problemas de extrapolação em alguma das partições
explore_part <- explore_partition_extrapolation(data = sp_swd)
View(explore_part$Mop_results)
# Plotar resultados
# Triângulos laranjas indicam pontos que estão fora do range dos dados de treino
# Isso significa que, quando for feito a predição para esse ponto, será uma extrapolação
plot_explore_partition(explore_part, variables = c("bio_6", "slope"))
plot_explore_partition(explore_part, variables = c("bio_12", "slope"))


#### Calibrar modelos candidatos e selecionar melhores modelos ####
?calibration

#Ver numero de cores disponíveis
parallel::detectCores()
# Usar 75% dos cores
ncores <- ceiling(parallel::detectCores() * 0.75)
ncores

m <- calibration(data = sp_swd,
                 error_considered = c(10, 15), #Erro considerado para calcular métricas (pode ser mais de 1)
                 omission_rate = 15, # Erro considerado para selecionar modelos (apenas 1)
                 remove_concave = TRUE, #Testar e remover curvas concavas?
                 parallel = TRUE, #Rodar em paralelo (mais rápido quando tem muitos modelos)
                 ncores = ncores) #Definir numero de cores para paralelização

m

# Ver todas as métricas de todos os modelos candidatos
View(m$calibration_results$Summary)

# Ver métricas de modelos selecionados
View(m$selected_models)
# Resumo da seleção de modelos: indices de modelos removidos e selecionados
m$summary

#Salvar modelos candidatos
saveRDS(m, file.path(sp_model, "candidate_models.rds"))

#Importar objeto novamente
m <- readRDS(file.path(sp_model, "candidate_models.rds"))

# Pode ser que você queria selecionar os modelos com base em outros valores de 
# omission rate ou AIC.
# Para selecionar modelos com base em outros valores de omission rate, esse valor
# deve ter sido um dos previamente determinados em omission_rate em calibration()
new_best_model <- select_models(calibration_results = m,
                                data = sp_swd,
                                algorithm  = "maxnet",
                                compute_proc = TRUE,
                                remove_concave = TRUE,
                                omission_rate = 10, #Agora 10 ao invés de 15
                                delta_aic = 10, #Agora 10 ao invés de 2
                                verbose = TRUE)
# Comparar modelos selecionamente previamente (omr = 10 e dAIC = 2) com modelos
# selecionados agora (omr = 15 e dAIC = 10)
m$summary$Selected #Modelos selecionados com omr = 15 e dAIC = 2
new_best_model$summary$Selected #Modelos selecionados com omr = 10 e dAIC = 10

# Caso queira usar os novos modelos selecionados, substitua os objetos:
# m$selected_models <- new_best_model$cand_final

# #Salvar novos modelos candidatos
# saveRDS(m, file.path(sp_model, "candidate_models.rds"))


### Ajustar modelos selecionados ####
# Podemos ajustar os modelos finais usando réplicas ou usando todos os dados de
# ocorrência em um único modelo (n_replicates = 1)

fm <- fit_selected(calibration_results = m, #Output de calibration()
                   replicate_method = "kfolds", #Método de partição (se n_replicates > 1)
                   n_replicates = 4, #Numero de réplicas
                   parallel = TRUE, #Em paralelo?
                   ncores = 4, #Numero de cores em paralelo
                   seed = 42) #Seed para definir partição dos dados
fm
# Alguns objetos resultantes:
names(fm$Models) #ID dos modelos ajustados
fm$thresholds #Thresholds para binarizar modelos individuais e consensos
# Theshold para binarizar corresponde ao threshold de omission rate determinado 
# em calibration

#Salvar modelos finais
saveRDS(fm, file.path(sp_model, "fitted_models.rds"))

#Importar objeto novamente
fm <- readRDS(file.path(sp_model, "fitted_models.rds"))


#### Projeção do modelo para cenário único ####
?predict_selected

p <- predict_selected(models = fm, #Modelos finais ajustados
                      raster_variables = v, #Variáveis
                      # mask = area_acessivel, #Mascara para cortar variaveis, aqui M da espécie
                      progress_bar = TRUE)

plot(p$Model_556$Partitions) #Predições de cada réplica
plot(p$Model_556$Model_consensus) #Consensos das réplicas
plot(p$General_consensus$mean) #Consensos entre os modelos (se houver mais de um modelo)

# Vamos usar a area acessivel para delimitar melhor a potencial distribuição da espécie
# Temos dois jeitos de fazer isso
# A mais comum, é simplesmente cortar o raster de predições usando a area acessivel como máscara
p_acessivel <- crop(p$General_consensus$mean, m_sp, mask = TRUE)
plot(p_acessivel)

#Ver com mapview
mapview(p_acessivel, col.regions = pals::brewer.rdylgn(7)) + 
  mapview(pts, cex = 2.5)

# Outra maneira, é usar a area_acessivel para definir como 0 a adequabilidade de toda área fora da área acessível
p_acessivel2 <- mask(p$General_consensus$mean, m_sp,
                     updatevalue = 0)
plot(c(p$General_consensus$mean,
       p_acessivel2),
     main = c("Sem M", "Com M"))

# Essa ultima abordagem é útil quando estamos trabalhando com várias espécies com Ms diferentes

# Quando ajustamos os modelos, definimos uma taxa de erro de 10% (ou 15%)
# Que significa que toleramos que 10% (ou 15%) dos nossos registros estejam em 
# condições ambientais que não representam o nicho ocupado pela espécie
# Podemos usar essa taxa para binarizar os modelos
# Para isso, usamos o valor de adequabilidade que deixará ~10% dos registros em 
# pixels inadequados (chamado threshold)
# Esse valor de threshold é armazenado no output de fit_selected()
fm$thresholds

# Vamos usar o threshold da média dos consensos
thr <- fm$thresholds$consensus$mean
thr

# Mapa binarizado
p_bin <- (p_acessivel >= thr) * 1 
plot(p_bin)
#Ver com mapview
mapview(p_bin, col.regions = c("gray", "forestgreen")) + mapview(pts, cex = 4)

#Vamos salvar o mapa binarizado
writeRaster(p_bin,
            file.path(sp_model, "Present_binarized.tif"), overwrite = TRUE)
writeRaster(p_acessivel,
            file.path(sp_model, "Present_continuous.tif"), overwrite = TRUE)
#### Curvas de resposta ####
# Qual o efeito de cada variável sobre a adequabilidade (quando todas as outras
# variáveis são mantidas constantes em sua média)?

# Ver variáveis disponíveis para curvas de resposta
sapply(fm$Models, function(x) names(x[[1]]$betas), simplify = FALSE)
# Além das curvas, o gráfico também mostra os limites da variável na área de
# calibração

# Curvas de respostas considerando todos os modelos
response_curve(models = fm,
               variable = "bio_5")
response_curve(models = fm,
               variable = "bio_6")
response_curve(models = fm,
               variable = "bio_12")
response_curve(models = fm,
               variable = "bio_15")
response_curve(models = fm,
               variable = "sand")


# Curvas de resposta de produtos - interação entre duas variáveis
bivariate_response(models = fm, modelID = "Model_556",
                   variable1 = "bio_15", variable2 = "bio_12")
bivariate_response(models = fm, modelID = "Model_556",
                   variable1 = "bio_5", variable2 = "bio_15")
bivariate_response(models = fm, modelID = "Model_556",
                   variable1 = "bio_5", variable2 = "bio_12")
bivariate_response(models = fm, modelID = "Model_556",
                   variable1 = "bio_5", variable2 = "sand")
bivariate_response(models = fm, modelID = "Model_556",
                   variable1 = "bio_12", variable2 = "sand")

#### Importância das variáveis ####
?var_importance

imp <- variable_importance(models = fm)
plot_importance(imp)
View(imp)
options(scipen = 999)

# Veja mais em:
browseURL("https://marlonecobos.github.io/kuenm2/")



