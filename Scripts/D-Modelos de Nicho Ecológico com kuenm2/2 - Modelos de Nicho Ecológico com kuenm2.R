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


# Carregar pacotes
library(kuenm2)
library(terra)
library(data.table)
library(dplyr)
library(mapview)

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
sp_model <- file.path("Models/kuenm/", sp)
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


#### Preparar dados ####
?prepare_data #Ajuda da função
sp_swd <- prepare_data(model_type = "glmnet",
                       occ = occ,
                       species = sp, x = "x", y = "y",
                       spat_variables = v,
                       mask = m_sp,
                       categorical_variables = "soilType",
                       do_pca = TRUE, 
                       deviance_explained = 95,
                       min_explained = 5, center = TRUE, scale = TRUE,
                       write_pca = FALSE, output_pca = NULL, nbg = 10000,
                       kfolds = 4, weights = NULL, min_number = 4,
                       min_continuous = 2,
                       features = c("l", "q", "p", "lq", "lqp"),
                       regm = c(0.1, 1, 3, 5),
                       include_xy = TRUE,
                       write_file = FALSE, file_name = NULL,
                       seed = 42)

sp_swd$formula_grid %>% nrow() #Ver numero de modelos candidatos
View(sp_swd$formula_grid) #Ver formulas de cada modelo candidato
View(sp_swd$calibration_data) #Ver dados de calibração

#Salvar dados de preparação
saveRDS(sp_swd, file.path(sp_model, "calibration_data.rds"))

# Ver distribuição espacial de dados de presença e de background/ausência

v_m <- crop(v, m_sp, mask = TRUE) #Cortar variáveis para o M da espécie
pbg <- explore_calibration_geo(data = sp_swd, spat_variables = v_m,
                               plot = FALSE)
plot(pbg)

### Histograma das variáveis em dados de presença e de background/ausência
explore_calibration_hist(data = sp_swd,
                         color_background = "#0000FF80",
                         color_presence = "#FF000080",
                         mfrow = c(2, 3), 
                         plot_median = TRUE,
                         breaks = "Scott")

#### Calibrar modelos candidatos e selecionar melhores modelos ####
?calibration

#Ver numero de cores disponíveis
parallel::detectCores()

m <- calibration(data = sp_swd,
                 test_concave = TRUE, #Testar e remover curvas concavas?
                 parallel = TRUE, #Rodar em paralelo (mais rápido quando tem muitos modelos)
                 ncores = 8, #Definir numero de cores para paralelização
                 progress_bar = TRUE,
                 parallel_type = "doSNOW",
                 return_replicate = TRUE,
                 omission_rate = c(5, 10), #Thesholds para calcular métricas (pode ser mais de um)
                 omrat_threshold = 10, #Threshold para selecionar modelos!
                 allow_tolerance = TRUE,
                 tolerance = 0.01,
                 AIC = "ws",
                 delta_aic = 2,
                 verbose = TRUE)
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
new_best_model <- sel_best_models(cand_models = m$calibration_results$Summary,
                                  model_type = "glmnet",
                                  test_concave = TRUE,
                                  omrat_threshold = 5, #Agora 5 ao invés de 10
                                  allow_tolerance = TRUE,
                                  tolerance = 0.01,
                                  AIC = "ws",
                                  significance = 0.05,
                                  delta_aic = 10, #Maior valor de deltaAIC: 10 ao invés de 2
                                  verbose = TRUE)
# Comparar modelos selecionamente previamente (omr = 10 e dAIC = 2) com modelos
# selecionados agora (omr = 5 e dAIC = 10)
m$summary$Selected #Modelos selecionados com omr = 10 e dAIC = 2
new_best_model$summary$Selected #Modelos selecionados com omr = 5 e dAIC = 10

# Caso queira usar os novos modelos selecionados, substitua os objetos:
# m$selected_models <- new_best_model$cand_final

# #Salvar novos modelos candidatos
# saveRDS(m, file.path(sp_model, "candidate_models.rds"))


### Ajustar modelos selecionados ####
# Podemos ajustar os modelos finais usando réplicas ou usando todos os dados de
# ocorrência em um único modelo (n_replicates = 1)

fm <- fit_selected(calibration_results = m,
                   n_replicates = 10, #Numero de réplicas
                   rep_type = "bootstrap", #Método de partição (se n_replicates > 1)
                   train_portion = 0.75, #Porção de pontos usados como treino (se n_replicates > 1)
                   write_models = FALSE,
                   file_name = NULL,
                   parallel = TRUE, #Em paralelo?
                   ncores = 6, #Numero de cores em paralelo
                   parallelType = "doSNOW",
                   progress_bar = TRUE,
                   verbose = TRUE,
                   seed = 42)
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
                      spat_var = v, #Variáveis
                      mask = m_sp, #Mascara para cortar variaveis, aqui M da espécie
                      consensus_per_model = TRUE,
                      consensus_general = TRUE,
                      consensus = c("median", "range", "mean", "stdev"),
                      clamping = FALSE, #Clamp valores para minimos e máximos da área de calibração
                      var_to_clamp = NULL, #Variáveis para fazer clamp
                      type = "cloglog",
                      progress_bar = TRUE)
plot(p$Model_432$Replicates) #Predições de cada réplica
plot(p$Model_432$Model_consensus) #Consensos das réplicas
plot(p$General_consensus) #Consensos entre os modelos (se houver mais de um modelo)

#Ver com mapview
mapview(p$General_consensus$mean) + mapview(pts, cex = 4)

# Mapa binarizado
p_bin <- (p$General_consensus$mean >= fm$thresholds$consensus$mean) * 1
plot(p_bin)
#Ver com mapview
mapview(p_bin) + mapview(pts, cex = 4)

#### Curvas de resposta ####
# Qual o efeito de cada variável sobre a adequabilidade (quando todas as outras
# variáveis são mantidas constantes em sua média)?

# Ver variáveis disponíveis para curvas de resposta
sapply(fm$Models, function(x) names(x[[1]]$betas), simplify = FALSE)
# Além das curvas, o gráfico também mostra os limites da variável na área de 
# calibração

# Curvas de respostas considerando todos os modelos
response_curve(models = fm,
               variable = "PC1", by_replicates = TRUE)
response_curve(models = fm,
               variable = "PC3", by_replicates = TRUE)
response_curve(models = fm,
               variable = "PC4", by_replicates = TRUE)
response_curve(models = fm,
               variable = "PC5", by_replicates = TRUE)
# Curvas de resposta por modelo
response_curve(models = fm, variable = "PC1",
               modelID = "Model_432", by_replicates = T)
response_curve(models = fm, variable = "PC4",
               modelID = "Model_432", by_replicates = T)

# Curvas de resposta de produtos - interação entre duas variáveis
resp2var(models = fm, modelID = "Model_432",
         variable1 = "PC1", variable2 = "PC4")
resp2var(models = fm, modelID = "Model_432",
         variable1 = "PC1", variable2 = "PC5")
resp2var(models = fm, modelID = "Model_432",
         variable1 = "PC3", variable2 = "PC5")

#### Importância das variáveis ####
?var_importance

imp <- var_importance(models = fm)
enmpa::plot_importance(imp)


#### Reduzir predição ####







