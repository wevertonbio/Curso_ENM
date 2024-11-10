#### Modelos de nicho com flexsdm ####
# Ver algoritmos disponíveis:
# https://sjevelazco.github.io/flexsdm/articles/v02_modeling.html

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

#Definir diretório para salvar modelos
sp_model <- file.path("Models/flexsdm/", sp)

# Importar dados de calibração
calib_bg <- fread(file.path(sp_model, "calib_background.gz"))
calib_psa <- fread(file.path(sp_model, "calib_psa.gz"))

#Se usou PCA, importar variaveis pca
v <- rast(file.path(sp_model, "pca_var", "Variables.tif"))
plot(v)

#Importar M
# Opções
list.files(file.path("M_poligonos/", sp), pattern = ".gpkg")
#Escolha uma das opções
m_sp <- vect(file.path("M_poligonos/", sp, "m_grinnell.gpkg"))

#Ver variaveis preditoras
colnames(calib_bg) %>% dput()

#### Ajustar modelo com Maxent ####
# Pontos de background
m_max_bg <- fit_max(
  data = calib_bg,
  response = "pr_ab",
  predictors = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6"), #Variaveis continuas
  #predictors_f = c("soilType"), #Variaveis categoricas
  partition = ".part",
  background = calib_bg %>% filter(pr_ab == 0),
  thr = c("max_sens_spec", "max_sorensen", 
          c("sensitivity", sens = '0.9')),
  clamp = TRUE,
  classes = "lpq",
  pred_type = "cloglog",
  regmult = 1)
# Ver métricas do modelo
View(m_max_bg$performance)
#Espacializar modelo
p_max_bg <- sdm_predict(models = m_max_bg, #Modelo
                        pred = v,  #Spatraster com preditores
                        thr = c("max_sens_spec", "max_sorensen", 
                                 c("sensitivity", sens = '0.9')),
                        predict_area = m_sp,
                        clamp = F,
                        pred_type = "cloglog")
plot(p_max_bg$max)

## Pontos de pseudoausencia
m_max_psa <- fit_max(
  data = calib_psa,
  response = "pr_ab",
  predictors = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6"), #Variaveis continuas
  #predictors_f = c("soilType"), #Variaveis categoricas
  partition = ".part",
  background = calib_psa %>% filter(pr_ab == 0),
  thr = c("max_sens_spec", "max_sorensen", 
          c("sensitivity", sens = '0.9')),
  clamp = TRUE,
  classes = "lpq",
  pred_type = "cloglog",
  regmult = 1)
# Ver métricas do modelo 
View(m_max_psa$performance) # VEJA VALOR DO AUC AGORA!!!
#Espacializar modelo
p_max_psa <- sdm_predict(models = m_max_psa, #Modelo
                        pred = v,  #Spatraster com preditores
                        thr = c("max_sens_spec", "max_sorensen", 
                                c("sensitivity", sens = '0.9')),
                        predict_area = m_sp,
                        clamp = F,
                        pred_type = "cloglog")
plot(p_max_psa$max)

#Comparar background e pseudo-ausencia
par(mfrow = c(2, 2)) #Gerar layout
plot(p_max_bg$max$max, main = "Background")
plot(p_max_psa$max$max, main = "Pseudoabsence")
plot(p_max_bg$max$max_sens_spec, main = "Background")
plot(p_max_psa$max$max_sens_spec, main = "Pseudoabsence")
par(mfrow = c(1, 1)) #Retornar layout original

# A diferença entre os mapas continuous não é tão grande
# A diferença é mais visível quando binarizamos

# Flexsdm também tem opção de hypertune com maxent (tune_max), Generalized 
# Boosted Regression (tune_gbm), Neural Networks (tune_net), Random Forest (tune_raf) 
# e Support Vector Machine (tune_svm)

#### Random forest (tuned) #### 
# Criar grid de parametros
tune_grid <- expand.grid(
  mtry = seq(1, 7, 1),
  ntree = c(400, 600, 800)
)
# Modelo com dados de background
m_raf_bg <- tune_raf(
  data = calib_bg,
  response = "pr_ab",
  predictors = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6"),
  predictors_f = NULL,
  fit_formula = NULL,
  partition = ".part",
  grid = tune_grid,
  thr = c("max_sens_spec", "max_sorensen", 
          c("sensitivity", sens = '0.9')),
  metric = "TSS",
  n_cores = 6)
View(m_raf_bg$hyper_performance) #Ver todas as metricas
View(m_raf_bg$performance) #Ver métricas dos modelos selecionados
#Espacializar modelo
p_raf_bg <- sdm_predict(models = m_raf_bg, #Modelo
                         pred = v,  #Spatraster com preditores
                         thr = c("max_sens_spec", "max_sorensen", 
                                 c("sensitivity", sens = '0.9')),
                         predict_area = m_sp,
                         clamp = F,
                         pred_type = "cloglog")
plot(p_raf_bg$raf)

# Modelo com dados de pseudoausencia
m_raf_psa <- tune_raf(
  data = calib_psa,
  response = "pr_ab",
  predictors = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6"),
  predictors_f = NULL,
  fit_formula = NULL,
  partition = ".part",
  grid = tune_grid,
  thr = c("max_sens_spec", "max_sorensen", 
          c("sensitivity", sens = '0.9')),
  metric = "TSS",
  n_cores = 6)
View(m_raf_psa$hyper_performance) #Ver todas as metricas
View(m_raf_psa$performance) #Ver métricas dos modelos selecionados
#Espacializar modelo
p_raf_psa <- sdm_predict(models = m_raf_psa, #Modelo
                         pred = v,  #Spatraster com preditores
                         thr = c("max_sens_spec", "max_sorensen", 
                                 c("sensitivity", sens = '0.9')),
                         predict_area = m_sp,
                         clamp = F,
                         pred_type = "cloglog")
plot(p_raf_psa$raf)
#Comparar background e pseudo-ausencia
par(mfrow = c(2, 2)) #Gerar layout
plot(p_raf_bg$raf$raf, main = "Background")
plot(p_raf_psa$raf$raf, main = "Pseudoabsence")
plot(p_raf_bg$raf$max_sens_spec, main = "Background")
plot(p_raf_psa$raf$max_sens_spec, main = "Pseudoabsence")
par(mfrow = c(1, 1)) #Retornar layout original

#### GLM ####
# Modelo com dados de background
m_glm_bg <- fit_glm(
  data = calib_bg,
  response = "pr_ab",
  predictors = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6"),
  predictors_f = NULL,
  fit_formula = NULL,
  partition = ".part",
  thr = c("max_sens_spec", "max_sorensen", 
          c("sensitivity", sens = '0.9')))
View(m_glm_bg$performance) #Ver métricas do modelo
#Espacializar modelo
p_glm_bg <- sdm_predict(models = m_glm_bg, #Modelo
                        pred = v,  #Spatraster com preditores
                        thr = c("max_sens_spec", "max_sorensen", 
                                c("sensitivity", sens = '0.9')),
                        predict_area = m_sp,
                        clamp = F,
                        pred_type = "cloglog")
plot(p_glm_bg$glm)

# Modelo com dados de pseudoausencia
m_glm_psa <- fit_glm(
  data = calib_psa,
  response = "pr_ab",
  predictors = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6"),
  predictors_f = NULL,
  fit_formula = NULL,
  partition = ".part",
  thr = c("max_sens_spec", "max_sorensen", 
          c("sensitivity", sens = '0.9')))
View(m_glm_psa$performance) #Ver métricas do modelo
#Espacializar modelo
p_glm_psa <- sdm_predict(models = m_glm_psa, #Modelo
                         pred = v,  #Spatraster com preditores
                         thr = c("max_sens_spec", "max_sorensen", 
                                 c("sensitivity", sens = '0.9')),
                         predict_area = m_sp,
                         clamp = F,
                         pred_type = "cloglog")
plot(p_glm_psa$glm)
#Comparar background e pseudo-ausencia
par(mfrow = c(2, 2)) #Gerar layout
plot(p_glm_bg$glm$glm, main = "Background")
plot(p_glm_psa$glm$glm, main = "Pseudoabsence")
plot(p_glm_bg$glm$max_sens_spec, main = "Background")
plot(p_glm_psa$glm$max_sens_spec, main = "Pseudoabsence")
par(mfrow = c(1, 1)) #Retornar layout original

#### Neural network ####
# Modelo com dados de background
m_net_bg <- fit_net(
  data = calib_bg,
  response = "pr_ab",
  predictors = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6"),
  predictors_f = NULL,
  fit_formula = NULL,
  partition = ".part",
  thr = c("max_sens_spec", "max_sorensen", 
          c("sensitivity", sens = '0.9')))
View(m_net_bg$performance) #Ver métricas do modelo
#Espacializar modelo
p_net_bg <- sdm_predict(models = m_net_bg, #Modelo
                        pred = v,  #Spatraster com preditores
                        thr = c("max_sens_spec", "max_sorensen", 
                                c("sensitivity", sens = '0.9')),
                        predict_area = m_sp,
                        clamp = F,
                        pred_type = "cloglog")
plot(p_net_bg$net)

# Modelo com dados de pseudoausencia
m_net_psa <- fit_net(
  data = calib_psa,
  response = "pr_ab",
  predictors = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6"),
  predictors_f = NULL,
  fit_formula = NULL,
  partition = ".part",
  thr = c("max_sens_spec", "max_sorensen", 
          c("sensitivity", sens = '0.9')))
View(m_net_psa$performance) #Ver métricas do modelo
#Espacializar modelo
p_net_psa <- sdm_predict(models = m_net_psa, #Modelo
                         pred = v,  #Spatraster com preditores
                         thr = c("max_sens_spec", "max_sorensen", 
                                 c("sensitivity", sens = '0.9')),
                         predict_area = m_sp,
                         clamp = F,
                         pred_type = "cloglog")
plot(p_net_psa$net)
#Comparar background e pseudo-ausencia
par(mfrow = c(2, 2)) #Gerar layout
plot(p_net_bg$net$net, main = "Background")
plot(p_net_psa$net$net, main = "Pseudoabsence")
plot(p_net_bg$net$max_sens_spec, main = "Background")
plot(p_net_psa$net$max_sens_spec, main = "Pseudoabsence")
par(mfrow = c(1, 1)) #Retornar layout original

#### GAM - Generalized Additive Models ####
# Modelo com dados de background
m_gam_bg <- fit_gam(
  data = calib_bg,
  response = "pr_ab",
  predictors = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6"),
  predictors_f = NULL,
  partition = ".part",
  thr = c("max_sens_spec", "max_sorensen", 
          c("sensitivity", sens = '0.9')))
View(m_gam_bg$performance) #Ver métricas do modelo
#Espacializar modelo
p_gam_bg <- sdm_predict(models = m_gam_bg, #Modelo
                        pred = v,  #Spatraster com preditores
                        thr = c("max_sens_spec", "max_sorensen", 
                                c("sensitivity", sens = '0.9')),
                        predict_area = m_sp,
                        clamp = F,
                        pred_type = "cloglog")
plot(p_gam_bg$gam)

# Modelo com dados de pseudoausencia
m_gam_psa <- fit_gam(
  data = calib_psa,
  response = "pr_ab",
  predictors = c("PC1", "PC2", "PC3", "PC4", "PC5", "PC6"),
  predictors_f = NULL,
  fit_formula = NULL,
  partition = ".part",
  thr = c("max_sens_spec", "max_sorensen", 
          c("sensitivity", sens = '0.9')))
View(m_gam_psa$performance) #Ver métricas do modelo
#Espacializar modelo
p_gam_psa <- sdm_predict(models = m_gam_psa, #Modelo
                         pred = v,  #Spatraster com preditores
                         thr = c("max_sens_spec", "max_sorensen", 
                                 c("sensitivity", sens = '0.9')),
                         predict_area = m_sp,
                         clamp = F,
                         pred_type = "cloglog")
plot(p_gam_psa$gam)
#Comparar background e pseudo-ausencia
par(mfrow = c(2, 2)) #Gerar layout
plot(p_gam_bg$gam$gam, main = "Background")
plot(p_gam_psa$gam$gam, main = "Pseudoabsence")
plot(p_gam_bg$gam$max_sens_spec, main = "Background")
plot(p_gam_psa$gam$max_sens_spec, main = "Pseudoabsence")
par(mfrow = c(1, 1)) #Retornar layout original

#### Ensemble de modelos ####

# Pontos de background
lista_de_modelos_bg <- list(m_max_bg, m_raf_bg, m_glm_bg, m_net_bg, m_gam_bg)

m_ens_bg <- fit_ensemble(
  models = lista_de_modelos_bg,
  ens_method = c("mean", "meanw", "meansup", "meanthr", "median"),
  thr = "max_sens_spec",
  thr_model = "max_sens_spec",
  metric = "TSS")
View(m_ens_bg$performance) #Ver métricas dos modelos

#Espacializar ensemble
p_ens_bg <- sdm_predict(models = m_ens_bg, #Modelo
                         pred = v,  #Spatraster com preditores
                         thr = c("max_sens_spec", "max_sorensen", 
                                 c("sensitivity", sens = '0.9')),
                         predict_area = m_sp,
                         clamp = F,
                         pred_type = "cloglog")
#Simple average of the different models
plot(p_ens_bg$mean)
#Weighted average of models based on their performance.
plot(p_ens_bg$meanw)
#Average of the best models (evaluation metric above the average)
plot(p_ens_bg$meansup) 
#Averaging performed only with those cells with suitability values above the 
#selected threshold.
plot(p_ens_bg$meanthr) 

# Pontos de pseudoausência
lista_de_modelos_psa <- list(m_max_psa, m_raf_psa, m_glm_psa, m_net_psa, m_gam_psa)

m_ens_psa <- fit_ensemble(
  models = lista_de_modelos_psa,
  ens_method = c("mean", "meanw", "meansup", "meanthr", "median"),
  thr = "max_sens_spec",
  thr_model = "max_sens_spec",
  metric = "TSS")
View(m_ens_psa$performance) #Ver métricas dos modelos

#Espacializar ensemble
p_ens_psa <- sdm_predict(models = m_ens_psa, #Modelo
                        pred = v,  #Spatraster com preditores
                        thr = c("max_sens_spec", "max_sorensen", 
                                c("sensitivity", sens = '0.9')),
                        predict_area = m_sp,
                        clamp = F,
                        pred_type = "cloglog")
#Simple average of the different models
plot(p_ens_psa$mean)
#Weighted average of models based on their performance.
plot(p_ens_psa$meanw)
#Average of the best models (evaluation metric above the average)
plot(p_ens_psa$meansup) 
#Averaging performed only with those cells with suitability values above the 
#selected threshold.
plot(p_ens_psa$meanthr) 

#Comparar background e pseudo-ausencia
par(mfrow = c(2, 2)) #Gerar layout
plot(p_ens_bg$meanw$meanw, main = "Background")
plot(p_ens_psa$meanw$meanw, main = "Pseudoabsence")
plot(p_ens_bg$meanw$max_sens_spec, main = "Background")
plot(p_ens_psa$meanw$max_sens_spec, main = "Pseudoabsence")
par(mfrow = c(1, 1)) #Retornar layout original

#### Reduzir "overprediction" ####
# Dealing with overprediction in species distribution models: 
# How adding distance constraints can improve model accuracy
#https://www.sciencedirect.com/science/article/pii/S0304380020302519

#Métodos a priori: gera uma variável de distância dos pontos de ocorrência, que
# pode ser usada como uma variável preditora do modelo.
# "A priori methods should be avoided in combination with complex algorithms"
?msdm_priori

# Métodos a posteriori: limita distribuição da espécie depois de binarizar os
# modelos no espaço geográfico.
?msdm_posteriori #Ver métodos
# Our results indicate that M-SDMs, except by the minimum convex method,
# can be useful to reduce model overprediction compared to ENMs

#Vamos testar os 4 métodos
# Threshold para binarizar
th <- "max_sens_spec"

# Occurrences Based Restriction (obr)
p_obr <- msdm_posteriori(
  records = calib_bg,
  x = "x",
  y = "y",
  pr_ab = "pr_ab",
  cont_suit = p_ens_bg$meanw$meanw, #SpatRaster com valores de adequabilidade
  method = "obr",
  thr = th)

# Only occurrences based restriction
p_pres <- msdm_posteriori(
  records = calib_bg,
  x = "x",
  y = "y",
  pr_ab = "pr_ab",
  cont_suit = p_ens_bg$meanw$meanw, #SpatRaster com valores de adequabilidade
  method = "pres",
  thr = th)

#Lower quantile
p_lq <- msdm_posteriori(
  records = calib_bg,
  x = "x",
  y = "y",
  pr_ab = "pr_ab",
  cont_suit = p_ens_bg$meanw$meanw, #SpatRaster com valores de adequabilidade
  method = "lq",
  thr = th)

# Minimum convex polygon
p_mcp <- msdm_posteriori(
  records = calib_bg,
  x = "x",
  y = "y",
  pr_ab = "pr_ab",
  cont_suit = p_ens_bg$meanw$meanw, #SpatRaster com valores de adequabilidade
  method = "mcp",
  thr = th)

# Buffered Minimum Convex Polygon
# Semelhante a limitar ocorrências para dentro do M da espécie!
p_bmcp <- msdm_posteriori(
  records = calib_bg,
  x = "x",
  y = "y",
  pr_ab = "pr_ab",
  cont_suit = p_ens_bg$mean$mean, #SpatRaster com valores de adequabilidade
  method = "bmcp",
  buffer = 100,
  thr = th,
  crs = crs(p_ens_bg$meanw$meanw))

#Comparar distribuições
#Continuo
all_p_cont <- c(p_ens_bg$meanw$max_sens_spec,
           p_obr[[1]], 
           p_pres[[1]],
           p_lq[[1]],
           p_mcp[[1]],
           p_bmcp[[1]])
names(all_p_cont) <- c("Original", "obr", "pres", "lq", "mcp", "bmcp")
plot(all_p_cont)

#Binariza
all_p_bin <- c(p_ens_bg$meanw$max_sens_spec,
                p_obr[[2]], 
                p_pres[[2]],
                p_lq[[2]],
                p_mcp[[2]],
                p_bmcp[[2]])
names(all_p_bin) <- c("Original", "obr", "pres", "lq", "mcp", "bmcp")
plot(all_p_bin)

# Função pode ser utilizada com dados do kuenm2 ou outro pacote de modelagem

