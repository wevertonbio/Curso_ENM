#### Projeções para o futuro com kuenm2 ####

#' Ao projetar a adequabilidade para outros cenários de tempo (passado ou futuro),
#' lidamos com várias incertezas.
#' Tempo: 2030, 2050, 2070, 2100
#' SSP: 1.26, 2.45, 3.70, 5.85
#' Dezenas de GCM's
#' 
#' Ideal é projetar modelos para vários cenários e analisar certezas (consensos) e
#' incertezas (variâncias).
#' 
#' kuenm2 facilita essas análises

# Carregar pacotes
library(kuenm2)
library(terra)
library(data.table)
library(dplyr)
library(mapview)
library(tidyterra)
library(ggplot2)

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

#Identificar diretório onde modelos foram salvos
sp_model <- file.path("Models/kuenm", sp)

#Importar M
# Opções
list.files(file.path("M_poligonos/", sp), pattern = ".gpkg")
#Escolha uma das opções
m_sp <- vect(file.path("M_poligonos/", sp, "m_grinnell.gpkg"))

#Importar modelos finais ajustados
fm <- readRDS(file.path(sp_model, "fitted_models.rds"))
fm

#### Preparar variáveis para projetar para o futuro ####
#' Para projetar para o futuro, precisamos organizar as variáveis em pastas 
#' aninhadas: GCM's dentro de SSP's dentro de Tempos/Anos
#' Se for usar as projeções disponíveis no WorldClim, o kuenm2 tem uma função
#' que organiza as projeções automaticamente. Baste colocar todas as variáveis
#' em uma mesma pasta
list.files("Variaveis_brutas/Futuro/wc2.1_5m/") #Ver variáveis na pasta

# Organizar variáveis
?organize_future_worldclim


# Criar pasta para salvar variáveis do futuro
out_dir_future <- file.path("Variaveis_Neotropico/Futuro")
out_dir_future #Ver pasta

# Vamos cortar as variáveis para o Neotropico
neot <- vect("Shapefiles/Neotropical.gpkg")

# Lembrar formato dos nomes das variáveis do presente
names(v) #bio_

#Variáveis fixas (que vão se manter constantes no futuro)
v_fixed <- c(v$clay, v$sand, v$soilType, v$slope)

#Organizar e renomear variáveis
organize_future_worldclim(input_dir = "Variaveis_brutas/Futuro/wc2.1_5m/", #Pasta com variaveis do futuro
                          output_dir = out_dir_future, #Pasta onde serão salvos variáveis processadas
                          name_format = "bio_", #Formato do nome
                          fixed_variables = v_fixed, #Variaveis fixas
                          mask = neot, #Mascara para corte (mesma que usou para variáveis do Presente)
                          overwrite = TRUE) #Sobrescrever arquivos?
# Checar pastas
list.dirs(out_dir_future)
#Importar uma variável para teste (copie um dos caminhos)
v_future <- rast("Variaveis_Neotropico/Futuro/2041-2060/ssp126/HadGEM3-GC31-LL/Variables.tiff")
names(v_future) #Nomes das variáveis
plot(v_future[[1:4]]) #plot de algumas variáveis

# Caso deseje utilizar projeções de outras fontes (CHELSA) ou tempos (passado), é
# preciso organizar as variáveis e pastas manualmente, seguindo esse formato :(
# Futuro/Ano/SSP/GCM, ex: Futuro/2100/ssp585/MIROC6/Variables.tif
# Passado/Período/GCM, ex: Passado/LGM/MIROC6/Variables.tif

#Extrair ano/periods, ssp e gcms
periods <- list.dirs(out_dir_future, recursive = FALSE, full.names = FALSE)
periods
scenarios <- list.dirs(file.path(out_dir_future, periods[1]),
                       recursive = FALSE, full.names = FALSE)
scenarios
gcms <- list.dirs(file.path(out_dir_future, periods[1], scenarios[1]),
                  recursive = FALSE, full.names = FALSE)
gcms

# Agora, vamos preparar um arquivo com os caminhos das projeções
pr <- prepare_proj(models = fm,
                   present_dir = "Variaveis_Neotropico/Presente/",
                   past_dir = NULL,
                   past_period = NULL,
                   past_gcm = NULL,
                   future_dir = out_dir_future,
                   future_period = periods,
                   future_pscen = scenarios,
                   future_gcm = gcms,
                   write_file = FALSE,
                   filename = NULL,
                   raster_pattern = ".tif*")
pr


#Create folder to save projection results
out_dir <- file.path(sp_model, "Projection_results/")
dir.create(out_dir, recursive = TRUE)

# Usar projection_data (pr) para projetar para vários cenários
p <- project_selected(models = fm,
                      projection_data = pr,
                      mask = m_sp,
                      out_dir = out_dir,
                      consensus_per_model = TRUE,
                      consensus_general = TRUE,
                      consensus = c("median", "range", "mean", "stdev"),
                      write_replicates = TRUE, #Para avaliar variância entre replicas depois
                      clamping = FALSE,
                      var_to_clamp = NULL,
                      type = "cloglog",
                      overwrite = TRUE,
                      parallel = TRUE,
                      ncores = 8,
                      parallelType = "doSNOW",
                      progress_bar = TRUE,
                      verbose = TRUE)
p
#Ver arquivos disponíveis
list.files(out_dir, recursive = TRUE, full.names = TRUE)
#Vamos importar alguns rasters para ver
p_present <- rast("Models//Araucaria angustifolia/Projection_results/Present/Present/Model_432_consensus.tiff")
plot(p_present)
p_future <- rast("Models//Araucaria angustifolia/Projection_results/Future/2041-2060/ssp585/HadGEM3-GC31-LL/Model_432_consensus.tiff")
plot(p_future)
#Comparar
plot(c(p_present$mean,
       p_future$mean),
     col = rev(terrain.colors(240)), main = c("Presente", "Futuro"),
     zlim = c(0, 1))

#' Como analisar tantos cenários diferentes?
#' Uma alternativa seria fazer uma média de todos os GCM's para cada combinação
#' de tempo-ssp. 
#' Mas isso poderia resultar em uma predição que não representa nenhum dos
#' GCMs
#' Outra alternativa é avaliar o consenso, ou seja, quantos GCMs predizem:
#' - Estabilidade (pixel continua adequado ou inadequado)
#' - Perda (pixel que era adequado se torna inadequado)
#' - Ganho (pixel que era inadequado se torna adequado)
#' 
#' Com o kuenm2 podemos fazer isso com a função proj_changes


#### Identificar consensos entre áreas de expansão e retração ####
?proj_changes

changes <- proj_changes(model_projections = p, #Objeto resultante de project_selected
                        reference_id = 1,
                        consensus = "median",
                        include_id = NULL,
                        user_thr = NULL,
                        by_gcm = TRUE,
                        by_change = TRUE,
                        general_summary = TRUE,
                        force_resample = TRUE,
                        write_results = FALSE,
                        output_dir = NULL,
                        overwrite = FALSE,
                        write_bin_models = FALSE, #Se quiser escrever os rasters binarizados
                        return_rasters = TRUE)
#Mapas binarizados
plot(changes$Binarized)
# Mudanças de acordo com cada gcm
plot(changes$Results_by_gcm)
# Plot de consensos entre GCMS
lapply(names(changes$Summary_changes), function(x){
  tidyterra::autoplot(changes$Summary_changes[[x]]) +
    ggtitle(x) + theme_light()}
)

#### Fontes de incertezas ####
# As fontes de incertezas podem vir de:
# - Diferentes modelos que foram selecionados (diferentes parâmetros)
# - Diferentes réplicas dentro de um mesmo modelo.
# - Diferentes GCMs
# Podemos calcular qual a maior fonte de variância com modvar
?modvar

var_models <- modvar(model_projections = p,
            by_replicate = TRUE,
            by_gcm = TRUE,
            by_model = TRUE,
            consensus = "median",
            write_files = FALSE,
            output_dir = NULL,
            return_rasters = TRUE,
            progress_bar = FALSE,
            verbose = TRUE,
            overwrite = FALSE)

# Plot de fontes de incertezas
lapply(names(var_models), function(x){
  tidyterra::autoplot(var_models[[x]]) + 
    ggtitle(x) + theme_light()
  }
)



#### Análise de extrapolação de risco com MOP ####
#' Imagine que na sua área de calibração (M), a temperarua varia de 10ºC a 30ºC
#' Porém, você vai projetar seu modelo para outro continente ou outro tempo, 
#' onde a temperatura varia de 5ºC a 35ºC.
#' Nessas faixas de temperatura (5-10ºC) e (30-35ºC) não temos informações se a
#' espécie sovreviveria.
#' O risco de extrapolação é alto, principalmente quando a adequabilidade é alta
#' próxima a esses extremos.
#' Para informar essa incerteza, podemos utilizar uma métrica chamada de 
#' mobility-oriented parity (MOP)
#' 

#Importar dados necessários
sp_swd <- readRDS(file.path(sp_model, "calibration_data.rds"))

#Criar pasta para salvar resultados
out_mop <- file.path(sp_model, "Projection_results/MOP")
dir.create(out_mop)

# Nesse examplo, vamos analisar o risco de extrapolação dos modelos para o Neotropico
kmop <- kuenm_mop(data = sp_swd,
                  subset_variables = TRUE,
                  mask = NULL,
                  fitted_models = fm,
                  projection_data = pr,
                  out_dir = out_mop,
                  type = "detailed", 
                  calculate_distance = FALSE,
                  where_distance = "in_range", distance = "euclidean",
                  scale = TRUE, center = TRUE, fix_NA = TRUE, percentage = 1,
                  comp_each = 2000, tol = NULL, rescale_distance = FALSE,
                  parallel = FALSE, n_cores = 1, progress_bar = TRUE,
                  overwrite = TRUE)

#Ver resultados
# Presente
mop_present_high <- rast("Models/Araucaria angustifolia/Projection_results/MOP/Present/Present_mop_towards_high_combined.tif")
mop_present_low<- rast("Models/Araucaria angustifolia/Projection_results/MOP/Present/Present_mop_towards_low_combined.tif")
mop_present <- c(mop_present_low, mop_present_high)
names(mop_present) <- c("Towards low", "Towards high")
panel(mop_present)

# Futuro
mop_future_high <- rast("Models/Araucaria angustifolia/Projection_results/MOP/Future/2061-2080/ssp585/ACCESS-CM2_mop_towards_high_combined.tif")
mop_future_low<- rast("Models/Araucaria angustifolia/Projection_results/MOP/Future/2061-2080/ssp585/ACCESS-CM2_mop_towards_low_combined.tif")
mop_future <- c(mop_future_low, mop_future_high)
names(mop_future) <- c("Towards low", "Towards high")
panel(mop_future)
