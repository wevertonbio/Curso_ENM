#### Projeções para o passado ####

# Versão nova do WorldClim não tem dados do passado 😭
# Versão nova do CHELSA (atualizou essa semana) não

# Vamos usar os dados de Krapp et al. 2021:
browseURL("https://www.nature.com/articles/s41597-021-01009-3")
# Dataset dos ultimos 800 mil anos (em intervalos de mil anos)
# Resolução: 0.5 graus
# GCM: HadCM3


# Precisamos fazer o "downscaling" das variáveis utilizando o método delta
# O método delta é uma técnica simples que calcula a diferença entre os valores 
# observados e modelados em um momento específico (geralmente o presente) e 
# aplica essa diferença aos valores modelados de outros períodos.
# Ex: passado é estimado com base em um presente de referencia
# Porém, podemos ter utilizado um presente de referencia diferente
# Para fazer o downscaling, utilizados:
# Delta = presente_referencia - passdo_referencia
# Passado_para_modelo = presente_modelo - Delta

# Downscaling precisa ser feito usando variáveis mensais

# Vamos usar o pacote pastclim para fazer isso
library(pastclim)
library(terra)

# Ver info sobre dados de Krapp2021
help("Krapp2021")

# Variáveis para baixar
# Temperatura
tavg_vars <- c(paste0("temperature_0", 1:9), paste0("temperature_", 10:12))
tavg_vars

# Definir períodos
periodos <- c(0, #Presente,
              -6000, #Holoceno médio
              -21000, #Ultimo Máximo Glacial
              -130000) #Ultimo interglacaial
#Referencia é 1950
periodos <- periodos - 1950
periodos[1] <- 0
periodos

# Definir pasta para salvar
pasta_passado <- "Passado_Krapp2021"
dir.create(pasta_passado)
set_data_path(path_to_nc = pasta_passado)
# Baixar dados do passado
download_dataset(dataset = "Krapp2021", bio_variables = tavg_vars)
# Importar dados
tavg_series <- region_series(
  bio_variables = tavg_vars,
  time_bp = periodos,
  dataset = "Krapp2021"
)


# Baixar dados mensais do presente do Worldclim
# Resolução: 5arcmin
download_dataset(dataset = "WorldClim_2.1_5m", bio_variables = tavg_vars)

# Importar dados do worldclim
tavg_obs_hres_all <- region_series(
  bio_variables = tavg_vars,
  time_ce = 1985,
  dataset = "WorldClim_2.1_5m"
)

# Obter range das variáveis (necessário para aplicar método delta)
tavg_obs_range <- range(unlist(lapply(tavg_obs_hres_all, 
                                      minmax, compute = TRUE)))
tavg_obs_range

# Obter mapa de topografia e batimetria em alta resolução
# download_etopo()
relief_rast <- load_etopo()
relief_rast <- terra::resample(relief_rast, tavg_obs_hres_all$temperature_01)
plot(relief_rast)

# Máscara de continente
# Baseado em reconstruções do nivel do mar de Spratt et al 2016,
land_mask_high_res <- make_land_mask(relief_rast = relief_rast,
                                     time_bp = periodos)
plot(land_mask_high_res, main = periodos)

# # Máscara de cobertura de gelo (Fonte: Beyer2020)
# download_dataset("Beyer2020", bio_variables = "biome")
# ice_mask_low_res <- get_ice_mask(time_bp = periodos, dataset = "Beyer2020")
# ice_mask_high_res <- downscale_ice_mask(
#   ice_mask_low_res = ice_mask_low_res,
#   land_mask_high_res = land_mask_high_res
# )
# plot(ice_mask_high_res)

# Aplicar delta downscaling para temperatura
tavg_downscaled_list <- list()
for (i in 1:12) {
  delta_rast <- delta_compute(x = tavg_series[[i]], 
                              ref_time = 0,
                              obs = tavg_obs_hres_all[[i]])
  
  
  tavg_downscaled_list[[i]] <- delta_downscale(
    x = tavg_series[[i]],
    delta_rast = delta_rast,
    x_landmask_high = land_mask_high_res,
    range_limits = tavg_obs_range
  )
}
tavg_downscaled <- terra::sds(tavg_downscaled_list)