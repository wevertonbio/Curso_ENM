#### Download de variáveis ambientais ####
# Não vamos rodar em aula, variaveis já foram baixadas em 'Variaveis_brutas'

#Dica: salve todas as variaveis em uma unica pasta raíz, separando apenas por fontes
# Ex: uma pasta para worldclim, outra para soilgrids, outra para earthenv
# Isso facilita processamento das variáveis

# Variáveis bioclimáticas do presente e do futuro:
# O que são variáveis bioclimáticas? https://worldclim.org/data/bioclim.html
# WorldClim: https://worldclim.org/data/index.html
# Reference: https://rmets.onlinelibrary.wiley.com/doi/epdf/10.1002/joc.5086
#
# CHELSA: https://chelsa-climate.org/
# Reference: https://www.nature.com/articles/sdata2017122

# Variáveis bioclimáticas extendidas do ENVIREM
# https://envirem.github.io/
# Reference: http://onlinelibrary.wiley.com/doi/10.1111/ecog.02880/full

# Variáveis bioclimáticas do passado
# CHELSA: https://chelsa-climate.org/chelsa-trace21k/
# Reference: https://cp.copernicus.org/articles/19/439/2023/cp-19-439-2023.html
#
# PaleoClim: http://www.paleoclim.org/
# Reference: https://www.nature.com/articles/sdata2018254

# Variáveis de solo:
# SoilGrids: https://soilgrids.org/
# Reference: https://soil.copernicus.org/articles/7/217/2021/
# Para download use: https://files.isric.org/soilgrids/latest/data_aggregated/
# Para baixar classes de solos mais prováveis, use: 
# https://files.isric.org/soilgrids/latest/data/wrb/

# Variáveis topográficas (altitude, declive, etc)
# https://www.earthenv.org/topography
# Reference: https://www.nature.com/articles/sdata201840

# Biome stability in the last 30k years
# Reference: https://onlinelibrary.wiley.com/doi/full/10.1111/geb.12694
# Link to download: https://onlinelibrary.wiley.com/action/downloadSupplement?doi=10.1111%2Fgeb.12694&file=geb12694-sup-0001-suppinfo1.zip
# Original resolution: 2.5arcmin (~4.5km x 4.5km)

# Gerar próprias variáveis de estabilidade climática
# https://github.com/hannahlowens/climateStability
# Reference: https://journals.ku.edu/jbi/article/view/9786

# Outras variáveis interessantes
# https://www.earthenv.org/
# Reference: https://www.nature.com/articles/sdata201840

# Mapbiomas (cobertura e uso do solo)
# https://brasil.mapbiomas.org/downloads/

# CUIDADO COM VARIÁVEIS QUE PODEM VARIAR MUITO COM O TEMPO (ex: cobertura florestal)
# Dados bioclimáticos são derivados de medições num intervalo de 30 anos
# Por isso, são mais seguros de associar a dados de ocorrência coletados num 
# grande intervalo de tempo
# O quanto outras variáveis refletem as condições do local quando a ocorrência 
# foi registrada? Ex: ponto coletado em 1981, cobertura florestal medida em 2010.
# O quanto outras variáveis sem projeções para o futuro vão mudar?
# Ex: pH do solo, que está correlacionado com vegetação e precipitação, pode ser
# mantida fixa numa projeção para o futuro?


# Ao baixar variáveis ambientais, dê preferência ao download direto pelo R, se disponível
# Isso mantém a reprodutibilidade do código

#### Download de variáveis ambientais do presente e do futuro do WorldClim ####
# Para fazer o download dessas variáveis, vamos usar o pacote geodata

#Carregar pacotes
library(geodata) #Baixar variáveis do WorldClim no presente e futuro
library(pbapply) #Funções para mostrar barra de progresso em loopings
library(parallel) #Para rodas coisas em paralelo

#Criar diretório para salvar variáveis brutas
dir.create("Variaveis_brutas")

#Baixar variáveis do presente no WorldClim
worldclim_global(var = "bio", 
                 res = 5, 
                 path = "Variaveis_brutas/")

#Baixar variáveis do futuro do Worldclim
?cmip6_world #Ver função
#Criar diretório
dir.create("Variaveis_brutas/Futuro")
#Como são muitas variáveis, vamos criar um grid de combinações de gcm, ssp e tempo
g <- expand.grid(model = c("ACCESS-CM2", "HadGEM3-GC31-LL", "MIROC6", 
                           "MPI-ESM1-2-LR", "MRI-ESM2-0"),
                 ssp = c("126", "585"),
                 time = c("2041-2060","2061-2080"))
#Criar cluster
parallel::detectCores() #Ver numero de cores disponiveis
ncores <- 5 #Determinar numero de cores a serem usados em paralelo
cl <- parallel::makeCluster(ncores) #Criar um cluster em cada core
parallel::clusterEvalQ(cl, {library("geodata")}) #Enviar pacotes necessários para cada cluster
parallel::clusterExport(cl, varlist = c("g")) #Enviar objetos necessários para cada cluster

#Looping para baixar variáveis
pblapply(1:nrow(g), function(i){ #A cada iteração, i sera uma linha de g
  g_i <- g[i,] #Obter combinação i do grid
  #Baixar combinação i
  geodata::cmip6_world(model = g_i$model, #Modelo i
                       ssp = g_i$ssp, #SSP i
                       time = g_i$time, #Time i
                       var = "bioc", #Baixar variáveis bioclimáticas
                       res = 5, #Resolução
                       path = "Variaveis_brutas/Futuro/") #Pasta para salvar
}, cl = cl)
parallel::stopCluster(cl) #Fechar cluster

#Outras variáveis baixadas direto do site

# Variáveis de solo (clay e sand)
# SoilGrids: https://soilgrids.org/
# Reference: https://soil.copernicus.org/articles/7/217/2021/
# Para download use: https://files.isric.org/soilgrids/latest/data_aggregated/

# Classes de solos mais prováveis: 
# https://files.isric.org/soilgrids/latest/data/wrb/

# Variáveis topográficas (declive)
# https://www.earthenv.org/topography
# Reference: https://www.nature.com/articles/sdata201840