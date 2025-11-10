#### Preparar dados para simulações do grinnell ####
# Não precisa rodar script novamente
# Variáveis já estão salvar em "Variaveis_brutas/paleoclim_10"

# Detalhes sobre a simulação em:
# https://escholarship.org/content/qt8hq04438/qt8hq04438.pdf

# Pacote original em:
# https://github.com/fmachados/grinnell

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

# Para rodar as simulações, precisamos de variáveis do presente e do passado
# Vamos usar as variáveis do paleoclim
# https://cran.r-project.org/web/packages/rpaleoclim/vignettes/rpaleoclim.html

# Carregar pacotes
library(RuHere)
library(terra)
library(grinnell)
library(rpaleoclim)
library(pbapply)

# Criar diretório para salvar resultados do M
dir.create("M_poligonos")


#### Download de variáveis do passado com rpaleoclim ####

#Criar pasta para salvar rasters
dir.create("Variaveis_brutas/paleoclim_10") #Vamos usar a resolução de 10arcmin

#Importar shapefile do neotropico para cortar variaveis
neot <- vect("Shapefiles/Neotropical.gpkg")
#Adicionar buffer de 100km
neot <- buffer(neot, width = 100*1000)

#Definir cenários do passado para baixar
times <- c("cur", #Current (1979 – 2013)
           "lh", #Late Holocene: Meghalayan 4.2-0.3 ka
           "mh", #Mid Holocene: Northgrippian   8.326-4.2 ka
           "eh", #Early Holocene: Greenlandian  11.7-8.326 ka
           "yds", #Pleistocene: Younger Dryas Stadial   12.9-11.7 ka
           "ba",  #Pleistocene: Bølling-Allerød   14.7-12.9 ka
           "hs1", #Pleistocene: Heinrich Stadial 1  17.0-14.7 ka
           "lgm", #Pleistocene: Last Glacial Maximum    ca. 21 ka
           "lig") #Pleistocene: Last Interglacial   ca. 130 ka

#### Baixar variáveis ####
# Já foi rodado
# pblapply(times, function(i){
#   try({
#     p <- paleoclim(period = i,
#                    resolution = "10m", #10arc-min of resolution
#                    as = "terra", 
#                    region = neot)
#     #Save variables
#     writeRaster(p, paste0("Variaveis_brutas/paleoclim_10/", i, ".tif"),
#                 overwrite = TRUE)
#   }) #End of try
# })
# Ver pasta
fs::dir_tree("Variaveis_brutas/paleoclim_10/")


# Observe que há um intervalo significativo entre o LGM (ca. 21 ka) e o LIG (ca. 130 ka). 
# Podemos preencher essa lacuna usando o mesmo método empregado pelo Oscillayers.
# O Oscillayers constrói variáveis ambientais que representam as condições 
# climáticas dos últimos 5,4 milhões de anos em intervalos de 10 ka.
# Essa construção se baseia em anomalias interpoladas entre as camadas bioclimáticas 
# do presente e do Último Máximo Glacial (LGM), escaladas em relação à curva de 
# temperatura média global do Plio-Pleistoceno, derivada das proporções estáveis 
# de isótopos de oxigênio em sedimentos bentônicos. Esta versão alternativa do 
# grinnell inclui uma função para gerar essas camadas. Vamos utilizar as variáveis
# atuais e do LGM, baixadas do PaleoClim, como entradas para essa função.

# Importar variaveis do presente e do lgm
current_variables <- rast("Variaveis_brutas/paleoclim_10/cur.tif")
plot(current_variables$bio_1)
lgm <- rast("Variaveis_brutas/paleoclim_10/lgm.tif")
plot(lgm$bio_1)
#Gerar layers entre LGM e LIG
oscillayer(current_variables = current_variables,
           lgm = lgm,
           final_period = 120, #Periodo final da interpolação (em milhares de anos atrás)
           precipitation_var = c("bio_12", "bio_13", "bio_14", #Identify precipitation variables to make corrections
                                 "bio_15", "bio_16", "bio_17"),
           write_files = TRUE,
           output_dir = "Variaveis_brutas/paleoclim_10", #Salvar variáveis na mesma pasta
           overwrite = TRUE,
           progress_bar = TRUE)
# Veja a pasta "Variaveis_brutas/paleoclim_10"
fs::dir_tree("Variaveis_brutas/paleoclim_10")
