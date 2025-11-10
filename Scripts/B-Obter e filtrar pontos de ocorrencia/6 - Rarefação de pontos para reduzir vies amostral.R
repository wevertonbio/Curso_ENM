#### RAREFAÇÃO DOS PONTOS PARA REDUZIR VIÉS AMOSTRAL ####

# Registros de ocorrência devem refletir as preferências da espécie
# E não as preferências de quem coleta a espécie

# Sugestões de artigos sobre maneiras de reduzir viés amostral:
# Rarefação no espaço geográfico
# https://nsojournals.onlinelibrary.wiley.com/doi/epdf/10.1111/ecog.01132
# https://scar.github.io/EGABIcourse19/course_material/literature/Boria%202014.pdf
# https://onlinelibrary.wiley.com/doi/abs/10.1111/jbi.14854

# Rarefação no espaço ambiental
# https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.13142
# https://nsojournals.onlinelibrary.wiley.com/doi/abs/10.1111/j.1600-0587.2013.00441.x

# Target-group: usar como background pixels mais bem amostrados
# https://onlinelibrary.wiley.com/doi/10.1111/ddi.13442

# Extrair pontos aleatórios no espaço ambiental
# https://www.sciencedirect.com/science/article/pii/S030438002400142X

# Remover todos os objetos #
rm(list = ls())

# Carregar pacotes
library(RuHere)
library(flexsdm) #Para filtrar pontos no espaço ambiental
library(data.table) #Importar e salvar tabelas
library(dplyr) #Manipulação de dataframes e uso do %>%
library(mapview) #Para visualizar mapa interativo
library(terra) #Manipulação de dados espaciais
library(ggplot2) #Plotar gráficos

#### Araucaria angustifolia ####
#Como sempre, começamos atribuindo o nome da espécie e o diretório a objetos
sp <- "Araucaria angustifolia"
sp_dir <- file.path("Ocorrencias/", sp)
sp_dir

# Importar registros 
occ <- fread(file.path(sp_dir, "5-Ocorrencias_unicas_CoordinateCleaner.gz"))

#Ver pontos
plot_here(occ, flags = "all")

# Vamos importar uma variável de temperatura para ver a distribuição dos pontos
# nessa dimensão
# Espacializar
pts <- spatialize(occ)
# Importar variavel
v <- rast("Variaveis_Neotropico/Presente/Variaveis.tiff")
# Plotar
plot(v$bio_1)
points(pts)
# Extrair valores de temperatura nos pontos
occ_temp <- extract(v$bio_1, pts, ID = FALSE)[[1]]
# Ver boxplot
boxplot(occ_temp)

#### Filtrar usando uma distância fixa ####
#' Agora, vamos filtrar os pontos usando uma distância fixa (ex: 10km) para
#' identificar clusters de ponto dentro desse raio

#' Vamos definir o ano como ordem de prioridade: quando identificar clusters de
#' pontos, manter o ponto mais recente e sinalizar os mais antigos

occ_10 <- thin_records(occ = occ,
                       d = 10, #Distancia
                       prioritary_column = "year") #Coluna para definir prioridade
# Quantos pontos foram sinalizados para remover (FALSE)?
table(occ_10$thin_flag)
# Vamos plotar
plot_here(occ_10, flags = "thin_flag", cex = 4)

# Remover pontos para rarefação
occ_10 <- remove_flagged(occ_10, "thin_flag")

#Vamos testar uma distancia maior, de 50 km
occ_50 <-  thin_records(occ = occ,
                        d = 50, #Distancia
                        prioritary_column = "year") #Coluna para definir prioridade
# Quantos pontos foram sinalizados para remover (FALSE)?
table(occ_50$thin_flag)
# Vamos plotar
plot_here(occ_50, flags = "thin_flag", cex = 4)
# Remover pontos para rarefação
occ_50 <- remove_flagged(occ_50, "thin_flag")


# Vamos ver como isso muda a "temperatura ótima" para a espécie
# Remover pontos "rarefeitos" e espacializar
pts_10 <- spatialize(occ_10)
pts_50 <- spatialize(occ_50)

# Extrair informações e salvar em dataframe
occ_temp <- extract(v$bio_1, pts, ID = FALSE)[[1]]
occ_temp <- data.frame("Distancia" = 0, "Temperatura" = occ_temp)
occ_temp_10 <- extract(v$bio_1, pts_10, ID = FALSE)[[1]]
occ_temp_10 <- data.frame("Distancia" = 10, "Temperatura" = occ_temp_10)
occ_temp_50 <- extract(v$bio_1, pts_50, ID = FALSE)[[1]]
occ_temp_50 <- data.frame("Distancia" = 50, "Temperatura" = occ_temp_50)
#Unir todas as informações
occ_temp_all <- rbind(occ_temp, occ_temp_10, occ_temp_50)
#Converte coluna de distancia para fator - para plot de boxplot
occ_temp_all$Distancia <- as.factor(occ_temp_all$Distancia)
# Plotar boxplot
ggplot(data = occ_temp_all) + 
  geom_boxplot(aes(Distancia, Temperatura, fill = Distancia)) +
  geom_violin(aes(Distancia, Temperatura, fill = Distancia), alpha = 0.1)
#Diferentes distâncias de filtragem irão dar diferentes informações para o modelo
# Média de temperatura de cada conjunto de dados
occ_temp_all %>% 
  group_by(Distancia) %>% 
  summarise(media = mean(Temperatura))


# Qual a melhor distância?
# Não existe melhor distância:
# https://onlinelibrary.wiley.com/doi/abs/10.1111/jbi.14854

#### Filtrar testando várias distâncias e autocorrelação espacial ####

# Primeira lei da geografia:
# Tudo está relacionado a tudo o mais, mas coisas próximas estão mais relacionadas do que coisas distantes

# O Índice de Moran (I de Moran) é um índice que compara a covariância espacial (o quanto os valores de "vizinhos" variam juntos) e a variância total dos dados.
# Valores positivos indicam agrupamento (autocorrelação espacial positiva)
# Valores negativos indicam dispersão (autocorrelação espacial negativa)
# Valores próximos de 0 indiam aleatoriedade (sem autocorrelação espacial)

# Uma das utilidades do I de Moran é reduzir vieses de amostragem de ocorrências de espécies

# Podemos testar várias distâncias e ver qual delas apresenta um balanço entre:
# Diminuir autocorrelação espacial vs manter o máximo de pontos
# Para isso, vamos usar a função filter_geo_moran
# Autocorrelação é baseada nas variaveis ambientais, e não pode considerar
# variáveis categoricas
soiltype_id <- which(names(v) == "soilType") #Qual variavel é soilType
v_cont <- v[[-soiltype_id]] #Remover soiltype
names(v_cont) #Conferir variaveis
# Testar várias distancias
occ_geo_moran <- filter_geo_moran(occ = occ, #Ocorrencias
                                  d = c(2, 5, 10, 15, 20, 25, 30), #Distancias
                                  prioritary_column = "year",
                                  raster_variables = v_cont) #Variaveis continuas
# Qual a distancia selecionada?
occ_geo_moran$Distance
#Ver resultados do indice de Moran
View(occ_geo_moran$imoran)
#Plotar pontos selecionados e comparar com distancia de 10 e 50 usadas anteriormente
occ_moran <- occ_geo_moran$occ %>% #Extrair pontos da lista resultante de filter_geo_moran
  remove_flagged(flags = "thin_flag") #Remover pontos rarefeitos
# Espacializar
pts_moran <- spatialize(occ_moran)

# Plotar
mapview(pts, col.regions = "black", cex = 5) + 
  mapview(pts_10, col.regions = "yellow", cex = 5) +
  mapview(pts_50,  col.regions = "red", cex = 5) + 
  mapview(pts_moran, col.regions = "green", cex = 5)

# Vamos ver como isso muda a "temperatura ótima" para a espécie e comparar com
# as distancias anteriores
occ_temp_moran <- extract(v$bio_1, pts_moran, ID = FALSE)[[1]]
occ_temp_moran <- data.frame("Distancia" = "Moran", "Temperatura" = occ_temp_moran)
#Unir todas as informações
occ_temp_all <- rbind(occ_temp_all, occ_temp_moran)
#Converte coluna de distancia para fator - para plot de boxplot
occ_temp_all$Distancia <- as.factor(occ_temp_all$Distancia)
# Plotar boxplot
ggplot(data = occ_temp_all) + 
  geom_boxplot(aes(Distancia, Temperatura, fill = Distancia)) +
  geom_violin(aes(Distancia, Temperatura, fill = Distancia), alpha = 0.1)
# Média de temperatura de cada conjunto de dados
occ_temp_all %>% 
  group_by(Distancia) %>% 
  summarise(media = mean(Temperatura))


#### Filtrar no espaço ambiental ####
?flexsdm::occfilt_env
# it is recommended to use a small number of bins between 2-5 if more than ten 
# variables are used.
# while the greater the number of bins, the greater records retained
occ_filt_env <- occfilt_env(data = occ, 
                            x = "decimalLongitude", y = "decimalLatitude", 
                            id = "record_id", env_layer = v_cont, nbins = 5)

# Criar coluna com nome da espécie e mudar nomes de colunas de longitude e latitude
occ_filt_env <- occ_filt_env %>% 
  dplyr::select(x = decimalLongitude, y = decimalLatitude) %>% 
  mutate(species = sp, .before = x)

#Comparar número de registros
nrow(occ)
nrow(occ_filt_env)
#Espacializar pontos
pts_filt_env <- spatialize(occ_filt_env, long = "x", lat = "y")
  
mapview(pts, col.regions = "black", cex = 5) + 
  mapview(pts_moran, col.regions = "green", cex = 5) +
  mapview(pts_filt_env, col.regions = "red", cex = 5)

# Vamos ver como isso muda a "temperatura ótima" para a espécie e comparar com
# as distancias anteriores
occ_temp_env <- extract(v$bio_1, pts_filt_env, ID = FALSE)[[1]]
occ_temp_env <- data.frame("Distancia" = "Ambiental", "Temperatura" = occ_temp_env)
#Unir todas as informações
occ_temp_all <- rbind(occ_temp_all, occ_temp_env)
#Converte coluna de distancia para fator - para plot de boxplot
occ_temp_all$Distancia <- as.factor(occ_temp_all$Distancia)
# Plotar boxplot
ggplot(data = occ_temp_all) +
  geom_boxplot(aes(Distancia, Temperatura, fill = Distancia)) +
  geom_violin(aes(Distancia, Temperatura, fill = Distancia), alpha = 0.1)
# Média de temperatura de cada conjunto de dados
occ_temp_all %>% 
  group_by(Distancia) %>% 
  summarise(media = mean(Temperatura))


#Escolha um dos pontos filtrados para usar no modelo lembrando que:

#' Precisamos capturar a preferência da espécie,
#' e não a preferência de quem estuda a espécie 
#' (e coleta em lugares mais acessíveis e conhecidos)

# Opções disponíveis:
nrow(occ_10) #Distância fixa de 10km
nrow(occ_50) #Distância fixa de 50km
nrow(occ_moran) #Distância que diminui autocorrelação espacial
nrow(occ_filt_env) #Distância fixa no espaço ambiental

#SALVAR CHECKPOINT
fwrite(occ_moran, 
       file.path(sp_dir, "6-Pontos_rarefeitos.gz"),
       compress = "gzip", row.names = FALSE)

# 🙏 Finalmente os pontos estão prontos para modelagem 🙏