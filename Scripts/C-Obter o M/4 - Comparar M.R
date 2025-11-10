#### Comparar distribuição das variáveis em cada M ####

# Para ver o efeito da escolha do M, vamos ver como as variáveis se distribuem
# dentro de cada M

# Carregar pacotes
library(dplyr) #Manipulação de dataframes e uso do %>%
library(mapview) #Para visualizar mapa interativo
library(terra) #Manipulação de dados espaciais
library(ggplot2) #Plotar gráficos
library(fs) #Remover extensao do nome do arquivo

# Importar variáveis no neotrópico
v <- rast("Variaveis_Neotropico/Presente/Variaveis.tiff")

#Vamos escolher uma variavel
v_x <- "bio_1"
v_x <- v[[v_x]]
plot(v_x)

#### Araucaria angustifolia ####
#Como sempre, começamos atribuindo o nome da espécie e o diretório a objetos
sp <- "Araucaria angustifolia"
# Diretório com Ms da espécie
m_dir <- file.path("M_poligonos/", sp)
m_dir


#Importar Ms
all_m_files <- list.files(m_dir, full.names = TRUE, pattern = "gpkg")
all_m_files
all_m <- lapply(all_m_files, vect) #Ler arquivos e salvar em uma lista
#Identificar Ms
names(all_m) <- basename(all_m_files) %>% #Obter apenas nome do arquivo, sem pasta
  fs::path_ext_remove() #Remover extensão do nome do arquivo
names(all_m)

#Distribuição da variavel em todo neotropico (sem M)
v_neot <- values(v_x, na.rm = TRUE, mat = FALSE) #Extrair valores do raster
v_neot <- data.frame(M = "Neotropico", value = v_neot) #Salvar valores em dataframe
# Cortar variavel usando cada um dos Ms e salvar resultados em dataframe
m_values <- lapply(names(all_m), function(i){
  v_i <- crop(v_x, all_m[[i]])
  v_i <- values(v_i, na.rm = TRUE, mat = FALSE) #Extrair valores do raster
  v_i <- data.frame(M = i, value = v_i) #Salvar valores em dataframe
  })
m_values <- bind_rows(m_values) #Unir dataframes
m_values <- bind_rows(v_neot, m_values)
table(m_values$M) #Numero de pixels por tipo de M

# Plotar boxplot
ggplot(data = m_values) + 
  geom_boxplot(aes(M, value, fill = M)) +
  ylab(names(v_x))

# Considerando todo neotropico como M, modelo "entenderia" que espécie acessou e
# tentou colonizar ambientes muito mais frios e muito mais quentes do que ela 
# realmente teve acesso.

# Já com o M mais restrito (m_mcp), modelo "entenderia" que espécie jamais acessou
# areas que provavelmente ela já acessou.