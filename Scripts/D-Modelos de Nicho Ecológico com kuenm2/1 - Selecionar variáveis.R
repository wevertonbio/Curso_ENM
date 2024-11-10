#### Selecionar variáveis ambientais para modelos ####

# Carregar pacotes
library(terra)
library(grinnell)
library(dplyr)
library(usdm) # Pacote para fazer VIF
library(ggcorrplot) # Plotar correlações
library(kuenm2) # Fazer PCA
library(factoextra) #Plotar gráfico de PCA e contribuição de variáveis

#' Vamos utilizar 3 métodos para selecionar variáveis:
#' Correlação de Pearson
#' VIF (Variance Inflation Factor)
#' PCA

# Importar variáveis
v <- rast("Variaveis_Neotropico/Presente/Variaveis.tiff")
names(v)

# Cortar variáveis pelo M da espécie
m <- vect("M_poligonos/Araucaria angustifolia/m_grinnell.gpkg") #Escolha um M
v <- crop(v, m, mask = TRUE)
plot(v)

# Excluir variáveis 08, 09, 18 e 19 por apresentarem variações bruscas
# Essas variações não são naturais, mas artefatos estatísticos pela maneira como
# são calculadas
# https://onlinelibrary.wiley.com/doi/abs/10.1111/aec.13234
id_remove <- which(names(v) %in% c("bio_8", "bio_9", "bio_18", "bio_19"))
v <- v[[-id_remove]]
names(v)

#Remover soilType (categórico)
id_soil <- which(names(v) == "soilType")
v <- v[[-id_soil]]
names(v)

# Converter raster em dataframe
df <- as.data.frame(v, na.rm = TRUE)

#### Correlação ####

# Calcular correlação
cor_v <- cor(df)

#Plotar gráfico com ggcorrplot
g_cor <- ggcorrplot(cor_v, #Matriz de correlação
                    method = "square", #Square or circle?
                    type = "lower", #"full", "lower" or "upper" display.
                    lab = TRUE, # Mostrar valor das correlações?
                    lab_size = 2.5, #Tamanho da fonte
                    hc.order = TRUE, # Reordenas variaveis de acordo com correlação?
                    outline.color = "white", # Cor em torno dos circulos
                    ggtheme = theme_minimal(), #Tema
                    legend.title = "Correlation") + #Título da legenda
  theme(legend.position = "right") #Posição da legenda
g_cor
# Problema de correlação: precisa avaliar variável por variável
# Vamos determinar um threshold e escolher uma variavel por vez
thr <- 0.7
# Começar escolhendo duas variáveis (escolha outras caso prefira)
var_to_keep <- c("bio_6", "bio_12")

# Identificar variaveis que não tem alta correlação (> thr) com variaveis mantidas
var_without_correlation <- rownames(abs(cor_v)[, var_to_keep])[
  apply(abs(cor_v)[, var_to_keep], 1, function(x) all(x <= 0.7))]
var_without_correlation #Escolha uma dessas e adicione a var_to_keep. Rode até aparecer 0

#Vai atualizando aqui e rode var_without_correlation de novo
var_to_keep <- c("bio_6", "bio_12", "bio_15", "bio_7", "bio_2", "bio_5",
                 "slope", "clay") 
#Vamos manter essas variáveis
var_to_keep
# Copie e cole as variáveis aqui para consultar depois
# "bio_6"  "bio_12" "bio_15" "bio_2"  "slope"  "clay"

#### VIF ####
# Uma Variável é usada como resposta e as restantes como preditoras num modelo linear
# Obtido R2 do modelo
# VIF é calculado pela fórmula: VIF = 1/(1 - R2)
# Se R2 for maior que 0.8, VIF será maior que 5 (moderado)
# Se R2 for maior que 0.9, VIF será maior que 10 (problemático)

?usdm::vif

# VIF clássico (apenas identifica variaveis sem nenhuma correlação com outra variável)
vif_classico <- vif(v)
vif_classico
# Nesse caso, manteríamos apenas clay, sand e slope
# Mas e se removessemos todas as variaveis correlacionadas com bio_6?
# O Vif de bio_6 diminuiria e ele seria selecionado.
# É o que faz as outras funções

# Vif cor - finds a pair of variables which has the maximum linear correlation 
# (greater than the threshold; th), and exclude the one with a greater VIF.
# The procedure is repeated untill no pair of variables with a high corrrelation 
# coefficient (grater than the threshold) remains.
# Nesse caso, podemos escolher variáveis que queremos manter
vif_cor <- vifcor(v, th = 0.7, #th é o threshold de correlação de Pearson
                  keep = c("bio_6", "bio_15")) #Variáveis para manter

vif_cor
vif_cor_to_keep <- vif_cor@results$Variables
vif_cor_to_keep

# Vif stepwise - calculates VIF for all variables, excludes the one with the 
# highest VIF (if it is greater than the threshold), repeat the procedure untill 
# no variables with a VIF greater than th remains.
vif_step <- vifstep(v, th = 10, #th é o threshold de VIF (< 10) 
                  keep = c("bio_6", "bio_15"))

vif_step
vif_step_to_keep <- vif_step@results$Variables
#Comparar variaveis selecionadas
vif_step_to_keep
vif_cor_to_keep 

#### PCA ####
# Sumariza variáveis em eixos de PCA
?perform_pca
pca_var <- perform_pca(spat_variables = v,
                       center = TRUE, scale = TRUE, 
                       deviance_explained = 95, # Seleciona eixos que explicam X% da variância
                       min_explained = 5) # Minimo que eixo precisa explicar para ser incluido
plot(pca_var$env) #Plotar eixos selecionados

# Plotar gráfico de PCA e contribuição de variáveis
# Eixos 1 e 2
pca_plot12 <- fviz_pca_var(pca_var$pca, #Resultado do pca
                           axes = c(1, 2), #Eixos
                           col.var="contrib", #Colorir por contribuição
                           gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), #Cores
                           repel = TRUE) # Evita sobreposição de texto
pca_plot12

# Eixos 3 e 4
pca_plot34 <- fviz_pca_var(pca_var$pca, #Resultado do pca
                           axes = c(3, 4), #Eixos
                           col.var="contrib", #Colorir por contribuição
                           gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), #Cores
                           repel = TRUE) # Evita sobreposição de texto
pca_plot34

# Eixos 5 e 6
pca_plot56 <- fviz_pca_var(pca_var$pca, #Resultado do pca
                           axes = c(5, 6), #Eixos
                           col.var="contrib", #Colorir por contribuição
                           gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), #Cores
                           repel = TRUE) # Evita sobreposição de texto
pca_plot56 

#Podemos salvar as PCA-variaveis
dir.create("PCA_variaveis") # Criar pasta
writeRaster(pca_var$env, "PCA_variaveis/Variaveis.tif") # Salvar

# A função perform_pca também permite projetar facilmente o PCA para outros cenários
# Como as funções do kuenm2 permitem fazer PCA internamente (incluindo projeções),
# não vamos fazer isso.
# PCA interno é vantajoso porque você não precisa escrever um PCA para cada espécie,
# já que o PCA muda de acordo com o M usado para cortar as variáveis.
# Para projetar PCA, veja o exemplo da função
?perform_pca

# Caso queira utilizar variáveis brutas, salve uma dessas opções em um bloco de notas 
# Ou aqui no script
#Não esqueça de adicionar soiltype, se achar necessário
dput(c(var_to_keep, "soilType")) #Variaveis selecionadas por correlação de Pearson
dput(c(vif_cor_to_keep, "soilType")) #Variaveis selecionadas por VIF-Correlação
dput(c(vif_step_to_keep, "soilType")) #Variaveis selecionadas por VIF-stepwise

# Minhas variaveis para copiar
c("bio_12", "bio_15", "bio_2", "bio_6", "bio_7", "sand", "slope", 
  "soilType")

