#### OBTER M DE POLÍGONOS ####

# O M da espécie deve representar as áreas acessíveis para espécies num relevante
# período de tempo.
# https://www.sciencedirect.com/science/article/abs/pii/S0304380011000780
# O M deve ser feito individualmente para cada espécie
# https://jacobccooper.github.io/pdfs/geb12678.pdf

# Existem três maneiras mais simples de estimar o M da espécie:
# Buffer, ecorregiões e mínimo polígono convexo + buffer

#Carregar pacotes
library(RuHere)
library(dplyr) #Manipulação de dataframes e uso do %>%
library(mapview) #Para visualizar mapa interativo
library(terra) #Manipulação de dados espaciais
library(data.table) #Importar e salvar tabelas

#### Araucaria angustifolia ####
#Como sempre, começamos atribuindo o nome da espécie e o diretório a objetos
sp <- "Araucaria angustifolia"
sp_dir <- file.path("Ocorrencias/", sp)
sp_dir

#Criar diretório para salvar M
m_dir <- file.path("M_poligonos/", sp)
m_dir
dir.create(m_dir, recursive = TRUE)

# Importar registros 
occ <- fread(file.path(sp_dir, "6-Pontos_rarefeitos.gz"))
#Espacializar pontos
pts <- spatialize(occ)
mapview(pts)

#### Buffer
m_buffer <- buffer(pts, #Pontos espacializads
                   width = 100 * 1000) %>% #Buffer em metros
  aggregate() #Unir circulos
#Ver M estimado por buffer
mapview(m_buffer, col.regions = "pink") + mapview(pts, cex = 5)

# Problemas com buffer:
# Pode gerar vários M's disjuntos (não é um problema em áreas invadidas)
# Considera que todos os registros representam populações capazes de dispersar
# Considera que populações podem dispersar igualmente para qualquer lado
# Qual o tamanho ideal do buffer?

#### Ecoregiões
# Alguns trabalhos usam ecorregiões com registros como estimativas do M

# Importar ecorregiões
# Ecorregiões baixadas de: https://storage.googleapis.com/teow2016/Ecoregions2017.zip
# Selecionar apenas ecorregioes do neotropico e salva em gpkg
eco <- vect("Shapefiles/Ecoregions2017.gpkg")

#Ver mapa
mapview(eco) + mapview(pts, cex = 4)

#Selecionar apenas ecorregioes com pontos
m_eco <- eco[is.related(eco, pts, "intersects")]
#Ver mapa
mapview(m_eco, col.regions = "yellow") + mapview(pts, cex = 4)

# Também podemos selecionar as ecorregiões com base no buffer gerado anteriormente
m_eco_buffer <- eco[is.related(eco, m_buffer, "intersects")]
#Ver mapa
mapview(m_buffer, col.regions = "pink") +
  mapview(m_eco, col.regions = "yellow") + 
  mapview(m_eco_buffer, col.regions = "red") + 
  mapview(pts, cex = 4)

# Problemas com ecorregiões:
# Pode gerar vários M's disjuntos (não é um problema em áreas invadidas)
# Considera que espécie é capaz de dispersar para toda uma ecorregião
# Sem buffer, pode não incluir áreas acessíveis para espécie na borda de outra 
# ecorregião sem registros
# Com buffer, pode gerar M muito grande.
# Pode selecionar polígonos distantes da ocorrencia da espécie quando ecorregiões
# são disjuntas. Ex: restingas.

#### Mínimo polígono convexo
m_mpc <- convHull(pts)
#Ver mapa
mapview(m_buffer, col.regions = "pink") +
  mapview(m_eco, col.regions = "yellow") + 
  mapview(m_eco_buffer, col.regions = "red") + 
  mapview(m_mpc, col.regions = "orange") +
  mapview(pts, cex = 4)

#Adicionar buffer em torno do minimo polígono convexo
m_mpc_buffer <- buffer(m_mpc,
                       width = 100 * 1000) #Distancia em metros

mapview(m_buffer, col.regions = "pink") + 
  mapview(m_eco, col.regions = "yellow") + 
  mapview(m_eco_buffer, col.regions = "red") + 
  mapview(m_mpc, col.regions = "orange") +
  mapview(m_mpc_buffer, col.regions = "blue") +
  mapview(pts, cex = 4)

# Problemas com mínimo polígono convexo
# Considera que todos os registros representam populações capazes de dispersar
# Considera que populações podem dispersar igualmente para qualquer lado
# Considera que todas as áreas dentro do polígono foram acessíveis pra espécie
# Ex: imagina uma espécie de planície que vive próximo a parte debaixo dos Andes

# Nesse exercício, vamos salvar todos os Ms
# Buffer
writeVector(m_buffer,
            file.path("M_poligonos/", sp, "m_buffer.gpkg"),
            overwrite = TRUE)
# Ecorregiões
writeVector(m_eco,
            file.path("M_poligonos/", sp, "m_eco.gpkg"),
            overwrite = TRUE)
# Ecorregiões com buffer
writeVector(m_eco_buffer,
            file.path("M_poligonos/", sp, "m_eco_buffer.gpkg"),
            overwrite = TRUE)
# Minimo polígono convexo
writeVector(m_mpc,
            file.path("M_poligonos/", sp, "m_mcp.gpkg"),
            overwrite = TRUE)
# Minimo polígono convexo com buffer
writeVector(m_mpc_buffer,
            file.path("M_poligonos/", sp, "m_mcp_buffer.gpkg"),
            overwrite = TRUE)

# Ver pasta e arquivos
fs::dir_tree("M_poligonos/")
