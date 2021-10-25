########################################
#   CHAMANDO BIBLIOTECAS IMPORTANTES
########################################

library(tidyverse) #pacote para manipulacao de dados
library(cluster) #algoritmo de cluster
library(dendextend) #compara dendogramas
library(factoextra) #algoritmo de cluster e visualizacao
library(fpc) #algoritmo de cluster e visualizacao
library(gridExtra) #para a funcao grid arrange
library(readxl)#Leitor de XLS
library(clv) #Distancia de cluster
library(knitr) #
library(tmap) #Para mapas
library(kableExtra) # para manipulacoes de base
library(factoextra) #Para PCA
library(PerformanceAnalytics) #Para mapa de correlacoes
library(reshape2)#para visualizacoes
library(psych) #testes estatisticos para pca
library(ggrepel) #para visualizacoes
library(ggplot2)
library(RColorBrewer)

########################################
#   PRIMEIROS PASSOS
########################################

#Leitura dos Dados
dados <- read.csv('dados_mapa_desigualdade.csv', sep = ';')

#Verificando a base de dados
View(dados)

#Transformando o index no codigo do distrito
rownames(dados) <- dados[,1]

#Removendo as colunas nao variaveis
dados <- dados[-1:-2]

#Verificando tipo das variaveis
str(dados)

#Removendo colunas que possuem valores nao numericos (nulos)
dados <- dados[,-which(sapply(dados, class) == "character")]

#Verificando o resultado
View(dados)

########################################
#   CLUSTERIZACAO - KMEANS
########################################

#Padronizar variaveis
dados_padrao <- scale(dados)

#Verificando Elbow
fviz_nbclust(dados_padrao, FUN = hcut, method ="wss")

#Criando clusters atraves de kmeans
dados_k4 = kmeans(dados_padrao, centers = 4)
dados_k5 = kmeans(dados_padrao, centers = 5)
dados_k6 = kmeans(dados_padrao, centers = 6)

#Criando graficos da distribuicoes de clusters
G4 = fviz_cluster(dados_k4, geom='point', data=dados_padrao) + ggtitle("k = 4")
G5 = fviz_cluster(dados_k5, geom='point', data=dados_padrao) + ggtitle("k = 5")
G6 = fviz_cluster(dados_k6, geom='point', data=dados_padrao) + ggtitle("k = 6")

#Imprimir graficos na mesma tela
grid.arrange(G4,G5,G6, nrow = 2)

#Verificando a distancia intra e extra cluster
dist_k4 <- cls.scatt.data(dados_padrao,dados_k4$cluster)
dist_k5 <- cls.scatt.data(dados_padrao,dados_k5$cluster)
dist_k6 <- cls.scatt.data(dados_padrao,dados_k6$cluster)

#Verificando a distancia extracluster
dist_k4$intercls.average
dist_k5$intercls.average
dist_k6$intercls.average

#Verificando a distancia intracluster
dist_k4$intracls.average
dist_k5$intracls.average
dist_k6$intracls.average

########################################
#   ADICIONANDO OS RESULTADOS
########################################

#Criando variaveis a partir do resultado
cluster_k4 <- data.frame(dados_k4$cluster)
cluster_k5 <- data.frame(dados_k5$cluster)
cluster_k6 <- data.frame(dados_k6$cluster)

#Puxando uma nova database para analise
mp_desigualdade <- read.csv('dados_mapa_desigualdade.csv', sep = ';')

#Adicionando as colunas
mp_desigualdade <- cbind(mp_desigualdade, cluster_k4)
mp_desigualdade <- cbind(mp_desigualdade, cluster_k5)
mp_desigualdade <- cbind(mp_desigualdade, cluster_k6)

########################################
#   OBSERVANDO OS RESULTADOS INICIAIS
########################################

#Carregando o mapa
load(file = "mapa_sp.RData")

#Visualizando o mapa
tm_shape(mapa_sp) + 
  tm_borders()

#Acrescentando informações ao mapa
mapa_sp@data$COD_DIST <- as.numeric(mapa_sp@data$COD_DIST)

#Unindo as bases
distritos_dados <- merge(mapa_sp, 
                         mp_desigualdade,
                         by.x = 'COD_DIST',
                         by.y = 'ID_REGIAO')

#Plotando os mapas
tmap_mode('view')

#Mapa para 4K
tm_shape(distritos_dados) +
  tm_fill('dados_k4.cluster', midpoint=0,palette="RdBu",
          style='cat', n=4,legend.show=T) +
  tm_borders(alpha = 0.8) +
  tm_text('NOME_DIST', size=0.7) 

#Mapa para 5K
tm_shape(distritos_dados) +
  tm_fill('dados_k5.cluster', midpoint=0,palette="RdBu",
          style='cat', n=5,legend.show=T) +
  tm_borders(alpha = 0.8) +
  tm_text('NOME_DIST', size=0.7)

#Mapa para 6K
tm_shape(distritos_dados) +
  tm_fill('dados_k6.cluster', midpoint=0,palette="RdBu",
          style='cat', n=6,legend.show=T) +
  tm_borders(alpha = 0.8) +
  tm_text('NOME_DIST', size=0.7)


########################################
#   CLASSIFICANDO OS RESULTADOS
########################################

#Medias das variaveis por grupo

#Media 4k
mediak4 <- mp_desigualdade %>%
  group_by(dados_k4.cluster) %>%
  summarise(n = n(),
            Favelas = mean(Favelas),
            Ofertadeempregoformal= mean(Ofertadeempregoformal),
            Acessoatransportedemassa= mean(Acessoatransportedemassa),
            AcessointernetMapeamentodasantenasáreakm. = mean(AcessointernetMapeamentodasantenasáreakm),
            Violênciacontraamulhertodas = mean(Violênciacontraamulhertodas),
            Mortesporintervençãopolicial= mean(Mortesporintervençãopolicial),
            Gravideznaadolescência = mean(Gravideznaadolescência),
            Mortalidadeinfantil= mean(Mortalidadeinfantil),
            ÍndicedoNívelSocioeconômicodasEscolasINSE= mean(ÍndicedoNívelSocioeconômicodasEscolasINSE),
            Idademédiaaomorrer= mean(Idademédiaaomorrer)    )
mediak4 <- data.frame(mediak4)
mediak4

#Media 5k
mediak5 <- mp_desigualdade %>%
  group_by(dados_k5.cluster) %>%
  summarise(n = n(),
            Favelas = mean(Favelas),
            Ofertadeempregoformal= mean(Ofertadeempregoformal),
            Acessoatransportedemassa= mean(Acessoatransportedemassa),
            AcessointernetMapeamentodasantenasáreakm. = mean(AcessointernetMapeamentodasantenasáreakm),
            Violênciacontraamulhertodas = mean(Violênciacontraamulhertodas),
            Mortesporintervençãopolicial= mean(Mortesporintervençãopolicial),
            Gravideznaadolescência = mean(Gravideznaadolescência),
            Mortalidadeinfantil= mean(Mortalidadeinfantil),
            ÍndicedoNívelSocioeconômicodasEscolasINSE= mean(ÍndicedoNívelSocioeconômicodasEscolasINSE),
            Idademédiaaomorrer= mean(Idademédiaaomorrer)    )
mediak5 <- data.frame(mediak5)
mediak5

#Media 6k
mediak6 <- mp_desigualdade %>%
  group_by(dados_k6.cluster) %>%
  summarise(n = n(),
            Favelas = mean(Favelas),
            Ofertadeempregoformal= mean(Ofertadeempregoformal),
            Acessoatransportedemassa= mean(Acessoatransportedemassa),
            AcessointernetMapeamentodasantenasáreakm. = mean(AcessointernetMapeamentodasantenasáreakm),
            Violênciacontraamulhertodas = mean(Violênciacontraamulhertodas),
            Mortesporintervençãopolicial= mean(Mortesporintervençãopolicial),
            Gravideznaadolescência = mean(Gravideznaadolescência),
            Mortalidadeinfantil= mean(Mortalidadeinfantil),
            ÍndicedoNívelSocioeconômicodasEscolasINSE= mean(ÍndicedoNívelSocioeconômicodasEscolasINSE),
            Idademédiaaomorrer= mean(Idademédiaaomorrer)    )
mediak6 <- data.frame(mediak6)
mediak6

#Trocando os numeros pela classificacao a partir da observacao das variaveis

#Para 4k
distritos_dados@data$dados_k4.cluster[distritos_dados@data$dados_k4.cluster == 1] <- 'Centro'
distritos_dados@data$dados_k4.cluster[distritos_dados@data$dados_k4.cluster == 2] <- 'Não Vulnerável'
distritos_dados@data$dados_k4.cluster[distritos_dados@data$dados_k4.cluster == 3] <- 'Muito Vulnerável'
distritos_dados@data$dados_k4.cluster[distritos_dados@data$dados_k4.cluster == 4] <- 'Vulnerável'

#Para 5k
distritos_dados@data$dados_k5.cluster[distritos_dados@data$dados_k5.cluster == 1] <- '1- Centro'
distritos_dados@data$dados_k5.cluster[distritos_dados@data$dados_k5.cluster == 2] <- '5- Muito Vulnerável'
distritos_dados@data$dados_k5.cluster[distritos_dados@data$dados_k5.cluster == 3] <- '3- Pouco Vulnerável'
distritos_dados@data$dados_k5.cluster[distritos_dados@data$dados_k5.cluster == 4] <- '4-Vulnerável'
distritos_dados@data$dados_k5.cluster[distritos_dados@data$dados_k5.cluster == 5] <- '2- Não Vulnerável'

#Para 6k
distritos_dados@data$dados_k6.cluster[distritos_dados@data$dados_k6.cluster == 1] <- 'Muito Vulnerável'
distritos_dados@data$dados_k6.cluster[distritos_dados@data$dados_k6.cluster == 2] <- 'Não Vulnerável'
distritos_dados@data$dados_k6.cluster[distritos_dados@data$dados_k6.cluster == 3] <- 'Centro'
distritos_dados@data$dados_k6.cluster[distritos_dados@data$dados_k6.cluster == 4] <- 'Médio Vulnerável'
distritos_dados@data$dados_k6.cluster[distritos_dados@data$dados_k6.cluster == 5] <- 'Vulnerável'
distritos_dados@data$dados_k6.cluster[distritos_dados@data$dados_k6.cluster == 6] <- 'Pouco Vulnerável'

########################################
#   RESULTADOS FINAIS
########################################

#Alterando o nome
colnames(distritos_dados@data)[63] <- 'Graus de Desigualdade (K=5)'

#Mapa para 5K
tm_shape(distritos_dados) +
  tm_fill('Graus de Desigualdade (K=5)', midpoint=0,palette= 'seq',
          style='cat', n=5,legend.show=T) +
  tm_layout(aes.palette = list(seq = "-RdYlGn"))+
tm_borders(alpha = 0.8) +
tm_text('NOME_DIST', size=0.7)

#Criando mapa estatíco
tmap_mode('plot')

#Mapa para 5K
tm_shape(distritos_dados) +
  tm_fill('Graus de Desigualdade (K=5)', midpoint=0,palette= 'seq',
          style='cat', n=5,legend.show=T) +
  tm_layout(aes.palette = list(seq = "-RdYlGn"))+
  tm_borders(alpha = 0.8)
