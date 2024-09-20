library(dplyr)
library(MVar.pt)
library(Kira)

dados   <- read.csv('ibge.csv', encoding ='UTF-8')
dados21 <- dados[dados$Ano == 2021, ]
row.names(dados21) <- 1:nrow(dados21)
colunas <- c('vlr_agro', 'vlr_ind', 'vlr_ser', 'vlr_pub',
             'vlr_imp', 'area','tot_pop','pib_capta', 'tot_dom_par','tot_dom_col')


# Clusters ----------------------------------------------------------------
# 3830 - São Paulo ; 5570 - Brasília; 3243 - Rio de Janeiro

dados4 <- na.omit(dados21[-1*c(5570,3243,3830), c(colunas, 'nom_mun', 'uf','Nom_reg')])

dados5 <- dados4[colunas]
library(Kira)


dist <- c("euclidean","maximum","manhattan","canberra","minkowski")
mtd  <- c("complete","ward.D","ward.D2","single","average","mcquitty","median", "centroid")

# for(i in dist){
#   for(j in mtd)
#   {hierarchical(dados4, titles = paste("Distancia:",i, "Metodo: ",j), 
#                 normalize =  T,casc = F, 
#                 distance = i, method = j,
#                 savptc = T, width = 3236, 
#                 height = 2000, res = 300)
#     }  
#   }


hierarchical(dados5, titles = 'Distância Canberra e método Ward-D', 
             normalize =  T,casc = F, 
             distance = 'canberra', method = 'ward.D',
             savptc = T, width = 3236, 
             height = 2000, res = 300)

set.seed(124105)
kmeans(dados5,normalize = T, num.groups = 4) #
kmeans(dados5,normalize = T, num.groups = 5) # R.sqt aumenta ok 0.04
fit <- kmeans(dados5,normalize = T, num.groups = 6) # R.sqt aumenta bastante 0.07
#kmeans(dados4,normalize = T, num.groups = 7) # R.sqt aumenta ok 0.03
# será optado por 6 grupos, tanto pelo endograma quanto pelo R


# Grupo 1: teve 10 cidades;
# Grupo 2: teve 175 cidades;
# Grupo 3: teve 28 cidades;
# Grupo 4: teve 36 cidades;
# Grupo 5: teve 125 cidades;
# Grupo 6: teve 5192 cidades;

dados5 <- fit$groups

g1 <- dados5[dados5$groups == 1,]
g2 <- dados5[dados5$groups == 2,]
g3 <- dados5[dados5$groups == 3,]
g4 <- dados5[dados5$groups == 4,]
g5 <- dados5[dados5$groups == 5,]
g6 <- dados5[dados5$groups == 6,]

estatisticas <- data.frame()
grupos <- list(g1, g2, g3, g4, g5, g6)
for(i in 1:length(grupos))
{
  grupo_atual <- grupos[[i]]
  medias   <- data.frame(t(apply(grupo_atual, 2, mean)))
  medianas <- data.frame(t(apply(grupo_atual, 2, median)))
  desvios  <- data.frame(t(apply(grupo_atual, 2, sd)) )
  estatisticas <- rbind(estatisticas,medias,medianas,desvios)
  
}
estatisticas$groups <- c(rep(1,3),rep(2,3),rep(3,3),rep(4,3),rep(5,3),rep(6,3))
row.names(estatisticas) = paste0(rep(c('Média', 'Mediana', 'Desvio'),length(grupos)),estatisticas$groups)
estatisticas <- round(estatisticas)
# Grupo 1: teve 10 cidades;
# Grupo 2: teve 175 cidades;
# Grupo 3: teve 28 cidades;
# Grupo 4: teve 36 cidades;
# Grupo 5: teve 125 cidades;
# Grupo 6: teve 5192 cidades;
round(estatisticas)
write.csv(round(estatisticas),file = 'estatisticas.csv')

# +++ altíssimo   ++ alto   + mediocre
# --- baixíssimo  -- baixo  - mediocre

#Grupo 1: --  vlr_agro, +++ vlr_ind, +++ vlr_ser, +++ vlr_pub, +++ vlr_imp, -   area, +++ tot_pop, -   pib_capta, +++ tot_dom_par, +++ tot_dom_col; n= 10
#Grupo 2: --  vlr_agro, +   vlr_ind, +   vlr_ser, ++  vlr_pub, +   vlr_imp, -   area, ++  tot_pop, -   pib_capta, +   tot_dom_par, +   tot_dom_col; n= 175
#Grupo 3: --  vlr_agro, --  vlr_ind, --  vlr_ser, --- vlr_pub, --- vlr_imp, +++ area, --  tot_pop, --- pib_capta, --  tot_dom_par, --  tot_dom_col; n= 28
#Grupo 4: --  vlr_agro, +++ vlr_ind, +++ vlr_ser, ++  vlr_pub, +   vlr_imp, -   area, ++  tot_pop, ++  pib_capta, ++  tot_dom_par, ++  tot_dom_col; n= 36      
#Grupo 5: +++ vlr_agro, +   vlr_ind, +   vlr_ser, --- vlr_pub, --  vlr_imp, +   area, --  tot_pop, +++ pib_capta, --- tot_dom_par, --  tot_dom_col; n= 125
#Grupo 6: --  vlr_agro, --  vlr_ind, --- vlr_ser, --- vlr_pub, --- vlr_imp, -   area, --- tot_pop, --- pib_capta, --- tot_dom_par, --- tot_dom_col; n= 5192
5192/(10+175+28+36+125+5192)
# Resumindo
# Grupo 1: pouquissimas cidades, com uma economia bem grande focada nos setores industriais e de serviço, grandes concentrações populacionais, com baixo pib/capita
# Grupo 2: algumas cidades, com a economia voltada para os setores industriais e de serviços, cidades de médio porte, com população considerável e baixo pib/capita
# Grupo 3: pouquissimas cidades, baixa economia, cidades gigantesca em area, com baixa população e baixíssimo pib per capita
# Grupo 4: poquissimas cidades, focadas em setores de serviços e industriais, com população considerável e pib per capita ok
# Grupo 5: algumas cidades, focadas quase totalmente em agricultura, com alto pib per capita, e baixa população
# Grupo 6: Mais de 90% das cidades, cidades pequenas, com baixa economia, baixa população e baixíssimo pib per capita




# Regras de associação ----------------------------------------------------

discretizar_c <- function(banco, colunas, n_faixas, banco_faixas)
{
  #lab1   = seq(0, 100, round(100/n_faixas,2))
  labels = c('Baixíssimo','Baixo','Mediocre','Alto','Altíssimo')
  resultado <- data.frame('a' = rep(NA, nrow(banco)))
  grupos    <- list()
  
  for(c in colunas)
  {
    faixas    <- c(0, seq(1/n_faixas, 1, 1/n_faixas)[1:(n_faixas-1)],1)
    faixas    <- quantile(banco_faixas[[c]], faixas)
    banco[,c] <- cut(as.numeric(banco[[c]]), faixas,include.lowest = T, labels = labels)
    resultado <- cbind(resultado,banco[,c])
    
  }
  
  resultado <- resultado[-1]
  names(resultado) <- colunas
  return(data.frame(resultado))
}


g1_d <- discretizar_c(g1[-ncol(g1)], colunas, 5, dados5)
g2_d <- discretizar_c(g2[-ncol(g2)], colunas, 5, dados5)
g3_d <- discretizar_c(g3[-ncol(g3)], colunas, 5, dados5)
g4_d <- discretizar_c(g4[-ncol(g4)], colunas, 5, dados5)
g5_d <- discretizar_c(g5[-ncol(g5)], colunas, 5, dados5)
g6_d <- discretizar_c(g6[-ncol(g6)], colunas, 5, dados5)

library("arules")
library("rCBA")

itemsets <- eclat(g1_d, parameter = list(supp = 0.2, maxlen = 3))
regras   <- ruleInduction(itemsets, confidence = .5)
summary(itemsets)
x <- inspect(regras); x[order(x$lift, decreasing = T),]

# G1
# Concentração de altíssimo nas colunas de valores e população, com exceção de agro
# {vlr_agro=Mediocre, pib_capta=Altíssimo} =>         {area=Mediocre}     0.2  1.0000000 2.500000
# {vlr_agro=Mediocre, area=Mediocre}       =>   {pib_capta=Altíssimo}     0.2  1.0000000 2.500000
# Os domicílios são homogêneos e poucos

itemsets <- eclat(g2_d, parameter = list(supp = 0.2, maxlen = 3))
regras   <- ruleInduction(itemsets, confidence = .5)
x <- inspect(regras); x[order(x$lift, decreasing = T),]
# G2
# {vlr_agro=Baixíssimo, vlr_ind=Altíssimo} =>       {area=Baixíssimo} 0.1314286  0.7666667 3.833333
# {vlr_agro=Baixíssimo}                    =>       {area=Baixíssimo} 0.1314286  0.7666667 3.833333
# vlr_ind=Altíssimo, area=Altíssimo}      =>    {vlr_agro=Altíssimo} 0.1942857  0.8292683 2.502103
# A maioria das regras com maiores lifs relacinam area e agricultura ou area com domicilios são correlacionados positivos
 

  itemsets <- eclat(g3_d, parameter = list(supp = 0.2, maxlen = 3))
regras   <- ruleInduction(itemsets, confidence = .5)
x <- inspect(regras); x[order(x$lift, decreasing = T),]
#G3
# Altíssimo liga em altíssimo para os maiores lifts, tudo é altíssimo
# {vlr_imp=Altíssimo, area=Altíssimo}   =>  {vlr_ser=Altíssimo} 0.2500000  1.0000000 4.000000
# {tot_pop=Alto, pib_capta=Baixíssimo} =>   {vlr_ser=Baixo} 0.2142857  1.0000000 3.111111


itemsets <- eclat(g4_d, parameter = list(supp = 0.2, maxlen = 3))
regras   <- ruleInduction(itemsets, confidence = .5)
x <- inspect(regras); x[order(x$lift, decreasing = T),]
#G4
# Altíssimo liga em altíssimo para os maiores lifts, tudo é altíssimo
# Poucas regras com lift > 1, todas elas sendo do pib per capita altíssimo
# {vlr_agro=Baixo, vlr_ind=Altíssimo} =>   {pib_capta=Altíssimo} 0.2500000  0.8181818 1.178182
# vlr_agro=Baixo} =>   {pib_capta=Altíssimo} 0.2500000  0.8181818 1.178182
# {vlr_ind=Altíssimo, area=Mediocre} =>   {pib_capta=Altíssimo} 0.2222222  0.8000000 1.152000
# {vlr_ser=Altíssimo, area=Mediocre} =>   {pib_capta=Altíssimo} 0.2222222  0.8000000 1.152000
itemsets <- eclat(g5_d, parameter = list(supp = 0.2, maxlen = 3))
regras   <- ruleInduction(itemsets, confidence = .5)
x <- inspect(regras); x[order(x$lift, decreasing = T),]
# G5
# Todas as regras com lidt alto relacionam => com população/domicilios
# {pib_capta=Altíssimo, tot_dom_par=Alto} =>          {tot_pop=Alto}   0.208  0.8965517 4.150702
# {tot_dom_par=Alto}                      =>          {tot_pop=Alto}   0.208  0.8666667 4.012346

itemsets <- eclat(g6_d, parameter = list(supp = 0.1, maxlen = 3))
regras   <- ruleInduction(itemsets, confidence = .5)
x <- inspect(regras); x[order(x$lift, decreasing = T),]
# g6
# Os pontos de cortes anteriores foram 0.2 no suporte devido ter poucas cidades nos grupos
# Aqui foi 0.1 pela amostra ser maior e considerando 0.2 nenhuma regra existiria
# Altíssimo => Altíssimo
# Baixíssimo => Baixíssimo
# A variáveil imposto e população foram as que mais apareceram nas principais regras
# {vlr_imp=Altíssimo, tot_dom_par=Altíssimo} =>      {vlr_ser=Altíssimo} 0.1022727  0.9743119 6.307516
# {vlr_ind=Altíssimo, vlr_ser=Altíssimo} =>      {vlr_imp=Altíssimo} 0.1057396  0.9682540 6.276123
# {vlr_ind=Baixíssimo, vlr_ser=Baixíssimo} =>     {vlr_imp=Baixíssimo} 0.1571649  0.9304447 4.348217

# No fim, independente do cluster e para maioria das variável altíssimo/alto em uma variável está relacionado com altíssimo/alto em outra variável, enquanto baixíssimo está relacionado com baixíssimo.

# As variáveis que fogem desse padrão foram, valor de agricultura, área e pib per capita  

# MAPA
cordenadas <- read.csv('cordenadas.csv'); names(cordenadas)[1] <- 'CD_MUN'
cordenadas$CD_MUN <- as.character(cordenadas$CD_MUN)
library(dplyr)
dados6 <- dados5 %>% left_join(dados21, by = c('vlr_agro', 'vlr_ind', 'vlr_ser', 'vlr_pub', 'vlr_imp', 'area'))
dados6 <- dados6 %>% left_join(cordenadas)
dados6 <- dados6[,c('groups', 'CD_MUN', 'cod_uf',  'nom_mun', 'latitude', 'longitude')]
dados6$groups <- as.factor(dados6$groups)

# Carregar os pacotes necessários
library(ggplot2)
library(sf)  # para lidar com dados espaciais
library(rnaturalearth)  # para obter dados de mapas
library(rnaturalearthdata)

# Carregar o shapefile do Brasil
brasil <- ne_countries(scale = "medium", country = "Brazil", returnclass = "sf")

# Visualizar os primeiros registros de dados6
head(dados6)

# Criar o gráfico
ggplot(data = brasil) +
  geom_sf() +  # Adiciona o mapa do Brasil
  geom_point(data = dados6, aes(x = longitude, y = latitude, color = groups), size = 2, alpha = 0.8) +
  labs(title = "Distribuição dos Grupos no Brasil", x = "Longitude", y = "Latitude") +
  theme_minimal() +
  theme(legend.title = element_text(size = 10), legend.text = element_text(size = 8)) +
  scale_color_discrete(name = "Grupos")  # Nome da legenda dos grupos
