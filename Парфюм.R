library(readxl)

data = read_excel('C:\\Users\\annap\\Desktop\\вуз\\3 курс\\ЭКОНОМЕТРИКА\\данные\\perfume_data.xlsx')
#  Проверим размерность данных
dim(data)
#стандартизируем значения признаков
T.stand<-scale(data[,2:8])

#анализируем склонность к кластеризации
library(cluster) ; 
library("factoextra")
get_clust_tendency(T.stand, n = 10,
                   gradient = list(low = "steelblue", high = "white"))

library(factoextra)
library(NbClust)
nb <- NbClust(T.stand, distance = "euclidean", min.nc = 2,
              max.nc = 5, method = "complete", index ="all")
nb$Best.nc
fviz_nbclust(nb) + theme_minimal()
if (any(class(best_nc) == "numeric") )
#Находим наилучшую метрику
d <- dist(T.stand, method = "euclidean")
library(vegan)
hc_list <- list(hc1 <- hclust(d,"com"),
                hc2 <- hclust(d,"single"), hc3 <- hclust(d,"ave"),
                hc4 <- hclust(d, "centroid"),hc5 <- hclust(d, "ward.D2"))
Coph <- rbind(
  MantelStat <- unlist(lapply(hc_list,
                              function (hc) mantel(d, cophenetic(hc))$statistic)),
  MantelP <- unlist(lapply(hc_list,
                           
                           function (hc) mantel(d, cophenetic(hc))$signif)))
colnames(Coph) <- c("Complete", "Single","Average",
                    "Centroid","Ward.D2")
rownames(Coph) <- c("W Мантеля","Р-значение")
round(Coph, 3)

#Строим дендрограмму

d<-dist(T.stand)
hclust(d, method = "centroid", members = NULL)

par(mfrow=c(1,1))
hc<-hclust(d, method = "centroid", members = NULL)
plot(hc,hang=-1,main="Дендограмма претендентов")
rect.hclust(hc, k=5, border="red")

hc<-hclust(d, method = "ave", members = NULL)
plot(hc,hang=-1,main="Дендограмма претендентов")
rect.hclust(hc, k=5, border="blue")

par(mfrow=c(1,1))
hc<-hclust(d, method = "ward.D2", members = NULL)
plot(hc,hang=-1,main="Дендограмма претендентов")
rect.hclust(hc, k=5, border="blue")

# 4 clusters
par(mfrow=c(1,1))
hc<-hclust(d, method = "centroid", members = NULL)
plot(hc,hang=-1,main="Дендограмма претендентов")
rect.hclust(hc, k=4, border="blue")

par(mfrow=c(1,1))
hc<-hclust(d, method = "ward.D2", members = NULL)
plot(hc,hang=-1,main="Дендограмма претендентов")
rect.hclust(hc, k=4, border="blue")

# agnes, diana
hc<-agnes(data, metric = "euclidean", stand = FALSE, method = "average")
plot(hc,hang=-1,main="Дендограмма целей")

hc<-diana(data, metric = "euclidean", stand = FALSE)
plot(hc,hang=-1,main="Дендограмма целей")

#K-MEANS
fviz_nbclust(T.stand, kmeans, method = "wss") +
  geom_vline(xintercept = 2, linetype = 2)

set.seed(120)
clus.pret<-kmeans(T.stand,4,iter.max=1000)
clus.pret$cluster
clus.pret$size

#Качество кластеризации
library(cluster)
RS<-clus.pret$betweenss/clus.pret$totss
#Построение диаграммы силуэтов
plot(silhouette(clus.pret$cluster,dist(T.stand)),col=c("red","blue","green"))

#Центроиды
options(digits=2)
t(clus.pret$centers)

#Дисперсии
clus.pret$totss
clus.pret$tot.withinss
clus.pret$withinss
clus.pret$betweenss

clus.pret$betweenss/clus.pret$totss

#Визуализация

fviz_cluster(clus.pret,data=T.stand,ellipse.type = "t", 
             ellipse.level = 0.7, values = c('purple', 'green', 'blue','yellow'))

fviz_cluster(clus.pret, data= T.stand ,ellipse.type = "norm",
             ellipce.level = 0.95,ellipce.alfa=0)+ stat_ellipse()


fviz_cluster(clus.pret, data= T.stand ,ellipse.type = "norm",
             ellipce.level = 0.95)+ stat_ellipse()


fviz_cluster(clus.pret, data= T.stand) + geom_point(color=('white') + stat_ellipse(type = "norm", linetype = 2) + stat_ellipse(type = "t"))
                                                    
