# Proyecto -----
# 
# Analisis Multivariado 
# 
# Mauricio Goméz
# Francisca Vilca Sanchez

# PAQUETES A USAR ----

library(rio)
library(dplyr)
library(beepr)
library(ggplot2)
library(ggrepel)
library(kableExtra)
library(TeachingDemos)
library(pracma)
library(mvnormtest)
library(MVN)
library(EnvStats)
library(FactoMineR)
library(factoextra)
library(tidyverse)
library(cluster)

datos <- read.csv("salud_fetos.csv",sep = ",",header = T)

str(datos)
View(datos)

table(datos$Health)
prop.table(table(datos$Health))

x11()
pairs(datos)
round(cor(datos),3)
corrplot::corrplot(cor(datos), method = "ellipse")

#Haciendo un PCA
PCA_centrado=prcomp(datos, center=TRUE, scale=TRUE)
PCA_centrado
fviz_screeplot(PCA, main="PCA",  addlabels = TRUE)
fviz_screeplot(PCA_centrado, main="PCA centrado y reescalado",  addlabels = TRUE)
summary(PCA_centrado)

fviz_pca_var(PCA_centrado,col.var="contrib",
             gradient.cols = c("yellow", "red"),
             repel = TRUE,  
)


#Nuevos datos ya reducidos con el PCA

new_datos <- PCA_centrado$x[,1:6]


# CON 6 VARIABLES ---------------------------------------------------

# Haciendo clusters con kmeans

modelo_1.1 <- kmeans(new_datos, centers = 3,nstart = 50)

new_data_1.1 = new_datos %>%  
  as.data.frame() %>% 
  mutate(cluster = modelo_1.1$cluster) %>%
  mutate(cluster = as.factor(cluster),
         tipo   = as.factor(datos$Health)) 


tabla_conf_1.1 <- table(new_data_1.1$cluster, new_data_1.1$tipo,
                      dnn = list("cluster", "grupo real"))

round(prop.table(tabla_conf_1.1),3)

# Haciendo clusters con modelo fanny

modelo_2.1 <- diana(new_datos)

plot(as.hclust(modelo_2.1), ylab = "", xlab = "", sub = "", main = "Diana", cex = 0.8)
clusters_diana <- cutree(tree = as.hclust(modelo_2.1), k = 3)
table(clusters_diana, tipo, dnn = list("clusters", "tipo de cáncer"))

x11()
modelo_2.2=agnes(new_datos,method="average" )
plot(as.hclust(modelo_2.2), ylab = "", xlab = "", sub = "", main = "Agnes", cex = 0.8)
clusters_diana <- cutree(tree = as.hclust(modelo_2.2), k = 3)
table(clusters_diana, tipo, dnn = list("clusters", "tipo de cáncer"))



new_data_2.1 = new_datos %>%  
  as.data.frame() %>% 
  mutate(cluster = modelo_2.1$) %>%
  mutate(cluster = as.factor(cluster),
         tipo   = as.factor(datos$Health))


tabla_conf_2.1 <- table(new_data_2.1$cluster, new_data_2.1$tipo,
                      dnn = list("cluster", "grupo real"))

round(prop.table(tabla_conf_2.1),3)

hkmeans_cluster <- hkmeans(x = datos, hc.metric = "euclidean",
                           hc.method = "complete", k = 3)

fviz_cluster(object = hkmeans_cluster, pallete = "jco", repel = TRUE) +
  theme_bw() + labs(title = "Hierarchical k-means Clustering")

fviz_cluster(object = modelo_2, repel = TRUE, ellipse.type = "norm",
             pallete = "jco") + 
  theme_bw() + 
  labs(title = "Fuzzy Cluster plot")





# kmeans datos sin PCA

modelo_1 <- kmeans(datos, centers = 3,nstart = 50)

new_data_1 = datos %>%  
  as.data.frame() %>% 
  mutate(cluster = modelo_1$cluster) %>%
  mutate(cluster = as.factor(cluster),
         tipo   = as.factor(datos$Health)) 

tabla_conf_1 <- table(new_data_1$cluster, new_data_1$tipo,
                      dnn = list("cluster", "grupo real"))

round(prop.table(tabla_conf_1),3)

# funny

modelo_2 <- fanny(datos, k=2)

new_data_2 = datos %>%  
  as.data.frame() %>% 
  mutate(cluster = modelo_2$cluster) %>%
  mutate(cluster = as.factor(cluster),
         tipo   = as.factor(datos$Health)) 

tabla_conf_2 <- table(new_data_2$cluster, new_data_2$tipo,
                      dnn = list("cluster", "grupo real"))

round(prop.table(tabla_conf_2),3)

hkmeans_cluster <- hkmeans(x = datos, hc.metric = "euclidean",
                           hc.method = "complete", k = 3)

fviz_cluster(object = hkmeans_cluster, pallete = "jco", repel = TRUE) +
  theme_bw() + labs(title = "Hierarchical k-means Clustering")

fviz_cluster(object = modelo_2, repel = TRUE, ellipse.type = "norm",
             pallete = "jco") + 
  theme_bw() + 
  labs(title = "Fuzzy Cluster plot")




fviz_pca_ind(PCA_centrado, col.ind = "#00AFBB",
             repel = TRUE, habillage = datos$Health)

a <- cbind(PCA_centrado$x,datos$Health)
x11()
a %>%
  as.data.frame() %>% 
  ggplot(aes(PC1,PC2, = Health))+
  geom_point()
