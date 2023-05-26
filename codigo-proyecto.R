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
library(patchwork)

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


# ideas profe -------------------------------------------------------------

correlaciones <- round(cor(datos),3)
corrplot::corrplot(cor(datos), method = "ellipse")

var_sign <- datos[,c(which(correlaciones[,22]> 0.3 | correlaciones[,22] < -0.3))]

# prueba con k_means

modelo_3 <- kmeans(var_sign, centers = 3,nstart = 50)

new_data_3 = var_sign %>%  
  as.data.frame() %>% 
  mutate(cluster = modelo_3$cluster) %>%
  mutate(cluster = as.factor(cluster),
         tipo   = as.factor(datos$Health)) 

tabla_conf_3 <- table(new_data_3$cluster, new_data_3$tipo,
                      dnn = list("cluster", "grupo real"))

round(prop.table(tabla_conf_3),3)

a <- var_sign %>% 
  mutate(Health_2 = factor(Health)) %>% 
  ggplot(aes(x = ALTV, y = ASTV, colour = Health_2)) +
  geom_point() +
  labs(title = "Datos reales")

b <- datos %>% 
  as.data.frame() %>% 
  mutate(cluster = as.factor(modelo_3$cluster)) %>% 
  ggplot(aes(x = ALTV, y = ASTV, colour = cluster)) +
  geom_point() +
  labs(title = "Asignaciones de clusters")

a+b

# mejor hacer PCA

PCA_centrado_2 <- prcomp(var_sign, center=TRUE, scale=TRUE)
PCA_centrado_2
fviz_screeplot(PCA_centrado_2, main="PCA centrado y reescalado",  addlabels = TRUE)
summary(PCA_centrado_2)

fviz_pca_var(PCA_centrado_2,col.var="contrib",
             gradient.cols = c("yellow", "red"),
             repel = TRUE,  
)

sel_pca <- PCA_centrado_2$x[,c("PC1","PC2")]

modelo_4 <- kmeans(sel_pca, centers = 3,nstart = 50)

new_data_4 = sel_pca %>%  
  as.data.frame() %>% 
  mutate(cluster = modelo_4$cluster) %>%
  mutate(cluster = as.factor(cluster),
         tipo   = as.factor(datos$Health)) 

tabla_conf_4 <- table(new_data_4$cluster, new_data_4$tipo,
                      dnn = list("cluster", "grupo real"))

round(prop.table(tabla_conf_4),3)

c <- sel_pca %>%  
  as.data.frame() %>% 
  mutate(Health_2 = factor(datos$Health)) %>% 
  ggplot(aes(x = PC1, y = PC2, colour = Health_2)) +
  geom_point() +
  labs(title = "Datos reales dps de PCA")

d <- sel_pca %>% 
  as.data.frame() %>% 
  mutate(cluster = as.factor(modelo_4$cluster)) %>% 
  ggplot(aes(x = PC1, y = PC2, colour = cluster)) +
  geom_point() +
  labs(title = "Asignaciones de clusters")

c
d
c+d

# k means con pca mejora
