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
<<<<<<< HEAD
library(FactoMineR)
library(factoextra)
library(tidyverse)
=======
library(factoextra)
>>>>>>> 820be7c3004e47f6bb9e632317db196390227087

datos <- read.csv("salud_fetos.csv",sep = ",",header = T)

str(datos)
View(datos)

<<<<<<< HEAD
x11()
pairs(datos)

PCA_centrado=prcomp(datos, center=TRUE, scale=TRUE)
PCA
PCA_centrado
fviz_screeplot(PCA, main="PCA",  addlabels = TRUE)
fviz_screeplot(PCA_centrado, main="PCA centrado y reescalado",  addlabels = TRUE)
summary(PCA_centrado)

corrplot::datos
fviz_pca_var(PCA_centrado,col.var="contrib",
             gradient.cols = c("yellow", "red"),
             repel = TRUE,  
)
rename(datos, )
=======
round(cor(datos),3)
corrplot::corrplot(cor(datos), method = "ellipse")

comp <- prcomp(datos, scale = T) 

screeplot(comp)

biplot(comp)

fviz_pca_biplot(comp, repel = TRUE,
                col.var = "#2E9FDF", # Variables color
                col.ind = "#696969"  # Individuals color
)

new_datos <- comp$rotation[,1:6]

modelo <- kmeans(datos, centers = 3,nstart = 50)

new_data = datos %>%  
  as.data.frame() %>% 
  mutate(cluster = modelo$cluster) %>%
  mutate(cluster = as.factor(cluster),
         tipo   = as.factor(datos$Health)) 

table(new_data$cluster, new_data$tipo,
      dnn = list("cluster", "grupo real"))
>>>>>>> 820be7c3004e47f6bb9e632317db196390227087
