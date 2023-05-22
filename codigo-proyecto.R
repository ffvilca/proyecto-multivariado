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
library(factoextra)

datos <- read.csv("salud_fetos.csv",sep = ",",header = T)

str(datos)
View(datos)

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
