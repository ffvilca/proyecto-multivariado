# Proyecto -----
# 
# Analisis Multivariado 
# 
# Mauricio Goméz
# Francisca Vilca Sanchez

# PAQUETES A USAR

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
library(MASS)
library(caret)


# Previsualización de los datos -------------------------------------



datos <- read.csv("salud_fetos.csv",sep = ",",header = T)
datos$Health <- as.factor(datos$Health)

str(datos)
View(datos)

table(datos$Health)
prop.table(table(datos$Health))


# Revisando Correlacion ----------------------------------------------


correlaciones <- round(cor(datos),3)
corrplot::corrplot(cor(datos), method = "circle")


# Seleccion variables significativas ---------------------------------


datos_2<- datos[,c(which(correlaciones[,22]> 0.2 | correlaciones[,22] < -0.2))]
str(datos_2)


# Creación de PCA ---------------------------------------------------

PCA_cent_res <- prcomp(datos_2, center=TRUE, scale=TRUE)
summary(PCA_cent_res)

fviz_screeplot(PCA_cent_res, main="PCA centrado y reescalado",  addlabels = TRUE)

PCA_cent_res$sdev

fviz_pca_var(PCA_cent_res,col.var="contrib",
             gradient.cols = c("yellow", "blue"),
             repel = TRUE,  
)

fviz_pca_ind(PCA_cent_res, col.ind = "cos2", 
             gradient.cols =c("gray", "purple", "beige"),
             repel = F, 
)

datos_3 <- PCA_cent_res$x[,c("PC1","PC2","PC3")]
head(datos_3)


# Metodo k-means ----------------------------------------------------

modelo_kms <- kmeans(datos_3, centers = 3,nstart = 50)

data_kms = datos_3 %>%  
  as.data.frame() %>% 
  mutate(cluster = modelo_kms$cluster) %>%
  mutate(cluster = as.factor(cluster),
         tipo   = as.factor(datos$Health)) 

tabla_conf_kms <- table(data_kms$cluster, data_kms$tipo,
                      dnn = list("cluster", "grupo real"))


confusionMatrix(table(data_kms$cluster,datos$Health))

graf1 <- datos_3 %>%  
  as.data.frame() %>%
  mutate(Salud_feto = ifelse(datos$Health == 1, "Sano",ifelse(datos$Health == 2, "Sospechoso","Patologico"))) %>% 
  ggplot(aes(x = PC1, y = PC2, colour = Salud_feto)) +
  geom_point() +
  labs(title = "Datos reales después de PCA") 

graf2 <- datos_3 %>% 
  as.data.frame() %>% 
  mutate(cluster = as.factor(modelo_kms$cluster)) %>% 
  ggplot(aes(x = PC1, y = PC2, colour = cluster)) +
  geom_point() +
  labs(title = "Asignaciones de clusters método K-means")

graf2

graf1 + graf2


# Metodo qda --------------------------------------------------------

datos_qda <- datos_3 %>%  
  as.data.frame() %>% 
  mutate(Salud_feto = factor(datos$Health))

modelo_qda <- qda(Salud_feto ~., datos_qda)

pred_qda <- predict(modelo_qda, datos_qda)$class

tabla_conf_qda <- table(pred_qda, datos$Health,
                      dnn = list("cluster", "grupo real"))

tabla_conf_qda

confusionMatrix(table(pred_qda,datos$Health))

graf3 <- datos_qda %>%  
  as.data.frame() %>%
  mutate(Salud_feto = ifelse(datos$Health == 1, "Sano",ifelse(datos$Health == 2, "Sospechoso","Patologico"))) %>% 
  ggplot(aes(x = PC1, y = PC2, colour = Salud_feto)) +
  geom_point() +
  labs(title = "Datos reales después de PCA")

graf3

graf4 <- datos_qda %>% 
  as.data.frame() %>% 
  mutate(cluster = pred_qda) %>% 
  ggplot(aes(x = PC1, y = PC2, colour = cluster)) +
  geom_point() +
  labs(title = "Asignaciones de clusters método qda")

graf4

graf3 + graf4

graf1 + graf2 + graf4
