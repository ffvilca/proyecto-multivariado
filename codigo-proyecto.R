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

datos <- rio::import("salud_fetos.csv")

str(datos)
View(datos)

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