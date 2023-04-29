getwd()

cultura <- foreign::read.spss("cohesion-acm/1_input/BBDD ENPC2017 04_2018_PUB.sav", reencode="utf-8", to.data.frame=TRUE)

library(FactoMineR)
library(tidyverse)
library(factoextra)

attach(cultura)

cultura.active <- cultura[1:5000, 1:12]

summary(cultura.active) [, 1:6] 

res_mca <- MCA(cultura.active, ncp = 5, graph = TRUE)
