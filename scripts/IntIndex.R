library(readxl)
library(dplyr)
library(sets)
library(ggplot2)
library(plotly)
library(scales)
library(BBmisc)
library(stringr)
library(Ckmeans.1d.dp)
#library(mailR)


lim_inf <- 1
lim_sup <- 3
output_name <- paste("data_output_INT_banco_1608_",lim_inf,"-",lim_sup,".csv",sep = "")



options(digits=4)
sets_options("universe", seq(0, 100, 0.01))

options(warn=-1)
suppressMessages(library(ggplot2)) 

setwd("C:/Users/ddelgadillo/OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA/Escritorio/Criterios/Indice/Categorias/indiceTodas/varContinuas")

data_indice <- read.csv2("data_int_banco_1608.csv",sep = ",",header = TRUE,dec = ".")
#head(data_indice)
data_indice$id <- as.numeric(data_indice$id)
data_indice$origen <- as.numeric(format(data_indice$origen, digits=4))
data_indice$vulnerabilidad <- as.numeric(format(data_indice$vulnerabilidad, digits=4))
data_indice$imp_economica <- as.numeric(format(data_indice$imp_economica, digits=4))
data_indice$seg_alimentaria <- as.numeric(format(data_indice$seg_alimentaria, digits=4))

data_indice <- data_indice[order(data_indice$id),]

rownames(data_indice) <- 1:nrow(data_indice)

variables <- set(
  
  origen = fuzzy_partition(varnames = c(Bajo = 20,
                                        Medio = 45,
                                        Alto = 70),sd = 0.01),
  
  
  imp_economica = fuzzy_variable(Incierto = fuzzy_trapezoid(corners = c(0,0.01,9.99,11), height = 1),
                                 Bajo = fuzzy_trapezoid(corners = c(11,25,30,45), height = 1),
                                 Medio = fuzzy_trapezoid(corners = c(30,45,50,65), height = 1),
                                 Alto = fuzzy_trapezoid(corners = c(50,65,70,85), height = 1)),
  
  
  vulnerabilidad = fuzzy_partition(varnames = c(Incierto = 0,
                                                No = 35,
                                                Si = 75), sd = 0.01),
  
  seg_alimentaria = fuzzy_variable(Incierto = fuzzy_trapezoid(corners = c(0,0.01,9.99,10), height = 1),
                                   Bajo = fuzzy_trapezoid(corners = c(10,25,30,45), height = 1),
                                   Medio = fuzzy_trapezoid(corners = c(30,45,50,65), height = 1),
                                   Alto = fuzzy_trapezoid(corners = c(50,65,70,85), height = 1)),
  
  
  
  int_banco = fuzzy_partition(varnames = c(Bajo = 15, 
                                           Medio = 55, 
                                           Alto = 90), sd = 12)
  
)

reglas <- set(
  fuzzy_rule(origen %is% Alto && imp_economica %is% Alto && vulnerabilidad %is% Si && seg_alimentaria %is% Alto, int_banco %is% Alto),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Alto && vulnerabilidad %is% Si && seg_alimentaria %is% Medio, int_banco %is% Alto),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Alto && vulnerabilidad %is% Si && seg_alimentaria %is% Bajo, int_banco %is% Alto),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Alto && vulnerabilidad %is% Si && seg_alimentaria %is% Incierto, int_banco %is% Alto),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Alto && vulnerabilidad %is% No && seg_alimentaria %is% Alto, int_banco %is% Alto),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Alto && vulnerabilidad %is% No && seg_alimentaria %is% Medio, int_banco %is% Medio),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Alto && vulnerabilidad %is% No && seg_alimentaria %is% Bajo, int_banco %is% Medio),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Alto && vulnerabilidad %is% No && seg_alimentaria %is% Incierto, int_banco %is% Medio),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Alto && vulnerabilidad %is% Incierto && seg_alimentaria %is% Alto, int_banco %is% Alto),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Alto && vulnerabilidad %is% Incierto && seg_alimentaria %is% Medio, int_banco %is% Medio),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Alto && vulnerabilidad %is% Incierto && seg_alimentaria %is% Bajo, int_banco %is% Medio),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Alto && vulnerabilidad %is% Incierto && seg_alimentaria %is% Incierto, int_banco %is% Medio),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Medio && vulnerabilidad %is% Si && seg_alimentaria %is% Alto, int_banco %is% Alto),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Medio && vulnerabilidad %is% Si && seg_alimentaria %is% Medio, int_banco %is% Medio),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Medio && vulnerabilidad %is% Si && seg_alimentaria %is% Bajo, int_banco %is% Medio),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Medio && vulnerabilidad %is% Si && seg_alimentaria %is% Incierto, int_banco %is% Medio),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Medio && vulnerabilidad %is% No && seg_alimentaria %is% Alto, int_banco %is% Medio),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Medio && vulnerabilidad %is% No && seg_alimentaria %is% Medio, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Medio && vulnerabilidad %is% No && seg_alimentaria %is% Bajo, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Medio && vulnerabilidad %is% No && seg_alimentaria %is% Incierto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Medio && vulnerabilidad %is% Incierto && seg_alimentaria %is% Alto, int_banco %is% Medio),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Medio && vulnerabilidad %is% Incierto && seg_alimentaria %is% Medio, int_banco %is% Medio),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Medio && vulnerabilidad %is% Incierto && seg_alimentaria %is% Bajo, int_banco %is% Medio),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Medio && vulnerabilidad %is% Incierto && seg_alimentaria %is% Incierto, int_banco %is% Medio),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Bajo && vulnerabilidad %is% Si && seg_alimentaria %is% Alto, int_banco %is% Alto),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Bajo && vulnerabilidad %is% Si && seg_alimentaria %is% Medio, int_banco %is% Medio),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Bajo && vulnerabilidad %is% Si && seg_alimentaria %is% Bajo, int_banco %is% Medio),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Bajo && vulnerabilidad %is% Si && seg_alimentaria %is% Incierto, int_banco %is% Medio),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Bajo && vulnerabilidad %is% No && seg_alimentaria %is% Alto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Bajo && vulnerabilidad %is% No && seg_alimentaria %is% Medio, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Bajo && vulnerabilidad %is% No && seg_alimentaria %is% Bajo, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Bajo && vulnerabilidad %is% No && seg_alimentaria %is% Incierto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Bajo && vulnerabilidad %is% Incierto && seg_alimentaria %is% Alto, int_banco %is% Medio),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Bajo && vulnerabilidad %is% Incierto && seg_alimentaria %is% Medio, int_banco %is% Medio),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Bajo && vulnerabilidad %is% Incierto && seg_alimentaria %is% Bajo, int_banco %is% Medio),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Bajo && vulnerabilidad %is% Incierto && seg_alimentaria %is% Incierto, int_banco %is% Medio),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Incierto && vulnerabilidad %is% Si && seg_alimentaria %is% Alto, int_banco %is% Alto),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Incierto && vulnerabilidad %is% Si && seg_alimentaria %is% Medio, int_banco %is% Medio),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Incierto && vulnerabilidad %is% Si && seg_alimentaria %is% Bajo, int_banco %is% Medio),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Incierto && vulnerabilidad %is% Si && seg_alimentaria %is% Incierto, int_banco %is% Medio),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Incierto && vulnerabilidad %is% No && seg_alimentaria %is% Alto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Incierto && vulnerabilidad %is% No && seg_alimentaria %is% Medio, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Incierto && vulnerabilidad %is% No && seg_alimentaria %is% Bajo, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Incierto && vulnerabilidad %is% No && seg_alimentaria %is% Incierto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Incierto && vulnerabilidad %is% Incierto && seg_alimentaria %is% Alto, int_banco %is% Medio),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Incierto && vulnerabilidad %is% Incierto && seg_alimentaria %is% Medio, int_banco %is% Medio),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Incierto && vulnerabilidad %is% Incierto && seg_alimentaria %is% Bajo, int_banco %is% Medio),
  fuzzy_rule(origen %is% Alto && imp_economica %is% Incierto && vulnerabilidad %is% Incierto && seg_alimentaria %is% Incierto, int_banco %is% Medio),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Alto && vulnerabilidad %is% Si && seg_alimentaria %is% Alto, int_banco %is% Alto),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Alto && vulnerabilidad %is% Si && seg_alimentaria %is% Medio, int_banco %is% Medio),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Alto && vulnerabilidad %is% Si && seg_alimentaria %is% Bajo, int_banco %is% Medio),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Alto && vulnerabilidad %is% Si && seg_alimentaria %is% Incierto, int_banco %is% Medio),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Alto && vulnerabilidad %is% No && seg_alimentaria %is% Alto, int_banco %is% Medio),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Alto && vulnerabilidad %is% No && seg_alimentaria %is% Medio, int_banco %is% Medio),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Alto && vulnerabilidad %is% No && seg_alimentaria %is% Bajo, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Alto && vulnerabilidad %is% No && seg_alimentaria %is% Incierto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Alto && vulnerabilidad %is% Incierto && seg_alimentaria %is% Alto, int_banco %is% Medio),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Alto && vulnerabilidad %is% Incierto && seg_alimentaria %is% Medio, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Alto && vulnerabilidad %is% Incierto && seg_alimentaria %is% Bajo, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Alto && vulnerabilidad %is% Incierto && seg_alimentaria %is% Incierto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Medio && vulnerabilidad %is% Si && seg_alimentaria %is% Alto, int_banco %is% Medio),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Medio && vulnerabilidad %is% Si && seg_alimentaria %is% Medio, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Medio && vulnerabilidad %is% Si && seg_alimentaria %is% Bajo, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Medio && vulnerabilidad %is% Si && seg_alimentaria %is% Incierto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Medio && vulnerabilidad %is% No && seg_alimentaria %is% Alto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Medio && vulnerabilidad %is% No && seg_alimentaria %is% Medio, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Medio && vulnerabilidad %is% No && seg_alimentaria %is% Bajo, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Medio && vulnerabilidad %is% No && seg_alimentaria %is% Incierto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Medio && vulnerabilidad %is% Incierto && seg_alimentaria %is% Alto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Medio && vulnerabilidad %is% Incierto && seg_alimentaria %is% Medio, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Medio && vulnerabilidad %is% Incierto && seg_alimentaria %is% Bajo, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Medio && vulnerabilidad %is% Incierto && seg_alimentaria %is% Incierto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Bajo && vulnerabilidad %is% Si && seg_alimentaria %is% Alto, int_banco %is% Medio),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Bajo && vulnerabilidad %is% Si && seg_alimentaria %is% Medio, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Bajo && vulnerabilidad %is% Si && seg_alimentaria %is% Bajo, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Bajo && vulnerabilidad %is% Si && seg_alimentaria %is% Incierto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Bajo && vulnerabilidad %is% No && seg_alimentaria %is% Alto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Bajo && vulnerabilidad %is% No && seg_alimentaria %is% Medio, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Bajo && vulnerabilidad %is% No && seg_alimentaria %is% Bajo, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Bajo && vulnerabilidad %is% No && seg_alimentaria %is% Incierto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Bajo && vulnerabilidad %is% Incierto && seg_alimentaria %is% Alto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Bajo && vulnerabilidad %is% Incierto && seg_alimentaria %is% Medio, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Bajo && vulnerabilidad %is% Incierto && seg_alimentaria %is% Bajo, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Bajo && vulnerabilidad %is% Incierto && seg_alimentaria %is% Incierto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Incierto && vulnerabilidad %is% Si && seg_alimentaria %is% Alto, int_banco %is% Medio),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Incierto && vulnerabilidad %is% Si && seg_alimentaria %is% Medio, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Incierto && vulnerabilidad %is% Si && seg_alimentaria %is% Bajo, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Incierto && vulnerabilidad %is% Si && seg_alimentaria %is% Incierto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Incierto && vulnerabilidad %is% No && seg_alimentaria %is% Alto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Incierto && vulnerabilidad %is% No && seg_alimentaria %is% Medio, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Incierto && vulnerabilidad %is% No && seg_alimentaria %is% Bajo, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Incierto && vulnerabilidad %is% No && seg_alimentaria %is% Incierto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Incierto && vulnerabilidad %is% Incierto && seg_alimentaria %is% Alto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Incierto && vulnerabilidad %is% Incierto && seg_alimentaria %is% Medio, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Incierto && vulnerabilidad %is% Incierto && seg_alimentaria %is% Bajo, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Medio && imp_economica %is% Incierto && vulnerabilidad %is% Incierto && seg_alimentaria %is% Incierto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Alto && vulnerabilidad %is% Si && seg_alimentaria %is% Alto, int_banco %is% Alto),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Alto && vulnerabilidad %is% Si && seg_alimentaria %is% Medio, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Alto && vulnerabilidad %is% Si && seg_alimentaria %is% Bajo, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Alto && vulnerabilidad %is% Si && seg_alimentaria %is% Incierto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Alto && vulnerabilidad %is% No && seg_alimentaria %is% Alto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Alto && vulnerabilidad %is% No && seg_alimentaria %is% Medio, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Alto && vulnerabilidad %is% No && seg_alimentaria %is% Bajo, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Alto && vulnerabilidad %is% No && seg_alimentaria %is% Incierto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Alto && vulnerabilidad %is% Incierto && seg_alimentaria %is% Alto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Alto && vulnerabilidad %is% Incierto && seg_alimentaria %is% Medio, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Alto && vulnerabilidad %is% Incierto && seg_alimentaria %is% Bajo, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Alto && vulnerabilidad %is% Incierto && seg_alimentaria %is% Incierto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Medio && vulnerabilidad %is% Si && seg_alimentaria %is% Alto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Medio && vulnerabilidad %is% Si && seg_alimentaria %is% Medio, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Medio && vulnerabilidad %is% Si && seg_alimentaria %is% Bajo, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Medio && vulnerabilidad %is% Si && seg_alimentaria %is% Incierto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Medio && vulnerabilidad %is% No && seg_alimentaria %is% Alto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Medio && vulnerabilidad %is% No && seg_alimentaria %is% Medio, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Medio && vulnerabilidad %is% No && seg_alimentaria %is% Bajo, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Medio && vulnerabilidad %is% No && seg_alimentaria %is% Incierto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Medio && vulnerabilidad %is% Incierto && seg_alimentaria %is% Alto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Medio && vulnerabilidad %is% Incierto && seg_alimentaria %is% Medio, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Medio && vulnerabilidad %is% Incierto && seg_alimentaria %is% Bajo, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Medio && vulnerabilidad %is% Incierto && seg_alimentaria %is% Incierto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Bajo && vulnerabilidad %is% Si && seg_alimentaria %is% Alto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Bajo && vulnerabilidad %is% Si && seg_alimentaria %is% Medio, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Bajo && vulnerabilidad %is% Si && seg_alimentaria %is% Bajo, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Bajo && vulnerabilidad %is% Si && seg_alimentaria %is% Incierto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Bajo && vulnerabilidad %is% No && seg_alimentaria %is% Alto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Bajo && vulnerabilidad %is% No && seg_alimentaria %is% Medio, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Bajo && vulnerabilidad %is% No && seg_alimentaria %is% Bajo, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Bajo && vulnerabilidad %is% No && seg_alimentaria %is% Incierto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Bajo && vulnerabilidad %is% Incierto && seg_alimentaria %is% Alto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Bajo && vulnerabilidad %is% Incierto && seg_alimentaria %is% Medio, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Bajo && vulnerabilidad %is% Incierto && seg_alimentaria %is% Bajo, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Bajo && vulnerabilidad %is% Incierto && seg_alimentaria %is% Incierto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Incierto && vulnerabilidad %is% Si && seg_alimentaria %is% Alto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Incierto && vulnerabilidad %is% Si && seg_alimentaria %is% Medio, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Incierto && vulnerabilidad %is% Si && seg_alimentaria %is% Bajo, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Incierto && vulnerabilidad %is% Si && seg_alimentaria %is% Incierto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Incierto && vulnerabilidad %is% No && seg_alimentaria %is% Alto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Incierto && vulnerabilidad %is% No && seg_alimentaria %is% Medio, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Incierto && vulnerabilidad %is% No && seg_alimentaria %is% Bajo, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Incierto && vulnerabilidad %is% No && seg_alimentaria %is% Incierto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Incierto && vulnerabilidad %is% Incierto && seg_alimentaria %is% Alto, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Incierto && vulnerabilidad %is% Incierto && seg_alimentaria %is% Medio, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Incierto && vulnerabilidad %is% Incierto && seg_alimentaria %is% Bajo, int_banco %is% Bajo),
  fuzzy_rule(origen %is% Bajo && imp_economica %is% Incierto && vulnerabilidad %is% Incierto && seg_alimentaria %is% Incierto, int_banco %is% Bajo)
  
)

modelF <- fuzzy_system(variables, reglas)

print(modelF)
par(mar = rep(2,4))
plot(modelF)

data_indice$rownums <- rownames(data_indice)
data_indice$rownums <- as.numeric(data_indice$rownums)


data_indice <- dplyr::filter(data_indice, rownums >=lim_inf & rownums <= lim_sup)



data_indice$rownm <- rownames(data_indice)
data_indice$rownm <- as.numeric(data_indice$rownm)




fuzz.7 <- function(y){
  print(paste("fuzzy Int Banco ",data_indice[y,'nombre_cientifico']))
  print(paste0('origen = ',data_indice[y,'origen']))
  print(paste0('imp_economica = ',data_indice[y,'imp_economica']))
  print(paste0('vulnerabilidad = ',data_indice[y,'vulnerabilidad']))
  print(paste0('seg_alimentaria = ',data_indice[y,'seg_alimentaria']))
  
  fuzzify <- sets::fuzzy_inference(modelF, list(origen = data_indice[y,'origen'],
                                                imp_economica = data_indice[y,'imp_economica'],
                                                vulnerabilidad = data_indice[y,'vulnerabilidad'],
                                                seg_alimentaria = data_indice[y,'seg_alimentaria']))
  print(gset_defuzzify(fuzzify, "largestofmax"))
  gset_defuzzify(fuzzify, "largestofmax")
  
}

data_indice$int_banco <- sapply(data_indice$rownm, fuzz.7) 
data_indice$int_banco <-  as.numeric(format(data_indice$int_banco, digits=5))





write.csv2(data_indice,output_name,sep = ",", row.names = FALSE,)
