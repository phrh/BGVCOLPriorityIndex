library(readxl)
library(dplyr)
library(sets)
library(scales)
library(BBmisc)
library(stringr)
library(Ckmeans.1d.dp)

start_time <- date()


lim_inf <- 1
lim_sup <- 5
output_name <- paste("data_output_SA_1208_",lim_inf,"-",lim_sup,".csv",sep = "")




options(digits=4)
sets_options("universe", seq(0, 100, 0.01))

options(warn=-1)
suppressMessages(library(ggplot2)) 

setwd("C:/Users/ddelgadillo/OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA/Escritorio/Criterios/Indice/Categorias/indiceTodas/varContinuas")


# seleccionCriterios <- read_excel("TablaCriterios5.2.xlsx", sheet = "Hoja1")
# #View(seleccionCriterios)
# seleccionCriterios <- filter(seleccionCriterios,borrar != "Borrar")
# seleccionCriterios <- filter(seleccionCriterios,!is.na(id))
# seleccionCriterios <- filter(seleccionCriterios, borrar == "No")
# 
# 
# temp <- sapply(seleccionCriterios, function(x) sum(is.na(x)))
# countNAs <- data.frame(col = names(seleccionCriterios), count_NA = temp)
# 
# seleccionCriterios$na_count <- apply(seleccionCriterios, 1, function(x) sum(is.na(x)))


# 
# d <- dplyr::select(seleccionCriterios,id, 
#                    categoria,
#                    grupo,
#                    clasificacion_FAO,
#                    nombre_cientifico,
#                    nombre_comun,
#                    reg_mincultura,plan_nacional_sa,
#                    )


dataSA <- read.csv("dataSA-1208.csv", sep=";")



dataSA$num_regiones <- dataSA$reg_mincultura


#num_regiones
dataSA$num_regiones <- as.numeric(dataSA$num_regiones)
dataSA$num_regiones_norm <- normalize(dataSA$num_regiones,method = "range",range = c(1,100))
num_regiones_summ <- fivenum(dataSA$num_regiones_norm, na.rm = TRUE)
dataSA$num_regiones_norm[is.na(dataSA$num_regiones_norm)] <- 0
dataSA$num_regiones_norm <- as.numeric(format(dataSA$num_regiones_norm, digits=3))


dataSA$plan_nacional_sa <- as.character(dataSA$plan_nacional_sa)
dataSA$plan_nacional <- dataSA$plan_nacional_sa
dataSA$plan_nacional[dataSA$plan_nacional_sa == "X"] <- 60
dataSA$plan_nacional[dataSA$plan_nacional == "x"] <- 60
dataSA$plan_nacional <- as.numeric(format(dataSA$plan_nacional, digits=3))

dataSA$plan_nacional[is.na(dataSA$plan_nacional)] <- 20

dataSA$plan_nacional_et[dataSA$plan_nacional == 20] <- 'No'
dataSA$plan_nacional_et[dataSA$plan_nacional == 60] <- 'Si'


dataSA$aporte_nutrientes <- as.numeric(format(dataSA$aporte_nutrientes, digits=3))
dataSA$precio_nutrientes <- as.numeric(format(dataSA$precio_nutrientes, digits=3))

variables_sa <- set(
  
  plan_nacional = fuzzy_partition(varnames = c(Yes = 60, 
                                               No = 20), sd = 0.01),
  
  reg_mincultura = fuzzy_variable(Undetermined = fuzzy_trapezoid(corners = c(-0.1,0.1,0.1,0.11), height = 1),
                                  Low = fuzzy_trapezoid(corners = c(0.11,
                                                                    1,
                                                                    num_regiones_summ[2]*1.1,
                                                                    num_regiones_summ[2]*1.25), 
                                                        height = 1),
                                  Medium = fuzzy_trapezoid(corners = c(num_regiones_summ[2]*1.05,
                                                                       num_regiones_summ[3]*1.1,
                                                                       num_regiones_summ[3]*1.15,
                                                                       num_regiones_summ[3]*1.35),
                                                           height = 1),
                                  High = fuzzy_trapezoid(corners = c(num_regiones_summ[3]*1.3,
                                                                     num_regiones_summ[4]*0.9,
                                                                     105,
                                                                     110),
                                                         height = 1)
                                  
  ),  
  aporte_nutrientes = fuzzy_variable(Undetermined = fuzzy_trapezoid(corners = c(0,0.01,9.99,12), height = 1),
                                     Low = fuzzy_trapezoid(corners = c(12,26,30,45), height = 1),
                                     Medium = fuzzy_trapezoid(corners = c(30,45,50,65), height = 1),
                                     High = fuzzy_trapezoid(corners = c(50,65,105,110), height = 1)),  
  
  precio_nutrientes = fuzzy_variable(Undetermined = fuzzy_trapezoid(corners = c(0,0.01,9.99,12), height = 1),
                                     Low = fuzzy_trapezoid(corners = c(12,27,30,45), height = 1),
                                     Medium = fuzzy_trapezoid(corners = c(30,45,50,65), height = 1),
                                     High = fuzzy_trapezoid(corners = c(50,65,105,110), height = 1)),
  
  
  
   seg_alimentaria = fuzzy_variable(Undetermined = fuzzy_trapezoid(corners = c(0,0.01,9.99,10), height = 1),
                                   Low = fuzzy_trapezoid(corners = c(10,25,30,45), height = 1),
                                   Medium = fuzzy_trapezoid(corners = c(30,45,50,65), height = 1),
                                   High = fuzzy_trapezoid(corners = c(50,65,70,85), height = 1))
  
  
)

reglas_sa <- set(
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Undetermined && aporte_nutrientes %is% Undetermined && precio_nutrientes %is% Undetermined, seg_alimentaria %is% Undetermined),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Undetermined && aporte_nutrientes %is% Undetermined && precio_nutrientes %is% Low, seg_alimentaria %is% Low),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Undetermined && aporte_nutrientes %is% Undetermined && precio_nutrientes %is% Medium, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Undetermined && aporte_nutrientes %is% Undetermined && precio_nutrientes %is% High, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Undetermined && aporte_nutrientes %is% Low && precio_nutrientes %is% Undetermined, seg_alimentaria %is% Low),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Undetermined && aporte_nutrientes %is% Low && precio_nutrientes %is% Low, seg_alimentaria %is% Low),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Undetermined && aporte_nutrientes %is% Low && precio_nutrientes %is% Medium, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Undetermined && aporte_nutrientes %is% Low && precio_nutrientes %is% High, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Undetermined && aporte_nutrientes %is% Medium && precio_nutrientes %is% Undetermined, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Undetermined && aporte_nutrientes %is% Medium && precio_nutrientes %is% Low, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Undetermined && aporte_nutrientes %is% Medium && precio_nutrientes %is% Medium, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Undetermined && aporte_nutrientes %is% Medium && precio_nutrientes %is% High, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Undetermined && aporte_nutrientes %is% High && precio_nutrientes %is% Undetermined, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Undetermined && aporte_nutrientes %is% High && precio_nutrientes %is% Low, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Undetermined && aporte_nutrientes %is% High && precio_nutrientes %is% Medium, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Undetermined && aporte_nutrientes %is% High && precio_nutrientes %is% High, seg_alimentaria %is% High),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Low && aporte_nutrientes %is% Undetermined && precio_nutrientes %is% Undetermined, seg_alimentaria %is% Low),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Low && aporte_nutrientes %is% Undetermined && precio_nutrientes %is% Low, seg_alimentaria %is% Low),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Low && aporte_nutrientes %is% Undetermined && precio_nutrientes %is% Medium, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Low && aporte_nutrientes %is% Undetermined && precio_nutrientes %is% High, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Low && aporte_nutrientes %is% Low && precio_nutrientes %is% Undetermined, seg_alimentaria %is% Low),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Low && aporte_nutrientes %is% Low && precio_nutrientes %is% Low, seg_alimentaria %is% Low),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Low && aporte_nutrientes %is% Low && precio_nutrientes %is% Medium, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Low && aporte_nutrientes %is% Low && precio_nutrientes %is% High, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Low && aporte_nutrientes %is% Medium && precio_nutrientes %is% Undetermined, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Low && aporte_nutrientes %is% Medium && precio_nutrientes %is% Low, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Low && aporte_nutrientes %is% Medium && precio_nutrientes %is% Medium, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Low && aporte_nutrientes %is% Medium && precio_nutrientes %is% High, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Low && aporte_nutrientes %is% High && precio_nutrientes %is% Undetermined, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Low && aporte_nutrientes %is% High && precio_nutrientes %is% Low, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Low && aporte_nutrientes %is% High && precio_nutrientes %is% Medium, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Low && aporte_nutrientes %is% High && precio_nutrientes %is% High, seg_alimentaria %is% High),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Medium && aporte_nutrientes %is% Undetermined && precio_nutrientes %is% Undetermined, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Medium && aporte_nutrientes %is% Undetermined && precio_nutrientes %is% Low, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Medium && aporte_nutrientes %is% Undetermined && precio_nutrientes %is% Medium, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Medium && aporte_nutrientes %is% Undetermined && precio_nutrientes %is% High, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Medium && aporte_nutrientes %is% Low && precio_nutrientes %is% Undetermined, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Medium && aporte_nutrientes %is% Low && precio_nutrientes %is% Low, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Medium && aporte_nutrientes %is% Low && precio_nutrientes %is% Medium, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Medium && aporte_nutrientes %is% Low && precio_nutrientes %is% High, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Medium && aporte_nutrientes %is% Medium && precio_nutrientes %is% Undetermined, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Medium && aporte_nutrientes %is% Medium && precio_nutrientes %is% Low, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Medium && aporte_nutrientes %is% Medium && precio_nutrientes %is% Medium, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Medium && aporte_nutrientes %is% Medium && precio_nutrientes %is% High, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Medium && aporte_nutrientes %is% High && precio_nutrientes %is% Undetermined, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Medium && aporte_nutrientes %is% High && precio_nutrientes %is% Low, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Medium && aporte_nutrientes %is% High && precio_nutrientes %is% Medium, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% Medium && aporte_nutrientes %is% High && precio_nutrientes %is% High, seg_alimentaria %is% High),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% High && aporte_nutrientes %is% Undetermined && precio_nutrientes %is% Undetermined, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% High && aporte_nutrientes %is% Undetermined && precio_nutrientes %is% Low, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% High && aporte_nutrientes %is% Undetermined && precio_nutrientes %is% Medium, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% High && aporte_nutrientes %is% Undetermined && precio_nutrientes %is% High, seg_alimentaria %is% High),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% High && aporte_nutrientes %is% Low && precio_nutrientes %is% Undetermined, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% High && aporte_nutrientes %is% Low && precio_nutrientes %is% Low, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% High && aporte_nutrientes %is% Low && precio_nutrientes %is% Medium, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% High && aporte_nutrientes %is% Low && precio_nutrientes %is% High, seg_alimentaria %is% High),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% High && aporte_nutrientes %is% Medium && precio_nutrientes %is% Undetermined, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% High && aporte_nutrientes %is% Medium && precio_nutrientes %is% Low, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% High && aporte_nutrientes %is% Medium && precio_nutrientes %is% Medium, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% High && aporte_nutrientes %is% Medium && precio_nutrientes %is% High, seg_alimentaria %is% High),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% High && aporte_nutrientes %is% High && precio_nutrientes %is% Undetermined, seg_alimentaria %is% High),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% High && aporte_nutrientes %is% High && precio_nutrientes %is% Low, seg_alimentaria %is% High),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% High && aporte_nutrientes %is% High && precio_nutrientes %is% Medium, seg_alimentaria %is% High),
  fuzzy_rule(plan_nacional %is% Yes && reg_mincultura %is% High && aporte_nutrientes %is% High && precio_nutrientes %is% High, seg_alimentaria %is% High),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Undetermined && aporte_nutrientes %is% Undetermined && precio_nutrientes %is% Undetermined, seg_alimentaria %is% Undetermined),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Undetermined && aporte_nutrientes %is% Undetermined && precio_nutrientes %is% Low, seg_alimentaria %is% Low),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Undetermined && aporte_nutrientes %is% Undetermined && precio_nutrientes %is% Medium, seg_alimentaria %is% Low),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Undetermined && aporte_nutrientes %is% Undetermined && precio_nutrientes %is% High, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Undetermined && aporte_nutrientes %is% Low && precio_nutrientes %is% Undetermined, seg_alimentaria %is% Low),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Undetermined && aporte_nutrientes %is% Low && precio_nutrientes %is% Low, seg_alimentaria %is% Low),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Undetermined && aporte_nutrientes %is% Low && precio_nutrientes %is% Medium, seg_alimentaria %is% Low),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Undetermined && aporte_nutrientes %is% Low && precio_nutrientes %is% High, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Undetermined && aporte_nutrientes %is% Medium && precio_nutrientes %is% Undetermined, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Undetermined && aporte_nutrientes %is% Medium && precio_nutrientes %is% Low, seg_alimentaria %is% Low),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Undetermined && aporte_nutrientes %is% Medium && precio_nutrientes %is% Medium, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Undetermined && aporte_nutrientes %is% Medium && precio_nutrientes %is% High, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Undetermined && aporte_nutrientes %is% High && precio_nutrientes %is% Undetermined, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Undetermined && aporte_nutrientes %is% High && precio_nutrientes %is% Low, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Undetermined && aporte_nutrientes %is% High && precio_nutrientes %is% Medium, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Undetermined && aporte_nutrientes %is% High && precio_nutrientes %is% High, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Low && aporte_nutrientes %is% Undetermined && precio_nutrientes %is% Undetermined, seg_alimentaria %is% Low),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Low && aporte_nutrientes %is% Undetermined && precio_nutrientes %is% Low, seg_alimentaria %is% Low),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Low && aporte_nutrientes %is% Undetermined && precio_nutrientes %is% Medium, seg_alimentaria %is% Low),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Low && aporte_nutrientes %is% Undetermined && precio_nutrientes %is% High, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Low && aporte_nutrientes %is% Low && precio_nutrientes %is% Undetermined, seg_alimentaria %is% Low),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Low && aporte_nutrientes %is% Low && precio_nutrientes %is% Low, seg_alimentaria %is% Low),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Low && aporte_nutrientes %is% Low && precio_nutrientes %is% Medium, seg_alimentaria %is% Low),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Low && aporte_nutrientes %is% Low && precio_nutrientes %is% High, seg_alimentaria %is% Low),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Low && aporte_nutrientes %is% Medium && precio_nutrientes %is% Undetermined, seg_alimentaria %is% Low),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Low && aporte_nutrientes %is% Medium && precio_nutrientes %is% Low, seg_alimentaria %is% Low),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Low && aporte_nutrientes %is% Medium && precio_nutrientes %is% Medium, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Low && aporte_nutrientes %is% Medium && precio_nutrientes %is% High, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Low && aporte_nutrientes %is% High && precio_nutrientes %is% Undetermined, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Low && aporte_nutrientes %is% High && precio_nutrientes %is% Low, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Low && aporte_nutrientes %is% High && precio_nutrientes %is% Medium, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Low && aporte_nutrientes %is% High && precio_nutrientes %is% High, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Medium && aporte_nutrientes %is% Undetermined && precio_nutrientes %is% Undetermined, seg_alimentaria %is% Low),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Medium && aporte_nutrientes %is% Undetermined && precio_nutrientes %is% Low, seg_alimentaria %is% Low),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Medium && aporte_nutrientes %is% Undetermined && precio_nutrientes %is% Medium, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Medium && aporte_nutrientes %is% Undetermined && precio_nutrientes %is% High, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Medium && aporte_nutrientes %is% Low && precio_nutrientes %is% Undetermined, seg_alimentaria %is% Low),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Medium && aporte_nutrientes %is% Low && precio_nutrientes %is% Low, seg_alimentaria %is% Low),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Medium && aporte_nutrientes %is% Low && precio_nutrientes %is% Medium, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Medium && aporte_nutrientes %is% Low && precio_nutrientes %is% High, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Medium && aporte_nutrientes %is% Medium && precio_nutrientes %is% Undetermined, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Medium && aporte_nutrientes %is% Medium && precio_nutrientes %is% Low, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Medium && aporte_nutrientes %is% Medium && precio_nutrientes %is% Medium, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Medium && aporte_nutrientes %is% Medium && precio_nutrientes %is% High, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Medium && aporte_nutrientes %is% High && precio_nutrientes %is% Undetermined, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Medium && aporte_nutrientes %is% High && precio_nutrientes %is% Low, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Medium && aporte_nutrientes %is% High && precio_nutrientes %is% Medium, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% Medium && aporte_nutrientes %is% High && precio_nutrientes %is% High, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% High && aporte_nutrientes %is% Undetermined && precio_nutrientes %is% Undetermined, seg_alimentaria %is% Low),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% High && aporte_nutrientes %is% Undetermined && precio_nutrientes %is% Low, seg_alimentaria %is% Low),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% High && aporte_nutrientes %is% Undetermined && precio_nutrientes %is% Medium, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% High && aporte_nutrientes %is% Undetermined && precio_nutrientes %is% High, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% High && aporte_nutrientes %is% Low && precio_nutrientes %is% Undetermined, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% High && aporte_nutrientes %is% Low && precio_nutrientes %is% Low, seg_alimentaria %is% Low),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% High && aporte_nutrientes %is% Low && precio_nutrientes %is% Medium, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% High && aporte_nutrientes %is% Low && precio_nutrientes %is% High, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% High && aporte_nutrientes %is% Medium && precio_nutrientes %is% Undetermined, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% High && aporte_nutrientes %is% Medium && precio_nutrientes %is% Low, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% High && aporte_nutrientes %is% Medium && precio_nutrientes %is% Medium, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% High && aporte_nutrientes %is% Medium && precio_nutrientes %is% High, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% High && aporte_nutrientes %is% High && precio_nutrientes %is% Undetermined, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% High && aporte_nutrientes %is% High && precio_nutrientes %is% Low, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% High && aporte_nutrientes %is% High && precio_nutrientes %is% Medium, seg_alimentaria %is% Medium),
  fuzzy_rule(plan_nacional %is% No && reg_mincultura %is% High && aporte_nutrientes %is% High && precio_nutrientes %is% High, seg_alimentaria %is% High)
  
)

modelF_sa <- fuzzy_system(variables_sa, reglas_sa)

print(modelF_sa)
par(mar = rep(2,4))
plot(modelF_sa)



dataSA$rownm <- rownames(dataSA)
dataSA$rownm <- as.numeric(dataSA$rownm)

#dataSA <- dplyr::filter(dataSA, rownm >=lim_inf & rownm <= lim_sup)
dataSA <- dplyr::filter(dataSA, id==20)

dataSA$rownums <- rownames(dataSA)
dataSA$rownums <- as.numeric(dataSA$rownums)

#print(output_name)
View(dataSA)


fuzz.4 <- function(y){
  print(paste(dataSA[y,'id'],"..."))
  print(paste("fuzzy Seguridad alimentaria ",dataSA[y,'nombre_cientifico']))
  print(paste0('plan_nacional = ', dataSA[y,'plan_nacional']))
  print(paste0('reg_mincultura = ', dataSA[y,'num_regiones_norm']))
  print(paste0('aporte_nutrientes = ', dataSA[y,'aporte_nutrientes']))
  print(paste0('precio_nutrientes = ', dataSA[y,'precio_nutrientes']))
  
  fuzzify <- sets::fuzzy_inference(modelF_sa, list(plan_nacional = dataSA[y,'plan_nacional'],
                                                   reg_mincultura = dataSA[y,'num_regiones_norm'],
                                                   aporte_nutrientes = dataSA[y,'aporte_nutrientes'],
                                                   precio_nutrientes = dataSA[y,'precio_nutrientes']))
  print(gset_defuzzify(fuzzify, "largestofmax"))
  gset_defuzzify(fuzzify, "largestofmax")
  
}



dataSA$seg_alimentaria <- sapply(dataSA$rownums, fuzz.4) 
dataSA$seg_alimentaria <-  as.numeric(format(dataSA$seg_alimentaria, digits=5))





write.csv2(dataSA,output_name,sep = ";")

# fuzzify <- sets::fuzzy_inference(modelF_sa, list(plan_nacional = 20,
#                                                  reg_mincultura = 29,
#                                                  aporte_nutrientes = 10,
#                                                  precio_nutrientes = 62))
