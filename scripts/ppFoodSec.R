library(readxl)
library(dplyr)
library(sets)
library(ggplot2)
library(plotly)
library(scales)
library(BBmisc)
library(stringr)
library(Ckmeans.1d.dp)

start_time <- date()


lim_inf <- 2
lim_sup <- 8
output_name <- paste("data/data_foodsec_bgv.csv",lim_inf,"-",lim_sup,".csv",sep = "")


options(digits=4)
sets_options("universe", seq(0, 100, 0.01))

options(warn=-1)
suppressMessages(library(ggplot2)) 

#setwd("C:/Users/ddelgadillo/OneDrive - AGROSAVIA - CORPORACION COLOMBIANA DE INVESTIGACION AGROPECUARIA/Escritorio/Criterios/Indice/Categorias/indiceTodas/varContinuas")

seleccionCriterios <- read_excel("TablaCriterios5.5.xlsx", sheet = "Hoja1")
#View(seleccionCriterios)
seleccionCriterios <- filter(seleccionCriterios,borrar != "Borrar")
seleccionCriterios <- filter(seleccionCriterios,!is.na(id))
seleccionCriterios <- filter(seleccionCriterios, borrar == "No")


temp <- sapply(seleccionCriterios, function(x) sum(is.na(x)))
countNAs <- data.frame(col = names(seleccionCriterios), count_NA = temp)

seleccionCriterios$na_count <- apply(seleccionCriterios, 1, function(x) sum(is.na(x)))



d <- dplyr::select(seleccionCriterios,id, 
                   categoria,
                   grupo,
                   clasificacion_FAO,
                   cultivo,
                   nombre_cientifico,
                   nombre_comun,
                   reg_mincultura,plan_nacional_sa,
                   Calcio, TipoCalcio,
                   Zinc, TipoZinc,
                   Hierro, TipoHierro,
                   Energia, TipoEnergia
                   )

dfc <- data.frame(id = seleccionCriterios$id,
                  categoria = seleccionCriterios$categoria,
                  grupo = seleccionCriterios$grupo,
                  nombre_cientifico = seleccionCriterios$nombre_cientifico,
                  cultivo = seleccionCriterios$cultivo,
                  Calcio = seleccionCriterios$Calcio, 
                  TipoCalcio = seleccionCriterios$TipoCalcio,
                  Zinc = seleccionCriterios$Zinc,
                  TipoZinc = seleccionCriterios$TipoZinc,
                  Hierro = seleccionCriterios$Hierro,
                  TipoHierro = seleccionCriterios$TipoHierro,
                  Energia = seleccionCriterios$Energia,
                  TipoEnergia = seleccionCriterios$TipoEnergia
                  )

dfc$Calcio[dfc$TipoCalcio == "T3"] <- NA
dfc$TipoCalcio[dfc$TipoCalcio == "T3"] <- NA

dfc$Zinc[dfc$TipoZinc == "T3"] <- NA
dfc$TipoZinc[dfc$TipoZinc == "T3"] <- NA

dfc$Hierro[dfc$TipoHierro == "T3"] <- NA
dfc$TipoHierro[dfc$TipoHierro == "T3"] <- NA

dfc$Hierro[dfc$TipoHierro == "T3"] <- NA
dfc$TipoHierro[dfc$TipoHierro == "T3"] <- NA


dfc$rownm <- rownames(dfc)



tipo_imputacion_calcio <- function(y){
  if(is.na(dfc[y,"TipoCalcio"])){
    3
  } else if(as.character(dfc[y,"TipoCalcio"]) == "T1"){
    1
  } else if(as.character(dfc[y,"TipoCalcio"]) == "T2"){
    2
  } else {
    4
  }
  
}

dfc$imputacion_sa_calcio <- sapply(dfc$rownm, tipo_imputacion_calcio)


tipo_imputacion_zinc <- function(y){
  if(is.na(dfc[y,"TipoZinc"])){
    3
  } else if(as.character(dfc[y,"TipoZinc"]) == "T1"){
    1
  } else if(as.character(dfc[y,"TipoZinc"]) == "T2"){
    2
  } else {
    4
  }
  
}

dfc$imputacion_sa_zinc <- sapply(dfc$rownm, tipo_imputacion_zinc)


tipo_imputacion_hierro <- function(y){
  if(is.na(dfc[y,"TipoHierro"])){
    3
  } else if(as.character(dfc[y,"TipoHierro"]) == "T1"){
    1
  } else if(as.character(dfc[y,"TipoHierro"]) == "T2"){
    2
  } else {
    4
  }
  
}

dfc$imputacion_sa_hierro <- sapply(dfc$rownm, tipo_imputacion_hierro)


tipo_imputacion_energia <- function(y){
  if(is.na(dfc[y,"TipoEnergia"])){
    3
  } else if(as.character(dfc[y,"TipoEnergia"]) == "T1"){
    1
  } else if(as.character(dfc[y,"TipoEnergia"]) == "T2"){
    2
  } else {
    4
  }
  
}

dfc$imputacion_sa_energia <- sapply(dfc$rownm, tipo_imputacion_energia)




dfc$mean_tipo <- rowMeans(dfc[c('imputacion_sa_calcio', 'imputacion_sa_hierro', 'imputacion_sa_zinc', 'imputacion_sa_energia')], na.rm=TRUE)

dfc$sum <- rowSums(dfc[c('imputacion_sa_calcio', 'imputacion_sa_hierro', 'imputacion_sa_zinc', 'imputacion_sa_energia')], na.rm=TRUE)



incertidumbre <- function(y){
  if(dfc[y,"sum"] <= 4){
    "No" 
  } else if(dfc[y,"sum"] > 4 & dfc[y,"sum"] <= 6){
    "Baja"
  } else if(dfc[y,"sum"] > 6 & dfc[y,"sum"] <= 9){
    "Media"
  } else if (dfc[y,"sum"] > 9 ){
    "Alta"
  } else{
    "Alta"
  }
}

dfc$incertidumbre_et <- sapply(dfc$rownm, incertidumbre)

dfc$incertidumbre[dfc$incertidumbre_et == "No"] <- 10
dfc$incertidumbre[dfc$incertidumbre_et == "Baja"] <- 30
dfc$incertidumbre[dfc$incertidumbre_et == "Media"] <- 60
dfc$incertidumbre[dfc$incertidumbre_et == "Alta"] <- 90


d <- dplyr::select(dfc,id, 
                   categoria,
                   grupo,
                   nombre_cientifico,cultivo,
                   Calcio,imputacion_sa_calcio,
                   Hierro,imputacion_sa_hierro, 
                   Zinc,imputacion_sa_zinc,
                   Energia,imputacion_sa_energia,
                   incertidumbre,incertidumbre_et
)



#Calcio
d$calcio <- as.numeric(d$Calcio)
d$calcio_norm <- normalize(d$calcio,method = "range",range = c(1,100))
calcio_summ <- fivenum(d$calcio_norm)
calcio_summ2 <- fivenum(d$calcio_norm,na.rm = TRUE)
d$calcio_norm[is.na(d$calcio_norm)] <- 0

#hierro
d$hierro <- as.numeric(d$Hierro)
d$hierro_norm <- normalize(d$hierro,method = "range",range = c(1,100))
hierro_summ <- fivenum(d$hierro_norm)
d$hierro_norm[is.na(d$hierro_norm)] <- 0

#zinc
d$zinc <- as.numeric(d$Zinc)
d$zinc_norm <- normalize(d$zinc,method = "range",range = c(1,100))
zinc_summ <- fivenum(d$zinc_norm)
d$zinc_norm[is.na(d$zinc_norm)] <- 0

#energia
d$energia <- as.numeric(d$Energia)
d$energia_norm <- normalize(d$energia,method = "range",range = c(1,100))
energia_summ <- fivenum(d$energia_norm)
d$energia_norm[is.na(d$energia_norm)] <- 0

View(d)

var_aporte <- set(
  calcio = fuzzy_variable(Undetermined = fuzzy_bell(center = 0, cross = 0.1, slope = 10, height = 1, chop = 0),
                          Low = fuzzy_trapezoid(corners = c(0.11,
                                                            1,
                                                            calcio_summ[2]*1.1,
                                                            calcio_summ[2]*1.25), 
                                                height = 1),
                          Medium = fuzzy_trapezoid(corners = c(calcio_summ[2]*1.1,
                                                               calcio_summ[3]*0.9,
                                                               calcio_summ[3]*1.1,
                                                               calcio_summ[3]*1.25),
                                                   height = 1),
                          High = fuzzy_trapezoid(corners = c(calcio_summ[3]*1.1,
                                                             calcio_summ[4]*0.9,
                                                             105,
                                                             110),
                                                 height = 1)
                          
  ),
  
  hierro = fuzzy_variable(Undetermined = fuzzy_bell(center = 0, cross = 0.1, slope = 10, height = 1, chop = 0),
                          Low = fuzzy_trapezoid(corners = c(0.11,
                                                            1,
                                                            hierro_summ[2]*1.1,
                                                            hierro_summ[2]*1.25), 
                                                height = 1),
                          Medium = fuzzy_trapezoid(corners = c(hierro_summ[2]*1.1,
                                                               hierro_summ[3]*0.9,
                                                               hierro_summ[3]*1.1,
                                                               hierro_summ[3]*1.25),
                                                   height = 1),
                          High = fuzzy_trapezoid(corners = c(hierro_summ[3]*1.1,
                                                             hierro_summ[4]*0.9,
                                                             105,
                                                             110),
                                                 height = 1)
                          
  ),
  
  zinc = fuzzy_variable(Undetermined = fuzzy_bell(center = 0, cross = 0.1, slope = 10, height = 1, chop = 0),
                        Low = fuzzy_trapezoid(corners = c(0.11,
                                                          1,
                                                          zinc_summ[2]*1.1,
                                                          zinc_summ[2]*1.25), 
                                              height = 1),
                        Medium = fuzzy_trapezoid(corners = c(zinc_summ[2]*1.1,
                                                             zinc_summ[3]*0.9,
                                                             zinc_summ[3]*1.1,
                                                             zinc_summ[3]*1.25),
                                                 height = 1),
                        High = fuzzy_trapezoid(corners = c(zinc_summ[3]*1.1,
                                                           zinc_summ[4]*0.9,
                                                           105,
                                                           110),
                                               height = 1)
                        
  ),
  
  energia = fuzzy_variable(Undetermined = fuzzy_bell(center = 0, cross = 0.1, slope = 10, height = 1, chop = 0),
                           Low = fuzzy_trapezoid(corners = c(0.11,
                                                             1,
                                                             energia_summ[2]*1.1,
                                                             energia_summ[2]*1.25), 
                                                 height = 1),
                           Medium = fuzzy_trapezoid(corners = c(energia_summ[2]*1.1,
                                                                energia_summ[3]*0.9,
                                                                energia_summ[3]*1.1,
                                                                energia_summ[3]*1.25),
                                                    height = 1),
                           High = fuzzy_trapezoid(corners = c(energia_summ[3]*1.1,
                                                              energia_summ[4]*0.9,
                                                              105,
                                                              110),
                                                  height = 1)
                           
  ),
  aporte_nutrientes = fuzzy_variable(Undetermined = fuzzy_trapezoid(corners = c(0,0.01,9.99,10), height = 1),
                                     Low = fuzzy_trapezoid(corners = c(10,26,30,45), height = 1),
                                     Medium = fuzzy_trapezoid(corners = c(30,45,50,65), height = 1),
                                     High = fuzzy_trapezoid(corners = c(50,65,105,110), height = 1))  
  
  
)

reg_aporte <- set(
  fuzzy_rule(calcio %is% Low && hierro %is% Low && zinc %is% Low && energia %is% Undetermined, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Low && hierro %is% Low && zinc %is% Medium && energia %is% Undetermined, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Low && hierro %is% Low && zinc %is% High && energia %is% Undetermined, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Low && hierro %is% Low && zinc %is% Undetermined && energia %is% Undetermined, aporte_nutrientes %is% Undetermined),
  fuzzy_rule(calcio %is% Low && hierro %is% Low && zinc %is% High && energia %is% Medium, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Low && hierro %is% Low && zinc %is% Medium && energia %is% Low, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Low && hierro %is% Low && zinc %is% Medium && energia %is% Medium, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Low && hierro %is% Low && zinc %is% Medium && energia %is% High, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Low && hierro %is% Low && zinc %is% High && energia %is% Low, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Low && hierro %is% Low && zinc %is% Undetermined && energia %is% High, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Low && hierro %is% Low && zinc %is% Low && energia %is% Low, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Low && hierro %is% Low && zinc %is% Low && energia %is% Medium, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Low && hierro %is% Low && zinc %is% Low && energia %is% High, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Low && hierro %is% Low && zinc %is% Undetermined && energia %is% Low, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Low && hierro %is% Low && zinc %is% Undetermined && energia %is% Medium, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Low && hierro %is% Low && zinc %is% High && energia %is% High, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Low && hierro %is% Medium && zinc %is% Low && energia %is% Undetermined, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Low && hierro %is% Medium && zinc %is% Medium && energia %is% Undetermined, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Low && hierro %is% Medium && zinc %is% High && energia %is% Undetermined, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Low && hierro %is% Medium && zinc %is% Undetermined && energia %is% Undetermined, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Low && hierro %is% Medium && zinc %is% Medium && energia %is% Medium, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Low && hierro %is% Medium && zinc %is% Low && energia %is% Medium, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Low && hierro %is% Medium && zinc %is% Low && energia %is% High, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Low && hierro %is% Medium && zinc %is% Medium && energia %is% Low, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Low && hierro %is% Medium && zinc %is% Medium && energia %is% High, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Low && hierro %is% Medium && zinc %is% Undetermined && energia %is% Low, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Low && hierro %is% Medium && zinc %is% Undetermined && energia %is% Medium, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Low && hierro %is% Medium && zinc %is% Undetermined && energia %is% High, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Low && hierro %is% Medium && zinc %is% Low && energia %is% Low, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Low && hierro %is% Medium && zinc %is% High && energia %is% Low, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Low && hierro %is% Medium && zinc %is% High && energia %is% Medium, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% Low && hierro %is% Medium && zinc %is% High && energia %is% High, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% Low && hierro %is% Undetermined && zinc %is% Low && energia %is% Low, aporte_nutrientes %is% Undetermined),
  fuzzy_rule(calcio %is% Low && hierro %is% Undetermined && zinc %is% Low && energia %is% Medium, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Low && hierro %is% Undetermined && zinc %is% Low && energia %is% High, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Low && hierro %is% Undetermined && zinc %is% Medium && energia %is% Low, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Low && hierro %is% Undetermined && zinc %is% Medium && energia %is% Medium, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Low && hierro %is% Undetermined && zinc %is% Medium && energia %is% High, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Low && hierro %is% Undetermined && zinc %is% High && energia %is% Low, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Low && hierro %is% Undetermined && zinc %is% High && energia %is% Medium, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Low && hierro %is% Undetermined && zinc %is% High && energia %is% High, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Low && hierro %is% Undetermined && zinc %is% Undetermined && energia %is% Low, aporte_nutrientes %is% Undetermined),
  fuzzy_rule(calcio %is% Low && hierro %is% Undetermined && zinc %is% Undetermined && energia %is% Medium, aporte_nutrientes %is% Undetermined),
  fuzzy_rule(calcio %is% Low && hierro %is% Undetermined && zinc %is% Undetermined && energia %is% High, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Low && hierro %is% Undetermined && zinc %is% Undetermined && energia %is% Undetermined, aporte_nutrientes %is% Undetermined),
  fuzzy_rule(calcio %is% Low && hierro %is% Undetermined && zinc %is% Low && energia %is% Undetermined, aporte_nutrientes %is% Undetermined),
  fuzzy_rule(calcio %is% Low && hierro %is% Undetermined && zinc %is% Medium && energia %is% Undetermined, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Low && hierro %is% Undetermined && zinc %is% High && energia %is% Undetermined, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Low && hierro %is% High && zinc %is% Low && energia %is% Undetermined, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Low && hierro %is% High && zinc %is% Medium && energia %is% Undetermined, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Low && hierro %is% High && zinc %is% High && energia %is% Undetermined, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% Low && hierro %is% High && zinc %is% Undetermined && energia %is% Undetermined, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Low && hierro %is% High && zinc %is% Low && energia %is% Medium, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Low && hierro %is% High && zinc %is% Low && energia %is% High, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Low && hierro %is% High && zinc %is% Undetermined && energia %is% Low, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Low && hierro %is% High && zinc %is% Low && energia %is% Low, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Low && hierro %is% High && zinc %is% Medium && energia %is% Low, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Low && hierro %is% High && zinc %is% Medium && energia %is% Medium, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% Low && hierro %is% High && zinc %is% Medium && energia %is% High, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% Low && hierro %is% High && zinc %is% High && energia %is% Low, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% Low && hierro %is% High && zinc %is% High && energia %is% Medium, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% Low && hierro %is% High && zinc %is% High && energia %is% High, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% Low && hierro %is% High && zinc %is% Undetermined && energia %is% Medium, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Low && hierro %is% High && zinc %is% Undetermined && energia %is% High, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Medium && hierro %is% Low && zinc %is% Low && energia %is% Undetermined, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Medium && hierro %is% Low && zinc %is% Medium && energia %is% Undetermined, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Medium && hierro %is% Low && zinc %is% High && energia %is% Undetermined, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Medium && hierro %is% Low && zinc %is% Undetermined && energia %is% Undetermined, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Medium && hierro %is% Low && zinc %is% High && energia %is% Medium, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% Medium && hierro %is% Low && zinc %is% Medium && energia %is% Low, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Medium && hierro %is% Low && zinc %is% Medium && energia %is% Medium, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Medium && hierro %is% Low && zinc %is% Medium && energia %is% High, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Medium && hierro %is% Low && zinc %is% High && energia %is% Low, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Medium && hierro %is% Low && zinc %is% Undetermined && energia %is% High, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Medium && hierro %is% Low && zinc %is% Low && energia %is% Low, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Medium && hierro %is% Low && zinc %is% Low && energia %is% Medium, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Medium && hierro %is% Low && zinc %is% Low && energia %is% High, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Medium && hierro %is% Low && zinc %is% Undetermined && energia %is% Low, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Medium && hierro %is% Low && zinc %is% Undetermined && energia %is% Medium, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Medium && hierro %is% Low && zinc %is% High && energia %is% High, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% Medium && hierro %is% Medium && zinc %is% Low && energia %is% Undetermined, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Medium && hierro %is% Medium && zinc %is% Medium && energia %is% Undetermined, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Medium && hierro %is% Medium && zinc %is% High && energia %is% Undetermined, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% Medium && hierro %is% Medium && zinc %is% Undetermined && energia %is% Undetermined, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Medium && hierro %is% Medium && zinc %is% Medium && energia %is% Medium, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% Medium && hierro %is% Medium && zinc %is% Low && energia %is% Medium, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Medium && hierro %is% Medium && zinc %is% Low && energia %is% High, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Medium && hierro %is% Medium && zinc %is% Medium && energia %is% Low, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Medium && hierro %is% Medium && zinc %is% Medium && energia %is% High, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% Medium && hierro %is% Medium && zinc %is% Undetermined && energia %is% Low, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Medium && hierro %is% Medium && zinc %is% Undetermined && energia %is% Medium, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Medium && hierro %is% Medium && zinc %is% Undetermined && energia %is% High, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Medium && hierro %is% Medium && zinc %is% Low && energia %is% Low, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Medium && hierro %is% Medium && zinc %is% High && energia %is% Low, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% Medium && hierro %is% Medium && zinc %is% High && energia %is% Medium, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% Medium && hierro %is% Medium && zinc %is% High && energia %is% High, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% Medium && hierro %is% Undetermined && zinc %is% Low && energia %is% Low, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Medium && hierro %is% Undetermined && zinc %is% Low && energia %is% Medium, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Medium && hierro %is% Undetermined && zinc %is% Low && energia %is% High, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Medium && hierro %is% Undetermined && zinc %is% Medium && energia %is% Low, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Medium && hierro %is% Undetermined && zinc %is% Medium && energia %is% Medium, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Medium && hierro %is% Undetermined && zinc %is% Medium && energia %is% High, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Medium && hierro %is% Undetermined && zinc %is% High && energia %is% Low, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Medium && hierro %is% Undetermined && zinc %is% High && energia %is% Medium, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Medium && hierro %is% Undetermined && zinc %is% High && energia %is% High, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Medium && hierro %is% Undetermined && zinc %is% Undetermined && energia %is% Low, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Medium && hierro %is% Undetermined && zinc %is% Undetermined && energia %is% Medium, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Medium && hierro %is% Undetermined && zinc %is% Undetermined && energia %is% High, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Medium && hierro %is% Undetermined && zinc %is% Undetermined && energia %is% Undetermined, aporte_nutrientes %is% Undetermined),
  fuzzy_rule(calcio %is% Medium && hierro %is% Undetermined && zinc %is% Low && energia %is% Undetermined, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Medium && hierro %is% Undetermined && zinc %is% Medium && energia %is% Undetermined, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Medium && hierro %is% Undetermined && zinc %is% High && energia %is% Undetermined, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Medium && hierro %is% High && zinc %is% Low && energia %is% Undetermined, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Medium && hierro %is% High && zinc %is% Medium && energia %is% Undetermined, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% Medium && hierro %is% High && zinc %is% High && energia %is% Undetermined, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% Medium && hierro %is% High && zinc %is% Undetermined && energia %is% Undetermined, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Medium && hierro %is% High && zinc %is% Low && energia %is% Medium, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% Medium && hierro %is% High && zinc %is% Low && energia %is% High, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% Medium && hierro %is% High && zinc %is% Undetermined && energia %is% Low, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Medium && hierro %is% High && zinc %is% Low && energia %is% Low, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Medium && hierro %is% High && zinc %is% Medium && energia %is% Low, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% Medium && hierro %is% High && zinc %is% Medium && energia %is% Medium, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% Medium && hierro %is% High && zinc %is% Medium && energia %is% High, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% Medium && hierro %is% High && zinc %is% High && energia %is% Low, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% Medium && hierro %is% High && zinc %is% High && energia %is% Medium, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% Medium && hierro %is% High && zinc %is% High && energia %is% High, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% Medium && hierro %is% High && zinc %is% Undetermined && energia %is% Medium, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Medium && hierro %is% High && zinc %is% Undetermined && energia %is% High, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% High && hierro %is% Low && zinc %is% Low && energia %is% Undetermined, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% High && hierro %is% Low && zinc %is% Medium && energia %is% Undetermined, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% High && hierro %is% Low && zinc %is% High && energia %is% Undetermined, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% High && hierro %is% Low && zinc %is% Undetermined && energia %is% Undetermined, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% High && hierro %is% Low && zinc %is% High && energia %is% Medium, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% High && hierro %is% Low && zinc %is% Medium && energia %is% Low, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% High && hierro %is% Low && zinc %is% Medium && energia %is% Medium, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% High && hierro %is% Low && zinc %is% Medium && energia %is% High, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% High && hierro %is% Low && zinc %is% High && energia %is% Low, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% High && hierro %is% Low && zinc %is% Undetermined && energia %is% High, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% High && hierro %is% Low && zinc %is% Low && energia %is% Low, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% High && hierro %is% Low && zinc %is% Low && energia %is% Medium, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% High && hierro %is% Low && zinc %is% Low && energia %is% High, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% High && hierro %is% Low && zinc %is% Undetermined && energia %is% Low, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% High && hierro %is% Low && zinc %is% Undetermined && energia %is% Medium, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% High && hierro %is% Low && zinc %is% High && energia %is% High, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% High && hierro %is% Medium && zinc %is% Low && energia %is% Undetermined, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% High && hierro %is% Medium && zinc %is% Medium && energia %is% Undetermined, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% High && hierro %is% Medium && zinc %is% High && energia %is% Undetermined, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% High && hierro %is% Medium && zinc %is% Undetermined && energia %is% Undetermined, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% High && hierro %is% Medium && zinc %is% Medium && energia %is% Medium, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% High && hierro %is% Medium && zinc %is% Low && energia %is% Medium, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% High && hierro %is% Medium && zinc %is% Low && energia %is% High, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% High && hierro %is% Medium && zinc %is% Medium && energia %is% Low, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% High && hierro %is% Medium && zinc %is% Medium && energia %is% High, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% High && hierro %is% Medium && zinc %is% Undetermined && energia %is% Low, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% High && hierro %is% Medium && zinc %is% Undetermined && energia %is% Medium, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% High && hierro %is% Medium && zinc %is% Undetermined && energia %is% High, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% High && hierro %is% Medium && zinc %is% Low && energia %is% Low, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% High && hierro %is% Medium && zinc %is% High && energia %is% Low, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% High && hierro %is% Medium && zinc %is% High && energia %is% Medium, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% High && hierro %is% Medium && zinc %is% High && energia %is% High, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% High && hierro %is% Undetermined && zinc %is% Low && energia %is% Low, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% High && hierro %is% Undetermined && zinc %is% Low && energia %is% Medium, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% High && hierro %is% Undetermined && zinc %is% Low && energia %is% High, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% High && hierro %is% Undetermined && zinc %is% Medium && energia %is% Low, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% High && hierro %is% Undetermined && zinc %is% Medium && energia %is% Medium, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% High && hierro %is% Undetermined && zinc %is% Medium && energia %is% High, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% High && hierro %is% Undetermined && zinc %is% High && energia %is% Low, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% High && hierro %is% Undetermined && zinc %is% High && energia %is% Medium, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% High && hierro %is% Undetermined && zinc %is% High && energia %is% High, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% High && hierro %is% Undetermined && zinc %is% Undetermined && energia %is% Low, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% High && hierro %is% Undetermined && zinc %is% Undetermined && energia %is% Medium, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% High && hierro %is% Undetermined && zinc %is% Undetermined && energia %is% High, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% High && hierro %is% Undetermined && zinc %is% Undetermined && energia %is% Undetermined, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% High && hierro %is% Undetermined && zinc %is% Low && energia %is% Undetermined, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% High && hierro %is% Undetermined && zinc %is% Medium && energia %is% Undetermined, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% High && hierro %is% Undetermined && zinc %is% High && energia %is% Undetermined, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% High && hierro %is% High && zinc %is% Low && energia %is% Undetermined, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% High && hierro %is% High && zinc %is% Medium && energia %is% Undetermined, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% High && hierro %is% High && zinc %is% High && energia %is% Undetermined, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% High && hierro %is% High && zinc %is% Undetermined && energia %is% Undetermined, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% High && hierro %is% High && zinc %is% Low && energia %is% Medium, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% High && hierro %is% High && zinc %is% Low && energia %is% High, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% High && hierro %is% High && zinc %is% Undetermined && energia %is% Low, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% High && hierro %is% High && zinc %is% Low && energia %is% Low, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% High && hierro %is% High && zinc %is% Medium && energia %is% Low, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% High && hierro %is% High && zinc %is% Medium && energia %is% Medium, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% High && hierro %is% High && zinc %is% Medium && energia %is% High, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% High && hierro %is% High && zinc %is% High && energia %is% Low, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% High && hierro %is% High && zinc %is% High && energia %is% Medium, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% High && hierro %is% High && zinc %is% High && energia %is% High, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% High && hierro %is% High && zinc %is% Undetermined && energia %is% Medium, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% High && hierro %is% High && zinc %is% Undetermined && energia %is% High, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Low && zinc %is% Low && energia %is% Undetermined, aporte_nutrientes %is% Undetermined),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Low && zinc %is% Medium && energia %is% Undetermined, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Low && zinc %is% High && energia %is% Undetermined, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Low && zinc %is% Undetermined && energia %is% Undetermined, aporte_nutrientes %is% Undetermined),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Low && zinc %is% High && energia %is% Medium, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Low && zinc %is% Medium && energia %is% Low, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Low && zinc %is% Medium && energia %is% Medium, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Low && zinc %is% Medium && energia %is% High, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Low && zinc %is% High && energia %is% Low, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Low && zinc %is% Undetermined && energia %is% High, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Low && zinc %is% Low && energia %is% Low, aporte_nutrientes %is% Undetermined),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Low && zinc %is% Low && energia %is% Medium, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Low && zinc %is% Low && energia %is% High, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Low && zinc %is% Undetermined && energia %is% Low, aporte_nutrientes %is% Undetermined),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Low && zinc %is% Undetermined && energia %is% Medium, aporte_nutrientes %is% Undetermined),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Low && zinc %is% High && energia %is% High, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Medium && zinc %is% Low && energia %is% Undetermined, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Medium && zinc %is% Medium && energia %is% Undetermined, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Medium && zinc %is% High && energia %is% Undetermined, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Medium && zinc %is% Undetermined && energia %is% Undetermined, aporte_nutrientes %is% Undetermined),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Medium && zinc %is% Medium && energia %is% Medium, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Medium && zinc %is% Low && energia %is% Medium, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Medium && zinc %is% Low && energia %is% High, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Medium && zinc %is% Medium && energia %is% Low, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Medium && zinc %is% Medium && energia %is% High, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Medium && zinc %is% Undetermined && energia %is% Low, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Medium && zinc %is% Undetermined && energia %is% Medium, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Medium && zinc %is% Undetermined && energia %is% High, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Medium && zinc %is% Low && energia %is% Low, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Medium && zinc %is% High && energia %is% Low, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Medium && zinc %is% High && energia %is% Medium, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Medium && zinc %is% High && energia %is% High, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Undetermined && zinc %is% Low && energia %is% Low, aporte_nutrientes %is% Undetermined),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Undetermined && zinc %is% Low && energia %is% Medium, aporte_nutrientes %is% Undetermined),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Undetermined && zinc %is% Low && energia %is% High, aporte_nutrientes %is% Undetermined),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Undetermined && zinc %is% Medium && energia %is% Low, aporte_nutrientes %is% Undetermined),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Undetermined && zinc %is% Medium && energia %is% Medium, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Undetermined && zinc %is% Medium && energia %is% High, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Undetermined && zinc %is% High && energia %is% Low, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Undetermined && zinc %is% High && energia %is% Medium, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Undetermined && zinc %is% High && energia %is% High, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Undetermined && zinc %is% Undetermined && energia %is% Low, aporte_nutrientes %is% Undetermined),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Undetermined && zinc %is% Undetermined && energia %is% Medium, aporte_nutrientes %is% Undetermined),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Undetermined && zinc %is% Undetermined && energia %is% High, aporte_nutrientes %is% Undetermined),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Undetermined && zinc %is% Undetermined && energia %is% Undetermined, aporte_nutrientes %is% Undetermined),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Undetermined && zinc %is% Low && energia %is% Undetermined, aporte_nutrientes %is% Undetermined),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Undetermined && zinc %is% Medium && energia %is% Undetermined, aporte_nutrientes %is% Undetermined),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% Undetermined && zinc %is% High && energia %is% Undetermined, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% High && zinc %is% Low && energia %is% Undetermined, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% High && zinc %is% Medium && energia %is% Undetermined, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% High && zinc %is% High && energia %is% Undetermined, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% High && zinc %is% Undetermined && energia %is% Undetermined, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% High && zinc %is% Low && energia %is% Medium, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% High && zinc %is% Low && energia %is% High, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% High && zinc %is% Undetermined && energia %is% Low, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% High && zinc %is% Low && energia %is% Low, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% High && zinc %is% Medium && energia %is% Low, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% High && zinc %is% Medium && energia %is% Medium, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% High && zinc %is% Medium && energia %is% High, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% High && zinc %is% High && energia %is% Low, aporte_nutrientes %is% Medium),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% High && zinc %is% High && energia %is% Medium, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% High && zinc %is% High && energia %is% High, aporte_nutrientes %is% High),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% High && zinc %is% Undetermined && energia %is% Medium, aporte_nutrientes %is% Low),
  fuzzy_rule(calcio %is% Undetermined && hierro %is% High && zinc %is% Undetermined && energia %is% High, aporte_nutrientes %is% Low)
  
)

modelF_aporte <- fuzzy_system(var_aporte, reg_aporte)
print(modelF_aporte)
par(mar = rep(2,4))
plot(modelF_aporte)

df <- dplyr::select(d,id, 
                    categoria,
                    grupo,
                    nombre_cientifico,
                    cultivo,
                    calcio,
                    calcio_norm,
                    hierro,
                    hierro_norm,
                    zinc,
                    zinc_norm,
                    energia,
                    energia_norm,
                    imputacion_sa_calcio,
                    imputacion_sa_hierro, 
                    imputacion_sa_zinc,
                    imputacion_sa_energia,
                    incertidumbre,incertidumbre_et)

df$rownm <- rownames(df)
df$rownm <- as.numeric(df$rownm)

View(df)



#df <- dplyr::filter(df, rownm >= lim_inf & rownm <= lim_sup)

df <- dplyr::filter(df, cultivo == "Papa")

#df <- dplyr::filter(df, id == 227)

df$rownums <- rownames(df)
df$rownums <- as.numeric(df$rownums)

df$calcio_norm = as.numeric(format(df$calcio_norm, digits=4))
df$hierro_norm = as.numeric(format(df$hierro_norm, digits=4))
df$zinc_norm = as.numeric(format(df$zinc_norm, digits=4))
df$energia_norm = as.numeric(format(df$energia_norm, digits=4))




#View(df)



fuzz.nut <- function(y){
  print(paste(df[y,'rownums'],"..."))
  print(paste("fuzzy aporte nutrientes ",df[y,'nombre_cientifico']))
  print(paste("Calcio aporte: ", df[y,'calcio_norm']))
  print(paste("Hierro aporte: ", df[y,'hierro_norm']))
  print(paste("Zinc aporte: ", df[y,'zinc_norm']))
  print(paste("Energia aporte: ", df[y,'energia_norm']))
  fuzzify <- sets::fuzzy_inference(modelF_aporte, list(calcio = df[y,'calcio_norm'],
                                                       hierro = df[y,'hierro_norm'],
                                                       zinc = df[y,'zinc_norm'],
                                                       energia = df[y,'energia_norm']))
  print(gset_defuzzify(fuzzify, "largestofmax"))
  gset_defuzzify(fuzzify, "largestofmax")
  
}



df$aporte_nutrientes <- sapply(df$rownums, fuzz.nut) 
df$aporte_nutrientes <-  as.numeric(format(df$aporte_nutrientes, digits=4))





write.csv2(df,output_name,sep = ",")







