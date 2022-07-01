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


lim_inf <- 1
lim_sup <- 3
output_name <- paste("data/data_eco_bgv",lim_inf,"-",lim_sup,".csv",sep = "")


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
                   nombre_cientifico,
                   nombre_comun,
                   cultivo,
                   un_estado_conservacion,
                   est_conservacion_BGRL,
                   rendimiento, valor_produccion_h,
                   porc_municipios,indice_lafay)

d$indice_lafay <- as.numeric(d$indice_lafay)
d$rendimiento <- as.numeric(d$rendimiento)

#hist(d$indice_lafay)
#hist(d$rendimiento)

d$LafayIndex <- normalize(d$indice_lafay,method = "range",range = c(1,100))
d$LafayIndex[is.na(d$LafayIndex)] <- 0

IL_center <- mean(d$LafayIndex[d$indice_lafay == 1],na.rm = TRUE)
IL_Low_corners <- c(1,IL_center*0.25,IL_center*0.5,IL_center*0.85)
IL_Med_corners <- c(IL_center*0.5,IL_center,IL_center,IL_center*1.5)
IL_Hig_corners <- c(IL_center*1.35,IL_center*2.5,100,105)



d$Yield <- d$rendimiento

Beverageandspicecrops <- dplyr::select(filter(d, d$clasificacion_FAO == "Beverage and spice crops" & !is.na(d$Yield)), id,Yield)
Beverageandspicecrops$Yield <- as.numeric(Beverageandspicecrops$Yield)
#hist(Beverageandspicecrops$Yield)
Beverageandspicecrops$norm <- normalize(Beverageandspicecrops$Yield,method = "range",range = c(1,100))

# c_rend <- Ckmeans.1d.dp(Beverageandspicecrops$Yield, 3)
# Beverageandspicecrops$cluster <- c_rend$cluster
rend <- Beverageandspicecrops 
# rm(Beverageandspicecrops)
# print(length(rend$id))

Cereals <- dplyr::select(filter(d, d$clasificacion_FAO == "Cereals"  & !is.na(d$Yield)), id,Yield)
Cereals$Yield <- as.numeric(Cereals$Yield)
#hist(Cereals$Yield)
Cereals$norm <- normalize(Cereals$Yield,method = "range",range = c(1,100))

# c_rend <- Ckmeans.1d.dp(Cereals$Yield, 3)
# Cereals$cluster <- c_rend$cluster
rend <- rbind(rend,Cereals)
# rm(Cereals)
# print(length(rend$id))

Fruitsandnuts <- dplyr::select(filter(d, d$clasificacion_FAO == "Fruits and nuts"  & !is.na(d$Yield)), id,Yield)
Fruitsandnuts$Yield <- as.numeric(Fruitsandnuts$Yield)
#hist(Fruitsandnuts$Yield)
Fruitsandnuts$norm <- normalize(Fruitsandnuts$Yield,method = "range",range = c(1,100))

# c_rend <- Ckmeans.1d.dp(Fruitsandnuts$Yield, 3)
# Fruitsandnuts$cluster <- c_rend$cluster
rend <- rbind(rend,Fruitsandnuts)
# rm(Fruitsandnuts)
# print(length(rend$id))

Leguminouscrops <- dplyr::select(filter(d, d$clasificacion_FAO == "Leguminous crops"  & !is.na(d$Yield)), id,Yield)
Leguminouscrops$Yield <- as.numeric(Leguminouscrops$Yield)
#hist(Leguminouscrops$Yield)
Leguminouscrops$norm <- normalize(Leguminouscrops$Yield,method = "range",range = c(1,100))

# c_rend <- Ckmeans.1d.dp(Leguminouscrops$Yield, 3)
# Leguminouscrops$cluster <- c_rend$cluster
rend <- rbind(rend,Leguminouscrops)
# rm(Leguminouscrops)
# print(length(rend$id))

Noclasificada <- dplyr::select(filter(d, d$clasificacion_FAO == "No clasificada"  & !is.na(d$Yield)), id,Yield)
Noclasificada$Yield <- as.numeric(Noclasificada$Yield)
#hist(Noclasificada$Yield)
Noclasificada$norm <- normalize(Noclasificada$Yield,method = "range",range = c(1,100))

# c_rend <- Ckmeans.1d.dp(Noclasificada$Yield, 3)
# Noclasificada$cluster <- c_rend$cluster
rend <- rbind(rend,Noclasificada)
# rm(Noclasificada)
# print(length(rend$id))

Oilseedcrops <- dplyr::select(filter(d, d$clasificacion_FAO == "Oilseed crops"  & !is.na(d$Yield)), id,Yield)
Oilseedcrops$Yield <- as.numeric(Oilseedcrops$Yield)
#hist(Oilseedcrops$Yield)
Oilseedcrops$norm <- normalize(Oilseedcrops$Yield,method = "range",range = c(1,100))

# c_rend <- Ckmeans.1d.dp(Oilseedcrops$Yield, 3)
# Oilseedcrops$cluster <- c_rend$cluster
rend <- rbind(rend,Oilseedcrops)
# rm(Oilseedcrops)
# print(length(rend$id))

Othercrops <- dplyr::select(filter(d, d$clasificacion_FAO == "Other crops"  & !is.na(d$Yield)), id,Yield)
Othercrops$Yield <- as.numeric(Othercrops$Yield)
#hist(Othercrops$Yield)
Othercrops$norm <- normalize(Othercrops$Yield,method = "range",range = c(1,100))

# c_rend <- Ckmeans.1d.dp(Othercrops$Yield, 3)
# Othercrops$cluster <- c_rend$cluster
rend <- rbind(rend,Othercrops)
# rm(Othercrops)
# print(length(rend$id))

Roottuber <- dplyr::select(filter(d, d$clasificacion_FAO == "Root/tuber crops with high starch or inulin conten"  & !is.na(d$Yield)), id,Yield)
Roottuber$Yield <- as.numeric(Roottuber$Yield)
#hist(Roottuber$Yield)
Roottuber$norm <- normalize(Roottuber$Yield,method = "range",range = c(1,100))

# c_rend <- Ckmeans.1d.dp(Roottuber$Yield, 3)
# Roottuber$cluster <- c_rend$cluster
rend <- rbind(rend,Roottuber)
# rm(Roottuber)
# print(length(rend$id))

Sugarcrops <- dplyr::select(filter(d, d$clasificacion_FAO == "Sugar crops"  & !is.na(d$Yield)), id,Yield)
Sugarcrops$Yield <- as.numeric(Sugarcrops$Yield)
#hist(Sugarcrops$Yield)
Sugarcrops$norm <- normalize(Sugarcrops$Yield,method = "range",range = c(1,100))

# c_rend <- Ckmeans.1d.dp(Sugarcrops$Yield, 3)
# Sugarcrops$cluster <- c_rend$cluster
rend <- rbind(rend,Sugarcrops)
# rm(Sugarcrops)
# print(length(rend$id))

Vegetablesandmelons <- dplyr::select(filter(d, d$clasificacion_FAO == "Vegetables and melons"  & !is.na(d$Yield)), id,Yield)
Vegetablesandmelons$Yield <- as.numeric(Vegetablesandmelons$Yield)
#hist(Vegetablesandmelons$Yield)
Vegetablesandmelons$norm <- normalize(Vegetablesandmelons$Yield,method = "range",range = c(1,100))

# c_rend <- Ckmeans.1d.dp(Vegetablesandmelons$Yield, 3)
# Vegetablesandmelons$cluster <- c_rend$cluster
rend <- rbind(rend,Vegetablesandmelons)
# rm(Vegetablesandmelons)
# print(length(rend$id))

Undetermined <- dplyr::select(filter(d, is.na(d$Yield)), id,Yield)
Undetermined$norm <- 0
rend <- rbind(rend,Undetermined)
# rm(Undetermined)
# print(length(rend$id))

rend$id <- as.numeric(rend$id)
rend <- rend[order(rend$id),]

d$Yield <- rend$norm

d$valor_produccion_h <- as.numeric(d$valor_produccion_h)
d$log_valor_produccion <- log10(d$valor_produccion_h)
d$log_valor_produccion <- as.numeric(d$log_valor_produccion)


d$norm_valor_produccion <- rescale(d$log_valor_produccion, 
                                   to = c(0, 95), 
                                   from = range(d$log_valor_produccion, 
                                                na.rm = FALSE, finite = TRUE))

d$norm_valor_produccion <- d$norm_valor_produccion + 5


d$norm_valor_produccion[is.na(d$norm_valor_produccion)] <- -30#-80


c_valor_produccion <- Ckmeans.1d.dp(d$norm_valor_produccion, 4)

d$norm_valor_produccion[d$norm_valor_produccion == -30] <- 0#-80

d$ProdCost <- d$norm_valor_produccion

PCcLow = c_valor_produccion$centers[2]
PCcMed = c_valor_produccion$centers[3]
PCcHig = c_valor_produccion$centers[4]

d$clusterPC <- c_valor_produccion$cluster


PCLowCorners <-c(4.99,
                 5,
                 PCcLow*1.05,
                 PCcMed*0.8)
PCMedCorners <- c(PCcLow*1.05,
                  PCcMed*0.8,
                  PCcMed*1.15,
                  PCcHig*0.9)
PCHigCorners <- c(PCcMed*1.15,
                  PCcHig*0.9,
                  105,
                  110)

d$porc_municipios <- as.numeric(d$porc_municipios)
d$norm_mun_cov <- rescale(d$porc_municipios, 
                          to = c(1, 100), 
                          from = range(d$porc_municipios, 
                                       na.rm = FALSE, finite = TRUE))

#d$porc_municipios <- d$norm_porc

d$norm_mun_cov[is.na(d$norm_mun_cov)] <- -30


c_mun_cov <- Ckmeans.1d.dp
d$norm_mun_cov[d$norm_mun_cov == -30] <- 0

d$MunCoverage <- d$norm_mun_cov




EconomicVars <- set(
  
  LafayIndex = fuzzy_variable(Undetermined = fuzzy_bell(center = 0, cross = 0.01, slope = 10, height = 1, chop = 0),
                              Low = fuzzy_trapezoid(corners = IL_Low_corners, height = 1),
                              Medium = fuzzy_trapezoid(corners = IL_Med_corners, height = 1),
                              High = fuzzy_trapezoid(corners = IL_Hig_corners, height = 1)
  ),
  
  Yield = fuzzy_variable(Undetermined = fuzzy_bell(center = 0, cross = 0.01, slope = 10, height = 1, chop = 0),
                         Low = fuzzy_trapezoid(corners = c(0.9,1,30,40), height = 1),
                         Medium = fuzzy_trapezoid(corners = c(30,40,60,70), height = 1),
                         High = fuzzy_trapezoid(corners = c(60,70,105,106), height = 1)
  ),
  
  
  ProdCost = fuzzy_variable(Undetermined = fuzzy_bell(center = 0, cross = 0.01, slope = 10, height = 1, chop = 0),
                            Low = fuzzy_trapezoid(corners = PCLowCorners, height = 1),
                            Medium = fuzzy_trapezoid(corners = PCMedCorners, height = 1),
                            High = fuzzy_trapezoid(corners = PCHigCorners, height = 1)),
  
  
  
  MunCoverage = fuzzy_variable(Undetermined = fuzzy_bell(center = 0, cross = 0.01, slope = 10, height = 1, chop = 0),
                               Low = fuzzy_trapezoid(corners = c(0.99,1,15,30), height = 1),
                               Medium = fuzzy_trapezoid(corners = c(20,25,50,60), height = 1),
                               High = fuzzy_trapezoid(corners = c(55,60,101,105), height = 1)),
  
  
  #EconomicImportance = fuzzy_variable(NoDecision = fuzzy_bell(center = 0, cross = 0.01, slope = 10, height = 1, chop = 0),
  #                                    Low = fuzzy_bell(center = 20, cross = 12, slope = 2, height = 1, chop = 0),
  #                                    Medium = fuzzy_bell(center = 50, cross = 12, slope = 2, height = 1, chop = 0),
  #                                    High = fuzzy_bell(center = 80, cross = 12, slope = 2, height = 1, chop = 0))
  
  
  EconomicImportance = fuzzy_variable(NoDecision = fuzzy_trapezoid(corners = c(0,0.01,9.99,10), height = 1),
                                      Low = fuzzy_trapezoid(corners = c(10,25,30,45), height = 1),
                                      Medium = fuzzy_trapezoid(corners = c(30,45,50,65), height = 1),
                                      High = fuzzy_trapezoid(corners = c(50,65,70,85), height = 1))
  
  
  
  #EconomicImportance = fuzzy_variable(NoDecision = fuzzy_bell(center = 0, cross = 0.01, slope = 10, height = 1, chop = 0),
  #                                    Low = fuzzy_bell(center = 40, cross = 12, slope = 2, height = 1, chop = 0),
  #                                    Medium = fuzzy_bell(center = 70, cross = 12, slope = 2, height = 1, chop = 0),
  #                                    High = fuzzy_bell(center = 100, cross = 12, slope = 2, height = 1, chop = 0))
  
  
)

EconomicRules <- set(
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Low && ProdCost %is% Low && MunCoverage %is% Undetermined, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Low && ProdCost %is% Medium && MunCoverage %is% Undetermined, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Low && ProdCost %is% High && MunCoverage %is% Undetermined, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Low && ProdCost %is% Undetermined && MunCoverage %is% Undetermined, EconomicImportance %is% NoDecision),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Low && ProdCost %is% High && MunCoverage %is% Medium, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Low && ProdCost %is% Medium && MunCoverage %is% Low, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Low && ProdCost %is% Medium && MunCoverage %is% Medium, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Low && ProdCost %is% Medium && MunCoverage %is% High, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Low && ProdCost %is% High && MunCoverage %is% Low, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Low && ProdCost %is% Undetermined && MunCoverage %is% High, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Low && ProdCost %is% Low && MunCoverage %is% Low, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Low && ProdCost %is% Low && MunCoverage %is% Medium, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Low && ProdCost %is% Low && MunCoverage %is% High, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Low && ProdCost %is% Undetermined && MunCoverage %is% Low, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Low && ProdCost %is% Undetermined && MunCoverage %is% Medium, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Low && ProdCost %is% High && MunCoverage %is% High, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Medium && ProdCost %is% Low && MunCoverage %is% Undetermined, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Medium && ProdCost %is% Medium && MunCoverage %is% Undetermined, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Medium && ProdCost %is% High && MunCoverage %is% Undetermined, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Medium && ProdCost %is% Undetermined && MunCoverage %is% Undetermined, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Medium && ProdCost %is% Medium && MunCoverage %is% Medium, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Medium && ProdCost %is% Low && MunCoverage %is% Medium, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Medium && ProdCost %is% Low && MunCoverage %is% High, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Medium && ProdCost %is% Medium && MunCoverage %is% Low, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Medium && ProdCost %is% Medium && MunCoverage %is% High, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Medium && ProdCost %is% Undetermined && MunCoverage %is% Low, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Medium && ProdCost %is% Undetermined && MunCoverage %is% Medium, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Medium && ProdCost %is% Undetermined && MunCoverage %is% High, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Medium && ProdCost %is% Low && MunCoverage %is% Low, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Medium && ProdCost %is% High && MunCoverage %is% Low, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Medium && ProdCost %is% High && MunCoverage %is% Medium, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Medium && ProdCost %is% High && MunCoverage %is% High, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Undetermined && ProdCost %is% Low && MunCoverage %is% Low, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Undetermined && ProdCost %is% Low && MunCoverage %is% Medium, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Undetermined && ProdCost %is% Low && MunCoverage %is% High, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Undetermined && ProdCost %is% Medium && MunCoverage %is% Low, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Undetermined && ProdCost %is% Medium && MunCoverage %is% Medium, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Undetermined && ProdCost %is% Medium && MunCoverage %is% High, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Undetermined && ProdCost %is% High && MunCoverage %is% Low, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Undetermined && ProdCost %is% High && MunCoverage %is% Medium, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Undetermined && ProdCost %is% High && MunCoverage %is% High, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Undetermined && ProdCost %is% Undetermined && MunCoverage %is% Low, EconomicImportance %is% NoDecision),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Undetermined && ProdCost %is% Undetermined && MunCoverage %is% Medium, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Undetermined && ProdCost %is% Undetermined && MunCoverage %is% High, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Undetermined && ProdCost %is% Undetermined && MunCoverage %is% Undetermined, EconomicImportance %is% NoDecision),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Undetermined && ProdCost %is% Low && MunCoverage %is% Undetermined, EconomicImportance %is% NoDecision),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Undetermined && ProdCost %is% Medium && MunCoverage %is% Undetermined, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% Undetermined && ProdCost %is% High && MunCoverage %is% Undetermined, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% High && ProdCost %is% Low && MunCoverage %is% Undetermined, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% High && ProdCost %is% Medium && MunCoverage %is% Undetermined, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% High && ProdCost %is% High && MunCoverage %is% Undetermined, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% High && ProdCost %is% Undetermined && MunCoverage %is% Undetermined, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% High && ProdCost %is% Low && MunCoverage %is% Medium, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% High && ProdCost %is% Low && MunCoverage %is% High, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% High && ProdCost %is% Undetermined && MunCoverage %is% Low, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% High && ProdCost %is% Low && MunCoverage %is% Low, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% High && ProdCost %is% Medium && MunCoverage %is% Low, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% High && ProdCost %is% Medium && MunCoverage %is% Medium, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% High && ProdCost %is% Medium && MunCoverage %is% High, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% High && ProdCost %is% High && MunCoverage %is% Low, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% High && ProdCost %is% High && MunCoverage %is% Medium, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% High && ProdCost %is% High && MunCoverage %is% High, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% High && ProdCost %is% Undetermined && MunCoverage %is% Medium, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Low && Yield %is% High && ProdCost %is% Undetermined && MunCoverage %is% High, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Low && ProdCost %is% Low && MunCoverage %is% Undetermined, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Low && ProdCost %is% Medium && MunCoverage %is% Undetermined, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Low && ProdCost %is% High && MunCoverage %is% Undetermined, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Low && ProdCost %is% Undetermined && MunCoverage %is% Undetermined, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Low && ProdCost %is% High && MunCoverage %is% Medium, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Low && ProdCost %is% Medium && MunCoverage %is% Low, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Low && ProdCost %is% Medium && MunCoverage %is% Medium, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Low && ProdCost %is% Medium && MunCoverage %is% High, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Low && ProdCost %is% High && MunCoverage %is% Low, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Low && ProdCost %is% Undetermined && MunCoverage %is% High, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Low && ProdCost %is% Low && MunCoverage %is% Low, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Low && ProdCost %is% Low && MunCoverage %is% Medium, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Low && ProdCost %is% Low && MunCoverage %is% High, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Low && ProdCost %is% Undetermined && MunCoverage %is% Low, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Low && ProdCost %is% Undetermined && MunCoverage %is% Medium, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Low && ProdCost %is% High && MunCoverage %is% High, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Medium && ProdCost %is% Low && MunCoverage %is% Undetermined, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Medium && ProdCost %is% Medium && MunCoverage %is% Undetermined, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Medium && ProdCost %is% High && MunCoverage %is% Undetermined, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Medium && ProdCost %is% Undetermined && MunCoverage %is% Undetermined, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Medium && ProdCost %is% Medium && MunCoverage %is% Medium, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Medium && ProdCost %is% Low && MunCoverage %is% Medium, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Medium && ProdCost %is% Low && MunCoverage %is% High, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Medium && ProdCost %is% Medium && MunCoverage %is% Low, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Medium && ProdCost %is% Medium && MunCoverage %is% High, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Medium && ProdCost %is% Undetermined && MunCoverage %is% Low, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Medium && ProdCost %is% Undetermined && MunCoverage %is% Medium, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Medium && ProdCost %is% Undetermined && MunCoverage %is% High, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Medium && ProdCost %is% Low && MunCoverage %is% Low, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Medium && ProdCost %is% High && MunCoverage %is% Low, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Medium && ProdCost %is% High && MunCoverage %is% Medium, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Medium && ProdCost %is% High && MunCoverage %is% High, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Undetermined && ProdCost %is% Low && MunCoverage %is% Low, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Undetermined && ProdCost %is% Low && MunCoverage %is% Medium, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Undetermined && ProdCost %is% Low && MunCoverage %is% High, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Undetermined && ProdCost %is% Medium && MunCoverage %is% Low, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Undetermined && ProdCost %is% Medium && MunCoverage %is% Medium, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Undetermined && ProdCost %is% Medium && MunCoverage %is% High, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Undetermined && ProdCost %is% High && MunCoverage %is% Low, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Undetermined && ProdCost %is% High && MunCoverage %is% Medium, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Undetermined && ProdCost %is% High && MunCoverage %is% High, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Undetermined && ProdCost %is% Undetermined && MunCoverage %is% Low, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Undetermined && ProdCost %is% Undetermined && MunCoverage %is% Medium, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Undetermined && ProdCost %is% Undetermined && MunCoverage %is% High, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Undetermined && ProdCost %is% Undetermined && MunCoverage %is% Undetermined, EconomicImportance %is% NoDecision),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Undetermined && ProdCost %is% Low && MunCoverage %is% Undetermined, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Undetermined && ProdCost %is% Medium && MunCoverage %is% Undetermined, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% Undetermined && ProdCost %is% High && MunCoverage %is% Undetermined, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% High && ProdCost %is% Low && MunCoverage %is% Undetermined, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% High && ProdCost %is% Medium && MunCoverage %is% Undetermined, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% High && ProdCost %is% High && MunCoverage %is% Undetermined, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% High && ProdCost %is% Undetermined && MunCoverage %is% Undetermined, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% High && ProdCost %is% Low && MunCoverage %is% Medium, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% High && ProdCost %is% Low && MunCoverage %is% High, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% High && ProdCost %is% Undetermined && MunCoverage %is% Low, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% High && ProdCost %is% Low && MunCoverage %is% Low, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% High && ProdCost %is% Medium && MunCoverage %is% Low, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% High && ProdCost %is% Medium && MunCoverage %is% Medium, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% High && ProdCost %is% Medium && MunCoverage %is% High, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% High && ProdCost %is% High && MunCoverage %is% Low, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% High && ProdCost %is% High && MunCoverage %is% Medium, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% High && ProdCost %is% High && MunCoverage %is% High, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% High && ProdCost %is% Undetermined && MunCoverage %is% Medium, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Medium && Yield %is% High && ProdCost %is% Undetermined && MunCoverage %is% High, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Low && ProdCost %is% Low && MunCoverage %is% Undetermined, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Low && ProdCost %is% Medium && MunCoverage %is% Undetermined, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Low && ProdCost %is% High && MunCoverage %is% Undetermined, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Low && ProdCost %is% Undetermined && MunCoverage %is% Undetermined, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Low && ProdCost %is% High && MunCoverage %is% Medium, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Low && ProdCost %is% Medium && MunCoverage %is% Low, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Low && ProdCost %is% Medium && MunCoverage %is% Medium, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Low && ProdCost %is% Medium && MunCoverage %is% High, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Low && ProdCost %is% High && MunCoverage %is% Low, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Low && ProdCost %is% Undetermined && MunCoverage %is% High, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Low && ProdCost %is% Low && MunCoverage %is% Low, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Low && ProdCost %is% Low && MunCoverage %is% Medium, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Low && ProdCost %is% Low && MunCoverage %is% High, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Low && ProdCost %is% Undetermined && MunCoverage %is% Low, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Low && ProdCost %is% Undetermined && MunCoverage %is% Medium, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Low && ProdCost %is% High && MunCoverage %is% High, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Medium && ProdCost %is% Low && MunCoverage %is% Undetermined, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Medium && ProdCost %is% Medium && MunCoverage %is% Undetermined, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Medium && ProdCost %is% High && MunCoverage %is% Undetermined, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Medium && ProdCost %is% Undetermined && MunCoverage %is% Undetermined, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Medium && ProdCost %is% Medium && MunCoverage %is% Medium, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Medium && ProdCost %is% Low && MunCoverage %is% Medium, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Medium && ProdCost %is% Low && MunCoverage %is% High, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Medium && ProdCost %is% Medium && MunCoverage %is% Low, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Medium && ProdCost %is% Medium && MunCoverage %is% High, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Medium && ProdCost %is% Undetermined && MunCoverage %is% Low, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Medium && ProdCost %is% Undetermined && MunCoverage %is% Medium, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Medium && ProdCost %is% Undetermined && MunCoverage %is% High, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Medium && ProdCost %is% Low && MunCoverage %is% Low, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Medium && ProdCost %is% High && MunCoverage %is% Low, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Medium && ProdCost %is% High && MunCoverage %is% Medium, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Medium && ProdCost %is% High && MunCoverage %is% High, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Undetermined && ProdCost %is% Low && MunCoverage %is% Low, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Undetermined && ProdCost %is% Low && MunCoverage %is% Medium, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Undetermined && ProdCost %is% Low && MunCoverage %is% High, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Undetermined && ProdCost %is% Medium && MunCoverage %is% Low, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Undetermined && ProdCost %is% Medium && MunCoverage %is% Medium, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Undetermined && ProdCost %is% Medium && MunCoverage %is% High, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Undetermined && ProdCost %is% High && MunCoverage %is% Low, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Undetermined && ProdCost %is% High && MunCoverage %is% Medium, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Undetermined && ProdCost %is% High && MunCoverage %is% High, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Undetermined && ProdCost %is% Undetermined && MunCoverage %is% Low, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Undetermined && ProdCost %is% Undetermined && MunCoverage %is% Medium, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Undetermined && ProdCost %is% Undetermined && MunCoverage %is% High, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Undetermined && ProdCost %is% Undetermined && MunCoverage %is% Undetermined, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Undetermined && ProdCost %is% Low && MunCoverage %is% Undetermined, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Undetermined && ProdCost %is% Medium && MunCoverage %is% Undetermined, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% High && Yield %is% Undetermined && ProdCost %is% High && MunCoverage %is% Undetermined, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% High && Yield %is% High && ProdCost %is% Low && MunCoverage %is% Undetermined, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% High && Yield %is% High && ProdCost %is% Medium && MunCoverage %is% Undetermined, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% High && Yield %is% High && ProdCost %is% High && MunCoverage %is% Undetermined, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% High && Yield %is% High && ProdCost %is% Undetermined && MunCoverage %is% Undetermined, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% High && Yield %is% High && ProdCost %is% Low && MunCoverage %is% Medium, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% High && Yield %is% High && ProdCost %is% Low && MunCoverage %is% High, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% High && Yield %is% High && ProdCost %is% Undetermined && MunCoverage %is% Low, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% High && Yield %is% High && ProdCost %is% Low && MunCoverage %is% Low, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% High && Yield %is% High && ProdCost %is% Medium && MunCoverage %is% Low, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% High && Yield %is% High && ProdCost %is% Medium && MunCoverage %is% Medium, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% High && Yield %is% High && ProdCost %is% Medium && MunCoverage %is% High, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% High && Yield %is% High && ProdCost %is% High && MunCoverage %is% Low, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% High && Yield %is% High && ProdCost %is% High && MunCoverage %is% Medium, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% High && Yield %is% High && ProdCost %is% High && MunCoverage %is% High, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% High && Yield %is% High && ProdCost %is% Undetermined && MunCoverage %is% Medium, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% High && Yield %is% High && ProdCost %is% Undetermined && MunCoverage %is% High, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Low && ProdCost %is% Low && MunCoverage %is% Undetermined, EconomicImportance %is% NoDecision),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Low && ProdCost %is% Medium && MunCoverage %is% Undetermined, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Low && ProdCost %is% High && MunCoverage %is% Undetermined, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Low && ProdCost %is% Undetermined && MunCoverage %is% Undetermined, EconomicImportance %is% NoDecision),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Low && ProdCost %is% High && MunCoverage %is% Medium, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Low && ProdCost %is% Medium && MunCoverage %is% Low, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Low && ProdCost %is% Medium && MunCoverage %is% Medium, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Low && ProdCost %is% Medium && MunCoverage %is% High, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Low && ProdCost %is% High && MunCoverage %is% Low, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Low && ProdCost %is% Undetermined && MunCoverage %is% High, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Low && ProdCost %is% Low && MunCoverage %is% Low, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Low && ProdCost %is% Low && MunCoverage %is% Medium, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Low && ProdCost %is% Low && MunCoverage %is% High, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Low && ProdCost %is% Undetermined && MunCoverage %is% Low, EconomicImportance %is% NoDecision),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Low && ProdCost %is% Undetermined && MunCoverage %is% Medium, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Low && ProdCost %is% High && MunCoverage %is% High, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Medium && ProdCost %is% Low && MunCoverage %is% Undetermined, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Medium && ProdCost %is% Medium && MunCoverage %is% Undetermined, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Medium && ProdCost %is% High && MunCoverage %is% Undetermined, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Medium && ProdCost %is% Undetermined && MunCoverage %is% Undetermined, EconomicImportance %is% NoDecision),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Medium && ProdCost %is% Medium && MunCoverage %is% Medium, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Medium && ProdCost %is% Low && MunCoverage %is% Medium, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Medium && ProdCost %is% Low && MunCoverage %is% High, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Medium && ProdCost %is% Medium && MunCoverage %is% Low, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Medium && ProdCost %is% Medium && MunCoverage %is% High, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Medium && ProdCost %is% Undetermined && MunCoverage %is% Low, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Medium && ProdCost %is% Undetermined && MunCoverage %is% Medium, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Medium && ProdCost %is% Undetermined && MunCoverage %is% High, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Medium && ProdCost %is% Low && MunCoverage %is% Low, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Medium && ProdCost %is% High && MunCoverage %is% Low, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Medium && ProdCost %is% High && MunCoverage %is% Medium, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Medium && ProdCost %is% High && MunCoverage %is% High, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Undetermined && ProdCost %is% Low && MunCoverage %is% Low, EconomicImportance %is% NoDecision),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Undetermined && ProdCost %is% Low && MunCoverage %is% Medium, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Undetermined && ProdCost %is% Low && MunCoverage %is% High, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Undetermined && ProdCost %is% Medium && MunCoverage %is% Low, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Undetermined && ProdCost %is% Medium && MunCoverage %is% Medium, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Undetermined && ProdCost %is% Medium && MunCoverage %is% High, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Undetermined && ProdCost %is% High && MunCoverage %is% Low, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Undetermined && ProdCost %is% High && MunCoverage %is% Medium, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Undetermined && ProdCost %is% High && MunCoverage %is% High, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Undetermined && ProdCost %is% Undetermined && MunCoverage %is% Low, EconomicImportance %is% NoDecision),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Undetermined && ProdCost %is% Undetermined && MunCoverage %is% Medium, EconomicImportance %is% NoDecision),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Undetermined && ProdCost %is% Undetermined && MunCoverage %is% High, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Undetermined && ProdCost %is% Undetermined && MunCoverage %is% Undetermined, EconomicImportance %is% NoDecision),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Undetermined && ProdCost %is% Low && MunCoverage %is% Undetermined, EconomicImportance %is% NoDecision),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Undetermined && ProdCost %is% Medium && MunCoverage %is% Undetermined, EconomicImportance %is% NoDecision),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% Undetermined && ProdCost %is% High && MunCoverage %is% Undetermined, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% High && ProdCost %is% Low && MunCoverage %is% Undetermined, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% High && ProdCost %is% Medium && MunCoverage %is% Undetermined, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% High && ProdCost %is% High && MunCoverage %is% Undetermined, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% High && ProdCost %is% Undetermined && MunCoverage %is% Undetermined, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% High && ProdCost %is% Low && MunCoverage %is% Medium, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% High && ProdCost %is% Low && MunCoverage %is% High, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% High && ProdCost %is% Undetermined && MunCoverage %is% Low, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% High && ProdCost %is% Low && MunCoverage %is% Low, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% High && ProdCost %is% Medium && MunCoverage %is% Low, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% High && ProdCost %is% Medium && MunCoverage %is% Medium, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% High && ProdCost %is% Medium && MunCoverage %is% High, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% High && ProdCost %is% High && MunCoverage %is% Low, EconomicImportance %is% Medium),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% High && ProdCost %is% High && MunCoverage %is% Medium, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% High && ProdCost %is% High && MunCoverage %is% High, EconomicImportance %is% High),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% High && ProdCost %is% Undetermined && MunCoverage %is% Medium, EconomicImportance %is% Low),
  fuzzy_rule(LafayIndex %is% Undetermined && Yield %is% High && ProdCost %is% Undetermined && MunCoverage %is% High, EconomicImportance %is% Medium)
  
)

EconomicFuzzyModel <- fuzzy_system(EconomicVars, EconomicRules)

#png(filename="EconomicFM_MemFun.png")
print(EconomicFuzzyModel)
#par(mar = rep(2,4))
#plot(EconomicFuzzyModel)
#dev.off()

par(mar = rep(2,4))
plot(EconomicFuzzyModel)


df <- select(d, id,
             nombre_cientifico, 
             categoria, 
             clasificacion_FAO,
             cultivo,
             indice_lafay, 
             LafayIndex, 
             rendimiento, 
             Yield, 
             valor_produccion_h, 
             ProdCost, 
             porc_municipios, 
             norm_mun_cov, 
             MunCoverage)

df2 <- data.frame(id = as.numeric(df$id),
                  nombre_cientifico = df$nombre_cientifico, 
                  categoria = df$categoria, 
                  clasificacion_FAO =df$clasificacion_FAO,
                  cultivo = df$cultivo,
                  indice_lafay = as.numeric(format(df$indice_lafay, digits = 3)), 
                  LafayIndex = as.numeric(format(df$LafayIndex, digits = 3)), 
                  rendimiento = as.numeric(format(df$rendimiento, digits = 3)), 
                  Yield = as.numeric(format(df$Yield, digits = 3)), 
                  valor_produccion_h = as.numeric(format(df$valor_produccion_h, digits = 7)), 
                  ProdCost = as.numeric(format(df$ProdCost, digits = 3)), 
                  porc_municipios = as.numeric(format(df$porc_municipios, digits = 3)), 
                  norm_mun_cov = as.numeric(format(df$norm_mun_cov, digits = 3)), 
                  MunCoverage = as.numeric(format(df$MunCoverage, digits = 3)))

df2 <- df2[order(df2$id),]

df2$rownums <- rownames(df2)
df2$rownums <- as.numeric(df2$rownums)


df2 <- dplyr::filter(df2, rownums >=lim_inf & rownums <= lim_sup)

#df2 <- dplyr::filter(df2, id == 222 | id == 227)

#df <- dplyr::filter(df, id = 3 | df == 7 | df == 15 | df == 18 | 
#                      df == 27 | df == 42 | df == 56 | df == 75 | df == 89 | 
#                      df == 121 | df == 131 | df == 201 | df == 219 | df == 303)

#df <- dplyr::filter(df, id == 18 | id == 75 | id == 143 | id == 165 | id == 220 | id == 222 | id == 227)




df2$rownm <- rownames(df2)
df2$rownm <- as.numeric(df2$rownm)

View(df2)

fuzzyEco <- function(y){
  print(paste("fuzzy Economic Index for ",df2[y,'nombre_cientifico']))
  print(paste0('LafayIndex = ', df2[y,'LafayIndex']))
  print(paste0('Yield = ', df2[y,'Yield']))
  print(paste0('ProdCost = ', df2[y,'ProdCost']))
  print(paste0('MunCoverage = ', df2[y,'MunCoverage']))
  
  fuzzify <- sets::fuzzy_inference(EconomicFuzzyModel, list(LafayIndex = df2[y,'LafayIndex'],
                                                            Yield = df2[y,'Yield'], 
                                                            ProdCost = df2[y,'ProdCost'], 
                                                            MunCoverage = df2[y,'MunCoverage']))
  print(gset_defuzzify(fuzzify, "largestofmax"))
  gset_defuzzify(fuzzify, "largestofmax")
  
}


df2$EconomicImportance <- sapply(df2$rownm, fuzzyEco) 
df2$EconomicImportance <-  as.numeric(format(df2$EconomicImportance, digits=5))


write.csv2(df2,output_name,sep = ",")

