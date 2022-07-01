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


output_name <- "data_bgv.csv"



options(digits=4)


data_indice <- read.csv2("data_noimp_bgv.csv", stringsAsFactors = FALSE)

rep <- read.csv2("representantes.csv", stringsAsFactors = FALSE)
rep$cultivo <- as.factor(rep$cultivo)

#data_indice$rownm <- row.names(data_indice)

seq <- c(1:dim(data_indice)[1])

crops <- levels(rep$cultivo)

#### ECO ####

data_indice$imp_economica <- NA
data_indice$pool_imp_economica <- NA

for (i in seq) {
  print(data_indice[i,'nombre_cientifico'])
  if (data_indice[i, "imp_economica_org"] != 8) {
    print(paste0("Imputation for ",data_indice[i,'nombre_cientifico'],", not necessary"))
    data_indice[i,"imp_economica"] <- data_indice[i, "imp_economica_org"]
    data_indice[i,"pool_imp_economica"] <- "No"
  } else if (data_indice[i, "imp_economica_org"] == 8){
    if (data_indice[i, "cultivo"] %in% crops){
      repTemp <- dplyr::filter(rep, cultivo == data_indice[i, "cultivo"])
      #print(repTemp[1,"Species"])
      dataTemp <- dplyr::filter(data_indice, nombre_cientifico == repTemp[1,"Species"])
      
      data_indice[i,"imp_economica"] <- dataTemp[1,"imp_economica_org"]
      #data_indice[i,"pool_imp_economica"] <- repTemp[1,"poolIdx"]
      if (dataTemp[1,"imp_economica_org"] == 8) {
        data_indice[i,"pool_imp_economica"] <- "No"
      } else {
        data_indice[i,"pool_imp_economica"] <- repTemp[1,"poolIdx"]
      }
      
      
    } else {
      data_indice[i,"imp_economica"] <- data_indice[i, "imp_economica_org"]
      data_indice[i,"pool_imp_economica"] <- "No"
    }
      
  } 
}

data_indice$seg_alimentaria <- NA
data_indice$pool_imp_sa <- NA

for (i in seq) {
  print(data_indice[i,'nombre_cientifico'])
  if (data_indice[i, "seg_alimentaria_org"] != 8) {
    print(paste0("Imputation for ",data_indice[i,'nombre_cientifico'],", not necessary"))
    data_indice[i,"seg_alimentaria"] <- data_indice[i, "seg_alimentaria_org"]
    data_indice[i,"pool_imp_sa"] <- "No"
  } else if (data_indice[i, "seg_alimentaria_org"] == 8){
    if (data_indice[i, "cultivo"] %in% crops){
      repTemp <- dplyr::filter(rep, cultivo == data_indice[i, "cultivo"])
      #print(repTemp[1,"Species"])
      dataTemp <- dplyr::filter(data_indice, nombre_cientifico == repTemp[1,"Species"])
      
      
      data_indice[i,"seg_alimentaria"] <- dataTemp[1,"seg_alimentaria_org"]
      if (dataTemp[1,"seg_alimentaria_org"] == 8) {
        data_indice[i,"pool_imp_sa"] <- "No"
      } else {
        data_indice[i,"pool_imp_sa"] <- repTemp[1,"poolIdx"]
      }
        
      
    } else {
      data_indice[i,"seg_alimentaria"] <- data_indice[i, "seg_alimentaria_org"]
      data_indice[i,"pool_imp_sa"] <- "No"
    }
    
  } 
}

write.csv(x = data_indice, file = "data_int_banco_2508_imp.csv",sep = ";",row.names = FALSE)
