### SDM code

library(tidyr)
library(tidyverse)

library(sdm)
library(usdm)
library(raster)

####### upload bio data

bio <- read.csv("output_data/02_species_occs_traits_clean.csv")
head(bio)
dim(bio)

length(unique(bio$stationcode)) ## 396

bio <- bio %>%
  dplyr::select(stationcode, Species, latitude, longitude) %>%
  distinct() ## 15882

str(bio)

spp_list <- sort(unique(bio$Species))

## define coords
coords <- bio[, c(1, 4,5)]
coords <- coords[!duplicated(coords),]
dim(coords) ## 396

########## upload flow data

flow <- read.csv( "output_data/02_delta_h_spp_sites.csv")
head(flow)
flow <- flow %>% dplyr::select(-X)
unique(flow$stationcode)

getwd()

## add coords to flow data 

sx2 <- full_join(coords, flow, by = "stationcode")
head(sx2)
s=1

  for(s in 1:length(spp_list)) {
  
  ## species data
  # s
  
  cat("Running species", s, "\n")
  ## define species
  sx <- spp_list[s]
  
  
  ## filter species, change to presence (1), remove duplicate sites - so left with only 1 occurence 
  df3 <- bio %>%
    filter(Species == sx) %>%
    mutate(Species = 1) %>%
    distinct() 
  head(df3)

  ## format p/a
  # colnames(df3)[2] <- paste(sx) ## species name as column name
  sitesx <- coords
  head(sitesx)
  head(data)
  
  ## add absences - i.e. other sampling sites
  data <- full_join(sitesx, df3, by=c("stationcode", "latitude", "longitude")) ## merge both dataframes by coord code
  data[is.na(data)] <- 0 ## change NAs to 0. df will have binary info now
  dim(data) ## 396
  

  df <- full_join(data, sx2, by=c("stationcode", "latitude", "longitude")) ## combine data for species and variables, creates format dataframe as needed for brt

  head(df)
  df <- df %>% dplyr::select(-c(stationcode))
  coordinates(df) <- c("latitude", "longitude")
  
  d2<-sdmData(Species~.,train=df) ## 
  d2
  
  m2<-sdm(~.,data=d2, methods=c('glm','tree','brt', 'fda','rbf' ),test.percent=30, replicatin="boot", n=10) ##replicatin="boot", n=10
  m2
  
  sts <- getEvaluation(m2, wtest="train", stat=c('TSS','Kappa','AUC', 'sensitivity', 'specificity', "threshold"),opt=2)
  
  sts$taxa <- sx
  sts
  try(sts$model[1:10] <- c("glm"),silent=T)
  try(sts$model[11:20] <- c("tree"),silent=T)
  try(sts$model[21:30] <- c("brt"),silent=T)
  try(sts$model[31:40] <- c("fda"),silent=T)
  try(sts$model[41:50] <- c("rbf"),silent=T)
  
  
  ## variable importance of train and testing data
  vi_train <- getVarImp(m2,id=1,wtest='training') # variable importance based on training dataset
  vi_test <- getVarImp(m2,id=1,wtest='test.dep')
  vi_train
  ## coerce to df and add species name
  vi_trainx <- as.data.frame(vi_train@varImportance)
  colnames(vi_trainx)[1] <- sx
  
  vi_testx <- as.data.frame(vi_test@varImportance)
  colnames(vi_testx)[1] <- sx
  
  
  
  ## write csv
  write.csv(vi_testx, paste(sx, "_var_imp_test.csv", sep=""))
  write.csv(vi_trainx, paste(sx, "_var_imp_train.csv", sep=""))
  
  # # ### create and save plots
  par(mar = rep(2, 4))
  jpeg(paste(sx,'_var_imp_auc_train.jpg', sep=""))
  plot(vi_train, 'auc')
  dev.off()
  #
  jpeg(paste(sx,'_acc_hyd_var_imp_cor_train.jpg', sep=""))
  plot(vi_train, 'cor')
  dev.off()
  #
  jpeg(paste(sx,'_acc_hyd_var_imp_auc_test.jpg', sep=""))
  plot(vi_test, 'auc')
  dev.off()
  #
  jpeg(paste(sx,'_acc_hyd_var_imp_cor_test.jpg', sep=""))
  plot(vi_test, 'cor')
  dev.off()
  
  jpeg(paste(sx,'_acc_hyd_roc.jpg', sep=""))
  roc(m2)
  dev.off()
  
  write.csv(sts, paste(sx, "_acc_hyd_stats.csv",sep=""))
  
  e1 <- ensemble(m2,newdata=preds,filename= paste(sx, "_acc_hyd_ensemble.grd",sep=""),
                 setting=list(method='weighted',stat='TSS',opt=2), overwrite=T) # paste(sx, "_uni_ensemble.tif",sep="")
  
  
  rm(e1)
  
}


