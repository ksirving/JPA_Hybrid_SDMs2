### SDM code

library(tidyr)
library(tidyverse)

library(sdm)
library(usdm)
library(raster)
getwd()
####### upload bio data

bio <- read.csv("output_data/02_species_occs_traits_clean.csv")
head(bio)
dim(bio)

length(unique(bio$stationcode)) ## 419

bio <- bio %>%
  dplyr::select(stationcode, Species, latitude, longitude) %>%
  distinct() ## 15882

str(bio)

spp_list <- sort(unique(bio$Species))

## define coords
coords <- bio[, c(1,3,4)]
coords <- coords[!duplicated(coords),]
dim(coords) ## 

########## upload flow data - regional

flow <- read.csv( "output_data/02_delta_h_spp_sites.csv")
head(flow)
flow <- flow %>% dplyr::select(-X)
unique(flow$stationcode)

sort(colnames(flow)[-1])

########## upload flow data - SOC

SOC <- read_rds("output_data/02_delta_h_SOC_sites.rds")
head(SOC)
names(SOC)
class(SOC)
# view(SOC)
SOC <- as.data.frame(SOC)

preds_df <- SOC %>%
  dplyr::select(site, region, Subbasin, COMID)

head(preds_df)
class(preds_df)

# SOC <- as.data.frame(SOC[,-c(1:2, 19:20)])

sort(colnames(SOC[c(3:18)]))

getwd()

sdmdir <- "/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/JPA_Hybrid_SDMs2/SDM/Results/"

## add coords to flow data 

sx2 <- full_join(coords, flow, by = "stationcode")
head(sx2)
s=1
s
predsx <- NULL

for(s in 109:length(spp_list)) {
  
  ## species data
  
  
  cat("Running species", s, "\n")
  ## define species
  sx <- spp_list[s]
  
  
  ## filter species, change to presence (1), remove duplicate sites - so left with only 1 occurence 
  df3 <- bio %>%
    filter(Species == sx) %>%
    mutate(Species = 1) %>%
    distinct() 
  # head(df3)
  
  ## format p/a
  # colnames(df3)[2] <- paste(sx) ## species name as column name
  sitesx <- coords
  # head(sitesx)
  
  
  ## add absences - i.e. other sampling sites
  data <- full_join(sitesx, df3, by=c("stationcode", "latitude", "longitude")) ## merge both dataframes by coord code
  data[is.na(data)] <- 0 ## change NAs to 0. df will have binary info now
  # dim(data) ## 396
  # head(data)
  
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
  
  # ?getVarImp
  ## variable importance of train and testing data
  vi_train <- getVarImp(m2,id=1:5,wtest='training') # variable importance based on training dataset
  vi_test <- getVarImp(m2,id=1:5,wtest='test.dep')
  # vi_train@varImportanceList
  ## coerce to df and add species name
  vi_trainx <- as.data.frame(vi_train@varImportanceMean)
  colnames(vi_trainx)[1] <- sx
  
  vi_testx <- as.data.frame(vi_test@varImportanceMean)
  colnames(vi_testx)[1] <- sx
  
  
  ## write csv
  write.csv(vi_testx, paste0(sdmdir,sx, "_var_imp_test.csv"))
  write.csv(vi_trainx, paste0(sdmdir,sx, "_var_imp_train.csv"))
  
  # # ### create and save plots
  par(mar = rep(2, 4))
  jpeg(paste0(sdmdir,sx, '_var_imp_auc_train.jpg'))
  plot(vi_train, 'auc')
  dev.off()
  #
  jpeg(paste0(sdmdir,sx, '_var_imp_cor_train.jpg'))
  plot(vi_train, 'cor')
  dev.off()
  #
  jpeg(paste0(sdmdir,sx, '_var_imp_auc_test.jpg'))
  plot(vi_test, 'auc')
  dev.off()
  #
  jpeg(paste0(sdmdir,sx, '_var_imp_cor_test.jpg'))
  plot(vi_test, 'cor')
  dev.off()
  
  jpeg(paste0(sdmdir,sx, '_roc.jpg'))
  roc(m2)
  dev.off()
  
  write.csv(sts, paste0(sdmdir,sx, "_stats.csv",sep=""))
  
  # ?ensemble
  ### need to add new data - LSPC flow!!!!!!!!!!!
  e1 <- ensemble(m2,newdata=SOC,filename= paste0(sdmdir,sx, "_ensemble.grd"),
                 setting=list(method='weighted',stat='TSS',opt=2), overwrite=T) # paste(sx, "_uni_ensemble.tif",sep="")
  #hi kt
  e1DF <- as.data.frame(e1)
  colnames(e1DF) <- paste(sx)
  # e1DF
  preds <- preds_df %>%
    mutate(Species = e1DF)
  
  # head(predsx)
  predsx <- cbind(predsx, preds[5])
  
}

head(predsx)
## save predictions

write.csv(predsx, "/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/JPA_Hybrid_SDMs2/SDM/Results/predictions/03_ensemble_predictions_SOC_all_species.csv" )

## stopped
## 65 


# Stats -------------------------------------------------------------------

setwd( "/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/JPA_Hybrid_SDMs2/SDM/Results/")
st <- list.files(pattern="stats")
length(st) ## 93
st
st[2]

# st <-st[-c(1)]
## creat dataframe
all_stats <- data.frame(matrix(nrow=4, ncol=12))
colnames(all_stats) <- c("sens_m", "sens_se", "TSS_m", "TSS_se", "Kappa_m", "Kappa_se", "sens_m", "sens_se", "spec_m", "spec_se", "species", "model")

head(all_stats)

## standard error function
std <- function(x) sd(x)/sqrt(length(x))
s=1
## read 1st file
stats <- read.csv(paste(st[s]))
head(stats)
dim(stats)
length(st)


## loop to add all files together
s
for(s in 1:length(st)) {
  
  statsx <- read.csv(paste(st[s]))
  # head(statsx)
  # dim(statsx)
  model <- unique(statsx$model)
  stats <- rbind(stats, statsx)
  
}

head(stats)

dim(stats) ## 
stats <- stats[, -1]

setwd("stats")

save(stats, file="all_stats.csv")

taxa_list <- unique(stats$taxa)

taxa_list

wdf <- data.frame(matrix(nrow=91, ncol=7))
colnames(wdf) <- c("species", "weighted_TSS", "weighted_SENS", "mean_TSS", "mean_SENS", "TSS_SE", "SENS_SE")

for(t in 1:length(taxa_list)) {
  
  tx <- paste(taxa_list[t])
  # tx
  
  tax_stats <- subset(stats, taxa==tx)
  head(tax_stats)
  # dim(tax_stats) ## 300   8
  tax_stats$potW <- tax_stats$TSS
  tax_stats$wTSS <- tax_stats$potW*tax_stats$TSS
  tax_stats$wsensitivity <- tax_stats$potW*tax_stats$sensitivity
  wdf[t, 1] <- paste(tx)
  wdf[t, 2] <- weighted.mean(tax_stats$TSS,tax_stats$potW) ## 0.61469
  wdf[t, 3] <- weighted.mean(tax_stats$sensitivity,tax_stats$potW) ## 0.8446933
  wdf[t, 4] <- mean(tax_stats$TSS)
  wdf[t, 5] <- mean(tax_stats$sensitivity)
  wdf[t, 6] <- std(tax_stats$TSS)
  wdf[t, 7] <- std(tax_stats$sensitivity)
}


wdf

# with(tax_stats, sum(tax_stats$TSS*tax_stats$potW)/sum(tax_stats$potW))
head(wdf)
tail(wdf)

# hC-H = mean TSS 
mean(wdf$weighted_TSS) 
mean(wdf$weighted_SENS)
mean(wdf$TSS_SE)
# TSS - 0.72 +/- 0.02
# SENS - 0.88

dim(wdf) # 93
wdf <- na.omit(wdf)
write.csv(wdf, "weighted_TSS_SENS.csv")


# Variable Importance -----------------------------------------------------
setwd( "/Users/katieirving/Documents/Documents - Katie’s MacBook Pro/git/JPA_Hybrid_SDMs2/SDM/Results/")


vp <- list.files(pattern="var_imp_train")

vp 


## rank variables first to last for each species

dfx <- NULL

v=2
## upload csvs one by one
for(v in 1:length(vp)) {
  
  var_imp_c <- read.csv(paste(vp[v]))
  
  top_vars_c <- var_imp_c[order(var_imp_c[,3], decreasing=TRUE),] ## order by corTest value
  
  SpeciesName <- colnames(top_vars_c)[2]
  colnames(top_vars_c)[2] <- "Species"
  
  
  df <- top_vars_c %>%
    dplyr::select(-X, -c(corTest.lower:AUCtest.upper)) %>%
    pivot_wider(names_from = Species, values_from = corTest.corTest) %>%
    mutate(Species = SpeciesName)
  
  
  dfx <- bind_rows(df, dfx)
  
  
}

dfx
getwd()

write.csv(acc_df, paste0(path,"stats/var_imp_per_species.csv"))



# Convert predictions to binary -------------------------------------------

#  convert probability map to binary

library(raster)
# species list
# path <- "/Users/katie/Documents/git/flow_clim_SDMs_germany/"
path <- "/Users/katieirving/Documents/git/flow_clim_SDMs_germany/"

spp_list<- taxa_list

# upload stream network
str_net_points <- shapefile("/Users/katieirving/Documents/manuscript3/sdms/wclim_acc_hydro/masked/binary/binary Alainites_muticus_wclim_acc_hyd_ensemble.shp")

#  upload predictions


en <- list.files(pattern="ensemble.grd")
en
sort(en)
#  upload thresholds
th <- list.files(pattern="stats")
th
th <- th[-12]
sort(th)
#  dataframe for threshold value and number of occurences
occs <- data.frame(matrix(ncol=3, nrow=93))
colnames(occs) <- c("species", "theshold_value", "no_occs")
# i=12
i =1
# 12
for(i in 1:length(spp_list)){
  #  take first raster of predictions
  en_r <- raster(en[i])
  
  #  take first threshold
  th_df <- read.csv(th[i])
  # th_df2 <- read.csv(th[9])
  
  #  mean threshold weighted on TSS
  th_v <- weighted.mean(th_df$threshold, th_df$TSS)
  
  #  mask raster by stream network
  en_rm <- mask(en_r, str_net_points)
  
  #  convert prediction raster to dataframe
  en_df <- as.data.frame(en_rm, na.rm=T, xy=T)
  head(en_df)
  dim(en_df)
  # colnames(en_df) <- c("layer", "x", "y")
  
  #  add column for binary predictions - if over threshold put 1, if under put 2
  en_df$binary <- as.numeric(as.character(ifelse(en_df$layer > th_v, paste("1"), paste("0"))))
  
  # 635
  occs[i,1] <- paste(spp_list[i])
  occs[i,2] <- th_v
  occs[i,3] <- sum(en_df$binary)
  #  convert binary to point file
  en_df <- en_df[c(1,2,4)]
  # head(en_df)
  coordinates(en_df) <- c("x", "y")
  shapefile(en_df, paste0("binary/binary_", en[i]), overwrite=T)
  
  
}
i

occs
write.csv(occs, "occs/acc_hyd_no_predicted_occs.csv")


