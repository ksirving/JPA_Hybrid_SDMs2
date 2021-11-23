## data formatting

## packages
library(tidyverse)
library(tidyr)
library(CSCI)     

## regional sites - add more from SOC
bio_sites <- read.csv("output_data/00_bio_sites.csv")
head(bio_sites)


## occurrences - statewide

occs <- read.csv("input_data/all_tax_data.csv")
head(occs)

## get only sites
state_sites <- occs %>%
  select(stationcode) %>%
  distinct()

dim(state_sites) ## 6186

## number of sites with bio data
sum(state_sites$stationcode %in% bio_sites$stationcode) ## 396

## subset state data to reg bio sites

occsSub <- occs %>%
  select(stationcode, sampledate, replicate, finalid) %>%
  distinct()

head(occsSub)

occsSub <- occsSub %>%
  filter(stationcode %in% bio_sites$stationcode)
  
## count how many species per site
length(occsSub$finalid)

str(occsSub)
sppcount <- occsSub %>%
  group_by(stationcode) %>%
  summarise(sppcount = length(finalid))

head(sppcount)

#### how many species?

species_known <- unique(occsSub$finalid) # 718

## uplaod traits from csci package
mydf<-loadMetaData()          
head(mydf)

unique(mydf$FunctionalFeedingGroup)

traits <- mydf %>%
  select(FinalID:Subphylum, Invasive, Source)

species_traits <- read.csv("output_data/01_species_tolerance_FFG_df.csv")
head(species_traits)
## remove  occurrence < 50

species_traits_red <- species_traits %>%
  filter(!count < 50) %>%
  select(-Species) %>%
  distinct()

head(species_traits_red)  
species_have_traits <- unique(species_traits_red$FinalID) ## 145

## how many species with trait data and found in bio sites

sum(species_have_traits %in% species_known) ## 145

## subset occurrence data to species that have traits
names(occsSub)
species_to_use <- occsSub %>%
  filter(finalid %in% species_have_traits) %>%
  rename(FinalID = finalid)

dim(species_to_use)

unique(species_to_use$FinalID) ## 145 yay!

## combine traits data with occurrence data 

all_data <- full_join(species_to_use, species_traits_red, by = "FinalID")
dim(all_data)
## add lat lon from bio sites

all_data <- full_join(all_data, bio_sites, by="stationcode")

write.csv(all_data, "output_data/02_species_occs_traits.csv")

all_data <- read.csv("output_data/02_species_occs_traits.csv")
head(all_data)
sort(unique(all_data$FinalID))

## remove any species with / e.g. Caloparyphus/ Euparyphus - assuming could be either species?
## combine species: Baetis, Cricotopus, Chironomus, Hydropsyche, Hydroptila, Simulium, Tricorythodes, Thienemanniella, Rhyacophila"

clean_data <- all_data %>% 
  filter(!FinalID %in% c("Bezzia/ Palpomyia","Caloparyphus/ Euparyphus", 
                        "Pericoma/ Telmatoscopus", "Zavrelimyia/ Paramerina")) %>% ## remove species with /
  mutate(Species = case_when(FinalID %in% c("Baetis", "Baetis adonis", "Baetis tricaudatus") ~ "Baetis",
                             FinalID %in% c("Cricotopus", "Cricotopus bicinctus group", "Cricotopus trifascia group") ~ "Cricotopus",
                             FinalID %in% c("Cryptochironomus", "Chironomidae", "Chironomini") ~ "Chironomidae",
                             FinalID %in% c("Hydropsyche", "Hydropsychidae") ~ "Hydropsychidae",
                             FinalID %in% c("Hydroptila", "Hydroptilidae") ~ "Hydroptilidae",
                             FinalID %in% c("Rhyacophila", "Rhyacophila betteni group") ~ "Rhyacophila",
                             FinalID %in% c("Tricorythodes", "Tricorythodes explicatus") ~ "Tricorythodes",
                             FinalID %in% c("Simulium", "Simulium argus", "Simulium piperi", "Simulium hippovorum") ~ "Simulium",
                             FinalID %in% c("Thienemanniella", "Thienemannimyia group") ~ "Thienemanniella",
                             FinalID %in% c("Microtendipes", "Microtendipes pedellus group", "Microtendipes rydalensis group") ~ "Microtendipes")) %>%
  mutate(FinalID2 = ifelse(is.na(Species), FinalID, Species)) %>%
  select(-FinalID, -Species) %>% rename(Species = FinalID2)


head(clean_data)
sort(unique(clean_data$FinalID))
sort(unique(clean_data$Species)) ## 125

unique(clean_data$sampledate)
unique(clean_data$replicate)
## take only 1 rep - take only rep 1 for now, change later

clean_data2 <- clean_data %>%
  filter(!replicate == 2)

dim(clean_data2) ## 36019
dim(clean_data) ## 38934

## save
write.csv(clean_data2, "output_data/02_species_occs_traits_clean.csv")

## check number of occurrences

tally <- clean_data2 %>%
  group_by(Species) %>%
  summarise(occs = length(stationcode))

tally



# env data ----------------------------------------------------------------

dh_data <- read.csv("/Volumes/Biology/San Juan WQIP_KTQ/Data/Working/Regional_Curves_CSCI_ASCI_Annie/Data/Flow/subset_woutLARsitematches/DeltaH_FINAL/deltaH_summary_badyrs_nonzero.csv")
head(dh_data)

all_data <- read.csv( "output_data/02_species_occs_traits_clean.csv")

spp_sites <- unique(all_data$stationcode)
spp_sites ## 396

dh_median <- dh_data %>%
  filter(summary.statistic =="median") %>%
  rename(stationcode = site) %>%
  filter(stationcode %in% spp_sites) %>%
  dplyr::select(stationcode, flow_metric, deltaH) %>%
  pivot_wider(names_from = "flow_metric", values_from = "deltaH")

head(dh_median)
## use median of current ffm value?

write.csv(dh_median, "output_data/02_delta_h_spp_sites.csv") 
## loads of NAs, what do do? remove site if 70% od data missing? remove metric if 70% data missing?


