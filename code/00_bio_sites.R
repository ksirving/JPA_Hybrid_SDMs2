### bioassessment sites

library(tidyverse)
library(tidyr)

getwd()

## upload data from SOC

csci <- read.csv("input_data/00_csci_delta_formatted_median_updated_Nov2021.csv")
head(csci)

csci <- csci %>%
  select(stationcode, latitude, longitude) %>%
  distinct()

dim(csci) ## 420

write.csv(csci, "output_data/00_bio_sites.csv")
