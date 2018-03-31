#Code for translating zip codes to msas
rm(list = ls())
library(readr)
library(dplyr)

#import data
#zcta_to_msa <- read_csv("~/Box Sync/nyc data science academy/project 1/data/zcta to msa.txt")
#msa_2010_acs <- read_csv("~/Box Sync/nyc data science academy/project 1/data/2010/MSA 2010 ACS.csv")[-2,c(1:4,13,27)]
#save(zcta_to_msa, file = 'R code/zcta_to_msa.Rda')
#save(msa_2010_acs, file = 'R code/msa_2010_acs.Rda')

#load data
load('R code/zcta_to_msa.Rda')
load('R code/msa_2010_acs.Rda')
load('R code/zbp00detail.Rda')
load("~/Box Sync/nyc data science academy/project 1/R code/zip_2010_o.Rda")

names(zip_2010_o)<- c('state', 'state_fips', 'zip')
state_filter <- c('13', '24', '25', '17', '39', '48', '26', '55', '22', '06', '42', '53', '11', '36', '12', '33', '04', '53','27', '08','29')

#filter data based on a list of states
zip_2010<- zip_2010_o %>% filter(state_fips %in% state_filter) %>% data.frame()
#data is fucked up, have to remove duplicate zip codes
zip_2010<-zip_2010[!duplicated(zip_2010$zip),]

#change name of data
names(msa_2010_acs)<-c(msa_2010_acs[1,])
msa_2010_acs<-msa_2010_acs[-1,]
names(zcta_to_msa)<- tolower(names(zcta_to_msa))

#filter relatinoship file so that zcta are unique. cutoff area of zip code in msa > 80. lost 3457 zip codes.  13 percent of zip codes
zcta_to_msa<- filter(zcta_to_msa, zareapct> 80)

#rename variable
zcta_to_msa<-zcta_to_msa[,1:2]
names(zcta_to_msa)<-c('zip', 'msa')
msa_2010_acs<-rename(msa_2010_acs, msa = Geo_CBSA)
zcta_to_msa$msa<-as.character(zcta_to_msa$msa)

#get state value onto zip codes. Zip codes in zcta_msa are unique, zip codes in zip_2010 are not
zcta_to_msa<-inner_join(zcta_to_msa, zip_2010, by = 'zip')
zcta_to_msa$Geo_FIPS<- paste(zcta_to_msa$state_fips, zcta_to_msa$msa, sep = '')

class(zcta_to_msa$Geo_FIPS)
class(msa_2010_acs$Geo_FIPS)
zip_msa<- inner_join(msa_2010_acs, zcta_to_msa, by = 'Geo_FIPS')
save(zip_msa, file='R code/zip_msa.Rda')

msa_2010_acs[duplicated(msa_2010_acs$Geo_FIPS),]
zcta_to_msa[duplicated(zcta_to_msa$Geo_FIPS),]


