###########ZCBP ##############
#Packages and importing of data
library(readr)
library(dplyr)
library(haven)
library(data.table)
library(reshape2)
# zbp14detail_o <- read_csv('~/Box Sync/nyc data science academy/project 1/data/2014/zbp14detail.txt')
# zip_2010_o <- read_csv("~/Box Sync/nyc data science academy/project 1/data/2010/zip 2010 long.csv")[-1,c(5,13,61)]
# save(zip_2010_o, file = 'R code/zip_2010.Rda')
# save(zbp14detail_o, file = 'R code/zbp14detail_o.Rda')
#load data
rm(list = ls())
setwd("/Users/williamkye/Box Sync/nyc data science academy/project 1")
load('R code/zbp14detail_o.Rda')

#create data to filter and rename naics code
naics<- c('722511', '722514', '722515', '722513', '311812', '722410', '445120','447110', '452910', '445110', '445310')
naics_names14<- c('restaurant14', 'cafeterias14', 'dessert14', 'fastfood14', 'bakery14', 'bars14', 'conv14', 'gasconv14', 'warehouse14', 'grocery14', 'liquor14')
naics14 <-data.frame(naics, naics_names14)

#convert factor into string
naics14<- data.frame(lapply(naics14, as.character), stringsAsFactors = FALSE)
names(zbp14detail_o)<-tolower(names(zbp14detail_o))

#merge zbp data with naics data to subset data
zip <- data.frame(zbp14detail_o$zip)
names(zip)<-'zip'
zip<-unique(zip)
zip$drop<-'drop'
zip$zip<-as.character(zip$zip)
zbp14detail<- inner_join(zbp14detail_o, naics14, by = "naics")
zbp14detail<- full_join(zip, zbp14detail, by = 'zip')
zbp14detail<-zbp14detail[,-2]
zbp14detail[is.na(zbp14detail)]<-0

#load data filtered by naics codes
#load('R code/zbp14detail.Rda')

#create state column
load('R code/zip_2010_o.Rda')
names(zip_2010_o)<- c('state', 'state_fips', 'zip')
#state_filter <- c('13', '24', '25', '17', '39', '48', '26', '55', '22', '06', '42', '53', '11', '36')
#filter data based on a list of states
zip_2010<- zip_2010_o# %>% filter(state_fips %in% state_filter) %>% data.frame()
#data is fucked up, have to remove duplicate zip codes
zip_2010<-zip_2010[!duplicated(zip_2010$zip),]
zbp14detail<- inner_join(zbp14detail, zip_2010, by = 'zip')

#PLEASE use data.table dcast
zbp14detailwide<-data.table::dcast(data.table(zbp14detail), zip~ naics_names14, value.var = c('est','n1_4', 'n5_9', 'n10_19', 'n20_49', 'n50_99', 'n100_249', 'n250_499', 'n500_999', 'n1000'))
state<- zbp14detail[!duplicated(zbp14detail$zip),c(1,14,15)]

#create wide data
zbp14detailwide<- inner_join(zbp14detailwide, state, by = "zip")
zbp14detailwide[is.na(zbp14detailwide)]<-0
#save(zbp14detailwide, file = "R code/zbp14detailwide.Rda")

#creating outcome variables
#load("R code/zbp14detailwide.Rda")
zbp14detailwide<-mutate(zbp14detailwide, food14 = est_bakery14 + est_cafeterias14 + est_dessert14 + est_fastfood14 + est_restaurant14)
zbp14detailwide<-mutate(zbp14detailwide, alcohol14 = est_bars14 + est_liquor14)
zbp14detailwide<-mutate(zbp14detailwide, freshfood14 = (est_grocery14 - (n1_4_grocery14+n5_9_grocery14 + n10_19_grocery14))+est_warehouse14)
zbp14detailwide<-mutate(zbp14detailwide, unfreshfood14 = n1_4_grocery14+n5_9_grocery14 + n10_19_grocery14+ est_conv14+est_gasconv14)

zbp14detailwide<-zbp14detailwide[,c(1,122:127)]

save(zbp14detailwide, file = 'R code/zbp14detailwide.Rda')
saveRDS(zbp14detailwide, 'R code/zbp14detailwide.rds')

