###########ZCBP ##############
#Packages and importing of data
library(readr)
library(dplyr)
library(haven)
library(data.table)
library(reshape2)
# zbp05detail_o <- read_csv('~/Box Sync/nyc data science academy/project 1/data/2005/zbp05detail.txt')
# zip_2010_o <- read_csv("~/Box Sync/nyc data science academy/project 1/data/2010/zip 2010 long.csv")[-1,c(5,13,61)]
# save(zip_2010_o, file = 'R code/zip_2010.Rda')
# save(zbp05detail_o, file = 'R code/zbp05detail_o.Rda')
#load data
rm(list = ls())
setwd("/Users/williamkye/Box Sync/nyc data science academy/project 1")
load('R code/zbp05detail_o.Rda')

#create data to filter and rename naics code
naics<- c('722110', '722212', '722213', '722211', '311812', '722410', '445120','447110', '452910', '445110', '445310')
naics_names05<- c('restaurant05', 'cafeterias05', 'dessert05', 'fastfood05', 'bakery05', 'bars05', 'conv05', 'gasconv05', 'warehouse05', 'grocery05', 'liquor05')
naics05 <-data.frame(naics, naics_names05)

#convert factor into string
naics05<- data.frame(lapply(naics05, as.character), stringsAsFactors = FALSE)
names(zbp05detail_o)<-tolower(names(zbp05detail_o))

#merge zbp data with naics data to subset data
zip <- data.frame(zbp05detail_o$zip)
names(zip)<-'zip'
zip<-unique(zip)
zip$drop<-'drop'
zip$zip<-as.character(zip$zip)
zbp05detail<- inner_join(zbp05detail_o, naics05, by = "naics")
zbp05detail<- full_join(zip, zbp05detail, by = 'zip')
zbp05detail<-zbp05detail[,-2]
zbp05detail[is.na(zbp05detail)]<-0
#save(zbp05detail, file = 'R code/zbp05detail.Rda')

#load data filtered by naics codes
#load('R code/zbp05detail.Rda')

#create state column
load('R code/zip_2010_o.Rda')
names(zip_2010_o)<- c('state', 'state_fips', 'zip')
#state_filter <- c('13', '24', '25', '17', '39', '48', '26', '55', '22', '06', '42', '53', '11', '36')
#filter data based on a list of states
zip_2010<- zip_2010_o #%>% filter(state_fips %in% state_filter) %>% data.frame()
#data is fucked up, have to remove duplicate zip codes
zip_2010<-zip_2010[!duplicated(zip_2010$zip),]
zbp05detail<- inner_join(zbp05detail, zip_2010, by = 'zip')

#PLEASE use data.table dcast
zbp05detailwide<-data.table::dcast(data.table(zbp05detail), zip~ naics_names05, value.var = c('est','n1_4', 'n5_9', 'n10_19', 'n20_49', 'n50_99', 'n100_249', 'n250_499', 'n500_999', 'n1000'))
state<- zbp05detail[!duplicated(zbp05detail$zip),c(1,14,15)]

#create wide data
zbp05detailwide<- inner_join(zbp05detailwide, state, by = "zip")
zbp05detailwide[is.na(zbp05detailwide)]<-0
#save(zbp05detailwide, file = "R code/zbp05detailwide.Rda")

#creating outcome variables
#load("R code/zbp05detailwide.Rda")
zbp05detailwide<-mutate(zbp05detailwide, food05 = est_bakery05 + est_cafeterias05 + est_dessert05 + est_fastfood05 + est_restaurant05)
zbp05detailwide<-mutate(zbp05detailwide, alcohol05 = est_bars05 + est_liquor05)
zbp05detailwide<-mutate(zbp05detailwide, freshfood05 = (est_grocery05 - (n1_4_grocery05+n5_9_grocery05 + n10_19_grocery05))+est_warehouse05)
zbp05detailwide<-mutate(zbp05detailwide, unfreshfood05 = n1_4_grocery05+n5_9_grocery05 + n10_19_grocery05+ est_conv05+est_gasconv05)

zbp05detailwide<-zbp05detailwide[,c(1,122:127)]

save(zbp05detailwide, file = 'R code/zbp05detailwide.Rda')
saveRDS(zbp05detailwide, 'R code/zbp05detailwide.rds')

