###########ZCBP ##############
#Packages and importing of data
library(readr)
library(dplyr)
library(haven)
library(data.table)
library(reshape2)
#zbp08detail_o <- read_csv('~/Box Sync/nyc data science academy/project 1/data/2008/zbp08detail.txt')
# zip_2010_o <- read_csv("~/Box Sync/nyc data science academy/project 1/data/2010/zip 2010 long.csv")[-1,c(5,13,61)]
# save(zip_2010_o, file = 'R code/zip_2010.Rda')
# save(zbp08detail_o, file = 'R code/zbp08detail_o.Rda')
#load data
rm(list = ls())
setwd("/Users/williamkye/Box Sync/nyc data science academy/project 1")
load('R code/zbp08detail_o.Rda')

#create data to filter and rename naics code
naics<- c('722110', '722212', '722213', '722211', '311812', '722410', '445120','447110', '452910', '445110', '445310')
naics_names08<- c('restaurant08', 'cafeterias08', 'dessert08', 'fastfood08', 'bakery08', 'bars08', 'conv08', 'gasconv08', 'warehouse08', 'grocery08', 'liquor08')
naics08 <-data.frame(naics, naics_names08)

#convert factor into string
naics08<- data.frame(lapply(naics08, as.character), stringsAsFactors = FALSE)
names(zbp08detail_o)<-tolower(names(zbp08detail_o))

#merge zbp data with naics data to subset data
zip <- data.frame(zbp08detail_o$zip)
names(zip)<-'zip'
zip<-unique(zip)
zip$drop<-'drop'
zip$zip<-as.character(zip$zip)
zbp08detail<- inner_join(zbp08detail_o, naics08, by = "naics")
zbp08detail<- full_join(zip, zbp08detail, by = 'zip')
zbp08detail<-zbp08detail[,-2]
zbp08detail[is.na(zbp08detail)]<-0
#save(zbp08detail, file = 'R code/zbp08detail.Rda')

#load data filtered by naics codes
#load('R code/zbp08detail.Rda')

#create state column
load('R code/zip_2010_o.Rda')
names(zip_2010_o)<- c('state', 'state_fips', 'zip')
#state_filter <- c('13', '24', '25', '17', '39', '48', '26', '55', '22', '06', '42', '53', '11', '36')
#filter data based on a list of states
zip_2010<- zip_2010_o #%>% filter(state_fips %in% state_filter) %>% data.frame()
#data is fucked up, have to remove duplicate zip codes
zip_2010<-zip_2010[!duplicated(zip_2010$zip),]
zbp08detail<- inner_join(zbp08detail, zip_2010, by = 'zip')

#PLEASE use data.table dcast
zbp08detailwide<-data.table::dcast(data.table(zbp08detail), zip~ naics_names08, value.var = c('est','n1_4', 'n5_9', 'n10_19', 'n20_49', 'n50_99', 'n100_249', 'n250_499', 'n500_999', 'n1000'))
state<- zbp08detail[!duplicated(zbp08detail$zip),c(1,14,15)]

#create wide data
zbp08detailwide<- inner_join(zbp08detailwide, state, by = "zip")
zbp08detailwide[is.na(zbp08detailwide)]<-0
#save(zbp08detailwide, file = "R code/zbp08detailwide.Rda")

#creating outcome variables
#load("R code/zbp08detailwide.Rda")
zbp08detailwide<-mutate(zbp08detailwide, food08 = est_bakery08 + est_cafeterias08 + est_dessert08 + est_fastfood08 + est_restaurant08)
zbp08detailwide<-mutate(zbp08detailwide, alcohol08 = est_bars08 + est_liquor08)
zbp08detailwide<-mutate(zbp08detailwide, freshfood08 = (est_grocery08 - (n1_4_grocery08+n5_9_grocery08 + n10_19_grocery08))+est_warehouse08)
zbp08detailwide<-mutate(zbp08detailwide, unfreshfood08 = n1_4_grocery08+n5_9_grocery08 + n10_19_grocery08+ est_conv08+est_gasconv08)

zbp08detailwide<-zbp08detailwide[,c(1,122:127)]

save(zbp08detailwide, file = 'R code/zbp08detailwide.Rda')
saveRDS(zbp08detailwide, 'R code/zbp08detailwide.rds')

