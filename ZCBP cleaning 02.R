###########ZCBP ##############
#Packages and importing of data
library(readr)
library(dplyr)
library(haven)
library(data.table)
library(reshape2)
# zbp02detail_o <- read_csv('~/Box Sync/nyc data science academy/project 1/data/2002/zbp02detail.txt')
# zip_2010_o <- read_csv("~/Box Sync/nyc data science academy/project 1/data/2010/zip 2010 long.csv")[-1,c(5,13,61)]
# save(zip_2010_o, file = 'R code/zip_2010.Rda')
# save(zbp02detail_o, file = 'R code/zbp02detail_o.Rda')
#load data
rm(list = ls())
setwd("/Users/williamkye/Box Sync/nyc data science academy/project 1")
load('R code/zbp02detail_o.Rda')

#create data to filter and rename naics code
naics<- c('722110', '722212', '722213', '722211', '311812', '722410', '445120','447110', '452910', '445110', '445310')
naics_names02<- c('restaurant02', 'cafeterias02', 'dessert02', 'fastfood02', 'bakery02', 'bars02', 'conv02', 'gasconv02', 'warehouse02', 'grocery02', 'liquor02')
naics02 <-data.frame(naics, naics_names02)

#convert factor into string
naics02<- data.frame(lapply(naics02, as.character), stringsAsFactors = FALSE)
names(zbp02detail_o)<-tolower(names(zbp02detail_o))

#merge zbp data with naics data to subset data
#merge zbp data with naics data to subset data
zip <- data.frame(zbp02detail_o$zip)
names(zip)<-'zip'
zip<-unique(zip)
zip$drop<-'drop'
zip$zip<-as.character(zip$zip)
zbp02detail<- inner_join(zbp02detail_o, naics02, by = "naics")
zbp02detail<- full_join(zip, zbp02detail, by = 'zip')
zbp02detail<-zbp02detail[,-2]
zbp02detail[is.na(zbp02detail)]<-0
#save(zbp02detail, file = 'R code/zbp02detail.Rda')

#load data filtered by naics codes
#load('R code/zbp02detail.Rda')

#create state column
load('R code/zip_2010_o.Rda')
names(zip_2010_o)<- c('state', 'state_fips', 'zip')
#state_filter <- c('13', '24', '25', '17', '39', '48', '26', '55', '22', '06', '42', '53', '11', '36')
#filter data based on a list of states
zip_2010<- zip_2010_o #%>% filter(state_fips %in% state_filter) %>% data.frame()
#data is fucked up, have to remove duplicate zip codes
zip_2010<-zip_2010[!duplicated(zip_2010$zip),]
zbp02detail<- inner_join(zbp02detail, zip_2010, by = 'zip')

#PLEASE use data.table dcast
zbp02detailwide<-data.table::dcast(data.table(zbp02detail), zip~ naics_names02, value.var = c('est','n1_4', 'n5_9', 'n10_19', 'n20_49', 'n50_99', 'n100_249', 'n250_499', 'n500_999', 'n1000'))
state<- zbp02detail[!duplicated(zbp02detail$zip),c(1,14,15)]

#create wide data
zbp02detailwide<- inner_join(zbp02detailwide, state, by = "zip")
zbp02detailwide[is.na(zbp02detailwide)]<-0
#save(zbp02detailwide, file = "R code/zbp02detailwide.Rda")

#creating outcome variables
#load("R code/zbp02detailwide.Rda")
zbp02detailwide<-mutate(zbp02detailwide, food02 = est_bakery02 + est_cafeterias02 + est_dessert02 + est_fastfood02 + est_restaurant02)
zbp02detailwide<-mutate(zbp02detailwide, alcohol02 = est_bars02 + est_liquor02)
zbp02detailwide<-mutate(zbp02detailwide, freshfood02 = (est_grocery02 - (n1_4_grocery02+n5_9_grocery02 + n10_19_grocery02))+est_warehouse02)
zbp02detailwide<-mutate(zbp02detailwide, unfreshfood02 = n1_4_grocery02+n5_9_grocery02 + n10_19_grocery02+ est_conv02+est_gasconv02)

zbp02detailwide<-zbp02detailwide[,c(1,122:127)]

save(zbp02detailwide, file = 'R code/zbp02detailwide.Rda')
saveRDS(zbp02detailwide, 'R code/zbp02detailwide.rds')

