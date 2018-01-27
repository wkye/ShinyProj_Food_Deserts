###########ZCBP ##############
#Packages and importing of data
library(readr)
library(dplyr)
library(haven)
library(data.table)
library(reshape2)
# zbp07detail_o <- read_csv('~/Box Sync/nyc data science academy/project 1/data/2007/zbp07detail.txt')
# zip_2010_o <- read_csv("~/Box Sync/nyc data science academy/project 1/data/2010/zip 2010 long.csv")[-1,c(5,13,61)]
# save(zip_2010_o, file = 'R code/zip_2010.Rda')
# save(zbp07detail_o, file = 'R code/zbp07detail_o.Rda')
#load data
rm(list = ls())
setwd("/Users/williamkye/Box Sync/nyc data science academy/project 1")
load('R code/zbp07detail_o.Rda')

#create data to filter and rename naics code
naics<- c('722110', '722212', '722213', '722211', '311812', '722410', '445120','447110', '452910', '445110', '445310')
naics_names07<- c('restaurant07', 'cafeterias07', 'dessert07', 'fastfood07', 'bakery07', 'bars07', 'conv07', 'gasconv07', 'warehouse07', 'grocery07', 'liquor07')
naics07 <-data.frame(naics, naics_names07)

#convert factor into string
naics07<- data.frame(lapply(naics07, as.character), stringsAsFactors = FALSE)
names(zbp07detail_o)<-tolower(names(zbp07detail_o))

#merge zbp data with naics data to subset data
zip <- data.frame(zbp07detail_o$zip)
names(zip)<-'zip'
zip<-unique(zip)
zip$drop<-'drop'
zip$zip<-as.character(zip$zip)
zbp07detail<- inner_join(zbp07detail_o, naics07, by = "naics")
zbp07detail<- full_join(zip, zbp07detail, by = 'zip')
zbp07detail<-zbp07detail[,-2]
zbp07detail[is.na(zbp07detail)]<-0
#save(zbp07detail, file = 'R code/zbp07detail.Rda')

#load data filtered by naics codes
#load('R code/zbp07detail.Rda')

#create state column
load('R code/zip_2010_o.Rda')
names(zip_2010_o)<- c('state', 'state_fips', 'zip')
#state_filter <- c('13', '24', '25', '17', '39', '48', '26', '55', '22', '06', '42', '53', '11', '36')
#filter data based on a list of states
zip_2010<- zip_2010_o# %>% filter(state_fips %in% state_filter) %>% data.frame()
#data is fucked up, have to remove duplicate zip codes
zip_2010<-zip_2010[!duplicated(zip_2010$zip),]
zbp07detail<- inner_join(zbp07detail, zip_2010, by = 'zip')

#PLEASE use data.table dcast
zbp07detailwide<-data.table::dcast(data.table(zbp07detail), zip~ naics_names07, value.var = c('est','n1_4', 'n5_9', 'n10_19', 'n20_49', 'n50_99', 'n100_249', 'n250_499', 'n500_999', 'n1000'))
state<- zbp07detail[!duplicated(zbp07detail$zip),c(1,14,15)]

#create wide data
zbp07detailwide<- inner_join(zbp07detailwide, state, by = "zip")
zbp07detailwide[is.na(zbp07detailwide)]<-0
#save(zbp07detailwide, file = "R code/zbp07detailwide.Rda")

#creating outcome variables
#load("R code/zbp07detailwide.Rda")
zbp07detailwide<-mutate(zbp07detailwide, food07 = est_bakery07 + est_cafeterias07 + est_dessert07 + est_fastfood07 + est_restaurant07)
zbp07detailwide<-mutate(zbp07detailwide, alcohol07 = est_bars07 + est_liquor07)
zbp07detailwide<-mutate(zbp07detailwide, freshfood07 = (est_grocery07 - (n1_4_grocery07+n5_9_grocery07 + n10_19_grocery07))+est_warehouse07)
zbp07detailwide<-mutate(zbp07detailwide, unfreshfood07 = n1_4_grocery07+n5_9_grocery07 + n10_19_grocery07+ est_conv07+est_gasconv07)

zbp07detailwide<-zbp07detailwide[,c(1,122:127)]

save(zbp07detailwide, file = 'R code/zbp07detailwide.Rda')
saveRDS(zbp07detailwide, 'R code/zbp07detailwide.rds')

