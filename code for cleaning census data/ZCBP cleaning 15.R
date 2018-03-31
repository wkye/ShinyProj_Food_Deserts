###########ZCBP ##############
#Packages and importing of data
library(readr)
library(dplyr)
library(haven)
library(data.table)
library(reshape2)
zbp15detail_o <- read_csv('~/Box Sync/nyc data science academy/project 1/data/2015/zbp15detail.txt')
zip_2010_o <- read_csv("~/Box Sync/nyc data science academy/project 1/data/2010/zip 2010 long.csv")[-1,c(5,13,61)]
save(zip_2010_o, file = 'R code/zip_2010.Rda')
save(zbp15detail_o, file = 'R code/zbp15detail_o.Rda')
#load data
rm(list = ls())
setwd("/Users/williamkye/Box Sync/nyc data science academy/project 1")
load('R code/zbp15detail_o.Rda')

#create data to filter and rename naics code
naics<- c('722511', '722514', '722515', '722513', '311812', '722410', '445120','447110', '452910', '445110', '445310')
naics_names15<- c('restaurant15', 'cafeterias15', 'dessert15', 'fastfood15', 'bakery15', 'bars15', 'conv15', 'gasconv15', 'warehouse15', 'grocery15', 'liquor15')
naics15 <-data.frame(naics, naics_names15)

#convert factor into string
naics15<- data.frame(lapply(naics15, as.character), stringsAsFactors = FALSE)
names(zbp15detail_o)<-tolower(names(zbp15detail_o))

#merge zbp data with naics data to subset data
zip <- data.frame(zbp15detail_o$zip)
names(zip)<-'zip'
zip<-unique(zip)
zip$drop<-'drop'
zip$zip<-as.character(zip$zip)
zbp15detail<- inner_join(zbp15detail_o, naics15, by = "naics")
zbp15detail<- full_join(zip, zbp15detail, by = 'zip')
zbp15detail<-zbp15detail[,-2]
zbp15detail[is.na(zbp15detail)]<-0
save(zbp15detail, file = 'R code/zbp15detail.Rda')

#load data filtered by naics codes
#load('R code/zbp15detail.Rda')

#create state column
load('R code/zip_2010_o.Rda')
names(zip_2010_o)<- c('state', 'state_fips', 'zip')
#state_filter <- c('13', '24', '25', '17', '39', '48', '26', '55', '22', '06', '42', '53', '11', '36')
#filter data based on a list of states
zip_2010<- zip_2010_o #%>% filter(state_fips %in% state_filter) %>% data.frame()
#data is fucked up, have to remove duplicate zip codes
zip_2010<-zip_2010[!duplicated(zip_2010$zip),]
zbp15detail<- inner_join(zbp15detail, zip_2010, by = 'zip')

#PLEASE use data.table dcast
zbp15detailwide<-data.table::dcast(data.table(zbp15detail), zip~ naics_names15, value.var = c('est','n1_4', 'n5_9', 'n10_19', 'n20_49', 'n50_99', 'n100_249', 'n250_499', 'n500_999', 'n1000'))
state<- zbp15detail[!duplicated(zbp15detail$zip),c(1,14,15)]

#create wide data
zbp15detailwide<- inner_join(zbp15detailwide, state, by = "zip")
zbp15detailwide[is.na(zbp15detailwide)]<-0
#save(zbp15detailwide, file = "R code/zbp15detailwide.Rda")

#creating outcome variables
#load("R code/zbp15detailwide.Rda")
zbp15detailwide<-mutate(zbp15detailwide, food15 = est_bakery15 + est_cafeterias15 + est_dessert15 + est_fastfood15 + est_restaurant15)
zbp15detailwide<-mutate(zbp15detailwide, alcohol15 = est_bars15 + est_liquor15)
zbp15detailwide<-mutate(zbp15detailwide, freshfood15 = (est_grocery15 - (n1_4_grocery15+n5_9_grocery15 + n10_19_grocery15))+est_warehouse15)
zbp15detailwide<-mutate(zbp15detailwide, unfreshfood15 = n1_4_grocery15+n5_9_grocery15 + n10_19_grocery15+ est_conv15+est_gasconv15)

zbp15detailwide<-zbp15detailwide[,c(1,122:127)]

save(zbp15detailwide, file = 'R code/zbp15detailwide.Rda')
saveRDS(zbp15detailwide, 'R code/zbp15detailwide.rds')

