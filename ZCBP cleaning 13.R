###########ZCBP ##############
#Packages and importing of data
library(readr)
library(dplyr)
library(haven)
library(data.table)
library(reshape2)
# zbp13detail_o <- read_csv('~/Box Sync/nyc data science academy/project 1/data/2013/zbp13detail.txt')
# zip_2010_o <- read_csv("~/Box Sync/nyc data science academy/project 1/data/2010/zip 2010 long.csv")[-1,c(5,13,61)]
# save(zip_2010_o, file = 'R code/zip_2010.Rda')
# save(zbp13detail_o, file = 'R code/zbp13detail_o.Rda')
#load data
rm(list = ls())
setwd("/Users/williamkye/Box Sync/nyc data science academy/project 1")
load('R code/zbp13detail_o.Rda')

#create data to filter and rename naics code
naics<- c('722511', '722514', '722515', '722513', '311812', '722410', '445120','447110', '452910', '445110', '445310')
naics_names13<- c('restaurant13', 'cafeterias13', 'dessert13', 'fastfood13', 'bakery13', 'bars13', 'conv13', 'gasconv13', 'warehouse13', 'grocery13', 'liquor13')
naics13 <-data.frame(naics, naics_names13)

#convert factor into string
naics13<- data.frame(lapply(naics13, as.character), stringsAsFactors = FALSE)
names(zbp13detail_o)<-tolower(names(zbp13detail_o))

#merge zbp data with naics data to subset data
zip <- data.frame(zbp13detail_o$zip)
names(zip)<-'zip'
zip<-unique(zip)
zip$drop<-'drop'
zip$zip<-as.character(zip$zip)
zbp13detail<- inner_join(zbp13detail_o, naics13, by = "naics")
zbp13detail<- full_join(zip, zbp13detail, by = 'zip')
zbp13detail<-zbp13detail[,-2]
zbp13detail[is.na(zbp13detail)]<-0

#load data filtered by naics codes
#load('R code/zbp13detail.Rda')

#create state column
load('R code/zip_2010_o.Rda')
names(zip_2010_o)<- c('state', 'state_fips', 'zip')
#state_filter <- c('13', '24', '25', '17', '39', '48', '26', '55', '22', '06', '42', '53', '11', '36')
#filter data based on a list of states
zip_2010<- zip_2010_o #%>% filter(state_fips %in% state_filter) %>% data.frame()
#data is fucked up, have to remove duplicate zip codes
zip_2010<-zip_2010[!duplicated(zip_2010$zip),]
zbp13detail<- inner_join(zbp13detail, zip_2010, by = 'zip')

#PLEASE use data.table dcast
zbp13detailwide<-data.table::dcast(data.table(zbp13detail), zip~ naics_names13, value.var = c('est','n1_4', 'n5_9', 'n10_19', 'n20_49', 'n50_99', 'n100_249', 'n250_499', 'n500_999', 'n1000'))
state<- zbp13detail[!duplicated(zbp13detail$zip),c(1,14,15)]

#create wide data
zbp13detailwide<- inner_join(zbp13detailwide, state, by = "zip")
zbp13detailwide[is.na(zbp13detailwide)]<-0
#save(zbp13detailwide, file = "R code/zbp13detailwide.Rda")

#creating outcome variables
#load("R code/zbp13detailwide.Rda")
zbp13detailwide<-mutate(zbp13detailwide, food13 = est_bakery13 + est_cafeterias13 + est_dessert13 + est_fastfood13 + est_restaurant13)
zbp13detailwide<-mutate(zbp13detailwide, alcohol13 = est_bars13 + est_liquor13)
zbp13detailwide<-mutate(zbp13detailwide, freshfood13 = (est_grocery13 - (n1_4_grocery13+n5_9_grocery13 + n10_19_grocery13))+est_warehouse13)
zbp13detailwide<-mutate(zbp13detailwide, unfreshfood13 = n1_4_grocery13+n5_9_grocery13 + n10_19_grocery13+ est_conv13+est_gasconv13)

zbp13detailwide<-zbp13detailwide[,c(1,122:127)]

save(zbp13detailwide, file = 'R code/zbp13detailwide.Rda')
saveRDS(zbp13detailwide, 'R code/zbp13detailwide.rds')

