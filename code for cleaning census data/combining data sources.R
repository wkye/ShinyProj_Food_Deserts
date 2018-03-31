rm(list = ls())
library(dplyr)
load('R data/zcta_to_msa.rda')
load('R data/zip_msa.rda')
filenames<-lapply(1:15, function(x) paste(c('R data/zbp',formatC(x, digits = 0, width = 2, flag = 0), 'detailwide.rds'), collapse = ''))
load('R data/zbp00detailwide.rda')
zbptotal<-zbp00detailwide
for (i in 1:15){
  data1<-readRDS(filenames[[i]])
  data1<-data1[,c(-2,-3)]
  zbptotal<- full_join(zbptotal, data1, by = 'zip')
}

zip_msa<-zip_msa[,c(1:7)]
zip_msa<-rename(zip_msa, msa = msa.x)
zbptotal<-inner_join(zbptotal, zip_msa, by = 'zip')
msa_lst<-c('35620', '35380', '33460', '31100', '16980', '26420', '19100', '37980', '47900', '12060', '14460','41860','19820', '33100', '42660', '33460', '41740', '45300', '19740', '41180')
zbptotal<- filter(zbptotal, msa %in%msa_lst)
zbptotal$msa<-factor(zbptotal$msa)

save(zbptotal, file = 'R data/zbptotal.rda')

table(zbptotal$Geo_QName)


rm(list = ls())
data<-readRDS('R data/zip_2000.rds')
filenames<- lapply(c('09.rds','10.rds','11.rds','12.rds','13.rds','14.rds'), function(x) paste0('R data/zip_20',x))

for (i in 1:6){
  data1<-readRDS(filenames[[i]])
  data<- full_join(data, data1, by = 'zip')
}

names(data)

load('R data/zbptotal.rda')

final_data<-inner_join(data, zbptotal, by='zip')


Geo_QName<- unique(final_data$Geo_QName)
new_names<- c(
  'Boston', 'Boston', 'New York', 'Philadelphia', 'New York', 'Washington DC', 'Washington DC', 'Philadelphia', 'Atlanta', 'Miami', 'Tampa Bay',
  'Detroit', 'Chicago', 'Minneapolis', 'Minneapolis', 'Chicago', 'St. Louis', 'St. Louis', 'New Orleans', 'Dallas', 'Houston', 'Denver', 'Los Angeles', 
  'San Diego', 'San Francisco', 'Seattle')
city_names<- data.frame(Geo_QName, new_names)
final_data<- inner_join(final_data, city_names, by = 'Geo_QName')

names(final_data)<-gsub('unfreshfood', 'unfreshgroceries', names(final_data))
names(final_data)<-gsub('food', 'fastfood', names(final_data))
names(final_data)<-gsub('freshfastfood', 'freshfood', names(final_data))
final_data1 <- final_data[,-c(2,3,5:9,14,18,20:24,28, 33,35:39,43, 48,50:54, 58,63,65:69, 73, 78,80:84, 88, 93,95:99,103,108:180)]
final_data<- final_data[,c(1,3,5:9,14,18,20:24,28, 33,35:39,43, 48,50:54, 58,63,65:69, 73, 78,80:84, 88, 93,95:99,103,108:180)]
x<-data.frame(sapply(final_data[3:7], function(x) as.integer((x/final_data$pop_00)*100)))
new_name=paste0('per_', names(x))
names(x)<-new_name
final_data<-cbind(final_data,x)
x<-data.frame(sapply(final_data[10:14], function(x) as.integer((x/final_data$pop_09)*100)))
new_name=paste0('per_', names(x))
names(x)<-new_name
final_data<-cbind(final_data,x)
x<-data.frame(sapply(final_data[17:21], function(x) as.integer((x/final_data$pop_10)*100)))
new_name=paste0('per_', names(x))
names(x)<-new_name
final_data<-cbind(final_data,x)
x<-data.frame(sapply(final_data[24:28], function(x) as.integer((x/final_data$pop_11)*100)))
new_name=paste0('per_', names(x))
names(x)<-new_name
final_data<-cbind(final_data,x)
x<-data.frame(sapply(final_data[31:35], function(x) as.integer((x/final_data$pop_12)*100)))
new_name=paste0('per_', names(x))
names(x)<-new_name
final_data<-cbind(final_data,x)
x<-data.frame(sapply(final_data[38:42], function(x) as.integer((x/final_data$pop_13)*100)))
new_name=paste0('per_', names(x))
names(x)<-new_name
final_data<-cbind(final_data,x)
x<-data.frame(sapply(final_data[45:49], function(x) as.integer((x/final_data$pop_14)*100)))
new_name=paste0('per_', names(x))
names(x)<-new_name
final_data<-cbind(final_data,x)
final_data<- final_data %>% mutate( per_race_black_01 = as.integer(((per_race_black_09-per_race_black_00)/9)*1)+per_race_black_00,
                                    per_race_black_02 = as.integer(((per_race_black_09-per_race_black_00)/9)*2)+per_race_black_00,
                                    per_race_black_03 = as.integer(((per_race_black_09-per_race_black_00)/9)*3)+per_race_black_00,
                                    per_race_black_04 = as.integer(((per_race_black_09-per_race_black_00)/9)*4)+per_race_black_00,
                                    per_race_black_05 = as.integer(((per_race_black_09-per_race_black_00)/9)*5)+per_race_black_00,
                                    per_race_black_06 = as.integer(((per_race_black_09-per_race_black_00)/9)*6)+per_race_black_00,
                                    per_race_black_07 = as.integer(((per_race_black_09-per_race_black_00)/9)*7)+per_race_black_00,
                                    per_race_black_08 = as.integer(((per_race_black_09-per_race_black_00)/9)*8)+per_race_black_00,
                                    per_race_white_01 = as.integer(((per_race_white_09-per_race_white_00)/9)*1)+per_race_white_00,
                                    per_race_white_02 = as.integer(((per_race_white_09-per_race_white_00)/9)*2)+per_race_white_00,
                                    per_race_white_03 = as.integer(((per_race_white_09-per_race_white_00)/9)*3)+per_race_white_00,
                                    per_race_white_04 = as.integer(((per_race_white_09-per_race_white_00)/9)*4)+per_race_white_00,
                                    per_race_white_05 = as.integer(((per_race_white_09-per_race_white_00)/9)*5)+per_race_white_00,
                                    per_race_white_06 = as.integer(((per_race_white_09-per_race_white_00)/9)*6)+per_race_white_00,
                                    per_race_white_07 = as.integer(((per_race_white_09-per_race_white_00)/9)*7)+per_race_white_00,
                                    per_race_white_08 = as.integer(((per_race_white_09-per_race_white_00)/9)*8)+per_race_white_00,
                                    per_race_asian_01 = as.integer(((per_race_asian_09-per_race_asian_00)/9)*1)+per_race_asian_00,
                                    per_race_asian_02 = as.integer(((per_race_asian_09-per_race_asian_00)/9)*2)+per_race_asian_00,
                                    per_race_asian_03 = as.integer(((per_race_asian_09-per_race_asian_00)/9)*3)+per_race_asian_00,
                                    per_race_asian_04 = as.integer(((per_race_asian_09-per_race_asian_00)/9)*4)+per_race_asian_00,
                                    per_race_asian_05 = as.integer(((per_race_asian_09-per_race_asian_00)/9)*5)+per_race_asian_00,
                                    per_race_asian_06 = as.integer(((per_race_asian_09-per_race_asian_00)/9)*6)+per_race_asian_00,
                                    per_race_asian_07 = as.integer(((per_race_asian_09-per_race_asian_00)/9)*7)+per_race_asian_00,
                                    per_race_asian_08 = as.integer(((per_race_asian_09-per_race_asian_00)/9)*8)+per_race_asian_00,
                                    per_race_hispanic_01 = as.integer(((per_race_hispanic_09-per_race_hispanic_00)/9)*1)+per_race_hispanic_00,
                                    per_race_hispanic_02 = as.integer(((per_race_hispanic_09-per_race_hispanic_00)/9)*2)+per_race_hispanic_00,
                                    per_race_hispanic_03 = as.integer(((per_race_hispanic_09-per_race_hispanic_00)/9)*3)+per_race_hispanic_00,
                                    per_race_hispanic_04 = as.integer(((per_race_hispanic_09-per_race_hispanic_00)/9)*4)+per_race_hispanic_00,
                                    per_race_hispanic_05 = as.integer(((per_race_hispanic_09-per_race_hispanic_00)/9)*5)+per_race_hispanic_00,
                                    per_race_hispanic_06 = as.integer(((per_race_hispanic_09-per_race_hispanic_00)/9)*6)+per_race_hispanic_00,
                                    per_race_hispanic_07 = as.integer(((per_race_hispanic_09-per_race_hispanic_00)/9)*7)+per_race_hispanic_00,
                                    per_race_hispanic_08 = as.integer(((per_race_hispanic_09-per_race_hispanic_00)/9)*8)+per_race_hispanic_00,
                                    per_race_other_01 = as.integer(((per_race_other_09-per_race_other_00)/9)*1)+per_race_other_00,
                                    per_race_other_02 = as.integer(((per_race_other_09-per_race_other_00)/9)*2)+per_race_other_00,
                                    per_race_other_03 = as.integer(((per_race_other_09-per_race_other_00)/9)*3)+per_race_other_00,
                                    per_race_other_04 = as.integer(((per_race_other_09-per_race_other_00)/9)*4)+per_race_other_00,
                                    per_race_other_05 = as.integer(((per_race_other_09-per_race_other_00)/9)*5)+per_race_other_00,
                                    per_race_other_06 = as.integer(((per_race_other_09-per_race_other_00)/9)*6)+per_race_other_00,
                                    per_race_other_07 = as.integer(((per_race_other_09-per_race_other_00)/9)*7)+per_race_other_00,
                                    per_race_other_08 = as.integer(((per_race_other_09-per_race_other_00)/9)*8)+per_race_other_00,
                                    inc_med_01 = as.integer(((inc_med_09-inc_med_00)/9)*1)+inc_med_00,
                                    inc_med_02 = as.integer(((inc_med_09-inc_med_00)/9)*2)+inc_med_00,
                                    inc_med_03 = as.integer(((inc_med_09-inc_med_00)/9)*3)+inc_med_00,
                                    inc_med_04 = as.integer(((inc_med_09-inc_med_00)/9)*4)+inc_med_00,
                                    inc_med_05 = as.integer(((inc_med_09-inc_med_00)/9)*5)+inc_med_00,
                                    inc_med_06 = as.integer(((inc_med_09-inc_med_00)/9)*6)+inc_med_00,
                                    inc_med_07 = as.integer(((inc_med_09-inc_med_00)/9)*7)+inc_med_00,
                                    inc_med_08 = as.integer(((inc_med_09-inc_med_00)/9)*8)+inc_med_00)

final_data<-inner_join(final_data, final_data1, by = 'zip')

names(final_data)
#final_data<-final_data[,c(1,8,15,22,29,36,43,50,53:116,123:206)]

save(final_data, file = 'R data/final data.rda')

load('/Users/williamkye/Box Sync/nyc data science academy/project_1/R data/shape.rda')

shape@data<- rename(shape@data, zip = ZCTA5CE10)
shape<-shape[order(shape$zip),]

shape@data[,'zip']<-as.character(shape@data[,'zip'])

final_data<- final_data[!duplicated(final_data$zip),]

filtered<-final_data

filtered<-filtered[!duplicated(filtered$zip),]
filtered<- filtered[order(filtered$zip),]

zips<-filtered$zip
zips

shape1<-shape

shape1<-shape1[shape1$zip %in% zips, ]

shape1@data<- left_join(shape1@data, filtered, by = 'zip')

nrow(shape1@data)

length(shape1@polygons)

shape1@data$INTPTLAT10<-as.numeric(as.character(shape1@data$INTPTLAT10))
shape1@data$INTPTLON10<-as.numeric(as.character(shape1@data$INTPTLON10))
names(shape1)


shape1$color<-0
save(shape1, file = 'R data/shape1.rda')

