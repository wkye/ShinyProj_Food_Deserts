rm(list = ls())
library(data.table)
library(readr)
library(dplyr)
load ('R code/zbptotal.rda')
zip<-vector('character')
zip<-zbptotal$zip
zip<-zip[!duplicated(zip)]
#zip_2000_o <- read_csv("data/2000/zip 2000.csv")
#save(zip_2000_o, file = 'R code/zip_2000_o')
load('R code/zip_2000_o')
names_variables<- zip_2000_o[1,]
names_2<-names(names_variables)
names_variables<-rbind(names_variables, names_2)
t_names_variables<-t(names_variables)
df_names_variables<-data.frame(t_names_variables)
names(zip_2000_o)<-zip_2000_o[1,]

variables_lst<-c('Geo_ZCTA5','Geo_AREALAND', 'Geo_POP100','SE_T003_001','SE_T008_002', 'SE_T008_003', 'SE_T008_004','SE_T008_005','SE_T008_006',
                 'SE_T008_007','SE_T008_008','SE_T008_009','SE_T008_010','SE_T008_011','SE_T008_012', 'SE_T008_013','SE_T015_003', 'SE_T015_004',
                 'SE_T015_005','SE_T015_006','SE_T015_007','SE_T015_008','SE_T015_009','SE_T015_010', 'SE_T069_001','SE_T085_001','SE_T092_001','SE_T092_002','SE_T092_003',
                 'SE_T092_004','SE_T092_005','SE_T092_006','SE_T092_007','SE_T092_008','SE_T092_009','SE_T092_010','SE_T092_011','SE_T092_012','SE_T092_013','SE_T092_014',
                 'SE_T092_015','SE_T092_016','SE_T092_017','SE_T093_001', 'SE_T179_001', 'SE_T179_002')

df_names_variables<-filter(df_names_variables, X1 %in% variables_lst)

new_names<-as.character(df_names_variables$X2)
new_names<-c('zip', 'area_00','pop_00','popden_00','age5_00', 'age5_9_00', 'age10_14_00', 'age_15_17_00', 'age18_24_00', 'age25_34_00', 'age_35_44_00', 'age45_54_00', 'age55_64_00', 'age65_74_00', 'age_75_84_00', 'age85_00','race_white_00', 'race_black_00', 'race_amerind_00', 'race_asian_00', 'race_islander_00', 'race_other_00', 'race_two_00', 'race_hispanic_00', 'employed_pop_00', 'employed_00','households_00','inc_10_00', 
             'inc_15_00', 'inc_20_00', 'inc_25_00', 'inc_30_00', 'inc_35_00', 'inc_40_00', 'inc_45_00','inc_50_00', 'inc60_00', 'inc75_00', 'inc100_00', 'inc125_00', 'inc150_00', 'inc200_00','inc_201_00', 'inc_med_00', 'pov_d_00', "below_pov_00")

zip_2000<-filter(zip_2000_o, Geo_ZCTA5 %in% zip)
zip_2000<-select(zip_2000, Geo_ZCTA5,Geo_AREALAND, Geo_POP100,SE_T003_001,SE_T008_002, SE_T008_003, SE_T008_004,SE_T008_005,SE_T008_006,
                 SE_T008_007,SE_T008_008,SE_T008_009,SE_T008_010,SE_T008_011,SE_T008_012, SE_T008_013,SE_T015_003, SE_T015_004,
                 SE_T015_005,SE_T015_006,SE_T015_007,SE_T015_008,SE_T015_009,SE_T015_010, SE_T069_001,SE_T085_001,SE_T092_001,SE_T092_002,SE_T092_003,
                 SE_T092_004,SE_T092_005,SE_T092_006,SE_T092_007,SE_T092_008,SE_T092_009,SE_T092_010,SE_T092_011,SE_T092_012,SE_T092_013,SE_T092_014,
                 SE_T092_015,SE_T092_016,SE_T092_017,SE_T093_001, SE_T179_001, SE_T179_002)

names(zip_2000)<-new_names
x<-data.frame(lapply(zip_2000[2:46], function(x) as.numeric(as.character(x))))
zip_2000_wk<-cbind(zip_2000[,1], x)
zip_2000_wk<- mutate(zip_2000_wk, children_00= age5_00 + age5_9_00 + age10_14_00 + age_15_17_00, youngadult_00 = age18_24_00 + age25_34_00 + age_35_44_00, middleage_00= age55_64_00, elderly_00 = age65_74_00 + age_75_84_00 + age85_00, race_other_00 = race_amerind_00 + race_islander_00 + race_other_00 + race_two_00)
zip_2000_wk<-select(zip_2000_wk, zip, area_00, pop_00, popden_00,race_white_00, race_black_00, race_asian_00, race_other_00,race_hispanic_00, employed_pop_00, employed_00, pov_d_00, below_pov_00, inc_med_00,children_00, youngadult_00, middleage_00)

save(zip_2000_wk, file = 'R code/zip_2000.rda')
saveRDS(zip_2000_wk, 'R code/zip_2000.rds')




