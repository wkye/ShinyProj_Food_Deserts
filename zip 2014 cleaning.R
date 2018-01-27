#ACS
rm(list = ls())
load ('R code/zbptotal.rda')
zip<-vector('character')
zip<-zbptotal$zip
zip<-zip[!duplicated(zip)]


#zip_2014_acs_o <- read_csv("data/2014/zip 2014 acs.csv")
#save(zip_2014_acs_o, file = 'R code/zip 2014 acs.rda')
load('R code/zip 2014 acs.rda')
names_variables<- zip_2014_acs_o[1,]
names_2<-names(names_variables)
names_variables<-rbind(names_variables, names_2)
t_names_variables<-t(names_variables)
df_names_variables<-data.frame(t_names_variables)
names(zip_2014_acs_o)<-zip_2014_acs_o[1,]

variables_lst<-c('Geo_ZCTA5','SE_T001_001','SE_T002_002', 'SE_T008_002','SE_T008_003','SE_T008_004','SE_T008_005','SE_T008_006','SE_T008_007','SE_T008_008','SE_T008_009','SE_T008_010','SE_T008_011','SE_T008_012','SE_T008_013', 'SE_T008_014', 'SE_T008_015', 'SE_T008_016','SE_T008_017','SE_T008_018','SE_T008_019','SE_T008_020','SE_T008_021','SE_T008_022','SE_T008_023','SE_T008_024','SE_T014_003','SE_T014_004','SE_T014_005','SE_T014_006','SE_T014_007','SE_T014_008','SE_T014_009','SE_T014_010','SE_T033_001', 'SE_T033_002', 'SE_T056_001', 'SE_T056_002', 'SE_T056_003','SE_T056_004','SE_T056_005','SE_T056_006','SE_T056_007','SE_T056_008','SE_T056_009','SE_T056_010','SE_T056_011','SE_T056_012','SE_T056_013','SE_T056_014', 'SE_T056_015','SE_T056_016','SE_T056_017', 'SE_T057_001', 'SE_T115_001', 'SE_T115_002')
df_names_variables<-filter(df_names_variables, X1 %in% variables_lst)

new_names<-as.character(df_names_variables$X2)
new_names<-c('zip','pop_14','popden_14','age5_14', 'age5_9_14', 'age10_14_14', 'age_15_17_14','age18_19_14','age20_14','age_21_14','age22_24_14', 'age25_29_14','age30_34_0','age35_39_14','age40_44','age45_49_14', 'age50_54_14', 'age55_59_14','age60_61_14','age62_64','age65_66_14','age67_69_14','age70_74_14',  'age75_74_14', 'age80_84_14', 'age85_14','race_white_14', 'race_black_14', 'race_amerind_14', 'race_asian_14', 'race_islander_14', 'race_other_14', 'race_two_14', 'race_hispanic_14', 'employed_pop_14', 'employed_14','households_14','inc_10_14', 
             'inc_15_14', 'inc_20_14', 'inc_25_14', 'inc_30_14', 'inc_35_14', 'inc_40_14', 'inc_45_14','inc_50_14', 'inc60_14', 'inc75_14', 'inc100_14', 'inc125_14', 'inc150_14', 'inc200_14','inc_201_14', 'inc_med_14', 'pov_d_14', 'below_pov_14')

zip_2014_acs<-filter(zip_2014_acs_o, Geo_ZCTA5 %in% zip)
zip_2014_acs<-select(zip_2014_acs, Geo_ZCTA5,SE_T001_001,SE_T002_002, SE_T008_002,SE_T008_003,SE_T008_004,SE_T008_005,SE_T008_006,SE_T008_007,SE_T008_008,SE_T008_009,SE_T008_010,SE_T008_011,SE_T008_012,SE_T008_013, SE_T008_014, SE_T008_015, SE_T008_016,SE_T008_017,SE_T008_018,SE_T008_019,SE_T008_020,SE_T008_021,SE_T008_022,SE_T008_023,SE_T008_024,SE_T014_003,SE_T014_004,SE_T014_005,SE_T014_006,SE_T014_007,SE_T014_008,SE_T014_009,SE_T014_010,SE_T033_001, SE_T033_002, SE_T056_001, SE_T056_002, SE_T056_003,SE_T056_004,SE_T056_005,SE_T056_006,SE_T056_007,SE_T056_008,SE_T056_009,SE_T056_010,SE_T056_011,SE_T056_012,SE_T056_013,SE_T056_014, SE_T056_015,SE_T056_016,SE_T056_017, SE_T057_001, SE_T115_001, SE_T115_002)

names(zip_2014_acs)<-new_names

x<-data.frame(lapply(zip_2014_acs[2:56], function(x) as.numeric(as.character(x))))
zip_2014_acs_wk<-cbind(zip_2014_acs[,1], x)

zip_2014_acs_wk<- mutate(zip_2014_acs_wk, children_14=age5_14+age5_9_14 + age10_14_14+ age_15_17_14, youngadult_14= age18_19_14 +age20_14 + age_21_14 +age22_24_14+age25_29_14 +age30_34_0, middleage_14= age35_39_14+age40_44+age45_49_14+age50_54_14, elderly_14= age55_59_14+age60_61_14+age62_64 +age65_66_14+age67_69_14+age70_74_14+age75_74_14+age80_84_14+age85_14, race_other_14 = race_amerind_14 +race_islander_14+race_other_14 +race_two_14)

zip_2014_acs_wk<-select(zip_2014_acs_wk, zip, pop_14, popden_14,race_white_14, race_black_14, race_asian_14, race_other_14,race_hispanic_14, employed_pop_14, employed_14, pov_d_14,inc_med_14, below_pov_14, children_14, youngadult_14, middleage_14)
save(zip_2014_acs_wk, file = 'R code/zip_2014.rda')
saveRDS(zip_2014_acs_wk, 'R code/zip_2014.rds')


