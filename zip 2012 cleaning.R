#ACS
rm(list = ls())
load ('R code/zbptotal.rda')
zip<-vector('character')
zip<-zbptotal$zip
zip<-zip[!duplicated(zip)]


#zip_2012_acs_o <- read_csv("data/2012/zip 2012 ACS.csv")
#save(zip_2012_acs_o, file = 'R code/zip 2012 acs.rda')
load('R code/zip 2012 acs.rda')
names_variables<- zip_2012_acs_o[1,]
names_2<-names(names_variables)
names_variables<-rbind(names_variables, names_2)
t_names_variables<-t(names_variables)
df_names_variables<-data.frame(t_names_variables)
names(zip_2012_acs_o)<-zip_2012_acs_o[1,]

variables_lst<-c('Geo_ZCTA5','SE_T001_001','SE_T002_002', 'SE_T008_002','SE_T008_003','SE_T008_004','SE_T008_005','SE_T008_006','SE_T008_007','SE_T008_008','SE_T008_009','SE_T008_010','SE_T008_011','SE_T008_012','SE_T008_013', 'SE_T008_014', 'SE_T008_015', 'SE_T008_016','SE_T008_017','SE_T008_018','SE_T008_019','SE_T008_020','SE_T008_021','SE_T008_022','SE_T008_023','SE_T008_024','SE_T014_003','SE_T014_004','SE_T014_005','SE_T014_006','SE_T014_007','SE_T014_008','SE_T014_009','SE_T014_010','SE_T033_001', 'SE_T033_002', 'SE_T056_001', 'SE_T056_002', 'SE_T056_003','SE_T056_004','SE_T056_005','SE_T056_006','SE_T056_007','SE_T056_008','SE_T056_009','SE_T056_010','SE_T056_011','SE_T056_012','SE_T056_013','SE_T056_014', 'SE_T056_015','SE_T056_016','SE_T056_017', 'SE_T057_001', 'SE_T115_001', 'SE_T115_002')
df_names_variables<-filter(df_names_variables, X1 %in% variables_lst)

new_names<-as.character(df_names_variables$X2)
new_names<-c('zip','pop_12','popden_12','age5_12', 'age5_9_12', 'age10_14_12', 'age_15_17_12','age18_19_12','age20_12','age_21_12','age22_24_12', 'age25_29_12','age30_34_0','age35_39_12','age40_44','age45_49_12', 'age50_54_12', 'age55_59_12','age60_61_12','age62_64','age65_66_12','age67_69_12','age70_74_12',  'age75_74_12', 'age80_84_12', 'age85_12','race_white_12', 'race_black_12', 'race_amerind_12', 'race_asian_12', 'race_islander_12', 'race_other_12', 'race_two_12', 'race_hispanic_12', 'employed_pop_12', 'employed_12','households_12','inc_10_12', 
             'inc_15_12', 'inc_20_12', 'inc_25_12', 'inc_30_12', 'inc_35_12', 'inc_40_12', 'inc_45_12','inc_50_12', 'inc60_12', 'inc75_12', 'inc100_12', 'inc125_12', 'inc150_12', 'inc200_12','inc_201_12', 'inc_med_12', 'pov_d_12', 'below_pov_12')

zip_2012_acs<-filter(zip_2012_acs_o, Geo_ZCTA5 %in% zip)
zip_2012_acs<-select(zip_2012_acs, Geo_ZCTA5,SE_T001_001,SE_T002_002, SE_T008_002,SE_T008_003,SE_T008_004,SE_T008_005,SE_T008_006,SE_T008_007,SE_T008_008,SE_T008_009,SE_T008_010,SE_T008_011,SE_T008_012,SE_T008_013, SE_T008_014, SE_T008_015, SE_T008_016,SE_T008_017,SE_T008_018,SE_T008_019,SE_T008_020,SE_T008_021,SE_T008_022,SE_T008_023,SE_T008_024,SE_T014_003,SE_T014_004,SE_T014_005,SE_T014_006,SE_T014_007,SE_T014_008,SE_T014_009,SE_T014_010,SE_T033_001, SE_T033_002, SE_T056_001, SE_T056_002, SE_T056_003,SE_T056_004,SE_T056_005,SE_T056_006,SE_T056_007,SE_T056_008,SE_T056_009,SE_T056_010,SE_T056_011,SE_T056_012,SE_T056_013,SE_T056_014, SE_T056_015,SE_T056_016,SE_T056_017, SE_T057_001, SE_T115_001, SE_T115_002)

names(zip_2012_acs)<-new_names

x<-data.frame(lapply(zip_2012_acs[2:56], function(x) as.numeric(as.character(x))))
zip_2012_acs_wk<-cbind(zip_2012_acs[,1], x)

zip_2012_acs_wk<- mutate(zip_2012_acs_wk, children_12=age5_12+age5_9_12 + age10_14_12+ age_15_17_12, youngadult_12= age18_19_12 +age20_12 + age_21_12 +age22_24_12+age25_29_12 +age30_34_0, middleage_12= age35_39_12+age40_44+age45_49_12+age50_54_12, elderly_12= age55_59_12+age60_61_12+age62_64 +age65_66_12+age67_69_12+age70_74_12+age75_74_12+age80_84_12+age85_12, race_other_12 = race_amerind_12 +race_islander_12+race_other_12 +race_two_12)

zip_2012_acs_wk<-select(zip_2012_acs_wk, zip, pop_12, popden_12,race_white_12, race_black_12, race_asian_12, race_other_12,race_hispanic_12, employed_pop_12, employed_12, pov_d_12,inc_med_12, below_pov_12, children_12, youngadult_12, middleage_12)
save(zip_2012_acs_wk, file = 'R code/zip_2012.rda')
saveRDS(zip_2012_acs_wk, 'R code/zip_2012.rds')




