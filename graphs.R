rm(list = ls())
library(glm.predict)
library(dplyr)
library(MASS)
setwd('/Users/williamkye/Box Sync/nyc data science academy/project_1/')
rm(list = ls())
load('R data/final data.rda')

data<-final_data %>% dplyr::select(dplyr::contains('14'))
zip<-final_data[,1]
data<-data.frame(zip,data)
new_names<-c('zip', 'inc_med','fastfood','alcohol', 'freshfood', 'unfreshgroceries', 'race_white', 'race_black', 'race_asian', 'race_other', 'race_hisp')
names(data)<-new_names
test<-data%>% mutate(race = (ifelse(race_white >= 60, 'white',
                                    ifelse(race_black >=60, 'black',
                                           ifelse(race_hisp >=60, 'hispanic','heterogeneous')))))
test$play<-0

test1<-test
test1<-mutate(test1, race_white = (ifelse(race == 'white', 80,
                                          ifelse(race == 'hispanic', 60,
                                                 ifelse(race =='heterogeneous', 40,
                                                        ifelse(race =='black',20, 0))))))
test1$play<-1 
test2<-rbind(test,test1)
scat_data<-test2
scat_data$race_white<- jitter(scat_data$race_white, factor = 1)
save(scat_data, file = 'R code/scat data.rda')


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

save(final_data, file = ('R data/final data.rda'))

cross_final<-final_data%>% dplyr::select(contains('14'))

cross_final<- cbind(final_data[,c('zip', 'area_00', 'msa')], cross_final)
cross_final<- mutate(cross_final, loginc_med_14 = log(inc_med_14))


names(final_data)<-gsub('unfreshfood', 'unfreshgroceries', names(final_data))
names(final_data)<-gsub('food', 'fastfood', names(final_data))

cross_final<-mutate(cross_final, 
                    per_black = 100*(race_black_14/pop_14),
                    per_white = 100*(race_white_14/pop_14),
                    per_asian = 100*(race_asian_14/pop_14),
                    per_hispanic = 100*(race_hispanic_14/pop_14),
                    per_other = 100*(race_other_14/pop_14),
                    below_pov_14 = 100*(below_pov_14/pop_14)
                    )
cross_final$area_00
save(cross_final, file = 'R data/cross_final')
###################################Fastfood#############################################################
###################################Fastfood#############################################################
###################################Fastfood#############################################################
m.fastfood_g<- glm.nb(fastfood14 ~ per_black +per_asian + per_white + per_hispanic
                    + below_pov_14+ loginc_med_14 + area_00+ area_00+ factor(msa), data = cross_final, offset(pop_14))
m.fastfood_bc<- glm.nb(fastfood14 ~ per_black +per_asian + per_other + per_hispanic
                    + below_pov_14 +loginc_med_14 + area_00+ factor(msa), data = cross_final, offset(pop_14))
summary(m.fastfood_g)
summary(m.fastfood_bc)
summary(m.fastfood_g)
m.fastfood_bc$coefficients
m.fastfood_bc$coefficients[c(2:3,5)]
bc_fastfood_coef<-data.frame(m.fastfood_bc$coefficients[c(2:3,5)])
bc_fastfood_coef$race<-c('% Black', '% Asian', '% Hispanic')
bc_fastfood_coef$text<- c(paste(sep ="<br>",
                            'estimate : -.007***',
                            'standard error: .003'),
                      paste(sep ="<br>",
                            'estimate: .016***',
                            'standard error: .000004'),
                      paste(sep ="<br>",
                            'estimate: .0006***',
                            'standard error: .000007'))
names(bc_fastfood_coef)<-c('coef', 'race','text')
                       
save(bc_fastfood_coef, file = 'bc_fastfood_coef.rda')
bc_fastfood<- plot_ly(bc_fastfood_coef, x =~ race, y =~ coef, type = 'bar', text =~ text, marker = list(color =c('skyblue', 'red', 'orange') ))  %>% layout(
  shapes=list(type='line', x0= -1, x1= 5, y0=1, y1=~1, line=list(dash='dot', width=1)),
  title = 'Negative Binomial Regression Results',
  yaxis = list(title = 'exponentiated coefficients', range = c(-.01,.025)),
  xaxis = list(title = '% Race (white as the reference)')) 
bc_fastfood 
#save(bc_fastfood, file = 'R code/bc_fastfood.rda')
bc_fastfood
class(bc_fastfood)

##fast food restaurants linegraph
mean(cross_final$per_hispanic, na.rm = T)/3
(mean(cross_final$per_hispanic,na.rm = T)-100)/3
mean(cross_final$per_other, na.rm = T)

seq(mean(cross_final$per_hispanic, na.rm = T) +3.20031, mean(cross_final$per_hispanic, na.rm = T) -21.79969, length.out = 100)

pr_bl_fastfood <- data.frame(per_black =seq(0,100, 1),
                             per_hispanic = seq(mean(cross_final$per_hispanic, na.rm = T) +4.26708, mean(cross_final$per_hispanic, na.rm = T) -29.06625, length.out = 101),
                             per_asian = seq(mean(cross_final$per_asian, na.rm = T) +4.26708, mean(cross_final$per_asian, na.rm = T) -29.06625, length.out = 101),
                             per_white = seq(mean(cross_final$per_white, na.rm = T) +4.26708, mean(cross_final$per_white, na.rm = T) -29.06625, length.out = 101),
                             below_pov_14 = mean(cross_final$below_pov_14, na.rm = T),
                             loginc_med_14 = mean(cross_final$loginc_med_14, na.rm = T),
                             area_00 = mean(cross_final$area_00, na.rm = T),
                             msa = '35620')
pr_bl_fastfood<-data.frame(predict(m.fastfood_g, pr_bl_fastfood, type="response", se.fit=TRUE))[,1:2]
names(pr_bl_fastfood)<-c('black_pr','black_se')

pr_wh_fastfood <- data.frame(per_white =seq(0,100, 1),
                             per_hispanic = seq(mean(cross_final$per_hispanic, na.rm = T) +20.19791, mean(cross_final$per_hispanic, na.rm = T) -13.13542, length.out = 101),
                             per_asian = seq(mean(cross_final$per_asian, na.rm = T) +20.19791, mean(cross_final$per_asian, na.rm = T) -13.13542, length.out = 101),
                             per_black = seq(mean(cross_final$per_black, na.rm = T) +20.19791, mean(cross_final$per_black, na.rm = T) -13.13542, length.out = 101),
                             below_pov_14 = mean(cross_final$below_pov_14, na.rm = T),
                             loginc_med_14 = mean(cross_final$loginc_med_14, na.rm = T),
                             area_00 = mean(cross_final$area_00, na.rm = T),
                             msa = '35620')
pr_wh_fastfood<-data.frame(predict(m.fastfood_g, pr_wh_fastfood, type="response", se.fit=TRUE))[,1:2]
names(pr_wh_fastfood)<-c('white_pr','white_se')

pr_as_fastfood <- data.frame(per_asian =seq(0,100, 1),
                             per_hispanic = seq(mean(cross_final$per_hispanic, na.rm = T) +2.25685, mean(cross_final$per_hispanic, na.rm = T) -31.07648, length.out = 101),
                             per_white = seq(mean(cross_final$per_white, na.rm = T) +2.25685, mean(cross_final$per_white, na.rm = T) -31.07648, length.out = 101),
                             per_black = seq(mean(cross_final$per_black, na.rm = T) +2.25685, mean(cross_final$per_black, na.rm = T) -31.07648, length.out = 101),
                             below_pov_14 = mean(cross_final$below_pov_14, na.rm = T),
                             loginc_med_14 = mean(cross_final$loginc_med_14, na.rm = T),
                             area_00 = mean(cross_final$area_00, na.rm = T),
                             msa = '35620')
pr_as_fastfood<-data.frame(predict(m.fastfood_g, pr_as_fastfood, type="response", se.fit=TRUE))[,1:2]
names(pr_as_fastfood)<-c('asian_pr','asian_se')

pr_hs_fastfood <- data.frame(per_hispanic =seq(0,100, 1),
                             per_white = seq(mean(cross_final$per_white, na.rm = T) +5.680687, mean(cross_final$per_white, na.rm = T) -27.65265, length.out = 101),
                             per_asian = seq(mean(cross_final$per_asian, na.rm = T) +5.680687, mean(cross_final$per_asian, na.rm = T) -27.65265, length.out = 101),
                             per_black = seq(mean(cross_final$per_black, na.rm = T) +5.680687, mean(cross_final$per_black, na.rm = T) -27.65265, length.out = 101),
                             below_pov_14 = mean(cross_final$below_pov_14, na.rm = T),
                             loginc_med_14 = mean(cross_final$loginc_med_14, na.rm = T),
                             area_00 = mean(cross_final$area_00, na.rm = T),
                             msa = '35620')
pr_hs_fastfood<-data.frame(predict(m.fastfood_g, pr_hs_fastfood, type="response", se.fit=TRUE))[,1:2]
names(pr_hs_fastfood)<-c('hispanic_pr','hispanic_se')

margins_fastfood<-cbind(pr_wh_fastfood, pr_bl_fastfood, pr_as_fastfood, pr_hs_fastfood)
margins_fastfood$percent<-seq(0,100,1)

##

p<- plot_ly(margins_fastfood, x=~percent, y=~ white_pr, name = 'percent white', 
            type = 'scatter', mode = 'lines',line = list(color = 'blue'), hoverinfo = 'text', text =~ paste(sep = "<br>",
                                                                                paste0('% white: ', percent),
                                                                                paste0('fastfood: ', as.integer(white_pr)))
                                                                               ) %>%
  add_trace(y =~ black_pr, name = 'percent black', mode = 'lines', line = list(color ='red'), text =~ paste(sep = "<br>",
                                                                                 paste0('% black: ', percent),
                                                                                 paste0('fastfood: ', as.integer(black_pr)))) %>%
  add_trace(y =~ hispanic_pr, name = 'percent hispanic' ,mode = 'lines', line = list(color ='green'),text =~ paste(sep = "<br>",
                                                                                       paste0('% hispanic: ', percent),
                                                                                       paste0('fastfood: ', as.integer(hispanic_pr)))) %>%
  add_trace(y =~ asian_pr, name = 'percent asian', mode = 'lines',line = list(color ='orange'), text =~ paste(sep = "<br>",
                                                                                 paste0('% asian: ', percent),
                                                                                 paste0('fastfood: ', as.integer(asian_pr)))) %>%
  layout(title = "Predicted number of fastfood restaurants by race",
         xaxis = list(title = "Percent (other as reference category)"),
         yaxis = list (title = "Number of fastfood restaurants"))

p
ln_fastfood<-p
save(ln_fastfood, file = 'R code/ln_fastfood1.rda')
bc_fastfood
p
ln_fastfood

###################################freshfood#############################################################
###################################freshfood#############################################################
###################################freshfood#############################################################
m.freshfood_g<- glm.nb(freshfood14 ~ per_black +per_asian + per_white + per_hispanic
                      + below_pov_14+ loginc_med_14 + area_00+ area_00+ factor(msa), data = cross_final, offset(pop_14))
m.freshfood_bc<- glm.nb(freshfood14 ~ per_black +per_asian + per_other + per_hispanic
                       + below_pov_14 +loginc_med_14 + area_00+ factor(msa), data = cross_final, offset(pop_14))
summary(m.freshfood_g)
summary(m.freshfood_bc)
summary(m.freshfood_g)
m.freshfood_bc$coefficients
m.freshfood_bc$coefficients[c(2:3,5)]
bc_freshfood_coef<-data.frame(m.freshfood_bc$coefficients[c(2:3,5)])
bc_freshfood_coef$race<-c('% Black', '% Asian', '% Hispanic')
bc_freshfood_coef$text<- c(paste(sep ="<br>",
                                'estimate : -.004***',
                                'standard error: .003'),
                          paste(sep ="<br>",
                                'estimate: .011***',
                                'standard error: .000004'),
                          paste(sep ="<br>",
                                'estimate: .006***',
                                'standard error: .000007'))
names(bc_freshfood_coef)<-c('coef', 'race','text')

save(bc_freshfood_coef, file = 'shiny/bc_freshfood_coef.rda')
bc_freshfood<- plot_ly(bc_freshfood_coef, x =~ race, y =~ coef, type = 'bar', text =~ text, marker = list(color =c('skyblue', 'red', 'orange') ))  %>% layout(
  shapes=list(type='line', x0= -1, x1= 5, y0=1, y1=~1, line=list(dash='dot', width=1)),
  title = 'Negative Binomial Regression Results',
  yaxis = list(title = 'exponentiated coefficients', range = c(-.01,.025)),
  xaxis = list(title = '% Race (white as the reference)')) 
bc_freshfood 
#save(bc_freshfood, file = 'R code/bc_freshfood.rda')
bc_freshfood
class(bc_freshfood)

##fast food restaurants linegraph
mean(cross_final$per_hispanic, na.rm = T)/3
(mean(cross_final$per_hispanic,na.rm = T)-100)/3
mean(cross_final$per_other, na.rm = T)

seq(mean(cross_final$per_hispanic, na.rm = T) +3.20031, mean(cross_final$per_hispanic, na.rm = T) -21.79969, length.out = 100)

pr_bl_freshfood <- data.frame(per_black =seq(0,100, 1),
                             per_hispanic = seq(mean(cross_final$per_hispanic, na.rm = T) +4.26708, mean(cross_final$per_hispanic, na.rm = T) -29.06625, length.out = 101),
                             per_asian = seq(mean(cross_final$per_asian, na.rm = T) +4.26708, mean(cross_final$per_asian, na.rm = T) -29.06625, length.out = 101),
                             per_white = seq(mean(cross_final$per_white, na.rm = T) +4.26708, mean(cross_final$per_white, na.rm = T) -29.06625, length.out = 101),
                             below_pov_14 = mean(cross_final$below_pov_14, na.rm = T),
                             loginc_med_14 = mean(cross_final$loginc_med_14, na.rm = T),
                             area_00 = mean(cross_final$area_00, na.rm = T),
                             msa = '35620')
pr_bl_freshfood<-data.frame(predict(m.freshfood_g, pr_bl_freshfood, type="response", se.fit=TRUE))[,1:2]
names(pr_bl_freshfood)<-c('black_pr','black_se')

pr_wh_freshfood <- data.frame(per_white =seq(0,100, 1),
                             per_hispanic = seq(mean(cross_final$per_hispanic, na.rm = T) +20.19791, mean(cross_final$per_hispanic, na.rm = T) -13.13542, length.out = 101),
                             per_asian = seq(mean(cross_final$per_asian, na.rm = T) +20.19791, mean(cross_final$per_asian, na.rm = T) -13.13542, length.out = 101),
                             per_black = seq(mean(cross_final$per_black, na.rm = T) +20.19791, mean(cross_final$per_black, na.rm = T) -13.13542, length.out = 101),
                             below_pov_14 = mean(cross_final$below_pov_14, na.rm = T),
                             loginc_med_14 = mean(cross_final$loginc_med_14, na.rm = T),
                             area_00 = mean(cross_final$area_00, na.rm = T),
                             msa = '35620')
pr_wh_freshfood<-data.frame(predict(m.freshfood_g, pr_wh_freshfood, type="response", se.fit=TRUE))[,1:2]
names(pr_wh_freshfood)<-c('white_pr','white_se')

pr_as_freshfood <- data.frame(per_asian =seq(0,100, 1),
                             per_hispanic = seq(mean(cross_final$per_hispanic, na.rm = T) +2.25685, mean(cross_final$per_hispanic, na.rm = T) -31.07648, length.out = 101),
                             per_white = seq(mean(cross_final$per_white, na.rm = T) +2.25685, mean(cross_final$per_white, na.rm = T) -31.07648, length.out = 101),
                             per_black = seq(mean(cross_final$per_black, na.rm = T) +2.25685, mean(cross_final$per_black, na.rm = T) -31.07648, length.out = 101),
                             below_pov_14 = mean(cross_final$below_pov_14, na.rm = T),
                             loginc_med_14 = mean(cross_final$loginc_med_14, na.rm = T),
                             area_00 = mean(cross_final$area_00, na.rm = T),
                             msa = '35620')
pr_as_freshfood<-data.frame(predict(m.freshfood_g, pr_as_freshfood, type="response", se.fit=TRUE))[,1:2]
names(pr_as_freshfood)<-c('asian_pr','asian_se')

pr_hs_freshfood <- data.frame(per_hispanic =seq(0,100, 1),
                             per_white = seq(mean(cross_final$per_white, na.rm = T) +5.680687, mean(cross_final$per_white, na.rm = T) -27.65265, length.out = 101),
                             per_asian = seq(mean(cross_final$per_asian, na.rm = T) +5.680687, mean(cross_final$per_asian, na.rm = T) -27.65265, length.out = 101),
                             per_black = seq(mean(cross_final$per_black, na.rm = T) +5.680687, mean(cross_final$per_black, na.rm = T) -27.65265, length.out = 101),
                             below_pov_14 = mean(cross_final$below_pov_14, na.rm = T),
                             loginc_med_14 = mean(cross_final$loginc_med_14, na.rm = T),
                             area_00 = mean(cross_final$area_00, na.rm = T),
                             msa = '35620')
pr_hs_freshfood<-data.frame(predict(m.freshfood_g, pr_hs_freshfood, type="response", se.fit=TRUE))[,1:2]
names(pr_hs_freshfood)<-c('hispanic_pr','hispanic_se')

margins_freshfood<-cbind(pr_wh_freshfood, pr_bl_freshfood, pr_as_freshfood, pr_hs_freshfood)
margins_freshfood$percent<-seq(0,100,1)

##

p<- plot_ly(margins_freshfood, x=~percent, y=~ white_pr, name = 'percent white', 
            type = 'scatter', mode = 'lines',line = list(color = 'blue'), hoverinfo = 'text', text =~ paste(sep = "<br>",
                                                                                                            paste0('% white: ', percent),
                                                                                                            paste0('freshfood: ', as.integer(white_pr)))
) %>%
  add_trace(y =~ black_pr, name = 'percent black', mode = 'lines', line = list(color ='red'), text =~ paste(sep = "<br>",
                                                                                                            paste0('% black: ', percent),
                                                                                                            paste0('freshfood: ', as.integer(black_pr)))) %>%
  add_trace(y =~ hispanic_pr, name = 'percent hispanic' ,mode = 'lines', line = list(color ='green'),text =~ paste(sep = "<br>",
                                                                                                                   paste0('% hispanic: ', percent),
                                                                                                                   paste0('freshfood: ', as.integer(hispanic_pr)))) %>%
  add_trace(y =~ asian_pr, name = 'percent asian', mode = 'lines',line = list(color ='orange'), text =~ paste(sep = "<br>",
                                                                                                              paste0('% asian: ', percent),
                                                                                                              paste0('freshfood: ', as.integer(asian_pr)))) %>%
  layout(title = "Predicted number of freshfood restaurants by race",
         xaxis = list(title = "Percent (other as reference category)"),
         yaxis = list (title = "Number of freshfood restaurants"))

p
ln_freshfood<-p
save(ln_freshfood, file = 'R code/ln_freshfood1.rda')
bc_freshfood
p
ln_freshfood

###################################unfreshgroceries#############################################################
###################################unfreshgroceries#############################################################
###################################unfreshgroceries#############################################################
m.unfreshgroceries_g<- glm.nb(unfreshgroceries14 ~ per_black +per_asian + per_white + per_hispanic
                      + below_pov_14+ loginc_med_14 + area_00+ area_00+ factor(msa), data = cross_final, offset(pop_14))
m.unfreshgroceries_bc<- glm.nb(unfreshgroceries14 ~ per_black +per_asian + per_other + per_hispanic
                       + below_pov_14 +loginc_med_14 + area_00+ factor(msa), data = cross_final, offset(pop_14))
summary(m.unfreshgroceries_g)
summary(m.unfreshgroceries_bc)
summary(m.unfreshgroceries_g)
m.unfreshgroceries_bc$coefficients
m.unfreshgroceries_bc$coefficients[c(2:3,5)]
bc_unfreshgroceries_coef<-data.frame(m.unfreshgroceries_bc$coefficients[c(2:3,5)])
bc_unfreshgroceries_coef$race<-c('% Black', '% Asian', '% Hispanic')
bc_unfreshgroceries_coef$text<- c(paste(sep ="<br>",
                                'estimate : .002***',
                                'standard error: .003'),
                          paste(sep ="<br>",
                                'estimate: .012***',
                                'standard error: .000004'),
                          paste(sep ="<br>",
                                'estimate: .007***',
                                'standard error: .000007'))
names(bc_unfreshgroceries_coef)<-c('coef', 'race','text')

save(bc_unfreshgroceries_coef, file = 'shiny/bc_unfreshgroceries_coef.rda')
bc_unfreshgroceries<- plot_ly(bc_unfreshgroceries_coef, x =~ race, y =~ coef, type = 'bar', text =~ text, marker = list(color =c('skyblue', 'red', 'orange') ))  %>% layout(
  shapes=list(type='line', x0= -1, x1= 5, y0=1, y1=~1, line=list(dash='dot', width=1)),
  title = 'Negative Binomial Regression Results',
  yaxis = list(title = 'exponentiated coefficients', range = c(-.01,.025)),
  xaxis = list(title = '% Race (white as the reference)')) 
bc_unfreshgroceries 
#save(bc_unfreshgroceries, file = 'R code/bc_unfreshgroceries.rda')
bc_unfreshgroceries
class(bc_unfreshgroceries)

##fast food restaurants linegraph
mean(cross_final$per_hispanic, na.rm = T)/3
(mean(cross_final$per_hispanic,na.rm = T)-100)/3
mean(cross_final$per_other, na.rm = T)

seq(mean(cross_final$per_hispanic, na.rm = T) +3.20031, mean(cross_final$per_hispanic, na.rm = T) -21.79969, length.out = 100)

pr_bl_unfreshgroceries <- data.frame(per_black =seq(0,100, 1),
                             per_hispanic = seq(mean(cross_final$per_hispanic, na.rm = T) +4.26708, mean(cross_final$per_hispanic, na.rm = T) -29.06625, length.out = 101),
                             per_asian = seq(mean(cross_final$per_asian, na.rm = T) +4.26708, mean(cross_final$per_asian, na.rm = T) -29.06625, length.out = 101),
                             per_white = seq(mean(cross_final$per_white, na.rm = T) +4.26708, mean(cross_final$per_white, na.rm = T) -29.06625, length.out = 101),
                             below_pov_14 = mean(cross_final$below_pov_14, na.rm = T),
                             loginc_med_14 = mean(cross_final$loginc_med_14, na.rm = T),
                             area_00 = mean(cross_final$area_00, na.rm = T),
                             msa = '35620')
pr_bl_unfreshgroceries<-data.frame(predict(m.unfreshgroceries_g, pr_bl_unfreshgroceries, type="response", se.fit=TRUE))[,1:2]
names(pr_bl_unfreshgroceries)<-c('black_pr','black_se')

pr_wh_unfreshgroceries <- data.frame(per_white =seq(0,100, 1),
                             per_hispanic = seq(mean(cross_final$per_hispanic, na.rm = T) +20.19791, mean(cross_final$per_hispanic, na.rm = T) -13.13542, length.out = 101),
                             per_asian = seq(mean(cross_final$per_asian, na.rm = T) +20.19791, mean(cross_final$per_asian, na.rm = T) -13.13542, length.out = 101),
                             per_black = seq(mean(cross_final$per_black, na.rm = T) +20.19791, mean(cross_final$per_black, na.rm = T) -13.13542, length.out = 101),
                             below_pov_14 = mean(cross_final$below_pov_14, na.rm = T),
                             loginc_med_14 = mean(cross_final$loginc_med_14, na.rm = T),
                             area_00 = mean(cross_final$area_00, na.rm = T),
                             msa = '35620')
pr_wh_unfreshgroceries<-data.frame(predict(m.unfreshgroceries_g, pr_wh_unfreshgroceries, type="response", se.fit=TRUE))[,1:2]
names(pr_wh_unfreshgroceries)<-c('white_pr','white_se')

pr_as_unfreshgroceries <- data.frame(per_asian =seq(0,100, 1),
                             per_hispanic = seq(mean(cross_final$per_hispanic, na.rm = T) +2.25685, mean(cross_final$per_hispanic, na.rm = T) -31.07648, length.out = 101),
                             per_white = seq(mean(cross_final$per_white, na.rm = T) +2.25685, mean(cross_final$per_white, na.rm = T) -31.07648, length.out = 101),
                             per_black = seq(mean(cross_final$per_black, na.rm = T) +2.25685, mean(cross_final$per_black, na.rm = T) -31.07648, length.out = 101),
                             below_pov_14 = mean(cross_final$below_pov_14, na.rm = T),
                             loginc_med_14 = mean(cross_final$loginc_med_14, na.rm = T),
                             area_00 = mean(cross_final$area_00, na.rm = T),
                             msa = '35620')
pr_as_unfreshgroceries<-data.frame(predict(m.unfreshgroceries_g, pr_as_unfreshgroceries, type="response", se.fit=TRUE))[,1:2]
names(pr_as_unfreshgroceries)<-c('asian_pr','asian_se')

pr_hs_unfreshgroceries <- data.frame(per_hispanic =seq(0,100, 1),
                             per_white = seq(mean(cross_final$per_white, na.rm = T) +5.680687, mean(cross_final$per_white, na.rm = T) -27.65265, length.out = 101),
                             per_asian = seq(mean(cross_final$per_asian, na.rm = T) +5.680687, mean(cross_final$per_asian, na.rm = T) -27.65265, length.out = 101),
                             per_black = seq(mean(cross_final$per_black, na.rm = T) +5.680687, mean(cross_final$per_black, na.rm = T) -27.65265, length.out = 101),
                             below_pov_14 = mean(cross_final$below_pov_14, na.rm = T),
                             loginc_med_14 = mean(cross_final$loginc_med_14, na.rm = T),
                             area_00 = mean(cross_final$area_00, na.rm = T),
                             msa = '35620')
pr_hs_unfreshgroceries<-data.frame(predict(m.unfreshgroceries_g, pr_hs_unfreshgroceries, type="response", se.fit=TRUE))[,1:2]
names(pr_hs_unfreshgroceries)<-c('hispanic_pr','hispanic_se')

margins_unfreshgroceries<-cbind(pr_wh_unfreshgroceries, pr_bl_unfreshgroceries, pr_as_unfreshgroceries, pr_hs_unfreshgroceries)
margins_unfreshgroceries$percent<-seq(0,100,1)

##

p<- plot_ly(margins_unfreshgroceries, x=~percent, y=~ white_pr, name = 'percent white', 
            type = 'scatter', mode = 'lines',line = list(color = 'blue'), hoverinfo = 'text', text =~ paste(sep = "<br>",
                                                                                                            paste0('% white: ', percent),
                                                                                                            paste0('unfreshgroceries: ', as.integer(white_pr)))
) %>%
  add_trace(y =~ black_pr, name = 'percent black', mode = 'lines', line = list(color ='red'), text =~ paste(sep = "<br>",
                                                                                                            paste0('% black: ', percent),
                                                                                                            paste0('unfreshgroceries: ', as.integer(black_pr)))) %>%
  add_trace(y =~ hispanic_pr, name = 'percent hispanic' ,mode = 'lines', line = list(color ='green'),text =~ paste(sep = "<br>",
                                                                                                                   paste0('% hispanic: ', percent),
                                                                                                                   paste0('unfreshgroceries: ', as.integer(hispanic_pr)))) %>%
  add_trace(y =~ asian_pr, name = 'percent asian', mode = 'lines',line = list(color ='orange'), text =~ paste(sep = "<br>",
                                                                                                              paste0('% asian: ', percent),
                                                                                                              paste0('unfreshgroceries: ', as.integer(asian_pr)))) %>%
  layout(title = "Predicted number of unfreshgroceries restaurants by race",
         xaxis = list(title = "Percent (other as reference category)"),
         yaxis = list (title = "Number of unfreshgroceries restaurants"))

p
ln_unfreshgroceries<-p
save(ln_unfreshgroceries, file = 'R code/ln_unfreshgroceries1.rda')
bc_unfreshgroceries
p
ln_unfreshgroceries

###################################alcohol#############################################################
###################################alcohol#############################################################
###################################alcohol#############################################################
m.alcohol_g<- glm.nb(alcohol14 ~ per_black +per_asian + per_white + per_hispanic
                      + below_pov_14+ loginc_med_14 + area_00+ area_00+ factor(msa), data = cross_final, offset(pop_14))
m.alcohol_bc<- glm.nb(alcohol14 ~ per_black +per_asian + per_other + per_hispanic
                       + below_pov_14 +loginc_med_14 + area_00+ factor(msa), data = cross_final, offset(pop_14))
summary(m.alcohol_g)
summary(m.alcohol_bc)
summary(m.alcohol_g)
m.alcohol_bc$coefficients
m.alcohol_bc$coefficients[c(2:3,5)]
bc_alcohol_coef<-data.frame(m.alcohol_bc$coefficients[c(2:3,5)])
bc_alcohol_coef$race<-c('% Black', '% Asian', '% Hispanic')
bc_alcohol_coef$text<- c(paste(sep ="<br>",
                                'estimate : -.009***',
                                'standard error: .003'),
                          paste(sep ="<br>",
                                'estimate: -.002***',
                                'standard error: .000004'),
                          paste(sep ="<br>",
                                'estimate: -,002***',
                                'standard error: .000007'))
names(bc_alcohol_coef)<-c('coef', 'race','text')

save(bc_alcohol_coef, file = 'shiny/bc_alcohol_coef.rda')
bc_alcohol<- plot_ly(bc_alcohol_coef, x =~ race, y =~ coef, type = 'bar', text =~ text, marker = list(color =c('skyblue', 'red', 'orange') ))  %>% layout(
  shapes=list(type='line', x0= -1, x1= 5, y0=1, y1=~1, line=list(dash='dot', width=1)),
  title = 'Negative Binomial Regression Results',
  yaxis = list(title = 'exponentiated coefficients', range = c(-.01,.025)),
  xaxis = list(title = '% Race (white as the reference)')) 
bc_alcohol 
#save(bc_alcohol, file = 'R code/bc_alcohol.rda')
bc_alcohol
class(bc_alcohol)

##fast food restaurants linegraph
mean(cross_final$per_hispanic, na.rm = T)/3
(mean(cross_final$per_hispanic,na.rm = T)-100)/3
mean(cross_final$per_other, na.rm = T)

seq(mean(cross_final$per_hispanic, na.rm = T) +3.20031, mean(cross_final$per_hispanic, na.rm = T) -21.79969, length.out = 100)

pr_bl_alcohol <- data.frame(per_black =seq(0,100, 1),
                             per_hispanic = seq(mean(cross_final$per_hispanic, na.rm = T) +4.26708, mean(cross_final$per_hispanic, na.rm = T) -29.06625, length.out = 101),
                             per_asian = seq(mean(cross_final$per_asian, na.rm = T) +4.26708, mean(cross_final$per_asian, na.rm = T) -29.06625, length.out = 101),
                             per_white = seq(mean(cross_final$per_white, na.rm = T) +4.26708, mean(cross_final$per_white, na.rm = T) -29.06625, length.out = 101),
                             below_pov_14 = mean(cross_final$below_pov_14, na.rm = T),
                             loginc_med_14 = mean(cross_final$loginc_med_14, na.rm = T),
                             area_00 = mean(cross_final$area_00, na.rm = T),
                             msa = '35620')
pr_bl_alcohol<-data.frame(predict(m.alcohol_g, pr_bl_alcohol, type="response", se.fit=TRUE))[,1:2]
names(pr_bl_alcohol)<-c('black_pr','black_se')

pr_wh_alcohol <- data.frame(per_white =seq(0,100, 1),
                             per_hispanic = seq(mean(cross_final$per_hispanic, na.rm = T) +20.19791, mean(cross_final$per_hispanic, na.rm = T) -13.13542, length.out = 101),
                             per_asian = seq(mean(cross_final$per_asian, na.rm = T) +20.19791, mean(cross_final$per_asian, na.rm = T) -13.13542, length.out = 101),
                             per_black = seq(mean(cross_final$per_black, na.rm = T) +20.19791, mean(cross_final$per_black, na.rm = T) -13.13542, length.out = 101),
                             below_pov_14 = mean(cross_final$below_pov_14, na.rm = T),
                             loginc_med_14 = mean(cross_final$loginc_med_14, na.rm = T),
                             area_00 = mean(cross_final$area_00, na.rm = T),
                             msa = '35620')
pr_wh_alcohol<-data.frame(predict(m.alcohol_g, pr_wh_alcohol, type="response", se.fit=TRUE))[,1:2]
names(pr_wh_alcohol)<-c('white_pr','white_se')

pr_as_alcohol <- data.frame(per_asian =seq(0,100, 1),
                             per_hispanic = seq(mean(cross_final$per_hispanic, na.rm = T) +2.25685, mean(cross_final$per_hispanic, na.rm = T) -31.07648, length.out = 101),
                             per_white = seq(mean(cross_final$per_white, na.rm = T) +2.25685, mean(cross_final$per_white, na.rm = T) -31.07648, length.out = 101),
                             per_black = seq(mean(cross_final$per_black, na.rm = T) +2.25685, mean(cross_final$per_black, na.rm = T) -31.07648, length.out = 101),
                             below_pov_14 = mean(cross_final$below_pov_14, na.rm = T),
                             loginc_med_14 = mean(cross_final$loginc_med_14, na.rm = T),
                             area_00 = mean(cross_final$area_00, na.rm = T),
                             msa = '35620')
pr_as_alcohol<-data.frame(predict(m.alcohol_g, pr_as_alcohol, type="response", se.fit=TRUE))[,1:2]
names(pr_as_alcohol)<-c('asian_pr','asian_se')

pr_hs_alcohol <- data.frame(per_hispanic =seq(0,100, 1),
                             per_white = seq(mean(cross_final$per_white, na.rm = T) +5.680687, mean(cross_final$per_white, na.rm = T) -27.65265, length.out = 101),
                             per_asian = seq(mean(cross_final$per_asian, na.rm = T) +5.680687, mean(cross_final$per_asian, na.rm = T) -27.65265, length.out = 101),
                             per_black = seq(mean(cross_final$per_black, na.rm = T) +5.680687, mean(cross_final$per_black, na.rm = T) -27.65265, length.out = 101),
                             below_pov_14 = mean(cross_final$below_pov_14, na.rm = T),
                             loginc_med_14 = mean(cross_final$loginc_med_14, na.rm = T),
                             area_00 = mean(cross_final$area_00, na.rm = T),
                             msa = '35620')
pr_hs_alcohol<-data.frame(predict(m.alcohol_g, pr_hs_alcohol, type="response", se.fit=TRUE))[,1:2]
names(pr_hs_alcohol)<-c('hispanic_pr','hispanic_se')

margins_alcohol<-cbind(pr_wh_alcohol, pr_bl_alcohol, pr_as_alcohol, pr_hs_alcohol)
margins_alcohol$percent<-seq(0,100,1)
scat_data




##

p<- plot_ly(margins_alcohol, x=~percent, y=~ white_pr, name = 'percent white', 
            type = 'scatter', mode = 'lines',line = list(color = 'blue'), hoverinfo = 'text', text =~ paste(sep = "<br>",
                                                                                                            paste0('% white: ', percent),
                                                                                                            paste0('alcohol: ', as.integer(white_pr)))
) %>%
  add_trace(y =~ black_pr, name = 'percent black', mode = 'lines', line = list(color ='red'), text =~ paste(sep = "<br>",
                                                                                                            paste0('% black: ', percent),
                                                                                                            paste0('alcohol: ', as.integer(black_pr)))) %>%
  add_trace(y =~ hispanic_pr, name = 'percent hispanic' ,mode = 'lines', line = list(color ='green'),text =~ paste(sep = "<br>",
                                                                                                                   paste0('% hispanic: ', percent),
                                                                                                                   paste0('alcohol: ', as.integer(hispanic_pr)))) %>%
  add_trace(y =~ asian_pr, name = 'percent asian', mode = 'lines',line = list(color ='orange'), text =~ paste(sep = "<br>",
                                                                                                              paste0('% asian: ', percent),
                                                                                                              paste0('alcohol: ', as.integer(asian_pr)))) %>%
  layout(title = "Predicted number of alcohol restaurants by race",
         xaxis = list(title = "Percent (other as reference category)"),
         yaxis = list (title = "Number of alcohol restaurants"))

p
ln_alcohol<-p
save(ln_alcohol, file = 'R code/ln_alcohol1.rda')
bc_alcohol
p
ln_alcohol