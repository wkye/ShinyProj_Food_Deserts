load('/Users/williamkye/Box Sync/nyc data science academy/project_1/shiny/scat.rda')
filtered<- scat_data%>%  dplyr::select(race_white, race, fastfood, play)

test<-plot_ly(filtered[c(0:2100,4257:6357),], x =~ race_white,
        y =~ fastfood,
        type = 'scatter',
        hoverinfo = 'text',
        frame =~play,
        color =~ race,
        colors = c('green', 'red', 'pink', 'skyblue'),
        text =~ paste(sep = '<br>',
                      paste('fastfood', fastfood, sep = ': '),
                     paste0('percent white: ',race_white)))%>% layout(title='fastfood',
                                                                      yaxis = list(title ='number of fast food restaurants'),
                                                                      xaxis = list(title = 'demographic group',showticklabels = F)
                     )
test

api_create(test, filename = "scat_542")

box<-plot_ly(filtered[c(0:2100,4257:6357),],
        x =~ race,
        y=~ fastfood,
        type = 'box',
        color =~ race,
        colors = c('green', 'red', 'pink', 'skyblue')) %>%layout(
          title = 'boxplot',
          yaxis = list(title = paste0('# of fast food restaurants')),
          xaxis = list(title = paste0('demographic group'),showticklabels = F))
api_create(box, filename = "box")


filtered<- scat_data%>%  dplyr::select(race_white, race, fastfood ,play) %>% filter(play==0)
x<-plot_ly(filtered,
           x =~ race_white,
           y=~ fastfood,
           type = 'scatter',
           hoverinfo = 'text',
           text =~ paste(sep = '<br>',
                         paste('fastfood', fastfood, sep = ': '),
                         paste0('percent white: ',race_white))
          #color =~ race_white
  )%>% layout(
    title = 'fastfood',
    yaxis = list(title = paste0('# of fast food restaurants')),
    xaxis = list(title = paste0('percent white'))
  )
api_create(x, filename = "scat")
api_create(x, filename = "scat")
bc_fastfood= bc_fastfood %>% layout(title = 'Fast Food Negative Binomial Regression')
bc_freshfood= bc_freshfood %>% layout(title = 'Fresh Food Negative Binomial Regression')
bc_alcohol=bc_alcohol %>% layout(title = 'Alcohol Negative Binomial Regression')
bc_unfreshgroceries=bc_unfreshgroceries%>% layout(title = 'Unfresh Groceries <br> Negative Binomial Regression'))
bc_unfreshgroceries
api_create(bc_fastfood, filename = "bc_fast")
api_create(bc_unfreshgroceries, filename = "bc_unfresh")
api_create(bc_freshfood, filename = "bc_fresh")
api_create(bc_alcohol, filename = "bc_alcohol")

name = as.character(sort(rep(1:16,8)))
time = c(rep(1:8,16))
fastfood = c(seq(2,16,2),seq(2,16,2)+ runif(8,0,2),seq(2,16,2)+ runif(8,0,2),seq(2,16,2)+ runif(8,0,2),
             seq(5,35,length.out = 8), seq(5,35,length.out = 8) + runif(8,5,7),seq(5,35,length.out = 8) + runif(8,5,7),seq(15,35,length.out = 8) + runif(8,5,7),
             seq(10,27,length.out = 8), seq(10,27,length.out = 8) + runif(8,10, 12),seq(10,27,length.out = 8) + runif(8,10,12),seq(10,27,length.out = 8) + runif(8,10,12),
             seq(7,16,length.out = 8), seq(7,16,length.out = 8) + runif(8,1,3),seq(7,16,length.out = 8) + runif(8,1,3),seq(7,16,length.out = 8) + runif(8,1,3))

runif(8,-2,5)

race = c(rep('white', 32), rep('black',32), rep('asian',32), rep('hispanic',32))
#race = rep(race,16)

x <- data.frame(name, time, fastfood, race)

ggplot(x, aes(time, fastfood)) + geom_point(aes(color = race)) +geom_line(aes(fill = name, color = race))
