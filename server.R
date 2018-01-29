
library(DT)
library(shiny)
library(googleVis)
library(leaflet)
library(dplyr)


shinyServer(function(input, output){
  colorpal <- reactive({
    var<- paste(as.character(input$food), formatC(input$time, width = 2, flag = '0'), sep = '')
    var1<-paste(as.character(input$food), '00', sep = '')
    colorBin('OrRd', shape1@data[,var], bins = c(seq(min(shape1@data[,var1], na.rm = T), quantile(shape1@data[,var1], .8,na.rm = T), length.out = 9),Inf))
  })
  ch_map<- reactive({
    shape1[shape1$new_names %in% input$city,] 
    
  })
  
  scat2<-reactive({
    output$scatter<-renderPlotly
  })
  
  output$map <- renderLeaflet({
    leaflet() %>%  setView(lng = -71.0589, lat = 42.3601, zoom = 12) %>%addTiles() 
  })

  output$predicted_values<-renderPlotly({
  var<-paste0('bc_', input$outcome)
  get(var)
   })
  
  # observe({
  #   var<-paste0('bc_', input$outcome)
  #   get(var)
  # })
  observe({
    if (input$collapse == 'original scatterplot'){
      output$scatter<-renderPlotly({
        filtered<- scat_data%>% select(race_white, race, input$scatter_outcome, play)
        plot_ly(filtered,
          x =~ race_white,
          y=~ get(input$scatter_outcome),
          type = 'scatter',
          frame = ~play,
          hoverinfo = 'text',
          text =~ paste(sep = '<br>',
                        paste(input$scatter_outcome, get(input$scatter_outcome), sep = ': '),
                        paste0('percent white: ',race_white))
          #color =~ race_white
        )%>% hide_legend() %>% animation_slider(hide = T) %>% layout(
          title = input$scatter_outcome,
          yaxis = list(title = paste0('# of ', input$scatter_outcome)),
          xaxis = list(title = paste0('percent white'))
        )
    })
    } else if (input$collapse == 'collapse into groups'){

        output$scatter<-renderPlotly({
          filtered<- scat_data%>% select(race_white, race,input$scatter_outcome, play)
          plot_ly(filtered,
                  x =~ race_white,
                  y=~ get(input$scatter_outcome),
                  type = 'scatter',
                  frame = ~play,
                  color =~ race,
                  colors = c('green', 'red', 'pink', 'skyblue'),
                  hoverinfo = 'text',
                  text =~ paste(sep = '<br>',
                                paste(input$scatter_outcome, get(input$scatter_outcome), sep = ': '),
                                race)
          )%>% animation_slider(hide = T)%>% animation_opts %>%layout(
            title = input$scatter_outcome,
            yaxis = list(title = paste0('# of ', input$scatter_outcome)),
            xaxis = list(title = paste0('demographic group'),showticklabels = F)
          )
        })
    } else{
      output$scatter<-renderPlotly({
        filtered<- scat_data%>% select(race, input$scatter_outcome)
        plot_ly(filtered,
                x =~ race,
                y=~ get(input$scatter_outcome),
                type = 'box',
                color =~ race,
                colors = c('green', 'red', 'pink', 'skyblue')) %>%layout(
          title = input$scatter_outcome,
          yaxis = list(title = paste0('# of ', input$scatter_outcome)),
          xaxis = list(title = paste0('demographic group'),showticklabels = F))
        
      })
      
    }
  })
  
  observe({
    output$bar_chart_fastfood<- renderPlotly({
      x<- c('White', 'Black', 'Asian', 'Hispanic')
      white_val<-margins_fastfood[margins_fastfood$percent == input$percent_white,][[1]]
      black_val<-margins_fastfood[margins_fastfood$percent == input$percent_black,][[3]]
      asian_val<-margins_fastfood[margins_fastfood$percent == input$percent_asian,][[5]]
      hispanic_val<-margins_fastfood[margins_fastfood$percent == input$percent_hispanic,][[7]]
      y<-c(white_val, black_val, asian_val, hispanic_val)
      data<-data.frame(x,y)
      plot_ly(data, x =~x, y =~y, marker = list(color = c('green', 'skyblue', 'red', 'orange'))) %>% 
        layout(
        title = ('predicted # fastfood'),
        yaxis = list(title = 'predicted # of fast food', range= c(0,150)),
        xaxis = list(title = 'Demographic Group')
      )
  })
  })
  observe({
    output$bar_chart_freshfood<- renderPlotly({
      x<- c('White', 'Black', 'Asian', 'Hispanic')
      white_val<-margins_freshfood[margins_freshfood$percent == input$percent_white,][[1]]
      black_val<-margins_freshfood[margins_freshfood$percent == input$percent_black,][[3]]
      asian_val<-margins_freshfood[margins_freshfood$percent == input$percent_asian,][[5]]
      hispanic_val<-margins_freshfood[margins_freshfood$percent == input$percent_hispanic,][[7]]
      y<-c(white_val, black_val, asian_val, hispanic_val)
      data<-data.frame(x,y)
      plot_ly(data, x =~x, y =~y, marker = list(color = c('green', 'skyblue', 'red', 'orange'))) %>% 
        layout(
          title = 'predicted # freshfood',
          yaxis = list(title = '# of freshfood', range =c(0,7)),
          xaxis = list(title = 'Demographic Group')
        )
    })
  })
  observe({
    output$bar_chart_unfreshgroceries<- renderPlotly({
      x<- c('White', 'Black', 'Asian', 'Hispanic')
      white_val<-margins_unfreshgroceries[margins_unfreshgroceries$percent == input$percent_white,][[1]]
      black_val<-margins_unfreshgroceries[margins_unfreshgroceries$percent == input$percent_black,][[3]]
      asian_val<-margins_unfreshgroceries[margins_unfreshgroceries$percent == input$percent_asian,][[5]]
      hispanic_val<-margins_unfreshgroceries[margins_unfreshgroceries$percent == input$percent_hispanic,][[7]]
      y<-c(white_val, black_val, asian_val, hispanic_val)
      data<-data.frame(x,y)
      plot_ly(data, x =~x, y =~y, marker = list(color = c('green', 'skyblue', 'red', 'orange'))) %>% 
        layout(
          title = 'predicted # unfreshgroceries',
          yaxis = list(title = '# of unfresh groceries', range = c(0,70)),
          xaxis = list(title = 'Demographic Group')
        )
    })
  })
  observe({
    output$bar_chart_alcohol<- renderPlotly({
      x<- c('White', 'Black', 'Asian', 'Hispanic')
      white_val<-margins_alcohol[margins_alcohol$percent == input$percent_white,][[1]]
      black_val<-margins_alcohol[margins_alcohol$percent == input$percent_black,][[3]]
      asian_val<-margins_alcohol[margins_alcohol$percent == input$percent_asian,][[5]]
      hispanic_val<-margins_alcohol[margins_alcohol$percent == input$percent_hispanic,][[7]]
      y<-c(white_val, black_val, asian_val, hispanic_val)
      data<-data.frame(x,y)
      plot_ly(data, x =~x, y =~y, marker = list(color = c('green', 'skyblue', 'red', 'orange'))) %>% 
        layout(
          title = 'predicted # alcohol',
          yaxis = list(title = '# of liquor stores', range= c(0,17)),
          xaxis = list(title = 'Demographic Group')
        )
    })
  })
  
  observe({
    pal = colorpal()
    black<- paste('per_race_black_', formatC(input$time, width = 2, flag = '0'), sep = '')
    white<- paste('per_race_white_', formatC(input$time, width = 2, flag = '0'), sep = '')
    hispanic<- paste('per_race_hispanic_', formatC(input$time, width = 2, flag = '0'), sep = '')
    other<- paste('per_race_other_', formatC(input$time, width = 2, flag = '0'), sep = '')
    asian<- paste('per_race_asian_', formatC(input$time, width = 2, flag = '0'), sep = '')
    income<- paste('inc_med_', formatC(input$time, width = 2, flag = '0'), sep = '')
    var<- paste(as.character(input$food), formatC(input$time, width = 2, flag = '0'), sep = '')
    proxy<- leafletProxy('map', data = ch_map()) %>% clearShapes()
    proxy <-  leafletProxy('map', data = ch_map()) %>% 
      addPolygons(fillColor =~ pal(get(var)),  weight = 1, color = 'black', smoothFactor = 1, opacity = 1.0, fillOpacity = 0.5, group = 'remove cities')  %>%
      clearControls() %>% addLegend(pal = pal, values =~ get(var), opacity = 0.7, title = NULL, position = "bottomright")  %>%
      addMarkers(~INTPTLON10,~INTPTLAT10, popup =~ paste(sep ="<br/>", paste0('Zip: ', zip),
                                                         paste0('White: ', get(white), '%'),
                                                         paste0('Black: ', get(black), '%'),
                                                         paste0('Hispanic: ', get(hispanic), '%'),
                                                         paste0('Asian: ', get(asian) , '%'),
                                                         paste0('Other: ', get(other), '%'),
                                                         paste0('Median Income: ', '$' , get(income))), group = 'zip code information')%>%
      addLayersControl( overlayGroups = c('zip code information'), options = layersControlOptions(collapsed = FALSE)) %>%
      hideGroup('zip code information')
  })
  observe({
    
  })

})
