
shinyServer(function(input, output) {

  output$table <- renderDataTable({
  
    if (input$categories=="9999") { 

       data %>%
           filter(min_age>=input$age&min_players<=input$players&max_players>=input$players&playingtime<=input$duration[2]&playingtime>=input$duration[1]
           &average_rating>input$average&users_rated>=input$raters) %>%
           select(game=name,year=year_published,minutes=playingtime,rating=round(average_rating,1)) %>%
           arrange(desc(rating))

      } else {

        catChosen <-as.numeric(input$categories) 
  
        games <- gameCategories %>%
        filter(category %in% catChosen) %>%
        unique(.)
        names <- games$game
 
        data %>%
           filter(min_age>=input$age&min_players<=input$players&max_players>=input$players&playingtime<=input$duration[2]&playingtime>=input$duration[1]
           &average_rating>input$average&users_rated>=input$raters&name %in% names) %>%
           select(game=name,year=year_published,minutes=playingtime,rating=round(average_rating,1)) %>%
           arrange(desc(rating))

     }
  }, options=list(pageLength=10, columnDefs= list(list(width="50%",targets=list(0)),list(className="rt",targets=list(1,2,3)))))

})
