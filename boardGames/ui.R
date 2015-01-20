
shinyUI(fluidPage(

 
  titlePanel(""),


  sidebarLayout(
    sidebarPanel(
      tags$head(includeCSS("custom.css")),
      
      h4(p("Select a board game to play or purchase")),
      h5(p("Use criteria below to suit your needs"),
         p("Dataset created by ",a("Rasmus Greve", href="https://github.com/rasmusgreve/BoardGameGeek")," from ",
         a("BoardGameGeek", href="https://boardgamegeek.com")," May 2014")),
      
      selectInput("categories","Categories (remove with back arrow or click/delete)",categoryChoice,multiple=TRUE, selected=9999),
      numericInput("players","Number of Players - use up/down arrows to adjust",value=4,min=1,max=10),
      sliderInput("duration","Game Length Range. NB many games do not provide info ",min=0,max=360,value=c(0,60),step=15),
      sliderInput("age","Minimum Age (often too high)",min=0,max=14,value=8,step=1),
      sliderInput("average","Minimum Rating out of 10",min=0,max=8,value=6,step=1),
      sliderInput("raters","Minimum Number of Raters",min=0,max=100,value=50,step=10)
    ),

   mainPanel(
     dataTableOutput("table")
    )
  )
))
