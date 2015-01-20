library(rvest)
library(shiny)
library(stringr)
library(RCurl) 
library(dplyr) 
library(ggvis) 
library(tidyr)

# raw data
   x <- getURL("https://raw.githubusercontent.com/rasmusgreve/BoardGameGeek/master/BoardGameGeek/data_w_right_ratings2014-05-02.csv?raw=true", ssl.verifypeer = FALSE)
   data <- read.csv(text = x, stringsAsFactors=F,sep=";")
   data$average_rating <- round(data$average_rating,2)

# Category Information
   page <- html("https://boardgamegeek.com/browse/boardgamecategory")

   catids <-html_nodes(page, "#main_content td a") %>%
    html_attr("href") 


    page  %>%
      html_nodes("#main_content td a") %>%
      html_attr("href") %>%
      str_extract(.,"[0-9]{1,4}")  %>% 
      as.numeric(.)-> ids

    page %>%
     html_nodes("#main_content td a") %>%
     html_text -> cats


    categories  <- data.frame(id=ids,category=cats)

    categories$category <- as.character(categories$category)

    categoryChoice <- c(9999,categories$id)
    names(categoryChoice)<- c("All",categories$category)


    ## game Categories

    gameCategories <-tbl_df(data %>%
                          select(game=name,categories) %>%
                          separate(categories,c(letters,"A","B"), extra="drop") %>%
                          gather(name,category,-game) %>%
                          filter(!is.na(category)) %>%
                          select(game,category))