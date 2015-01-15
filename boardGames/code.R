## 538 published a couple of articles - well woth reading
## A) http://fivethirtyeight.com/features/designing-the-best-board-game-on-the-planet/
## b) http://fivethirtyeight.com/features/the-worst-board-games-ever-invented/

## the analysis was based on the https://boardgamegeek.com/ rankings

## There were a few tables and graphs which I will approximately recreate



library(RCurl) # downloading data
library(dplyr) # data manipulation
library(ggvis) # visualization


# The raw data came from a school project by Rasmus Greve, who was extremely helpful in explaining it in detail
# This May 2015 was used in article. No updating is planned

## download data
x <- getURL("https://raw.githubusercontent.com/rasmusgreve/BoardGameGeek/master/BoardGameGeek/data_w_right_ratings2014-05-02.csv?raw=true", ssl.verifypeer = FALSE)
data <- read.csv(text = x, stringsAsFactors=F,sep=";") 

# quick check
glimpse(data) #Observations: 62204: 27 variables 

# I will limit to fields used in article,rename
# and set to tbl_df

data <- tbl_df(data %>%
  select(id=X.U.FEFF.id,name,year=year_published,players=max_players,time=playingtime,ratings=users_rated,av=average_rating))


##A.1 User Game Rating by Year (min 50 ratings)##
#--------------------------------------------------#

# NB I have used ggvis to enable better tooltips but
# some of the aesthetics are not as good as the presumed ggplot used in article

## create values for the tooltip

tt_a <- function(x) {
  if(is.null(x)) return(NULL)
  row <- data[x$id == data$id,c("name","year","av","ratings") ]
  paste0( format(row), collapse = "<br />")
}
 
# restrict data and create chart
# hover point of details inc number of ratings
data %>%
  filter(ratings>49&year>=1950)  %>% 
  ggvis(~year,~av, key := ~id) %>%
  layer_points(size:=10, opacity:= 0.2) %>%
  add_tooltip(tt_a, "hover") %>%
  add_axis("x", format="####") %>%
  add_axis("y", title="Average User rating")


##A.2 Av Rating by maximum players ##

## I have not been yet been able to recreate this chart
## It is not clear whether any filtering has been done
## or weighted averages based on number of users
## I will contact author for info

## From data it appears that recommended players is often
## much less than maximum anyways


data %>%
  filter(ratings>49) %>%
   group_by(players) %>%
    summarize(av=round(weighted.mean(av,ratings, na.rm=TRUE),2),games=n())


# This shows very little variation between 1-5 users
# also note that the data may be a bit suspect and the 
# preponderance of games for an even number of max players

##A.3 Playing Time ##

# for this graph we need to do some binning of the time taken field

cut(data$time,breaks=c(1,15,45,75,105,135,165,195,225,255),labels=c("<= 15","30","60","90","120","150","180","210","240"))

data$length <- cut(data$time,breaks=c(1,15,45,75,105,135,165,195,225,255,285,315,345,375),labels=c("up to 15","30","60","90","120","150","180","210","240","270","300","330","360"))

## The only way I have found to create the mean line
## is to mutate the original data and coece to data.frame
## I'd welcome more elegant options


df <-data.frame(data %>%
  filter(!is.na(length)&ratings>49) %>%
  group_by(length) %>%
 mutate(mean=mean(av)))

df %>%
  ggvis(~length,~av,key := ~id) %>%
  layer_points(size:=10, opacity:= 0.2)  %>% 
  layer_lines(x = ~length, y = ~mean, stroke:="red") %>%
  add_tooltip(tt_a, "hover") %>%
  add_axis("x", title="Av Playing Time(+/- 15 minutes)") %>%
  add_axis("y", title="Average User rating")

