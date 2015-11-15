
library(leaflet)
library(maps)

library(data.table)
library(dplyr)
library(ggplot2)


data.dir <- '.'
data1000 <- fread(sprintf('%s/StatFest/data/Combine_affil_1000.csv', data.dir), header = TRUE)

df<-data.frame(data1000)
summary(df)

#top500location_table1 <-
#  df %>%
#  group_by(zip_code) 

#top500location_table1
#top500location_table1 <-
#  df %>%  
  #mutate(amountbyzip =sum(tran_amt)) %>%    #VAR*
  #group_by(zip_code) %>%
  #arrange(desc(tran_amt))


#user_table2[ ,c("timestamp.x","timestamp.y","rating.x", "rating.y", "num_ratings_bymovie.y")] <- list(NULL)

#install.packages("RCurl")
library(RCurl)
library(RJSONIO)

devtools::install_github("gsk3/taRifx.geo")

bGeoCode <- function(str, BingMapsKey){
  require(RCurl)
  require(RJSONIO)
  u <- URLencode(paste0("http://dev.virtualearth.net/REST/v1/Locations?q=", str, "&maxResults=1&key=", BingMapsKey))
  d <- getURL(u)
  j <- fromJSON(d,simplify = FALSE) 
  if (j$resourceSets[[1]]$estimatedTotal > 0) {
    lat <- j$resourceSets[[1]]$resources[[1]]$point$coordinates[[1]]
    lng <- j$resourceSets[[1]]$resources[[1]]$point$coordinates[[2]]
  }
  else {    
    lat <- lng <- NA
  }
  c(lat,lng)
}  

#head(data500)
zipvector <- df[ ,"zip_code"]
#zipvector
sampleset <- paste(zipvector, sep = ",")

#sampleset <- head(zipcode)

resultset <- list()
for(i in 1:length(sampleset)){
  resultset[[i]] <- bGeoCode(sampleset[i], "0UXFRxU1Sr7Dn9S6Bqfi~bitZrYJWnVMkaValW1_x9w~Aqivw5pfkWFR0x9PRhYQHyNYMztm4gY5ZuPtBJKXzg4hjh-omRFSheKC2WTvVT7p")
  
} 
#double bracket [[i]] means accessing an element, not just a list
summary(resultset)
head(resultset)


longlattest <- data.frame(matrix(unlist(resultset), nrow=905, byrow=T),stringsAsFactors=FALSE)
#head(longlattest)
colnames(longlattest) <- c("latitude", "longitude")

id<- 1:nrow(df)
df <- cbind(index=id, df)
#head(df)
id<- 1:nrow(longlattest)
longlat <- cbind(index=id, longlattest)
#head(longlat)

data <- merge(df, longlat, by="index")
#head(data)
View(data)

scrape_pop <- paste0("<strong>City: </strong>", data$city, 
                     "<br><strong>Party Affiliation: </strong>",data$partyaffil, 
                     "<br><strong>Donor Employer: </strong>", data$employer,
                     #"<br><strong>Committee Party Affiliation: </strong>", data$CMTE_PTY_AFFILIATION,
                     "<br><strong>Donation Amount: </strong>", data$transaction_amt) #,

#pal <- colorFactor(c("navy", "red", "gray"), domain = c("ship", "pirate"))
pal <- colorFactor(c("blue3", "tomato", "gold"), domain = c("DEM", "REP", "Undisclosed"))

m3 <- leaflet(data = data) %>% addTiles('http://{s}.tile.stamen.com/toner-lite/{z}/{x}/{y}.png',
                                      attribution = 'Map tiles by <a href="http://stamen.com">Stamen Design</a>, <a href="http://creativecommons.org/licenses/by/3.0">CC BY 3.0</a> &mdash; Map data &copy; <a href="http://www.openstreetmap.org/copyright">OpenStreetMap</a>') %>% 
  setView(-73.9983273, 40.7471983, zoom = 5) %>%
  addCircles(lat = ~ data[ ,"latitude"], lng = ~ data[ ,"longitude"], #color = ~pal2, #?
             fillOpacity = .2,
             radius = data$transaction_amt/10, 
             color= ~pal(data$partyaffil),
             popup= scrape_pop,
             weight = 0)
m3

