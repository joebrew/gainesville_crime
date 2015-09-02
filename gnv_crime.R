# http://www.r-bloggers.com/visualising-thefts-using-heatmaps-in-ggplot2/

library(ggplot2)
library(dplyr)
library(ggmap)
library(readr)
crime <- read_csv('https://data.cityofgainesville.org/api/views/gvua-xt9q/rows.csv?accessType=DOWNLOAD')
if(nrow(crime) > 1000){
  write_csv(crime, 'C:/Users/BrewJR/Desktop/crime.csv')
}
gnv <- get_map(location = 'Gainesville', zoom = 12, maptype = ('toner-lite'))
ggmap(gnv)

#########
# CLEAN IT UP A BIT
#########
# write a function to clean lat lon points
clean_up <- function(x){
  
  # split the string to keep only the lat, lon, part
  a <- do.call(rbind, strsplit(as.character(x), "\n"))
  aa <- a[,3]
  
  # now split at the comma
  b <- do.call(rbind, strsplit(as.character(aa), ","))
  bb <- b#[,2]
  
  # make df
  bb <- data.frame(bb)
  
  # fix names
  names(bb) <- c("lat", "lon")
  
  # remove parentheses
  bb$lat <- as.numeric(gsub("\\(|\\)", "", bb$lat))
  bb$lon <- as.numeric(gsub("\\(|\\)", "", bb$lon))
  
  return(bb)
}

temp <- clean_up(crime$location)
crime <- cbind(crime, temp)


#####
# GET COUNTS BY EXACT LOCATION
#####

# Round
x <- crime %>% 
  mutate(lon = round(lon, digits = 2),
         lat = round(lat, digits = 2))

agg <- x %>%
  group_by(lat, lon) %>%
  summarise(n = n()) %>%
  data.frame() %>%
  arrange(desc(n)) 
agg <- agg[which(!is.na(agg$n)),]

# Transform n
# agg$n <- agg$n ^ (1/3)

#####
# MAP
#####
ggmap(gnv) +
  geom_tile(data = agg, aes(x = lon, y = lat, alpha = n),
            fill = 'red') +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank())