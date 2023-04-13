##calculating stop wait times ca. fall 2018 - winter 2019

#setup
source("source/calculating_headways.R")
library(dplyr)
library(sf)
library(mapview)
headways_fil <- ungroup(headways_fil)

##calculate mean headways by stop
stop_waits <- headways_fil %>% 
              group_by(stop_id) %>%
              summarise(mean_wait = mean(headway_m, na.rm = TRUE))

#check the shortest mean wait
slice_min(stop_waits, mean_wait)

#looks good. means of 0 were filtered when we did headways


#join it to stops table
stop_join <- inner_join(stop_waits, gtfs_fil$stops)

##make it spatial
coordinates = c("stop_lon", "stop_lat")

stop_join <- st_as_sf(stop_join, coords = coordinates, crs = 4326) 

#check if we can plot geometry
plot(stop_join$geometry)
  
#plotting all waits
waits_plot <- ggplot(stop_join) +
  geom_sf(aes(color = mean_wait), size = 1.5) +
  scale_colour_distiller(palette = "YlGnBu", direction = 1)

#grab 25 longest waits
top_25 <- stop_join %>%
  slice_max(mean_wait, n = 25) 
top_25$mean_wait <- round(top_25$mean_wait, digits = 2)

#where are the stops with the top 25 longest waits?

mapview(top_25)


