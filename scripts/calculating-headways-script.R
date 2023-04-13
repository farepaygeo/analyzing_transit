##calculating headways, canonical as of 4/6/2023
library(gtfstools)
library(dplyr)
#cleaning
gtfs <- read_gtfs("./data/gtfs 11_2018.zip")
class(gtfs)

summary(gtfs)

##filter down gtfs to just bus routes
gtfs <- filter_by_route_type(gtfs, route_type = 3)
##from 4 types to 1
unique(gtfs$routes$route_type) 

##create vector of route short names
comm_names <- as.character(95:850)
##subset route ids that have a match in the comm_names vector
route_ids <- gtfs$routes[which(gtfs$routes$route_short_name %in% comm_names), "route_id"]
##pass the route ids to a new vector
comm_ids <- c(route_ids$route_id)
##filter by route id
gtfs_fil <- filter_by_route_id(gtfs, comm_ids, keep = FALSE) ##seeya

length(unique(gtfs_fil$routes$route_id))

patterns <- get_stop_times_patterns(gtfs_fil)
length(unique(patterns$pattern_id))
#resolution somewhere between trip_id and route_id

gtfs_fil <- convert_time_to_seconds(gtfs_fil)
head(gtfs_fil$stop_times$arrival_time_secs)

#filter by weekday
m_f <- c("monday", "tuesday", "wednesday", "thursday", "friday")
sa_su <- c("saturday", "sunday")

gtfs_fil_m_f <- filter_by_weekday(gtfs_fil, m_f, combine = "and")
gtfs_fil_sa_su <- filter_by_weekday(gtfs_fil, sa_su, combine = "or")

##ok cleaned, calculate headways
library(ggplot2)

##in the first line we are linking patterns to stop_times via trip_id
stop_times_patt <- left_join(gtfs_fil_m_f$stop_times, patterns, by="trip_id")%>%
  ##then we pipe the joined table to another join, route_id via trip_id
  left_join(gtfs_fil_m_f$trips[ , c("trip_id", "route_id")], by = "trip_id", keep = FALSE)%>%
  ##then we pipe it one more time to join route_short_name via route_id
  left_join(gtfs_fil_m_f$routes[ , c("route_id", "route_short_name")], by = "route_id", keep = FALSE)
names(stop_times_patt)

stop_times_patt_grp <- group_by(stop_times_patt, pattern_id, stop_id) %>% arrange(arrival_time_secs, .by_group = TRUE)


headways <- mutate(stop_times_patt_grp, diff = arrival_time_secs-lag(arrival_time_secs)) %>%
  ##convert seconds to minutes
  mutate(headway_m = diff/60)
range(headways$headway_m, na.rm = TRUE)

headways_fil <- headways[headways$headway_m > 3 & headways$headway_m < 90,]
range(headways_fil$headway_m, na.rm = TRUE)

#plot
headways_mean <- headways_fil %>% group_by(route_short_name)%>%
  summarise(mean_hw = mean(headway_m))
##spit out plot
ggplot(headways_mean, aes(x=mean_hw)) +
  ##MTA colors
  geom_histogram(binwidth = 5, color = "#FDB90B", fill = "#A30330", na.rm = TRUE) +
  scale_x_continuous(name = "Mean Headways by Routes (minutes)", breaks=seq(5,90,5))+
  ylab("Count (Routes)") +
  theme(panel.grid.minor.y = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank())


##output
write.csv(headways_mean, "./data/headways_mean.csv")
write.csv(headways_fil, "data/headways_fil.csv")
write.csv(headways, "data/headways.csv")
write_gtfs(gtfs_fil, "data/gtfs_fil.zip")
write_gtfs(gtfs_fil_m_f, "data/gtfs_fil_m_f.zip")
write_gtfs(gtfs_fil_sa_su, "data/gtfs_fil_sa_su.zip")
write.csv(patterns, "data/patterns.csv")