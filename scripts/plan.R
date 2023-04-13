###presentation
###calculating headways
###calculating wait times
###longest routes by time
###when does service "close"
###measuring stability/change
### census data tab
###
###
###
###
###
###
###
###
##scratch stuff

sample <- filter(gtfs_fil$routes, route_id %in% clink)

clink <- c("10984", "10985", "10986", "10987", "10988", "10989", "10990", "10991", "10992", "10993", "10994", "10995")

sample <- filter(headways_2018_fil, route_id %in% clink)



##experiment with different grouping var
##now associate the headways table you just created with stop times, which as the rest of the info we need
trips_patt <- left_join(gtfs_fil$stop_times, trips_patt, by = "trip_id")

##subset weekday service
weekday_trips <- filter(trips_patt, service_id == 1)

##group by stop ID and pattern ID
weekday_trips_grp <- group_by(weekday_trips, stop_id)

##calculate difference in seconds between arrivals at each stop, by trip
weekday_trips_grp <- mutate(weekday_trips_grp, diff = departure_time_secs-lag(departure_time_secs))

headways_2018 <- mutate(weekday_trips_grp, headways_m = diff/60)

##convert negative values to positive
headways_2018$headways_m <- abs(headways_2018$headways_m) 

##filter headways less than 3 and greater than 90 minutes per wong
headways_2018_fil <- filter(headways_2018, headways_m > 3, headways_m < 90)

sample2 <- sample <- filter(headways_2018_fil, route_id %in% clink)

sample_waits2 <- sample2 %>% 
  group_by(stop_id) %>%
  summarise(mean_wait = mean(headways_m, na.rm = TRUE))


##calculate stop waits with patterns
##stops with longest wait ca. nov 2018

headways_2018 <- ungroup(headways_2018)

stop_waits <- headways_2018 %>% 
  group_by(stop_id) %>%
  summarise(mean_wait = mean(headways_m, na.rm = TRUE))


slice_min(stop_waits, mean_wait)



min_waits <- stop_waits %>%
  slice_min(mean_wait)

check <- min_waits$stop_id

zeroes <- filter(headways_2018, stop_id %in% check)

stop_join <- inner_join(stop_waits, gtfs_fil$stops)

coordinates = c("stop_lon", "stop_lat")

stop_join <- st_as_sf(stop_join, coords = coordinates, crs = 4326)

plot(stop_join$geometry)

st_write(stop_join, "stop_new.shp")

##upon exploring, this is interesting but it's obvious that some stops might only receive service 2x day or less
##let's find out how many trips serve each stop
stop_times <- gtfs_fil$stop_times %>% group_by(stop_id)
stop_times_summ <- stop_times %>% summarise(trip_count = length(unique(trip_id)))

stop_join <- merge(stop_join, stop_times_summ)

stop_join <- arrange(stop_join, trip_count, mean_wait)

#lets remove stops that are served by <10 trips a day

stop_join <- filter(stop_join, trip_count > 10)

ggplot(stop_join, aes(x=trip_count, y=mean_wait))+
  geom_point(shape=1)


##try this on our sample
sample_waits <- sample %>% 
  group_by(stop_id) %>%
  summarise(mean_wait = mean(headways_m, na.rm = TRUE))

filter(sample, stop_id == "13")

##looks like NaNs are stops that only have 1 trip


##calculate stop waits w/ short + long headways removed






##calculate stop waits for daytime service
gtfs <- read_gtfs("data/gtfs 11_2018.zip") ##reading in gtfs
gtfs <- filter_by_route_type(gtfs, route_type = 3) ##subset bus routes
route_ids <- as.character(c(10933:10982, "10997")) ##we are going to get rid of these routes. they include commuter routes and expresslink buses
gtfs_fil <- filter_by_route_id(gtfs, route_id = route_ids, keep = FALSE) ##seeya

##associate each trip with a pattern
patterns <- get_stop_times_patterns(gtfs_fil)

##add fields for arrivals and departures in seconds after midnight
gtfs_fil <- convert_time_to_seconds(gtfs_fil)

#filter for just daytime trips (let's say between 05:30:00 and 23:30:00)
gtfs_day <- filter_by_time_of_day(gtfs_fil, "05:00:00", "23:59:59",
                                  full_trips = TRUE)


##associate pattern IDs with trips table
trips_patt_day <- gtfs_day$trips %>%
  left_join(patterns, by="trip_id")

##now associate the headways table you just created with stop times, which as the rest of the info we need
trips_patt_day <- left_join(gtfs_day$stop_times, trips_patt, by = "trip_id")

##subset weekday service
weekday_trips_day <- filter(trips_patt, service_id == 1)

##group by stop ID and pattern ID
weekday_trips_grp_day <- group_by(weekday_trips_day, pattern_id, stop_sequence)

##calculate difference in seconds between arrivals at each stop, by trip
weekday_trips_grp_day <- mutate(weekday_trips_grp_day, diff = departure_time_secs-lag(departure_time_secs))

##convert diff column to minutes
headways_2018_day <- mutate(weekday_trips_grp_day, headways_m = diff/60)

##convert negative values to positive
headways_2018_day$headways_m <- abs(headways_2018_day$headways_m) 

##filter headways less than 3 and greater than 90 minutes per wong
headways_2018_fil_day <- filter(headways_2018, headways_m > 3, headways_m < 90) ##this also gets rid of NAs, which feels kind of sketchy but it'll work for this analysis.

##group by pattern ID
headways_2018_fil <- headways_2018_fil %>% group_by (pattern_id)




######wait times day-only vs. 24h
##stops with longest wait ca. nov 2018
##day
headways_2018_day <- ungroup(headways_2018_day)

stop_waits_day <- headways_2018_day %>% 
  group_by(stop_id) %>%
  summarise(mean_wait = mean(headways_m, na.rm = TRUE))

#24h
headways_2018_all <- ungroup(headways_2018)

stop_waits_all <- headways_2018 %>% 
  group_by(stop_id) %>%
  summarise(mean_wait = mean(headways_m, na.rm = TRUE))


slice_min(stop_waits_day, mean_wait)

slice_min(stop_waits_all, mean_wait)

#fil 24h
headways_2018_fil_all <- ungroup(headways_2018_fil)

stop_waits_fil_all <- headways_2018_fil_all %>% 
  group_by(stop_id) %>%
  summarise(mean_wait = mean(headways_m, na.rm = TRUE))


#fil day
headways_2018_fil_day <- ungroup(headways_2018_fil_day)

stop_waits_fil_day <- headways_2018_fil_day %>% 
  group_by(stop_id) %>%
  summarise(mean_wait = mean(headways_m, na.rm = TRUE))

#fil 12h
headways_2018_fil_12h <- ungroup(headways_2018_fil_12h)

stop_waits_fil_12h <- headways_2018_fil_12h %>% 
  group_by(stop_id) %>%
  summarise(mean_wait = mean(headways_m, na.rm = TRUE))


min_waits <- stop_waits %>%
  slice_min(mean_wait)

check <- min_waits$stop_id

zeroes <- filter(headways_2018, stop_id %in% check)

stop_join <- inner_join(stop_waits, gtfs_fil$stops)

coordinates = c("stop_lon", "stop_lat")

stop_join <- st_as_sf(stop_join, coords = coordinates, crs = 4326)

plot(stop_join$geometry)

st_write(stop_join, "stop_new.shp")

##upon exploring, this is interesting but it's obvious that some stops might only receive service 2x day or less
##let's find out how many trips serve each stop
stop_times <- gtfs_fil$stop_times %>% group_by(stop_id)
stop_times_summ <- stop_times %>% summarise(trip_count = length(unique(trip_id)))

stop_join <- merge(stop_join, stop_times_summ)

stop_join <- arrange(stop_join, trip_count, mean_wait)

#lets remove stops that are served by <10 trips a day

stop_join <- filter(stop_join, trip_count > 10)

ggplot(stop_join, aes(x=trip_count, y=mean_wait))+
  geom_point(shape=1)


##try this on our sample
sample_waits <- sample %>% 
  group_by(stop_id) %>%
  summarise(mean_wait = mean(headways_m, na.rm = TRUE))

filter(sample, stop_id == "13")

##looks like NaNs are stops that only have 1 trip


###

##stops with longest wait ca. nov 2018

headways_2018 <- ungroup(headways_2018)

stop_waits <- headways_2018 %>% 
  group_by(stop_id) %>%
  summarise(mean_wait = mean(headways_m, na.rm = TRUE))


slice_min(stop_waits, mean_wait)



min_waits <- stop_waits %>%
  slice_min(mean_wait)

check <- min_waits$stop_id

zeroes <- filter(headways_2018, stop_id %in% check)

stop_join <- inner_join(stop_waits, gtfs_fil$stops)

coordinates = c("stop_lon", "stop_lat")

stop_join <- st_as_sf(stop_join, coords = coordinates, crs = 4326)

plot(stop_join$geometry)

st_write(stop_join, "stop_new.shp")

##upon exploring, this is interesting but it's obvious that some stops might only receive service 2x day or less
##let's find out how many trips serve each stop
stop_times <- gtfs_fil$stop_times %>% group_by(stop_id)
stop_times_summ <- stop_times %>% summarise(trip_count = length(unique(trip_id)))

stop_join <- merge(stop_join, stop_times_summ)

stop_join <- arrange(stop_join, trip_count, mean_wait)


##calculate stop waits for 12h service day
gtfs <- read_gtfs("data/gtfs 11_2018.zip") ##reading in gtfs
gtfs <- filter_by_route_type(gtfs, route_type = 3) ##subset bus routes
route_ids <- as.character(c(10933:10982, "10997")) ##we are going to get rid of these routes. they include commuter routes and expresslink buses
gtfs_fil <- filter_by_route_id(gtfs, route_id = route_ids, keep = FALSE) ##seeya

##associate each trip with a pattern
patterns <- get_stop_times_patterns(gtfs_fil)

##add fields for arrivals and departures in seconds after midnight
gtfs_fil <- convert_time_to_seconds(gtfs_fil)

#filter for just daytime trips (let's say between 05:30:00 and 23:30:00)
gtfs_12h <- filter_by_time_of_day(gtfs_fil, "07:00:00", "19:00:00",
                                  full_trips = TRUE)


##associate pattern IDs with trips table
trips_patt_12h <- gtfs_12h$trips %>%
  left_join(patterns, by="trip_id")

##now associate the headways table you just created with stop times, which as the rest of the info we need
trips_patt_12h <- left_join(gtfs_12h$stop_times, trips_patt_12h, by = "trip_id")

##subset weekday service
weekday_trips_12h <- filter(trips_patt_12h, service_id == 1)

##group by stop ID and pattern ID
weekday_trips_grp_12h <- group_by(weekday_trips_12h, pattern_id, stop_sequence)

##calculate difference in seconds between arrivals at each stop, by trip
weekday_trips_grp_12h <- mutate(weekday_trips_grp_12h, diff = departure_time_secs-lag(departure_time_secs))

##convert diff column to minutes
headways_2018_12h <- mutate(weekday_trips_grp_12h, headways_m = diff/60)

##convert negative values to positive
headways_2018_12h$headways_m <- abs(headways_2018_12h$headways_m) 

##filter headways less than 3 and greater than 90 minutes per wong
headways_2018_fil_12h <- filter(headways_2018_12h, headways_m > 3, headways_m < 90) ##this also gets rid of NAs, which feels kind of sketchy but it'll work for this analysis.

##group by pattern ID
headways_2018_fil_12h <- headways_2018_fil_12h %>% group_by (pattern_id)



###how long of a service period is represented by each stop?
###these stops have high trip counts...

sample_6 <- sample_6 %>% group_by(pattern_id, stop_id)
> sample_6 <- sample_6 %>% arrange(departure_time_secs, .by_group = TRUE)
###calculating headways using diff group_by and arrange
weekday_trips_asc <- group_by(weekday_trips, pattern_id, stop_id)

weekday_trips_asc <- weekday_trips_asc %>% arrange(departure_time_secs, .by_group = TRUE)

##calculate difference in seconds between arrivals at each stop, by trip
weekday_trips_asc <- mutate(weekday_trips_asc, diff = departure_time_secs-lag(departure_time_secs))

##convert diff column to minutes
headways_2018_asc <- mutate(weekday_trips_asc, headways_m = diff/60)

##convert negative values to positive
headways_2018$headways_m <- abs(headways_2018$headways_m) 

##filter headways less than 3 and greater than 90 minutes per wong
headways_2018_asc_fil <- filter(headways_2018_asc, headways_m > 3, headways_m < 90) ##this also gets rid of NAs, which feels kind of sketchy but it'll work for this analysis.

headways_2018_asc_fil <- headways_2018_asc[headways_2018_asc$headways_m > 3 & headways_2018_asc$headways_m < 90,]


##lets get stop_waits
##calculate mean headways by stop
stop_waits_asc <- headways_2018_asc_fil %>% 
  group_by(stop_id) %>%
  summarise(mean_wait = mean(headways_m, na.rm = TRUE))

stop_join_asc <- inner_join(stop_waits_asc, gtfs_fil$stops)

coordinates = c("stop_lon", "stop_lat")

stop_join_asc <- st_as_sf(stop_join_asc, coords = coordinates, crs = 4326)

sample_14 <- filter(stop_join_asc, stop_id %in% cstops)


##when does service shut down?

last_serv <- headways_2018_asc_fil %>% 
  group_by(stop_id) %>%
  summarise(last_serv = max(departure_time_secs)) %>%
  mutate(hours = last_serv/60)

stop_last_serv <- inner_join(last_serv, gtfs_fil$stops)
  
stop_last_serv<- st_sf(stop_last_serv, cooords = coordinates, crs = 4326)

commuter_route_ids <- gtfs$routes[which(gtfs$routes$route_short_name %in% commuter_routes), "route_id"]

commuter_route_ids_ids <- c(commuter_route_ids$route_id)


#filter by weekday
m_f <- c("monday", "tuesday", "wednesday", "thursday", "friday")
sa_su <- c("saturday", "sunday")
gtfs_fil_m_f <- filter_by_weekday(gtfs_fil, m_f, combine = "and")
gtfs_fil_sa_su <- filter_by_weekday(gtfs_fil, sa_su, combine = "and" )

weekday_trips <- filter(trips_patt, service_id == 1)
unique(weekday_trips$service_id)

unique(gtfs_fil_wd$trips$service_id)

#lets get route_name in here as well
stop_times_patt <- left_join(gtfs_fil_m_f$stop_times, patterns, by="trip_id") %>%
  left_join(gtfs_fil_m_f$trips[ , c("trip_id", "route_id")], by = "trip_id", keep = FALSE)%>%
  left_join(gtfs_fil_m_f$routes[ , c("route_id", "route_short_name")], by = "route_id", keep = FALSE)

stop_times_patt <- stop_times_patt %>% left_join(gtfs_fil_m_f$trips[ , c("trip_id", "route_id")], by = "trip_id", keep = FALSE)%>% left_join(gtfs_fil_m_f$routes[ , c("route_id", "route_short_name")], by = "route_id", keep = FALSE)


#headways by time of day scratch
headways_fil$arrival_time <- as.POSIXct(headways_fil$arrival_time) #fuck some of these times aren't prepended by zeroes. figure that out later

head_day_plot <- ggplot(f_hw, aes(x = f_t, y = f_hw)) +
  geom_line()

#maybe group_by trip_id, then take the first headway for each trip
#
#
#
#
#

hw_day_plot <- group_by(headways_fil, trip_id)

f_hw <- summarise(hw_day_plot, f_hw = first(headway_m), f_t = first(arrival_time_secs)) #this kind of works