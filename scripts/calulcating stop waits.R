##stops with longest wait ca. nov 2018

headways_fil <- ungroup(headways_fil)

##calculate mean headways by stop
stop_waits <- headways_fil %>% 
              group_by(stop_id) %>%
              summarise(mean_wait = mean(headway_m, na.rm = TRUE))

#check the shortest mean wait
slice_min(stop_waits, mean_wait)


##more checking
min_waits <- stop_waits %>%
            slice_min(mean_wait)

check <- min_waits$stop_id

zeroes <- filter(headways_2018, stop_id %in% check)

#join it to stops table
stop_join <- inner_join(stop_waits, gtfs_fil$stops)

##make it spatial
coordinates = c("stop_lon", "stop_lat")

stop_join <- st_as_sf(stop_join, coords = coordinates, crs = 4326) 

plot(stop_join$geometry)

ggplot(stop_join) +
  geom_sf(aes(color = mean_wait), size = 1.5) +
  
#plotting all waits
waits_plot <- ggplot(stop_join) +
  geom_sf(aes(color = mean_wait), size = 1.5) +
  scale_colour_distiller(palette = "YlGnBu", direction = 1)

#grab 25 longest waits
top_25 <- stop_join %>%
  slice_max(mean_wait, n = 25) 
top_25$mean_wait <- round(top_25$mean_wait, digits = 2)


waits_plot +
  geom_sf(top_10, mapping = aes(geometry = geometry)) +
  


top_10 <- stop_join %>%
  slice_max(mean_wait, n = 10)

top_10$mean_wait <- round(top_10$mean_wait, digits = 2)


ggplot(top_25)+
  geom_sf()+
  geom_sf_label(aes(label = mean_wait))

ggplot(top_10)+
  geom_sf()+
  geom_sf_label(aes(label = mean_wait))


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

#lets remove stops that are served by <10 trips a day

stop_join <- filter(stop_join, trip_count > 10)

ggplot(stop_join, aes(x=trip_count, y=mean_wait))+
  geom_point(shape=1)


##try this on our sample
sample_waits <- sample %>% 
  group_by(stop_id) %>%
  summarise(mean_wait = mean(headways_m, na.rm = TRUE))

filter(sample, stop_id == "13")

sample_2

sample_3 <- filter(stop_join, stop_id == "2329")

plot(sample_3$geometry)

sample_4 <- filter(headways_2018_fil, stop_id == "2329")

sample_5 <- filter(headways_2018_fil, stop_id %in% sample_stops)

summary(sample_5$headways_m)

charles <- read.csv("C:\\Users\\farep\\Documents\\GitHub\\farepay\\exploring_MTA\\charles_stops.csv")

cstops <- as.character(charles$stop_id)

sample_6 <- filter(headways_2018_fil, stop_id %in% cstops)

summary(sample_6$headways_m)

highc <- filter(sample_6, headways_m == 85)

sample_7 <- mutate(sample_7, diff = departure_time_secs-lag(departure_time_secs))

sample_7 <- mutate(sample_7, headways_m = diff/60)

##convert negative values to positive
sample_7$headways_m <- abs(sample_7$headways_m) 


sample_6 <- mutate(sample_6, diff = departure_time_secs-lag(departure_time_secs))

sample_6 <- mutate(sample_6, headways_m = diff/60)

##convert negative values to positive
sample_6$headways_m <- abs(sample_6$headways_m) 

counties <- c("Baltimore", "Baltimore city", "Anne Arundel county", "Anne Arundel County", "Baltimore County", "Baltimore county")

pt.18 <- get_acs(geography = "tract", 
                 variables = c("pt" = pt,
                               "bus" = bus,
                               "totpop" = tp),
                 state = "MD",
                 year = 2018,
                 survey = "acs5",
                 output = "wide",
                 geometry = TRUE,
                 cb = TRUE) %>% 
  filter(str_detect(NAME, "Baltimore")|str_detect(NAME, "Anne Arundel")|str_detect(NAME, "Howard")) %>%
  st_transform(4326)
pt.18$year <- 2018

j1 <- st_join(pt.18, stop_join_asc, join = st_within)

j2 <- st_join(stop_join_asc, pt.18, join = st_within)

point_mean <- j2 %>% group_by(GEOID) %>% summarise(tract_mean = mean(mean_wait))

j3 <- st_join(pt.18, point_mean)

plot.tm <- ggplot(j3, aes(fill = tract_mean))+
  geom_sf()+
  scale_fill_continuous_sequential(palette = "Greens")+
  theme_void()