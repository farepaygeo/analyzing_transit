
#route bar plot

headways_fil <- ungroup(headways_fil)

headways_rt <- group_by(headways_fil, route_short_name) %>% summarise(mean = mean(headway_m), median = median(headway_m), count = length(unique(trip_id)))%>% arrange(mean)

ggplot(na.omit(headways_rt), aes(x = reorder(route_short_name, -mean), y = mean, label = count))+
  geom_col(aes(fill = median)) +
  coord_flip() +
  geom_text(check_overlap = TRUE, color = "white", position = position_dodge(0.9), hjust = 0) +
    scale_fill_distiller(palette = "PuBuGn", direction = 1, name = "Median") +
    xlab("Bus Routes") +
  scale_y_continuous(name = "Mean Headway (min.)", breaks=seq(5,60,5))   +
  theme_dark() +
  ggtitle("MTA Bus Routes - Headways and Trip Counts")

scale_y_continuous(name = "Mean Headway (min.)", breaks=seq(5,60,5))


ylab("Mean Headway (minutes)")

  theme_dark()


  
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