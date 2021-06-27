library(ggmap)
register_google(key = "########################################", account_type = "standard")

ggmap(get_map("New York City", maptype = "roadmap"))

nydata_streets<- nydata[is.na(nydata$ON.STREET.NAME) == FALSE,]


contmap<- read.csv("cont.csv", stringsAsFactors = FALSE)

nydata_streets_contfact<- merge(x=nydata_streets, 
                                y= contmap, by=("CONTRIBUTING.FACTOR.VEHICLE.1"), all.x = TRUE)


#library(dplyr)
nystreets <- arrange(nydata_streets_contfact %>%
  group_by(ON.STREET.NAME, Cont, hour) %>%
  summarise(TOTAL.killed = sum(NUMBER.OF.PERSONS.KILLED, na.rm = TRUE),
            TOTAL.injured = sum(NUMBER.OF.PERSONS.INJURED, na.rm = TRUE),
            TOTAL.incidents = n_distinct(UNIQUE.KEY, na.rm = TRUE)), desc(TOTAL.incidents))

nystreets <- nystreets[is.na(nystreets$Cont) == FALSE,]



nystreets$daysplit <- ifelse(nystreets$hour<7, "Night", "Day")

nystreets_summary <- arrange(nystreets %>%
                       group_by(ON.STREET.NAME, Cont, daysplit) %>%
                       summarise(TOTAL.killed = sum(TOTAL.killed, na.rm = TRUE),
                                 TOTAL.injured = sum(TOTAL.injured, na.rm = TRUE),
                                 TOTAL.incidents = sum(TOTAL.incidents, na.rm = TRUE)),
                     desc(TOTAL.incidents))


daymap<-nystreets_summary[nystreets_summary$daysplit=="Day",]

nightmap<-nystreets_summary[nystreets_summary$daysplit=="Night",]

nightmap<-nightmap[(nightmap$Cont=="DUI" | nightmap$Cont=="OverSpeeding"),]
daymap<-daymap[(daymap$Cont=="DriverFault" | daymap$Cont=="DriverDistracted"),]

night<- arrange(data.frame(summarise(group_by(nightmap, ON.STREET.NAME),
                                     Total = sum(TOTAL.incidents))), desc(Total))

day <- arrange(data.frame(summarise(group_by(daymap, ON.STREET.NAME),
                                     Total = sum(TOTAL.incidents))), desc(Total))

#Selecting top 20 streets
night<-night[1:20,]
day<-day[1:20,]

#getting all coordinates for top 20 streets
night <- merge(x= night, y = nydata, by = c("ON.STREET.NAME"), all.x = TRUE)

day <- merge(x= day, y = nydata, by = c("ON.STREET.NAME"), all.x = TRUE)

#map settings
theme_set(theme_dark())
HoustonMap <- qmap("new york", zoom = 11, maptype = c("roadmap"),
                   color = "black")

#Night Map plot
HoustonMap +
  geom_point(aes(x = LONGITUDE, y = LATITUDE,
                 colour = Total),
             data = night, size = 0.5)

#Day Map plot
HoustonMap +
  geom_point(aes(x = LONGITUDE, y = LATITUDE,
                 colour = Total),
             data = day, size = 0.5)
