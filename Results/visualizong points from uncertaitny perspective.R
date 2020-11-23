#visualizong points from uncertaitny perspective 
library(ggplot2)
data(land)
map <- map_data("world")

Tremarctos_ornatus <-
  load("data/Tremarctos_ornatus.csv",
    escape_double = FALSE,
    trim_ws = TRUE)


T_ornatus <- dplyr::select(Tremarctos_ornatus,
  decimalLatitude,
  decimalLongitude,
  coordinateUncertaintyInMeters,
  countryCode,
  species)

T_ornatus_NA <- dplyr::filter(T_ornatus, is.na(coordinateUncertaintyInMeters))

T_ornatus_Ec <- dplyr::filter(Tremarctos_ornatus, countryCode == 'EC')
save(T_ornatus_Ec, file = "../Chapter_uno/data/T_ornatus_Ec.rda")
load(file = "../Chapter_uno/data/T_ornatus_Ec.rda")

Tremarctos_ornatus <- Tremarctos_ornatus
T_ornatus_Co <- dplyr::filter(Tremarctos_ornatus, countryCode == 'CO')
save(Tremarctos_ornatus, file = "../Chapter_uno/data/Tremarctos_ornatus.rda")

p <- ggplot(data = Tremarctos_ornatus,
            aes(x = decimalLongitude, y = decimalLatitude, color= coordinateUncertaintyInMeters)) +
  geom_polygon(
    data = map,
    aes(x = long, y = lat, group = group),
    fill = "white",
    colour = "black"
  ) +
  coord_fixed(xlim = c(-85,-70), ylim = c(-5, 10)) +
  
  #coordinate w UncertaintyInMeters
  geom_point(
    aes(x = decimalLongitude, y = decimalLatitude, color = coordinateUncertaintyInMeters),
    shape = 19,
    alpha = 0.5
  ) +
  scale_colour_gradient(low = "green", high = "red", name = "Uncertainty in meters") +
  
  #coordinate w/o UncertaintyInMeters
  geom_point(
    data = Tremarctos_ornatus %>% filter (is.na(coordinateUncertaintyInMeters)),
    aes(x = decimalLongitude, y = decimalLatitude),
    shape = 111,
    color = "blue"
  ) + 
  scale_color_manual(labels = NA, values = "blue")
  
  theme_bw() +
  labs(x = "Longitude" , y = "Latitude", title = "T. ornatus occurrences and associated \n uncertainty in meters")
  
#----
#observed data
library(ggplot2)
data(land)
map <- map_data("world")
ggplot(data = Tremarctos_ornatus,
       aes(x = decimalLongitude, y = decimalLatitude)) +
  geom_polygon(
    data = map,
    aes(x = long, y = lat, group = group, color = factor(countryCode)),
    fill = "NA",
    colour = "black"
  ) +
  coord_fixed(xlim = c(-85,-70), ylim = c(-5, 10)) +
  
  
  #coordinate w/o UncertaintyInMeters 
  geom_point(shape=".", alpha = 0.5, colour = "red")+
  ##coordinate w UncertaintyInMeters

  geom_point(
    aes(size  = coordinateUncertaintyInMeters),
    shape = 1,
    colour = "blue",
    alpha = 0.2
  ) +
  
  geom_point(
    aes(size  = 1000<coordinateUncertaintyInMeters),
    shape = 1,
    colour = "green",
    alpha = 0.2
  )+


  ##coordinate w UncertaintyInMeters
  geom_point(aes(x = decimalLongitude, y = decimalLatitude),
             shape = ".",
             fill = "red",
             alpha = 0.5
             ) +
  #Zero uncertinty
  geom_point(aes(size  = 0),
             colour = "pink",
             shape = 1,
             alpha = 0.05) +
  #Labels
  guides(size = guide_legend("Uncetainty in Meters"), shape = ) +
  labs(x = "Longitude" , y = "Latitude", title = "T. ornatus occurrences and  associated \n georeference uncertainty in meters")
