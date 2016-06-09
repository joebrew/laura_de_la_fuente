library(foreign)
library(readstata13)
library(dplyr)
library(readr)
library(sp)
library(raster)
library(leaflet)
library(ggplot2)
library(rgeos)
library(maptools)
library(plotly)

# Read in tesfam
tesfam <- read.dta13('TESFAM_variables_selecion_02.dta')

# Read in coords
coords <- read_csv('Coordenadas.csv')
coords <- coords[!duplicated(coords$Family_id),]

# Join tesfam and coords
tesfam <- left_join(x = tesfam,
                    y = coords,
                    by = c('familynum' = 'Family_id')) %>%
  # filter(!duplicated(familynum)) %>%
  mutate(lon = LongUTM,
         lat = LatUTM,
         x = LongUTM,
         y = LatUTM)

# Make tesfam spatial
tesfam <- tesfam[!is.na(tesfam$x),]
coordinates(tesfam) <- ~x+y

# Make tesfam latlong
proj4string(tesfam) <- CRS("+proj=utm +zone=36 +south")
tesfam <- spTransform(tesfam,CRS("+proj=longlat +ellps=WGS84 +datum=WGS84"))

# Overwrite lat long
tesfam$lon <- coordinates(tesfam)[,1]
tesfam$lat <- coordinates(tesfam)[,2]

# Get mozambique shapefile
moz <- getData('GADM', country = 'MOZ', level = 2)
moz3 <- getData('GADM', country = 'MOZ', level = 3)

# Subset to manhica
man <- moz[moz@data$NAME_2 == 'Manhiça',]
man3 <- moz3[moz3@data$NAME_2 == 'Manhiça',]

# Fortify
moz3_fortified <- fortify(moz3, region = 'NAME_3')
man3_fortified <- fortify(man3, region = 'NAME_3')


# Create a color based on testgroup
tesfam$color <-
  ifelse(tesfam$testgroup == 'HBT', 'blue',
         ifelse(tesfam$testgroup == 'VCT', 'darkorange',
                ifelse(tesfam$testgroup == "PICT", 'darkgreen',
                       NA)))

# Create a shape based on attrition
tesfam$shape <- 
  ifelse(tesfam$abandono_3mo == 1, 17,
         ifelse(tesfam$abandono_3mo == 0, 16, NA))

# Plot using base map
pdf('map_3_months.pdf')
plot(man3, col = 'grey', border = 'white')
points(tesfam, 
       col = adjustcolor(tesfam$color, alpha.f = 0.7), 
       pch = tesfam$shape, 
       cex = 0.2)

# Add legend
legend('topleft',
       col = c('blue', 'darkorange', 'darkgreen'),
       legend = c('HBT', 'VCT', 'PICT'),
       title = 'Testing venue',
       pch = 1)
legend('bottomright',
       pch = c(17, 16),
       legend = c('Lost to follow-up', 'Followed-up'),
       title = 'Linkage at 3 months')
title(main = 'HIV cascade for TESFAM cohort at 3 months')
dev.off()

cols <- c('blue', 'darkorange', 'darkgreen')
g <- 
  ggplot() +
coord_map() +
  geom_polygon(data = man3_fortified,
               aes(x = long, y =lat, group = group), 
               fill = 'grey', color = 'white') +
  geom_point(data = tesfam@data %>% 
               mutate(`Follow up` = ifelse(abandono_3mo == 1, 'Lost',
                                                                'Not lost')),
              aes(x = lon, y = lat,
                  color = testgroup,
                  pch = `Follow up`)) +
  scale_color_manual(name = 'Testing venue',
                     values = cols) +
  xlab('Longitude') +
  ylab('Latitude') +
  labs(title = 'HIV cascade spatial distribution',
       subtitle = '3 months')
g
ggplotly(g)

# Leaflet
library(leaflet)
leaflet() %>%
  addProviderTiles('Stamen.Watercolor') %>%
  addCircleMarkers(lng = tesfam$lon,
                  lat = tesfam$lat,
                  fillColor = tesfam$color,
                  opacity = 0)
