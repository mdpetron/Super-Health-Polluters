###### Mike Petroni 5/20/2021
# This study ranks EGU's based on health costs, EJ considerations, and Carbon Emissions

###
######## Maps
###

library(tmap)
library(sf)

#generator map 
#here we do a four panel map with marginal emissions costs 

facmap <- st_as_sf(as.data.frame(dat_fac), coords = c("LON", "LAT"))
summary(facmap$marg_pm)
library(spData)
library(tmaptools)
us_states2163 = st_transform(us_states, 2163)

nox <- tm_shape(us_states2163) +
  tm_polygons() +
  tm_shape(facmap) +
  tm_dots(size = .1, shape = 18, col = "marg_nox",
          breaks = c(1000, 5000, 10000, 50000, 100000, 500000, 1000000, 5000000),
          palette = "YlOrRd",
          legend.show = FALSE,
          title = "2018 Marginal NOx Health Costs")  +
  tm_layout(title= 'Nitrogen Oxides', 
            title.position = c('left', 'bottom'),
            title.size = .8)
pm <- tm_shape(us_states2163) +
  tm_polygons() +
  tm_shape(facmap) +
  tm_dots(size = .1, shape = 18, col = "marg_pm",
          breaks = c(1000, 5000, 10000, 50000, 100000, 500000, 1000000, 5000000),
          palette = "YlOrRd",
          legend.show = FALSE,
          title = "2018 Marginal NOx Health Costs")  +
  tm_layout(title= 'Primary Particulate Matter 2.5', 
            title.position = c('left', 'bottom'),
            title.size = .8)
so2 <- tm_shape(us_states2163) +
  tm_polygons() +
  tm_shape(facmap) +
  tm_dots(size = .1, shape = 18, col = "marg_so2",
          breaks = c(1000, 5000, 10000, 50000, 100000, 500000, 1000000, 5000000),
          palette = "YlOrRd",
          legend.show = FALSE,
          title = "2018 Marginal NOx Health Costs")  +
  tm_layout(title= 'Sulphur Dioxide', 
            title.position = c('left', 'bottom'),
            title.size = .8)

legend <- tm_shape(us_states2163) +
  tm_borders(col = "#ffffff") + tm_add_legend(type = "fill", 
                col = c(tmaptools::get_brewer_pal("YlOrRd", n = 7, plot = F)),
                labels = c("$1,000 to $5,000", "$5,000 to $10,000",
                           "$10,000 to $50,000", "$50,000 to $100,000",
                           "$100,000 to $500,000", "$500,000 to $1,000,000",
                           "$1,000,000 to $5,000,000"),
                title = "Marginal Emissions Cost Per Ton") +
  tm_legend(legend.outside = F,
    legend.title.size = 3.8,
    legend.text.size = 2.3,
    legend.bg.color = "white",
    legend.bg.alpha = 1,
    legend.position = c("left", "top")) + 
  tm_layout(frame = FALSE,
            legend.width = .95)

tmap_arrange(nox, pm, so2, legend)



#Population characteristics map 

RColorBrewer::brewer.pal(6, "YlOrRd") 

#full on 
us_states2163 = st_transform(us_states, 2163)

tm_shape(us_states2163) +
  tm_polygons(col = "#989898") +
  tm_shape(facmap) +
  tm_dots(size = "VUL_POP_50k", shape = 18, alpha = .5,
          col = "VUL_POP_50k",
          palette = "viridis",
          style = "pretty",
          title = "Total Vulnerable Population Within 50km",
          title.size = "Total Vulnerable Population Within 50km") +
  tm_layout(legend.outside = T,
            frame = FALSE)


  tm_fill(col = "tot_cost_em",
          style = "quantile",
          title = "Emissions Health Costs 2018",
          legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f"), "%"))) 

#EJ map

################## data ########
# we use the generator, ej, and bg pop center point data from the data aqusion file
generators2 <- dat12 %>% filter(STATE_CODE == 'AL') %>%
  dplyr::select(LON, LAT, ORISPL, PNAME) %>% distinct()
############ make a map of the buffer analysis 
#select facility
sdf1 <- dat_fac %>% filter(PNAME == "James H Miller Jr")
#make facility spatial
sdf <- dat_fac %>% filter(PNAME == "James H Miller Jr")
coordinates(sdf) <- c("LON", "LAT")
#obtain the polygons for block groups in facility state
bama <- block_groups(state = "AL")
#make buffers 
mybuff10k <- st_as_sf(buffer(sdf, width = 10000))
mybuff5k <- st_as_sf(buffer(sdf, width = 5000))
mybuff50k <- st_as_sf(buffer(sdf, width = 50000))
#get intersections for the 50k intersection
inter50k <- st_intersection(points, mybuff50k)
st_geometry(inter50k) = NULL
inter50k <- inter50k %>%
  mutate(GEOID = paste0(STATEFP,COUNTYFP,TRACTCE,BLKGRPCE))
bama2 <- subset(bama, bama$GEOID %in% inter50k$GEOID)
bama2 <- merge(bama2, EJscreen, by.x = "GEOID", by.y = "ID", all.x = T)

# make buffers outside of SF
mybuff10k <- buffer(sdf, width = 10000)
mybuff5k <- buffer(sdf, width = 5000)
mybuff50k <- buffer(sdf, width = 50000)

#intersect buffer with blockgroup population centerpoints
inter10k <- st_intersection(points, mybuff10k)
inter5k <- st_intersection(points, mybuff5k)
inter50k <- st_intersection(points, mybuff50k)

bama1 <- subset(bama, bama$GEOID %in% inter10k$GEOID)
bama2 <- subset(bama, bama$GEOID %in% inter50k$GEOID)

bama1 <- merge(bama1, EJscreen, by.x = "GEOID", by.y = "ID", all.x = T)
bama2 <- merge(bama2, EJscreen, by.x = "GEOID", by.y = "ID", all.x = T)

###make a sf obj of the facility
sdf2 <- st_as_sf(as.data.frame(sdf1), coords = c("LON", "LAT"))
bama2$VULEOPCT <- bama2$VULEOPCT*100 
################# maps ############

#set the border
bbox_new <- st_bbox(bama2) # current bounding box
xrange <- bbox_new$xmax - bbox_new$xmin # range of x values
yrange <- bbox_new$ymax - bbox_new$ymin # range of y values
# bbox_new[1] <- bbox_new[1] - (0.25 * xrange) # xmin - left
bbox_new[3] <- bbox_new[3] + (1 * xrange) # xmax - right
# bbox_new[2] <- bbox_new[2] - (0.25 * yrange) # ymin - bottom
#bbox_new[4] <- bbox_new[4] + (0.2 * yrange) # ymax - top
bbox_new <- bbox_new %>%  # take the bounding box ...
  st_as_sfc() # ... and make it a sf polygon

map1a   <-   tm_shape(bama2, bbox = bbox_new) + tm_borders() +
  tm_fill(col = "VULEOPCT",
          style = "quantile",
          title = "EJSCREEN Demographic Index - \nLow Income and Minority Percentage",
          legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f"), "%"))) +
  tm_shape(mybuff50k) +
  #tm_fill(col = "#f2f2f2", alpha = .3) +
  tm_borders(lwd = 2) +
  tm_shape(mybuff10k) +
  #tm_fill(col = "#f2f2f2", alpha = .3) +
  tm_borders(lwd = 2) +
  tm_shape(mybuff5k) +
  tm_borders(lwd = 2) +
  #tm_fill(col = "#f2f2f2", alpha = .3) + 
   tm_shape(sdf2) + tm_symbols(size = .5, shape = 18, col = "#000000") +
  tm_compass(type = "4star", position = c(0.01, 0.83),
             size = 3) +
  tm_scale_bar(breaks = c(0, 10),
               position = c(0.4, 0.01),
               text.size = 1) +
  tm_legend(#legend.outside = TRUE,
            legend.title.size = 2,
            legend.text.size = 1.5,
            legend.bg.color = "white",
            legend.bg.alpha = 1,
            legend.position = c("right", "top"), 
            main.title= 'Alabama Power Company (Miller Power Plant)', 
            title.position = c('left', 'top'))
   #tm_text("Name", just = "bottom", xmod = 0.5, size = 0.8)

map1a


## NEW YORK!
#state shape
shapeny <- st_read(dsn = "C:/Users/Mike Petroni/Documents/GitHub/NYenviroScreen/data/NYS_Civil_Boundaries_SHP",
                   layer = "State_Shoreline")
#counties 
nycounties <- st_read(dsn = "C:/Users/Mike Petroni/Documents/GitHub/NYenviroScreen/data/NYS_Civil_Boundaries_SHP",
                      layer = "Counties_Shoreline")
pejas_shp <- readOGR(dsn = "./Data/PEJA_NY_2021", layer = "PEJA")
pejas_shp <- subset(pejas_shp, pejas_shp$PEJA == "Yes")

NY_facs_mp <- st_as_sf(as.data.frame(NY_facs), coords = c("LON", "LAT")) 

NY_facs_mp$EJ_benefit_per <- NY_facs_mp$EJ_benefit_per*100

tm_shape(shapeny) +
  tm_polygons(col = "#cbcbcb") +
  tm_shape(pejas_shp) +
  tm_fill(col = "#8a6363") +
  tm_shape(NY_facs_mp) +
  tm_dots(size = "EJ_benefit", shape = 18, alpha = .9,
          scale = 2,
          shapes.style = "pretty",
          sizes.legend = c(200000, 2000000, 20000000, 200000000),
          sizes.legend.labels = c("$200,000", "$2Mil", "$20Mil", "$200Mil"),
          shapes.legend.fill = "grey80",
          col = "EJ_benefit_per",
          palette = "viridis",
          style = "pretty",
          legend.format=list(fun=function(x) paste0(formatC(x, digits=0, format="f"), " %")),
          title = "Share of Total Co-Pollutant Reduction \nBenefit in EJ Communities Within 50Km",
          title.size = "Generator Co-Pollutant Reduction \nEJ Benefit Within 50km") +
  tm_add_legend(type = "fill", 
                col = "#8a6363",
                labels = c("Potential Environmental Justice Area")) +
  tm_layout(legend.outside = T,
            frame = FALSE) +
  tm_legend(legend.title.size = 2,
            legend.text.size = 1.5)


