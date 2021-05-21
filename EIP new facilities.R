###### MIke Petroni 4/6/2021
#This study investigates health costs of permitted projects in the USA 

#set our wd
setwd("C:/Users/Mike Petroni/Documents/GitHub/Super-Health-Polluters/")

#packages
library(rgdal)
library(dplyr)
library(readxl)
library(readr)
library(rgeos)
library(geosphere)
library(gdistance)

# proposed projects
EIP_raw  <- read_excel("C:/Users/Mike Petroni/Downloads/EIP Emissions Increase Database and Pipelines Inventory_March 31, 2021.xlsx")
EIP <- EIP_raw
# valuation 
EASIUR <- readOGR(dsn = "./Data/EASIUR", layer = "Marginal-Social-Cost")

#here we will address valuation of these proposed project emissions
############################## spatial merge 
#project units onto the same map as EASIUR grid 
projection(EASIUR)
coordinates(EIP) <- c("Longitude", "Latitude")
CRS.new <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
proj4string(EIP) <- CRS.new 

#we may have a problem with AK and HI

#here is the spatial intersection
EIP <- over(EIP, EASIUR) 
EIP.df <- as.data.frame(EIP)

EIP.df <- dplyr::bind_cols(EIP_raw, EIP.df)

############################
### assign marginal costs to facilities by stack height
# ASSUME medium stack height.... 

EIP.df <- EIP.df %>%
  mutate(tot_cost_PM = as.numeric(`PM2.5 tpy`*PEC0M),
         tot_cost_SO2 = as.numeric(`SO2 tpy`*SO20M),
         tot_cost_NOX = as.numeric(`NOx tpy`*NOX0M))

EIP.df <- EIP.df %>% dplyr::select(1:51, 174:176)

EIP.sumary <- EIP.df %>% dplyr::select(1:21, 52:54)

EIP.sumary <- EIP.sumary %>% dplyr::rowwise() %>% dplyr::mutate(Health_Costs = sum(tot_cost_PM,
                                                       tot_cost_SO2,
                                                       tot_cost_NOX, na.rm = T)) %>% ungroup()
sum(EIP.df$tot_cost_PM, na.rm = T)
sum(EIP.df$tot_cost_SO2, na.rm = T)
sum(EIP.df$tot_cost_NOX, na.rm = T)

LA <- EIP.sumary %>% filter(State == "LA")
names(LA)
LAshort <- LA %>% dplyr::select(2,3,4,8,22,23,24,25)

write.csv(LAshort, "LAshort.csv")

sum(LAshort$Health_Costs)*1.37


#ok nearly 1 Billion in costs 

#lets make some charts 
# I envision a SANKEY chart here with flows from cost to sector to type

library(plotly)

fig <- plot_ly(
  type = "sankey",
  orientation = "h",
  
  node = list(
    label = c("A1", "A2", "B1", "B2", "C1", "C2"),
    color = c("blue", "blue", "blue", "blue", "blue", "blue"),
    pad = 15,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  
  link = list(
    source = c(0,1,0,2,3,3),
    target = c(2,3,3,4,4,5),
    value =  c(7,4,2,8,4,2)
  )
)
fig <- fig %>% layout(
  title = "Pre-Mature Mortality Costs of Oil and Gas Projects",
  font = list(
    size = 10
  )
)

fig







