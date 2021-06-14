###### Mike Petroni 5/20/2021
# This study ranks EGU's based on health costs, EJ considerations, and Carbon Emissions

###
######## Data Aquisition and Synthesis 
###

# set our working directory
setwd("C:/Users/Mike Petroni/Documents/GitHub/Super-Health-Polluters/")

# packages
library(rgdal)
library(dplyr)
library(readxl) 
library(readr)
library(rgeos)
library(geosphere)
library(gdistance)

####################################################################
###### Step 1: data aquisition and synthesis
## Emissions data ###

## Air Markets Program Division (AMPD) - unit characteristics 
# website - https://ampd.epa.gov/ampd/
# run a query for annula data from 1980-2021 and grab all atributes and programs
# https://ampd.epa.gov/ampd/#?bookmark=28023
ampd_emis <- read_csv("Data/AMPD 1980-2021/emission_04-06-2021_155945422.csv")
# unit types and fuel
unit_type_adder <- ampd_emis %>% filter(Year == 2018) %>%
  dplyr::select(`Unit Type`, `Fuel Type (Primary)`,
         `Facility ID (ORISPL)`, `Unit ID`) %>%
  mutate(facunitID = paste(`Facility ID (ORISPL)`,`Unit ID`)) %>% distinct()

# Emissions & Generation Resource Integrated Database (eGRID) - emissions data
# website - https://www.epa.gov/egrid 
# pmmethods - https://www.epa.gov/egrid/egrid-pm25-methodology 
# download.file("https://www.epa.gov/sites/production/files/2021-02/egrid2019_data.xlsx",
#               destfile = "Data/eGrid/egrid2019_data.xlsx", mode = "wb")
egrid18 <- read_excel("Data/eGrid/egrid2018_data_v2 (1).xlsx", 
                      sheet = "UNT18", skip = 1)
egrid18_fac <- read_excel("Data/eGrid/egrid2018_data_v2 (1).xlsx", 
                      sheet = "PLNT18", skip = 1)
egrid18_gen <- read_excel("Data/eGrid/egrid2018_data_v2 (1).xlsx", 
                      sheet = "GEN18", skip = 1)
egrid18_pm <-read_excel("Data/eGrid/egrid_draft_pm2.5_emissions_7-20-20.xlsx", 
                        sheet = "2018 PM Unit-level Data", skip = 1)

# National Emissions Inventory (NEI)
# General Documentation from - https://www.epa.gov/air-emissions-inventories/2017-national-emissions-inventory-nei-data
# SmokeFlateFile and EGU flat files - https://gaftp.epa.gov/air/nei/2017/doc/flat_files/ 
# download.file("https://gaftp.epa.gov/Air/emismod/2017/2017emissions/2017NEI_SmokeFlatFile_POINT_20200412_sectorized.zip",
#               destfile = "Data/NEI/2017NEI_SmokeFlatFile_POINT_20200412_sectorized.zip")
# unzip("Data/NEI/2017NEI_SmokeFlatFile_POINT_20200412_sectorized.zip")
# facility file
nei17_facility <- read_csv("Data/NEI/emis_sum_fac_9861.csv")
# egu SMOKE model input file 
egucems_nei17 <- read_csv("Data/NEI/egucems_2017NEI_POINT_20200412.csv")
egunoncems_nei17 <- read_csv("Data/NEI/egunoncems_2017NEI_POINT_20200412.csv")

# this will give us a source for stack heights 
nei_stack_key <- egucems_nei17 %>% 
  dplyr::select(oris_facility_code, oris_boiler_id,
         longitude, latitude, facility_id,
         stkhgt) %>% distinct()
nei_stack_key_2 <- egunoncems_nei17 %>% 
  dplyr::select(oris_facility_code, oris_boiler_id,
         longitude, latitude, facility_id, stkhgt) %>% distinct() 
nei_stack_key <- left_join(nei_stack_key, nei_stack_key_2)

# Combine with EASIUR
# Marginal Social Costs of Emissions in the US 
# https://barney.ce.cmu.edu/~jinhyok/easiur/ 
# this is a reduced form modeling run for 148X112 grids 
EASIUR <- readOGR(dsn = "./Data/EASIUR", layer = "Marginal-Social-Cost")

# this contains three stack height options and 4 season options 
# if we can get stack heights we will use those and with Anual average (An)
# https://ampd.epa.gov/ampd/#?bookmark=28023

# EPA ECHO Air Emissions dataset for ID crosswalks 
air <- read_csv("data/POLL_RPT_COMBINED_EMISSIONS.csv")

# # 2018 RSEI facility file for TRIFD to Facility ID crosswalk
# rsei_fac <- read_csv("D:/Downloads Overflow/RSEIv238_spring_Public_Release_Data/facility_data_rsei_v238.csv") 
# 
# # Risk Screening Environmental Indicators tract file   
# # http://abt-rsei.s3-website-us-east-1.amazonaws.com/?prefix=microdata2018/census_full/
# # download.file("http://abt-rsei.s3.amazonaws.com/microdata2018/census_full/censusmicroblockgroup2018_2018.csv.gz",
# #               destfile = "data/censusmicroblockgroup2018_2018.csv.gz")
# # gunzip("data/censusmicroblockgroup2018_2018.csv.gz")
# rsei_block <- read_csv("data/censusmicroblockgroup2018_2018.csv",
#                        col_names = F)
# names(rsei_block)  <- c("GEOID10",
#                         "ReleaseNumber",
#                         "ChemicalNumber",
#                         'FacilityNumber',
#                         "Media",
#                         'Conc',
#                         "ToxConc",
#                         "Score",
#                         "CSCORE",
#                         "NCSCORE",
#                         "POP")

# we are going to bring in EJSCREEN for demogrphics 
EJscreen <- read_csv("data/EJscreen/EJSCREEN_2019_USPR.csv")

# Plant retirements dates from EIA
# https://www.eia.gov/electricity/monthly/epm_table_grapher.php?t=table_6_06 
# download.file("https://www.eia.gov/electricity/monthly/xls/table_6_06.xlsx",
#               destfile = "Data/EIA/table_6_06.xlsx", mode = "wb")
retire <- read_excel("Data/EIA/table_6_06.xlsx", 
                     skip = 1)
retire_adder <- retire %>% dplyr::select(1,2,8,9,11)
names(retire_adder) <- c("retire_y",
                         "retire_m",
                         "ORISPL",
                         "UNITID", 
                         "technology") 
retire_adder$ORISPL <- as.character(retire_adder$ORISPL)

####################################################################
###### Step 2: Synthesis for Valuing Emissions

# get our 2018 emissions and stack hieghts prepped for merge with EASIUR
my_egrid_1 <- egrid18 %>% dplyr::select(ORISPL, UNITID, UNTOPST,BOTFIRTY,
                                 FUELU1, NOXAN, SO2AN, SO2CTLDV, NOXCTLDV,
                                 UNTYRONL)
my_egrid_2 <- egrid18_pm %>% dplyr::select(ORISPL, UNITID, PM25AN, PM25RT)
mu_egrid_fac_coords <- egrid18_fac %>%
  dplyr::select(ORISPL, LAT, LON, NAMEPCAP,
         PNAME, PSTATABB, PSTATABB, CNTYNAME) %>% distinct()

# merge 
dat <- left_join(my_egrid_1, my_egrid_2)
dat <- left_join(dat, mu_egrid_fac_coords)

# merge stack heights 
nei_stack_key$ORISPL <- nei_stack_key$oris_facility_code
nei_stack_key$UNITID <- nei_stack_key$oris_boiler_id
nei_stack_key$oris_facility_code <- NULL
nei_stack_key$oris_boiler_id <- NULL
dat1 <- left_join(dat, nei_stack_key) 

# ok we have some duplicates, lets inspect
dat1$facunitID <- paste(dat1$ORISPL, dat1$UNITID)
dat1$dupes <- duplicated(dat1$facunitID)
datdupes <- dat1 %>% filter(dupes == TRUE)

# so we have multiple stack heights and multiple lat long coords 
############# ASUMPTION
# lets take the egrid lat lon and the mean stack height
dat2 <- dat1 %>% group_by(facunitID) %>%
  mutate(stkhgt_mean = mean(stkhgt, na.rm =T),
         stkhgt_max = max(stkhgt, na.rm = T),
         NOXAN = sum(NOXAN, na.rm = T),
         SO2AN = sum(SO2AN, na.rm = T),
         PM25AN = sum(PM25AN, na.rm = T)) %>%
  ungroup() %>%
  dplyr::select(-longitude, -latitude, -stkhgt, -PM25RT, -dupes) %>% distinct()
# check duplicates again
dat2 <- dat2 %>% group_by(facunitID) %>% mutate(countdupes = n()) %>% ungroup()
datdupes <- dat2 %>% filter(countdupes > 1)
addback <- datdupes %>% filter(!is.na(BOTFIRTY)) %>% dplyr::select(-countdupes)

# ok we have a fuel type issues 
# lets do a manual remove of these duplicates
# keep the one with the most data
dat3 <- dat2 %>% filter(!facunitID %in% datdupes$facunitID) %>% dplyr::select(-countdupes)
dat3 <- rbind(dat3, addback)

# duplicates removed
# remove facilities without emissions #####################################
dat4 <- dat3 %>% filter((NOXAN + SO2AN + PM25AN)!=0)
# check for NAs
dat3$totemis = dat3$NOXAN + dat3$SO2AN + dat3$PM25AN
summary(dat3$totemis)

############################## spatial merge 
# we cannot estiamte costs in AK and HI with this dataset
# project units onto the same map as EASIUR grid 
projection(EASIUR)
coordinates(dat4) <- c("LON", "LAT")
CRS.new <- CRS("+proj=longlat +datum=NAD83 +no_defs +ellps=GRS80 +towgs84=0,0,0")
proj4string(dat4) <- CRS.new 
# here is the spatial intersection
us.y.b <- over(dat4, EASIUR) 
us.y.df <- as.data.frame(dat4)
# this is main data set
dat5 <- cbind(us.y.df, us.y.b)

# assign marginal costs to facilities by stack height  ###########################
# If no stake height just do a means replacement 
dat6 <- dat5 %>% 
  mutate(stkhgt_mean_meters = stkhgt_mean*.3048, #convert to meters
         stkhgt_mean_meters_source = 
           ifelse(!is.na(stkhgt_mean_meters) | !is.nan(stkhgt_mean_meters),
                  "NEI2017",
                  "Mean Replacement"), #label
         stkhgt_m_m = 
           ifelse(!is.na(stkhgt_mean_meters) | !is.nan(stkhgt_mean_meters),
                  stkhgt_mean_meters,
                  mean(stkhgt_mean_meters, na.rm =T))) #means replacement 

# remove the ones that did not get a grid assignment and inspect 
noassign <- dat6 %>% filter(is.na(NOX2A))
unique(noassign$PSTATABB)

############# ASUMPTION
# here we use a custom range 5-250 = 150, less than 5 = 0, and 250+ = 300
# assign the cost based on stkhgt
dat6 <- dat6 %>% filter(!is.na(NOX2A)) %>% 
  mutate(marg_nox = ifelse(stkhgt_m_m > 5 & stkhgt_m_m <= 250,
                           NOX0M,
                           ifelse(stkhgt_m_m > 250,
                                  NOX0H, NOX0A)),
         marg_so2 = ifelse(stkhgt_m_m > 5 & stkhgt_m_m <= 250,
                           SO20M,
                           ifelse(stkhgt_m_m > 250,
                                  SO20H, SO20A)),
         marg_pm = ifelse(stkhgt_m_m > 5 & stkhgt_m_m <= 250,
                           PEC0M,
                           ifelse(stkhgt_m_m > 250,
                                  PEC0H, PEC0A)))

#### total cost by facility ###########################
# convert to metric ton, multiply 
# they are metric p11 of technical support doc 
# https://www.epa.gov/sites/production/files/2020-01/documents/egrid2018_technical_support_document.pdf
datsum <- dat6 %>%
  mutate(tot_cost_PM = marg_pm*PM25AN,
         tot_cost_SO2 = marg_so2*SO2AN,
         tot_cost_NOX = marg_nox*NOXAN)
datsum <- datsum %>%
  group_by(ORISPL, CNTYNAME,
           LAT, LON, PNAME, PSTATABB) %>%
  summarise(tot_cost_PM = sum(tot_cost_PM, na.rm = T),
            tot_cost_NOX = sum(tot_cost_NOX, na.rm = T),
            tot_cost_SO2 = sum(tot_cost_SO2, na.rm = T)) %>% ungroup()
datsum <- datsum %>%
  mutate(total_em_cost = tot_cost_PM + tot_cost_NOX + tot_cost_SO2)

# bring in generation data from eGrid
gen_dat <- egrid18_gen %>% mutate(facunitID = paste(ORISPL, GENID)) %>%
  dplyr::select(facunitID, GENNTAN, GENYRONL, GENYRRET)
dat7 <- left_join(dat6, gen_dat)
# calculate cost per MWh
dat7 <- dat7 %>%
  mutate(tot_cost_PM = marg_pm*PM25AN,
         tot_cost_SO2 = marg_so2*SO2AN,
         tot_cost_NOX = marg_nox*NOXAN,
         tot_cost_em = tot_cost_PM + tot_cost_SO2 + tot_cost_PM,
         tot_cost_per_MWh = tot_cost_em/GENNTAN)
datsum_MWh <- dat7 %>% filter(GENNTAN > 0) %>%
  mutate(cost_per_MWh_rank = ntile(tot_cost_per_MWh, 100)) %>% 
  dplyr::select(ORISPL,facunitID, FUELU1, CNTYNAME, 
                LAT, LON, PNAME, PSTATABB, GENNTAN, contains("tot_cost"))

# # SAVE
# write_rds(dat7, "costperunit_midsave.rds")
# # RESTART
# dat7 <- readRDS("costperunit_midsave.rds") 

#################################################################################################
###### Step 3: Synthesis for Creating EJscores 

# merge TRI, ORISPL, and RSEI Facility Number 
air_ids <- air %>% dplyr::select(REGISTRY_ID, PGM_SYS_ACRNM, PGM_SYS_ID) %>% distinct()
air_TRI <- air_ids %>% filter(PGM_SYS_ACRNM %in% c("TRIS")) %>% 
  dplyr::select(REGISTRY_ID, PGM_SYS_ID)
names(air_TRI) = c("REGISTRY_ID", "TRIFD")
air_CAMDBS <- air_ids %>% filter(PGM_SYS_ACRNM %in% c("CAMDBS")) %>% 
  dplyr::select(REGISTRY_ID, PGM_SYS_ID)
names(air_CAMDBS) = c("REGISTRY_ID", "ORISPL")
key <- left_join(air_CAMDBS, air_TRI)
datsum_MWh$ORISPL <- as.character(datsum_MWh$ORISPL)

# Do a manual FRS look up for some of the top facilites without a match
REGISTRY_ID <- c(110045626989,
                 110020047554,
                 110044834668,
                 110010534261,
                 110070828199,
                 110001351625,
                 110043792401,
                 110000585910,
                 110064387346,
                 110041120578)
TRIFD <- c("21226BRNDN1000B",
           "26134PLSNTNO1PO",
           "47630LMNMCHIGHW",
           "43616SNRFN1819W",
           "48054KKSCR65HAT",
           NA,
           "11105CSTLS1710S",
           "25265RSHGSRT62G",
           "21226BRNDN1000B",
           "44622DVRLG303EB")
ORISPL <- c(602,
            6004,
            6705,
            50965,
            1743,
            50647,
            55375,
            6264,
            1554,
            2914)
# add in the maunual matches
idadd <- data.frame(REGISTRY_ID, TRIFD, ORISPL)
idadd$ORISPL <- as.character(idadd$ORISPL)
key2 <- key %>% filter(!ORISPL %in% idadd$ORISPL)
key3 <- bind_rows(key2, idadd)
               
# add back into larger df 
dat7$ORISPL <- as.character(dat7$ORISPL)
dat8 <- left_join(dat7, key3)

# add the RSEI facility number 
dat9 <- left_join(dat8, rsei_fac, by = c("TRIFD" = "FacilityID")) 

# create a function that adds these metrics to our top100
# ejscore
# score
# ejscore ratio
# total vulnerable pop 
# vulnerable pop ratio

getej <- function(facID) {
  #iso the fac
  fac <- rsei_block %>% filter(FacilityNumber == facID)
  fac_agg <- fac %>% group_by(GEOID10, POP) %>%
    summarise(Score = sum(Score)) %>% ungroup()
  #now lets pull in EJscreen
  MLej <- EJscreen %>% filter(ID %in% fac_agg$GEOID10)
  #totalscore? 
  score <- sum(fac_agg$Score)
  #combine and find the EJscore 
  fac_agg$GEOID10 <- as.character(fac_agg$GEOID10)
  MLcombo <- left_join(MLej, fac_agg , by = c("ID" = "GEOID10"))
  #ejscore 
  MLcombo <- MLcombo %>% mutate(ejscore = Score*VULEOPCT,
                                Vulpop = ACSTOTPOP*VULEOPCT)
  ejscore <- sum(MLcombo$ejscore)
  #ejscoreratio 
  ejscore_ratio <- sum(MLcombo$ejscore)/sum(MLcombo$Score)
  #total vulberable pop within 50k
  Vulpop_50k <- sum(MLcombo$Vulpop)
  FacilityNumber <- facID
  dat <- data.frame(FacilityNumber, score, ejscore, ejscore_ratio, Vulpop_50k)
  gc()
  return(dat)
}

#run function for all EGUs
faclist <- lapply(unique(dat9$FacilityNumber), function (x)  getej(x))
faclist1 <- faclist[[1]]
for (i in 2:length(faclist)) faclist1 <- bind_rows(faclist1, faclist[[i]])
faclist1 <- faclist1 %>% filter(!is.na(FacilityNumber))
faclist2 <- left_join(dat9, faclist1, by = "FacilityNumber")
## SAVE
write_rds(faclist2, "faclist2.rds")
names(faclist2)

##add in CO2
# what about carbon? 
# I think this is in Egrid 
egrid18co2 <- egrid18 %>% dplyr::select(ORISPL, UNITID, CO2AN) %>%
  mutate(ORISPL = as.character(ORISPL))
dat10 <- left_join(faclist2, egrid18co2)
# bring in retirements 
dat11 <- left_join(dat10, retire_adder) %>% distinct()

#reduce to power plants 
#lets just look at the power plants 
dat12 <- dat11 %>% group_by(ORISPL) %>% mutate(fac_genntan = sum(GENNTAN, na.rm =T)) %>%
  ungroup() %>% filter(fac_genntan > 0)

##SAVE
write_rds(dat12, "EGUdata.rds")

## EJ buffers ####################################################################################
#make a function that obtains buffer information for each facility 
library(raster)

# #get centerpoints 
# fips <- unique(substr(EJscreen$GEOID, 1, 2))
# getcents <- function(fip){
#   url <- ifelse(nchar(fip) > 1, 
#                 paste0("https://www2.census.gov/geo/docs/reference/cenpop2010/blkgrp/CenPop2010_Mean_BG", fip, ".txt"),
#                 paste0("https://www2.census.gov/geo/docs/reference/cenpop2010/blkgrp/CenPop2010_Mean_BG0", fip, ".txt"))
#   dat <- read_csv(url)
#   gc()
# }
# 
# fiplist <- lapply(fips, function (x)  getcents(x))
# fiplist1 <- fiplist[[1]]
# for (i in 2:length(fiplist)) fiplist1 <- rbind(fiplist1, fiplist[[i]])
# fipslistraw <- fiplist1
# fiplist1$LATITUDE <- as.numeric(fiplist1$LATITUDE)
# fiplist1$LONGITUDE <- as.numeric(fiplist1$LONGITUDE)
# #make buffers and intersect points 
# coordinates(fiplist1) <- c("LONGITUDE", "LATITUDE")
# points <- st_as_sf(fiplist1)
# library(sf)


#THIS list is off by 12301 Block groups 
#try with the center points 
fips <- unique(substr(EJscreen$GEOID, 1, 2))
getcents2 <- function(fip){
  print(fip)
  shape <- block_groups(fip)
  dat <- as.data.frame(shape)
  dat <- dat %>% dplyr::select(GEOID, INTPTLAT, INTPTLON)
  gc()
  return(dat)
}

tst <- getcents2(fips[1])

fiplist <- lapply(fips, function (x)  getcents2(x))
fiplist1 <- fiplist[[1]]
for (i in 2:length(fiplist)) fiplist1 <- rbind(fiplist1, fiplist[[i]])
fipslistraw <- fiplist1
fiplist1$INTPTLAT <- as.numeric(fiplist1$INTPTLAT)
fiplist1$INTPTLON <- as.numeric(fiplist1$INTPTLON)
#make buffers and intersect points 
coordinates(fiplist1) <- c("INTPTLON", "INTPTLAT")
points <- st_as_sf(fiplist1)
#save
write_rds(points, "bgcenters.rds")
library(sf)


#make a list of unique facilites 
generators <- dat12 %>% dplyr::select(LON, LAT, ORISPL) %>% distinct()

EJbuffers <- function(fac) {
  #make buffers
  print(fac)
  sdf <- generators[fac,]
  coordinates(sdf) <- c("LON", "LAT")
  mybuff10k <- st_as_sf(buffer(sdf, width = 10000))
  mybuff5k <- st_as_sf(buffer(sdf, width = 5000))
  mybuff50k <- st_as_sf(buffer(sdf, width = 50000))
  #intersect buffer with blockgroup population centerpoints
  inter10k <- st_intersection(points, mybuff10k)
  inter5k <- st_intersection(points, mybuff5k)
  inter50k <- st_intersection(points, mybuff50k)
  #grabEJSCREEN stats or each intersect
  #10k
  st_geometry(inter10k) = NULL
  ej10k <- EJscreen %>%
    filter(ID %in% inter10k$GEOID) %>%
    mutate(vulpop = VULEOPCT*ACSTOTPOP) %>%
    summarise(ACSTOTPOP_10k = sum(ACSTOTPOP),
              VULEOPCT_10k = sum(vulpop)/sum(ACSTOTPOP))
  #5k
  st_geometry(inter5k) = NULL
  ej5k <- EJscreen %>%
    filter(ID %in% inter5k$GEOID) %>%
    mutate(vulpop = VULEOPCT*ACSTOTPOP) %>%
    summarise(ACSTOTPOP_5k = sum(ACSTOTPOP),
              VULEOPCT_5k = sum(vulpop)/sum(ACSTOTPOP))
  #50k
  st_geometry(inter50k) = NULL
  ej50k <- EJscreen %>%
    filter(ID %in% inter50k$GEOID) %>%
    mutate(vulpop = VULEOPCT*ACSTOTPOP) %>%
    summarise(ACSTOTPOP_50k = sum(ACSTOTPOP),
              VULEOPCT_50k = sum(vulpop)/sum(ACSTOTPOP))
  #combine
  ejdat <- cbind(ej5k, ej50k, ej10k)
  ejdat$ORISPL <- sdf$ORISPL
  
return(ejdat)
  
}
#run and combine 
ejlist <- lapply(1:nrow(generators), function (x)  EJbuffers(x))
ejlist1 <- ejlist[[1]]
for (i in 2:length(ejlist)) ejlist1 <- rbind(ejlist1, ejlist[[i]])

dat12 <- left_join(dat12, ejlist1)

##SAVE
write_rds(dat12, "EGUdata.rds")

#Toxics #################################################################

#issues, only a few facs have REGISTRY_IDS and even less have RSEI SCORES

#lets try another round of matching 
FRS_PROGRAM_LINKS <- read_csv("C:/Users/Mike Petroni/Downloads/frs_downloads (1)/FRS_PROGRAM_LINKS.csv")

air_tox <- air %>% filter(REGISTRY_ID %in% dat12$REGISTRY_ID) %>%
  filter(PGM_SYS_ACRNM == "EIS", REPORTING_YEAR == 2017)

unique(FRS_PROGRAM_LINKS$PGM_SYS_ACRNM)

mylinks <- FRS_PROGRAM_LINKS %>% filter(PGM_SYS_ID %in% dat12$ORISPL,
                                        PGM_SYS_ACRNM == "CAMDBS")

key <- mylinks %>% select(PGM_SYS_ID, REGISTRY_ID, PRIMARY_NAME, STATE_CODE)
key$ORISPL <- key$PGM_SYS_ID
dat12$REGISTRY_ID <- NULL
dat12 <- left_join(dat12, key)

nareg <- dat12 %>% filter(is.na(REGISTRY_ID)) 
sum(nareg$GENNTAN, na.rm =T)
reg <- dat12 %>% filter(!is.na(REGISTRY_ID)) 
sum(reg$GENNTAN, na.rm =T)

#key for those 800 facilies 
air_tox2 <- air %>% filter(REGISTRY_ID %in% mylinks$REGISTRY_ID) %>%
  filter(PGM_SYS_ACRNM == "EIS", REPORTING_YEAR == 2017)

nei_adds <- anti_join(air_tox2, air_tox)
nei_adds <- nei_adds %>% filter(!grepl("Primary", POLLUTANT_NAME),
                                !grepl("Volatile", POLLUTANT_NAME),
                                !grepl("Carbon mono", POLLUTANT_NAME),
                                !grepl("Nitrogen ox", POLLUTANT_NAME))
unique(nei_adds$POLLUTANT_NAME)
unique(nei_adds$PGM_SYS_ID)
sum(nei_adds$ANNUAL_EMISSION)

#can we toxweight the NEI? 
RSEI_chem <- read_csv("https://www.epa.gov/sites/production/files/2020-12/chemical_data_rsei_v239.csv")
tri_NEI_cross <- read_excel("Data/tri-nei-crosswalk (1).xlsx", 
                            sheet = "NEI to TRI Crosswalk")

#try the facility file.... 
emis_sum_fac_15420 <- read_csv("Data/NEI/emis_sum_fac_15420.csv")
mynei <- emis_sum_fac_15420 %>% filter(`eis facility id` %in% air_tox2$PGM_SYS_ID)

#now get the RSEI toxweight for each of the releases
poll_key <- left_join(tri_NEI_cross, RSEI_chem, by = c("TRI Chemical code" = "CASStandard")) 
poll_key <- poll_key %>% select(`NEI Pollutant Type`, `NEI pollutant code`, `NEI Pollutant Code Description`,
                                ITW)
emis_sum_fac_15420 <- left_join(emis_sum_fac_15420, poll_key,
                                by = c("pollutant code" = "NEI pollutant code"))

#now lets toxweight the NEI HAPS
neiweighted <- emis_sum_fac_15420 %>% filter(`NEI Pollutant Type` == "HAP") %>%
  mutate(AirHaz = `total emissions`*ITW) %>%
  group_by(`eis facility id`) %>% 
  summarise(Fac_AirHaz = sum(AirHaz, na.rm = T),
            NAICS = `naics description`[1],
            Name = `site name`[1]) %>% ungroup()
#facinating 

#now lets link these to our CAMDBS ids 
eis_links <- FRS_PROGRAM_LINKS %>% filter(PGM_SYS_ACRNM == "EIS")
eis_links$PGM_SYS_ID <- as.character(eis_links$PGM_SYS_ID)
neiweighted$`eis facility id` <- as.character(neiweighted$`eis facility id`)
neiweighted <- left_join(neiweighted, eis_links, by = c("eis facility id" = "PGM_SYS_ID"))
#and the camdbs
camdbs_link <- FRS_PROGRAM_LINKS %>% filter(PGM_SYS_ACRNM == "CAMDBS")
camdbs_link <- camdbs_link %>% select(REGISTRY_ID, PGM_SYS_ID) 
names(camdbs_link) <- c("REGISTRY_ID", "ORISPL")
neiweighted <- left_join(neiweighted, camdbs_link, by = "ORISPL")

#how much coverage do we get? 
tst <- left_join(dat_fac, neiweighted)
#total fac
length(unique(tst$ORISPL))
tsthap <- tst %>% filter(!is.na(Fac_AirHaz))
#total HAP facs covered
length(unique(tsthap$ORISPL))
#cover of HAP data by generation
sum(tsthap$GENNTAN)/sum(tst$GENNTAN)
#what is not covered by RSEI
tst2 <- tst %>% filter(is.na(score), !is.na(Fac_AirHaz))
tst3 <- emis_sum_fac_15420 %>% filter(`eis facility id` %in% tst2$`eis facility id`)
tst3 <- tst3 %>% filter(`pollutant type(s)` == "HAP")
sum(tst3$`total emissions`)
unique(tst3$`pollutant code`)

#benzidine from Martin
View(emis_sum_fac_15420 %>% filter(`eis facility id` == "4207311"))


#### NYCase Study  ##########################################################
#steps: 
#grab the peja and get the center points 
# download.file("http://gis.ny.gov/gisdata/fileserver/?DSID=1273&file=PEJA.zip",
#               destfile = "Data/PEJA_NY_2021.zip")
# unzip("Data/PEJA_NY_2021.zip")
library(foreign)
pejas <- read.dbf("Data/PEJA_NY_2021/PEJA.dbf")

#next subset our center point file to indicate PEJA
pejas_y <- pejas %>% filter(PEJA == "Yes")
points$NY <- ifelse(points$GEOID %in% pejas$GEOID, 1, 0)
points$PEJA <- ifelse(points$GEOID %in% pejas_y$GEOID, 1, 0)

#now we make a function that splits the emissions costs 
#make a list of unique facilites 
generators_NY <- dat12 %>%
  filter(PSTATABB == "NY") %>% dplyr::select(LON, LAT, ORISPL) %>% distinct()
fac = 1
EJbuffers_NY <- function(fac) {
  #make buffers
  print(fac)
  sdf <- generators_NY[fac,]
  coordinates(sdf) <- c("LON", "LAT")
  mybuff50k <- st_as_sf(buffer(sdf, width = 50000))
  #intersect buffer with blockgroup population centerpoints
  inter50k <- st_intersection(points, mybuff50k)
  #grabEJSCREEN stats or each intersect
  #50k
  st_geometry(inter50k) = NULL
  #how many people in the buffer?
  ej50k <- EJscreen %>%
    filter(ID %in% inter50k$GEOID) %>%
    mutate(vulpop = VULEOPCT*ACSTOTPOP) %>%
    summarise(ACSTOTPOP_50k = sum(ACSTOTPOP),
              VULEOPCT_50k = sum(vulpop)/sum(ACSTOTPOP))
  #how many people in the buffer in NY
  ej50k_NY <- EJscreen %>%
    filter(ID %in% inter50k$GEOID & ID %in% pejas$GEOID) %>%
    mutate(vulpop = VULEOPCT*ACSTOTPOP) %>%
    summarise(ACSTOTPOP_50k = sum(ACSTOTPOP),
              VULEOPCT_50k = sum(vulpop)/sum(ACSTOTPOP))
  #how many people in the buffer in NY in a PEJA
  ej50k_NY_PEJA <- EJscreen %>%
    filter(ID %in% inter50k$GEOID & ID %in% pejas_y$GEOID) %>%
    mutate(vulpop = VULEOPCT*ACSTOTPOP) %>%
    summarise(ACSTOTPOP_50k = sum(ACSTOTPOP),
              VULEOPCT_50k = sum(vulpop)/sum(ACSTOTPOP))
  
  #calculate the benefit percentage of a reduction here
  EJ_benefit_per = ej50k_NY_PEJA$ACSTOTPOP_50k/ej50k_NY$ACSTOTPOP_50k
  NY_benefit_per = ej50k_NY$ACSTOTPOP_50k/ej50k$ACSTOTPOP_50k
  #makedf
  ejdat <- data.frame(EJ_benefit_per, NY_benefit_per)
  ejdat$ORISPL <- sdf$ORISPL
  
  return(ejdat)
  
}

NY_ejlist <- lapply(1:nrow(generators_NY), function (x)  EJbuffers_NY(x))
NY_ejlist1 <- NY_ejlist[[1]]
for (i in 2:length(NY_ejlist)) NY_ejlist1 <- rbind(NY_ejlist1, NY_ejlist[[i]])






