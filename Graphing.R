###### Mike Petroni 5/20/2021
# This study ranks EGU's based on health costs, EJ considerations, and Carbon Emissions

###
######## Graphs
###

# top 25 NY

topfacs <- NY_facs %>% arrange(desc(EJ_benefit_per)) %>%
  filter(is.na(Retire_y), GENNTAN > 100000) %>%
  slice(1:25) %>% arrange(desc(-EJ_benefit_per))

plot_ly(NY_facs, x =~EJ_benefit_per, y =~tot_cost_em,
        size = ~GENNTAN, color =~`Main Fuel Cat`) %>%
  layout(yaxis = list(title = "Co-Pollutant Cost 2018 ($)"),
         xaxis = list(title ="Estimated Percent Benefiting Potential Environmental Justice Areas",
                      tickformat = "%"))

ggplot(NY_facs, aes(x=EJ_benefit_per,
                    y=tot_cost_em,
                    size = GENNTAN/1000000,
                    fill = `Main Fuel Cat`)) +
  geom_point(alpha=1, shape=21, color="black") +
  scale_size(range = c(1, 10), name="2018 Generation (GWh)") +
  scale_fill_manual(name="Main Fuel Category",
                      values = c("#101010",
                                  "#0096FF",
                                  "#FF4500",
                                  "#6a0dad")) +
  #theme_ipsum() +
  #theme(legend.position="bottom") +
  ylab("Estimated Co-Pollutant Health Cost 2018 ($)") +
  xlab("Estimated Percent Benefiting NY Potential Environmental Justice Areas if Removed") +
  scale_y_continuous(labels=scales::dollar_format()) +
  scale_x_continuous(labels = function(x) paste0(x*100, "%")) +
  guides(fill = guide_legend(override.aes = list(size = 4) ) ) + theme_bw()
  #theme(legend.position = "none")

plot_ly(topfacs, y = ~paste0(PNAME, ", ",
                             PSTATABB, ", ",
                             `Main Fuel Cat`,
                             " "), x=~rank_TOX_fac, type = "bar",
        orientation = 'h', name = "HAP Hazard Percentile",
        marker = list(color = '#61de2a')) %>%
  add_trace(x=~rank_5k_fac + rank_10k_fac + rank_50k_fac, name = 'Vulnerable Pop Percentile',
            marker = list(color = '#800000')) %>%
  add_trace(x=~rank_healthcost_fac, name = 'Health Cost Percentile',
            marker = list(color = '#63C8F2')) %>% 
  add_trace(x=~rank_co2_fac, name = 'CO2 Percentile',
            marker = list(color = '#8d99a5')) %>% 
  layout(barmode = 'stack',
         yaxis = list(title = "",
                      categoryorder = "array",
                      categoryarray = topfacs$totrank_fac),
         xaxis = list(title ="Weighted Sum Model Score - Percentiles"))


# top 25 by score PERCENTILES

topfacs <- dat_fac %>% arrange(desc(totrank_fac)) %>%
  filter(is.na(Retire_y), GENNTAN > 500000) %>%
  slice(1:25) %>% arrange(desc(-totrank_fac))

plot_ly(topfacs, y = ~paste0(PNAME, ", ",
                             PSTATABB, ", ",
                             `Main Fuel Cat`,
                             " "), x=~rank_TOX_fac, type = "bar",
        orientation = 'h', name = "HAP Hazard Percentile",
        marker = list(color = '#61de2a')) %>%
  add_trace(x=~rank_5k_fac + rank_10k_fac + rank_50k_fac, name = 'Vulnerable Pop Percentile',
            marker = list(color = '#800000')) %>%
  add_trace(x=~rank_healthcost_fac, name = 'Health Cost Percentile',
            marker = list(color = '#63C8F2')) %>% 
  add_trace(x=~rank_co2_fac, name = 'CO2 Percentile',
            marker = list(color = '#8d99a5')) %>% 
  layout(barmode = 'stack',
         yaxis = list(title = "",
                      categoryorder = "array",
                      categoryarray = topfacs$totrank_fac),
         xaxis = list(title ="Weighted Sum Model Score - Percentiles"))

# top 25 by score ZSCORES

topfacs <- dat_fac %>% arrange(desc(scalerank_fac)) %>%
  filter(is.na(Retire_y), GENNTAN > 500000) %>%
  slice(1:25) %>% arrange(desc(-scalerank_fac))

plot_ly(topfacs, y = ~paste0(PNAME, ", ",
                             PSTATABB, ", ",
                             `Main Fuel Cat`,
                             " "), x=~scale_TOX_fac, type = "bar",
        orientation = 'h', name = "HAP Hazard Z-Score",
        marker = list(color = '#61de2a')) %>%
  add_trace(x=~scale_5k_fac + scale_10k_fac + scale_50k_fac, name = 'Vulnerable Pop Z-Score',
            marker = list(color = '#800000')) %>%
  add_trace(x=~scale_healthcost_fac, name = 'Health Cost Z-Score',
            marker = list(color = '#63C8F2')) %>% 
  add_trace(x=~scale_co2_fac, name = 'CO2 Z-Score',
            marker = list(color = '#8d99a5')) %>% 
  layout(barmode = 'stack',
         yaxis = list(title = "",
                      categoryorder = "array",
                      categoryarray = topfacs$scalerank_fac),
         xaxis = list(title ="Weighted Sum Model Score - Z-Scores"))


#top 25 by cost 
fuels <- dat12 %>% group_by(FUELU1) %>%
  summarise(total_cost = sum(tot_cost_em, na.rm =T)) %>%
  arrange(desc(total_cost)) %>%
  slice(1:15)

plot_ly(fuels, x = ~FUELU1, y=~total_cost, type = "bar")

topfacs <- dat_fac %>% arrange(desc(tot_cost_em)) %>%
  filter(is.na(Retire_y), GENNTAN > 500000) %>%
  slice(1:25) %>% arrange(desc(-tot_cost_em))

plot_ly(topfacs, y = ~paste0(PNAME, ", ",
                             PSTATABB, ", ",
                             `Main Fuel Cat`,
                             " "), x=~tot_cost_PM, type = "bar",
        orientation = 'h', name = "Direct Particulate Matter",
        marker = list(color = '#808080')) %>%
  add_trace(x=~tot_cost_SO2, name = 'Sulphur Dioxide',
                marker = list(color = '#ae8b0c')) %>%
  add_trace(x=~tot_cost_NOX, name = 'Nitrogen Oxides',
            marker = list(color = '#842000')) %>% 
  layout(barmode = 'stack',
         yaxis = list(title = "",
                              categoryorder = "array",
                              categoryarray = topfacs$tot_cost_em),
         xaxis = list(title ="Estimated Health Costs in $ (2018)",
                      tickformat=",d"))

# top 25 by score Population Index

topfacs <- dat_fac %>% arrange(desc(VUL_POP_50k)) %>%
  filter(is.na(Retire_y), GENNTAN > 100000) %>%
  slice(1:25) %>% arrange(desc(-VUL_POP_50k))

plot_ly(topfacs, y = ~paste0(PNAME, ", ",
                             PSTATABB, ", ",
                             `Main Fuel Cat`,
                             " "), x=~VUL_POP_5k, type = "bar",
        orientation = 'h', name = 'Vulnerable Population Within 5km',
        marker = list(color = '#800000')) %>%
  add_trace(x=~VUL_POP_10k - VUL_POP_5k, name = 'Vulnerable Population Within 10km',
            marker = list(color = '#B22222')) %>% 
  add_trace(x=~VUL_POP_50k - VUL_POP_10k, name = 'Vulnerable Population Within 50Km',
            marker = list(color = '#F08080')) %>% 
  layout(barmode = 'stack',
         yaxis = list(title = "",
                      categoryorder = "array",
                      categoryarray = topfacs$totrank_fac),
         xaxis = list(title ="Facilities (100,000 MWh or more) with Highest Nearby Vulnerable Populations"))

#top 25 by HAP #####################################

#facility pie to show the disproportionality 

#get our chems and create other catagory
nei_chem <- emis_sum_fac_15420 %>% filter(`NEI Pollutant Type` == "HAP") %>%
  mutate(AirHaz = `total emissions`*ITW) %>%
  group_by(`eis facility id`, `pollutant desc`) %>% 
  summarise(Fac_AirHaz = sum(AirHaz, na.rm = T)) %>% ungroup()
topchems <- nei_chem %>% group_by(`pollutant desc`) %>%
  summarise(tot_HAZ = sum(Fac_AirHaz, na.rm=T)) %>%
  arrange(desc(tot_HAZ)) %>% slice(1:11)
nei_chem$Chemical <- ifelse(nei_chem$`pollutant desc` %in% topchems$`pollutant desc`,
                            nei_chem$`pollutant desc`, "Other")

#get our facilities and group others
toxfacs <- dat_fac %>% dplyr::select(ORISPL, `eis facility id`, PNAME,PSTATABB,
                                     `Main Fuel Cat`, Fac_AirHaz)
toxfacs1 <- toxfacs %>% arrange(desc(Fac_AirHaz)) %>% slice(1:10)
toxfacs <- dat_fac %>% dplyr::select(ORISPL, `eis facility id`, PNAME,PSTATABB,
                                     `Main Fuel Cat`)
nei_chem$`eis facility id` <- as.character(nei_chem$`eis facility id`)
nei_df <- left_join(nei_chem, toxfacs)

nei_df$fac <- ifelse(nei_df$ORISPL %in% toxfacs1$ORISPL,
                           paste0(nei_df$PNAME, ", ", nei_df$PSTATABB, " - ", nei_df$`Main Fuel Cat`, " "),
                           ifelse((!nei_df$ORISPL %in% toxfacs1$ORISPL) & nei_df$`Main Fuel Cat` == "Gas",
                                  "Other Generators - Gas ",
                                  ifelse((!nei_df$ORISPL %in% toxfacs1$ORISPL) & nei_df$`Main Fuel Cat` == "Coal",
                                         "Other Generators - Coal ", "Other Generators - Other Fuels ")))

order = c(
       "Martin Lake, TX - Coal ",        
"Sandow, TX - Coal ",              "Labadie, MO - Coal ",            
       "Seminole (136), FL - Coal ",     
"Gen J M Gavin, OH - Coal ",       "Cumberland, TN - Coal ",         
"Marshall, NC - Coal ",            "Belews Creek, NC - Coal ",       
"Keystone, PA - Coal ",            
"Allen, TN - Coal ",
"Other Generators - Coal ", 
"Other Generators - Gas ",   
"Other Generators - Other Fuels "
)

nei_df <- nei_df %>% group_by(fac, Chemical) %>%
  summarise(`HAP Hazard Score` = sum(Fac_AirHaz, na.rm =T)) %>% ungroup() %>%
  filter(!is.na(fac)) %>%
  arrange(desc(`HAP Hazard Score`)) 

plot_ly(nei_df, y = ~fac, x=~`HAP Hazard Score`, type = "bar",
        orientation = 'h', color = ~Chemical,
        colors = "Set3") %>%
  layout(barmode = 'stack',
         yaxis = list(title = "",
                      categoryorder = "array",
                      categoryarray = rev(order),
         xaxis = list(title ="HAP Hazard Score")))
































#QA Plant 31 Paper Mill

tst <- dat12 %>% filter(PNAME == "Martin Lake") %>% select(contains("tot_cost"),
                                                                ORISPL, PNAME,
                                                                CNTYNAME, PSTATABB)
tst2 <- dat_fac %>% filter(PNAME == "Shawnee") %>% select(contains("tot_cost"))

sum(tst2$tot_cost_NOX) + sum(tst2$tot_cost_SO2) + sum(tst2$tot_cost_PM)
sum(tst$tot_cost_NOX) + sum(tst$tot_cost_SO2) + sum(tst$tot_cost_PM)
tst$tot_cost_em[1] + tst$tot_cost_SO2[1] + tst$tot_cost_PM[1]

tst2$tot_cost_NOX + tst2$tot_cost_SO2 + tst2$tot_cost_PM


#lets make a mitigation equity curve 
#how do we want to do this?
#so we take the top 30 plants and stack them next to each other by cummulative 
######## health costs ###############
dat_30 <- dat_fac %>% filter(is.na(Retire_y)) %>% arrange(desc(tot_cost_em))
dat_30 <- dat_30[1:30,]
dat_30$cum_co2_reduction <- cumsum(dat_30$CO2AN)/1000000
dat_30$tot_cost_em <- dat_30$tot_cost_em/1000000
dat_30$CO2AN <- dat_30$CO2AN/1000000
dat_30$`Fuel Category` <- dat_30$FUELS

library(plotly)
plot_ly(dat_30, y = ~tot_cost_em, x = ~cum_co2_reduction, color = ~FUELS, type = "bar")
library(ggplot2)
dat_30$wm <- dat_30$cum_co2_reduction - dat_30$CO2AN
dat_30$wt <- with(dat_30, wm + (cum_co2_reduction - CO2AN)/2)
p <- ggplot(dat_30, aes(ymin = 0, label = PNAME))
p1 <- p + geom_rect(aes(xmin = wm, xmax = cum_co2_reduction,
                        ymax = tot_cost_em , fill = `Fuel Category`), colour = "black")

group.colors <- c("Coal" = "#585858",
                  "Coal, Oil" = "#7f7053",
                  "Gas" ="#433deb",
                  "Gas, Other" = "#00A36C")

p1 + scale_fill_manual(values=group.colors) +
  labs(y ="Annual pre-mature mortality cost (Millions)",
       x = "Cumulative annual megatons CO2 reduced") +
  scale_y_continuous(labels=scales::dollar_format()) +
  theme_light() +
  theme(legend.position = c(0.8, 0.8),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))

# for the labs
dat_30 <- dat_30 %>%
  mutate(name = paste(PNAME, CNTYNAME, PSTATABB, `Fuel Category`, sep = ", "))

######## Score ###############

#gas plant coverage

gas <- dat_fac %>% filter(grepl("Gas", FUELS))

dat_30 <- dat_fac %>% filter(is.na(Retire_y)) %>% arrange(desc(totrank_ejscore_gen_fac))
dat_30 <- dat_30[1:30,]
dat_30$cum_co2_reduction <- cumsum(dat_30$CO2AN)/1000000
dat_30$totrank_ejscore_gen_fac <- dat_30$totrank_ejscore_gen_fac/100000000
dat_30$CO2AN <- dat_30$CO2AN/1000000
dat_30$`Fuel Category` <- dat_30$`Main Fuel`

library(ggplot2)
dat_30$wm <- dat_30$cum_co2_reduction - dat_30$CO2AN
dat_30$wt <- with(dat_30, wm + (cum_co2_reduction - CO2AN)/2)
p <- ggplot(dat_30, aes(ymin = 0, label = PNAME))
p1 <- p + geom_rect(aes(xmin = wm, xmax = cum_co2_reduction,
                        ymax = totrank_ejscore_gen_fac,
                        fill = `Fuel Category`), colour = "black")

group.colors <- c("Coal" = "#585858",
                  "Coal, Oil" = "#7f7053",
                  "Gas" ="#433deb",
                  "Gas, Oil" ="#433deb",
                  "Coal, Gas" ="#433deb",
                  "Oil, Gas" ="#433deb",
                  "Gas, Other" = "#00A36C",
                  "Gas, Coal, Oil" = "#660033",
                  "Gas, Coal" = "#003366",
                  "Oil" = "#6600cc")

p1 + scale_fill_manual(values=group.colors) +
  labs(y ="Weighted product score of six ranked criteria",
       x = "Cumulative annual megatons CO2 reduced") +
  #scale_y_continuous(labels=scales::dollar_format()) +
  theme_light() +
  theme(legend.position = c(0.7, 0.7),
        legend.background = element_blank(),
        legend.box.background = element_rect(colour = "black"))

# for the labs
dat_30 <- dat_30 %>%
  mutate(name = paste(PNAME, CNTYNAME, PSTATABB, `Fuel Category`, sep = ", "))







plot_ly(data = dat_fac %>% filter(GENNTAN > 1000000), x =~totrank_fac, y=~GENNTAN,
        color = ~`Main Fuel`, size = ~VUL_POP_50k)

plot_ly(data = dat_fac %>% filter(GENNTAN > 18924), y =~GENNTAN, x=~tot_cost_em,
        color = ~`Main Fuel`, size = ~VUL_POP_50k)








superslim <- mydfslim %>%
  mutate(name = paste(PNAME, CNTYNAME, PSTATABB, UNITID, FUELU1, sep = ",")) %>%
  dplyr::select(name, contains("rank"))

write.csv(superslim, "prelimexport.csv")


#ok this is cool

#lets make some bubble charts 

plot_ly(data = mydfslim, x =~rank_healthcost, y=~rank_Vulpop, z = ~rank_co2, color = ~FUELU1)
plot_ly(data = mydfslim, x =~tot_cost_em, y=~Vulpop_50k, z = ~CO2AN, color = ~FUELU1)
hist(superslim$rank_healthcost)
#lets make a list per load zone

zone_adder <- egrid18_fac %>% dplyr::select(ORISPL, BACODE, NERC, SUBRGN, SRNAME)
zone_adder$ORISPL <- as.character(zone_adder$ORISPL)
mydfslim_z <- left_join(mydfslim, zone_adder)

#how many zones 
n_distinct(mydfslim_z$BACODE)
unique(mydfslim_z$NERC)
unique(mydfslim_z$SRNAME)
#use NERC?

#lets test more criteria
#totrank_2

#lets look into a sensativity analysis 

#lets deal with facilities that do not have RSEI Scores 
#how many?
nrow(mydfslim %>% filter(is.na(TRIFD))) #alot









#what about disproportionality?
#per plant
datsum <- datsum %>% mutate(costrank = ntile(total_em_cost, 100))
costbyntile <- datsum %>% group_by(costrank) %>%
  summarise(totalcost = sum(total_em_cost)) %>% ungroup()
plot(costbyntile$totalcost ~ costbyntile$costrank )
costbyntile$totalcost[100]/sum(costbyntile$totalcost)

#look at cost per mwh at facility level
top100 <- top100 %>% group_by(ORISPL, PNAME, PSTATABB, TRIFD, REGISTRY_ID) %>%
  summarise(FUELS = paste(unique(FUELU1), collapse = ", "),
            GENNTAN = sum(GENNTAN, na.rm = T), 
            tot_cost_em = sum(tot_cost_em, na.rm = T)) %>% ungroup() 

plot_ly(top100, x = ~GENNTAN, y = ~tot_cost_em, color = ~ FUELS, popup = ~PNAME)



#big problem.... lots of these powerplants do not have RSEI info in 2018.. 
# lets double check this 

chemTable <- read_csv("C:/Users/Mike Petroni/Documents/RSEI Propublica/QC/V234_Public_Data_Release_Tables/Chemical.csv")
relTable <- read_csv("C:/Users/Mike Petroni/Documents/RSEI Propublica/QC/V234_Public_Data_Release_Tables/Release.csv")
elTable <- read_csv("C:/Users/Mike Petroni/Documents/RSEI Propublica/QC/V234_Public_Data_Release_Tables/Elements.csv")
subTable <- read_csv("C:/Users/Mike Petroni/Documents/RSEI Propublica/QC/V234_Public_Data_Release_Tables/Submission.csv")
medTable <- read_csv("C:/Users/Mike Petroni/Documents/RSEI Propublica/QC/V234_Public_Data_Release_Tables/MEDIA.csv")
head(relTable)

datrsei14 <- left_join(elTable, relTable)
datrsei14 <- left_join(datrsei14, subTable)
datrsei14 <- left_join(datrsei14, rsei_fac)

#do our facs have score? 

tst <- datrsei14 %>% filter(FacilityNumber %in% faclist1$FacilityNumber) %>%
  group_by(FacilityNumber, SubmissionYear) %>% summarise(score = sum(Score))


datrsei14$SubmissionYear


#which ones dont we have? 

needem <- faclist1 %>% filter(!FacilityNumber %in% tst$FacilityNumber) %>% filter(Vulpop_50k == 0)
needem2 <- top100r %>% filter(FacilityNumber %in% needem$FacilityNumber)
#do they have NEI emissions? 
# omg they must!   


top100r





# What about the proposed projects? 
EIP  <- read_excel("C:/Users/Mike Petroni/Downloads/EIP Emissions Increase Database and Pipelines Inventory_March 31, 2021.xlsx")



nrow(datsum %>% filter(costrank == 100))

grou
stkhgt_mean_meters_source = 
  ifelse(!is.na(stkhgt_mean_meters),
         "NEI2017",
         "Mean Replacement"),
stkhgt_m_m = 
  ifelse(!is.na(stkhgt_mean_meters),
         stkhgt_mean_meters,
         mean(stkhgt_mean_meters, na.rm =T)))

View(highstack %>% dplyr::select(NOX0H, NOX1H,
                                 NOX2H, NOX3H,
                                 NOX4H))


(highstack$NOX1H + highstack$NOX2H + highstack$NOX3H + highstack$NOX4H)/4




############ make a map of the buffer analysis 

bama <- block_groups(state = "AL")

sdf <- generators[1,]
coordinates(sdf) <- c("LON", "LAT")
mybuff10k <- buffer(sdf, width = 10000)
inter10k <- st_intersection(points, mybuff10k)
inter10k <- inter10k %>%
  mutate(GEOID = paste0(STATEFP,COUNTYFP,TRACTCE,BLKGRPCE))
st_geometry(inter10k) = NULL
inter10k <- inter10k %>%
  mutate(GEOID = paste0(STATEFP,COUNTYFP,TRACTCE,BLKGRPCE))
ej10k <- EJscreen %>%
  filter(ID %in% inter10k$GEOID)

bama1 <- subset(bama, bama$GEOID %in% ej10k$ID)

leaflet() %>% addTiles() %>% addPolygons(mybuff10k)

plot(mybuff10k)
plot(inter10k, add = T)

ggplot(data = bama1) +
  coord_sf(xlim = c(-80.15, -74.12), ylim = c(20.65, 25.97), expand = FALSE)

mybuff5k <- st_as_sf(buffer(sdf, width = 5000))
mybuff50k <- st_as_sf(buffer(sdf, width = 50000))
#intersect buffer with blockgroup population centerpoints
inter10k <- st_intersection(points, mybuff10k)
inter5k <- st_intersection(points, mybuff5k)
inter50k <- st_intersection(points, mybuff50k)
#grabEJSCREEN stats or each intersect
#10k
st_geometry(inter10k) = NULL
inter10k <- inter10k %>%
  mutate(GEOID = paste0(STATEFP,COUNTYFP,TRACTCE,BLKGRPCE))
ej10k <- EJscreen %>%
  filter(ID %in% inter10k$GEOID) %>%
  mutate(vulpop = VULEOPCT*ACSTOTPOP) %>%
  summarise(ACSTOTPOP_10k = sum(ACSTOTPOP),
            VULEOPCT_10k = sum(vulpop)/sum(ACSTOTPOP))
#5k
st_geometry(inter5k) = NULL
inter5k <- inter5k %>%
  mutate(GEOID = paste0(STATEFP,COUNTYFP,TRACTCE,BLKGRPCE))
ej5k <- EJscreen %>%
  filter(ID %in% inter5k$GEOID) %>%
  mutate(vulpop = VULEOPCT*ACSTOTPOP) %>%
  summarise(ACSTOTPOP_5k = sum(ACSTOTPOP),
            VULEOPCT_5k = sum(vulpop)/sum(ACSTOTPOP))
#50k
st_geometry(inter50k) = NULL
inter50k <- inter50k %>%
  mutate(GEOID = paste0(STATEFP,COUNTYFP,TRACTCE,BLKGRPCE))
ej50k <- EJscreen %>%
  filter(ID %in% inter50k$GEOID) %>%
  mutate(vulpop = VULEOPCT*ACSTOTPOP) %>%
  summarise(ACSTOTPOP_50k = sum(ACSTOTPOP),
            VULEOPCT_50k = sum(vulpop)/sum(ACSTOTPOP))
#combine
ejdat <- cbind(ej5k, ej50k, ej10k)
ejdat$ORISPL <- sdf$ORISPL





#merge example Astoria generating station 

ampd_astoria <- ampd_emis %>%
  filter(grepl("Aurora", `Facility Name`),
         Year == 2017)

nei_astoria <- egucems_nei17 %>%
  filter(oris_facility_code == ampd_astoria$`Facility ID (ORISPL)`[1])


nei_astoria_fac <- nei17_facility %>%
  filter(grepl("Aurora", `site name`))

nei_astoria_fac <- nei17_facility %>%
  filter(`eis facility id` == ampd_astoria$`Facility ID (ORISPL)`[1])



ampd_astoria <- ampd_emis %>%
  filter(`Facility Latitude` == 41.8151,
         Year == 2017)

nei_astoria <- egucems_nei17 %>%
  filter(latitude == 41.8151)


nei_astoria_fac <- nei17_facility %>%
  filter(`site latitude` == 41.8151)

nei_astoria_fac <- nei17_facility %>%
  filter(`eis facility id` == ampd_astoria$`Facility ID (ORISPL)`[1])



#so lets grab what we want here

nei_pol_key <- nei17_facility %>%
  dplyr::select(`pollutant code`, `pollutant desc`, `pollutant type(s)`) %>%
  distinct()



#looks like we dont have all the PM estimates for facilities here... 
#how many do we have... 

#lets try a merge with just NOX
nei_stack_key_merge <- egunoncems_nei17 %>% 
  dplyr::select(oris_facility_code, oris_boiler_id,
                longitude, latitude, facility_id, poll, ann_value, 52:64, stkhgt) %>%
  filter(poll == "NOX") %>% distinct() 

ampd_17 <- ampd_emis %>%
  filter(Year == 2017) %>%
  mutate(oris_facility_code = `Facility ID (ORISPL)`,
         oris_boiler_id = `Unit ID`)

tst <- left_join(ampd_17, nei_stack_key_merge)

unique(nei_stack_key$poll)
unique(egunoncems_nei17$poll)


# 
