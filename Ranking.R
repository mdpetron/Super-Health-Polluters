###### Mike Petroni 5/20/2021
# This study ranks EGU's based on health costs, EJ considerations, and Carbon Emissions

###
######## Analysis, Ranking
###


######## Ranking ###########
# setwd("C:/Users/Mike Petroni/Documents/GitHub/Super-Health-Polluters/")
# write_rds(dat12, "EGUdata.rds")
# dat12 <- read_rds("EGUdata.rds")

# Adust cost estimates to reflect selected VSL and Concentration Function
# https://iopscience.iop.org/article/10.1088/1748-9326/ab49bc
vsladj <- 11.2/8.8
dat12$tot_cost_em <- dat12$tot_cost_em*vsladj
rateadj <- -15.1+15.2*1.29
dat12$tot_cost_em <- dat12$tot_cost_em*rateadj

#how many facs have stack parameters?
stackcheck <- dat12 %>% dplyr::select(ORISPL, stkhgt_mean_meters_source) %>% distinct()
summary(as.factor(stackcheck$stkhgt_mean_meters_source))

#lets rank by EGU and by Facility 
# dat_egu <- dat12 %>% mutate(rank_score_fac = percent_rank(score)*100,
#                                 rank_gen_egu = percent_rank(-GENNTAN)*100,
#                                 rank_ejscoreratio_fac = percent_rank(ejscore_ratio)*100,
#                                 rank_Vulpop_fac = percent_rank(Vulpop_50k)*100,
#                                 rank_healthcost_egu = percent_rank(tot_cost_em)*100,
#                                 rank_co2_egu = percent_rank(CO2AN)*100) %>% rowwise() %>%
#   mutate(totrank_egu = sum(c(rank_Vulpop_fac, rank_healthcost_egu, rank_co2_egu), na.rm = T),
#          totrank_ejscore_gen_edu = sum(c(rank_Vulpop_fac, rank_healthcost_egu,
#                            rank_co2_egu, rank_score_fac, rank_gen_egu,
#                            rank_ejscoreratio_fac), na.rm = T))

#add fuel catagory

unique(dat12$FUELU1)
dat12$FUEL_CAT <- ifelse(dat12$FUELU1 %in% c("BIT",
                                       "LIG",
                                       'SUB',
                                       'RC',
                                       "SGC",
                                       "COG"), "Coal",
                         ifelse(dat12$FUELU1 %in% c("NG",
                                              "BU"), "Gas",
                                ifelse(dat12$FUELU1 %in% c("DFO",
                                                     "JF",
                                                     'KER',
                                                     'PC',
                                                     "RG",
                                                     "RFO",
                                                     "WO"), "Oil",
                                       ifelse(dat12$FUELU1 %in% c("BFG",
                                                            "OG",
                                                            'TDF'), "Other Fossil", "Other"))))

#make a simple dataframe for ranking 
dat_fac <- dat12 %>% group_by(ORISPL, PNAME, CNTYNAME, PSTATABB) %>% arrange(desc(GENNTAN)) %>%
  summarise(GENNTAN = sum(GENNTAN, na.rm = T),
            TOTCAP = sum(NAMEPCAP, na.rm = T),
            LON = LON[1],
            LAT = LAT[1],
            VUL_POP_10k = VULEOPCT_10k[1]*ACSTOTPOP_10k[1],
            VUL_POP_5k = VULEOPCT_5k[1]*ACSTOTPOP_5k[1],
            VUL_POP_50k = VULEOPCT_50k[1]*ACSTOTPOP_50k[1],
            Retire_y = retire_y[1],
            FUELS = paste(unique(FUEL_CAT), collapse = ", "),
            `Main Fuel Cat` = FUEL_CAT[1],
            `Main Fuel` = FUELU1[1],
            marg_nox = marg_nox[1]*vsladj*rateadj,
            marg_pm = marg_pm[1]*vsladj*rateadj,
            marg_so2 = marg_so2[1]*vsladj*rateadj,
            tot_cost_NOX = sum(tot_cost_NOX, na.rm = T),
            tot_cost_SO2 = sum(tot_cost_SO2, na.rm = T),
            tot_cost_PM = sum(tot_cost_PM, na.rm = T),
            CO2AN = sum(CO2AN, na.rm = T),
            #score = score[1],
            stkhgt_mean_meters_source = stkhgt_mean_meters_source[1],
            REGISTRY_ID = REGISTRY_ID[1]) %>% ungroup()
            #ejscore = ejscore[1],
            #ejscore_ratio = ejscore_ratio[1],
            #Vulpop_50k = Vulpop_50k[1]
#add the tox info
neiweighted_adder <- neiweighted %>% dplyr::select(1,2,4,17)

dat_fac <- left_join(dat_fac, neiweighted_adder)

#remove duplicates by taking the higher value
dat_fac <- dat_fac %>% group_by(ORISPL) %>% arrange(desc(Fac_AirHaz)) %>% slice(1) %>% ungroup()
dat_fac <- dat_fac %>% rowwise() %>%
  mutate(tot_cost_em = sum(c(tot_cost_NOX, tot_cost_SO2, tot_cost_PM), na.rm =T)) %>% ungroup()

median(dat_fac$GENNTAN)

dat_fac <- dat_fac %>%
mutate(rank_TOX_fac = (percent_rank(Fac_AirHaz)*100)/2,
       rank_gen_fac = percent_rank(-GENNTAN)*100,
       rank_5k_fac = (percent_rank(VUL_POP_5k)*100)/3,
       rank_10k_fac = (percent_rank(VUL_POP_10k)*100)/3,
       rank_50k_fac = (percent_rank(VUL_POP_50k)*100)/3,
       rank_healthcost_fac = percent_rank(tot_cost_em)*100,
       rank_co2_fac = percent_rank(CO2AN)*100, #percentiles
       scale_TOX_fac = scale(Fac_AirHaz)/2,
       scale_gen_fac = scale(-GENNTAN),
       scale_5k_fac = scale(VUL_POP_5k)/3,
       scale_10k_fac = scale(VUL_POP_10k)/3,
       scale_50k_fac = scale(VUL_POP_50k)/3,
       scale_healthcost_fac = scale(tot_cost_em),
       scale_co2_fac = scale(CO2AN)) %>% rowwise() %>%
  mutate(totrank_fac = sum(c(rank_TOX_fac, rank_healthcost_fac,
                             rank_co2_fac, rank_5k_fac,
                             rank_10k_fac, rank_50k_fac), na.rm = T),
         scalerank_fac = sum(c(scale_TOX_fac, scale_healthcost_fac,
                             scale_co2_fac, scale_5k_fac,
                             scale_10k_fac, scale_50k_fac), na.rm = T),
         totrank_gen_fac = sum(c(rank_TOX_fac, rank_healthcost_fac,
                                 rank_co2_fac, rank_5k_fac,
                                 rank_10k_fac, rank_50k_fac, rank_gen_fac), na.rm = T)) %>% ungroup()

#get a total emissions cost

## NY list 

NY_facs <- left_join(NY_ejlist1, dat_fac)
NY_facs$EJ_benefit <- NY_facs$tot_cost_em*NY_facs$EJ_benefit_per

#LOVE IT!


