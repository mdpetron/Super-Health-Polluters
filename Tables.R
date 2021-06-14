###### Mike Petroni 6/14/2021
# This study ranks EGU's based on health costs, EJ considerations, and Carbon Emissions

###
######## Tables 
###

#lets start with a table 

sum_dat <- dat_fac %>% arrange(desc(totrank_fac)) %>%
  summarise(ORISPL = ORISPL,
            Facility = paste(PNAME, CNTYNAME, PSTATABB, sep = ", "),
            `Net Power Output 2018 (GWh)` = round(GENNTAN*.001, 1),
            `Primary Fuel Category` = `Main Fuel Cat`,
            `Announced Retirment Year` = Retire_y,
            `Weighted Sum Model Score (Prioritization Rank)` = paste0(round(totrank_fac,1),
                                                                      " (",
                                                                      dense_rank(-totrank_fac),
                                                                      ")"),
            `Emissions Health Costs (Millions) [Percentile]` = paste0("$",round(tot_cost_em/1000000,1),
                                                                       " [",
                                                                       round(rank_healthcost_fac, 0),
                                                                       "]"),
            `Hazardous Air Pollutant Hazard (Billions) [Percentile]` = paste0(round(Fac_AirHaz/1000000000,1),
                                                                              " [",
                                                                              round(rank_TOX_fac, 0),
                                                                              "]"),
            `Carbon Dioxide Emissions (Megatons) [Percentile]` = paste0(round(CO2AN/1000000,2),
                                                                        " [",
                                                                        round(rank_co2_fac, 0),
                                                                        "]"),
            `Vulnerable Population within 5km (total) [Percentile]` = paste0(formatC(round(VUL_POP_5k,0), format="d", big.mark=","),
                                                                             " [",
                                                                             round(rank_5k_fac*3, 0),
                                                                             "]"),
            `Vulnerable Population within 10km (total) [Percentile]` = paste0(formatC(round(VUL_POP_10k,0), format="d", big.mark=","),
                                                                              " [",
                                                                              round(rank_10k_fac*3, 0),
                                                                              "]"),
            `Vulnerable Population within 50km (total) [Percentile]` = paste0(formatC(round(VUL_POP_50k,0), format="d", big.mark=","),
                                                                              " [",
                                                                              round(rank_50k_fac*3, 0),
                                                                              "]"))

write.csv(sum_dat, "National_Summary.csv")                                                                   

NY_sum_dat <- NY_facs %>% filter(PSTATABB == "NY") %>% arrange(desc(totrank_fac)) %>%
  summarise(ORISPL = ORISPL,
            Facility = paste(PNAME, CNTYNAME, PSTATABB, sep = ", "),
            `Net Power Output 2018 (GWh)` = round(GENNTAN*.001, 1),
            `Primary Fuel Category` = `Main Fuel Cat`,
            `Announced Retirment Year` = Retire_y,
            `Weighted Sum Model Score (Prioritization Rank)` = paste0(round(totrank_fac,1),
                                                                      " (",
                                                                      dense_rank(-totrank_fac),
                                                                      ")"),
            `Emissions Health Costs (Millions) [Percentile]` = paste0("$", round(tot_cost_em/1000000,1),
                                                                       " [",
                                                                       round(rank_healthcost_fac, 0),
                                                                       "]"),
            `Hazardous Air Pollutant Hazard (Billions) [Percentile]` = paste0(round(Fac_AirHaz/1000000000,1),
                                                                              " [",
                                                                              round(rank_TOX_fac, 0),
                                                                              "]"),
            `Carbon Dioxide Emissions (Megatons) [Percentile]` = paste0(round(CO2AN/1000000,2),
                                                                        " [",
                                                                        round(rank_co2_fac, 0),
                                                                        "]"),
            `Vulnerable Population within 50km (total) [Percentile]` = paste0(formatC(round(VUL_POP_50k,0), format="d", big.mark=","),
                                                                              " [",
                                                                              round(rank_50k_fac*3, 0),
                                                                              "]"),
            `Estimated Potential Co-Pollutant Reduction Benefit in PEJAs [Percentage of Total NY Benefit]` = paste0("$",formatC(round(EJ_benefit,0), format="d", big.mark=","),
                                                                             " [",
                                                                             paste0(round(EJ_benefit_per*100, 0), "%"),
                                                                             "]"))


write.csv(NY_sum_dat, "NY_Summary.csv") 

