



##############################

print("Recoded Attempt")


print("Run Regressions by party id")

for (pp in c("Independent",  "Republican", "Democrat"  )){ 
  print(paste0("Regression for ", pp,"s"))
  nam <- paste0('reg_party_', pp)
  # Run a consistent regression for each  party 
  national.design.Climate.nov2022 = svydesign(data = droplevels(CaltechClimate_Nov2022 %>% filter(PartyID_recode == pp) %>%
                                                                  mutate(Race = as.factor(case_when(Race == "White" ~ "White", 
                                                                                                    Race == "Black or African American" ~ "Black",
                                                                                                    TRUE ~ "Other"))),
                                                                Religion = as.factor(case_when(Religion %in% c("Jewish","Muslim","Some other Religion","Dont Know")  ~ "Other",
                                                                                               TRUE                                                                  ~ Religion)),
                                                                Gender   = as.factor(case_when(Gender %in% c("Non-Binary/Fluid", "Prefer not to say")  ~ "Other",
                                                                                               TRUE                                                            ~ Gender))) , 
                                              weights = ~weight, id = ~1)
  
  assign(nam, svyglm(formula = vote.cong~ 
                       Race+
                       Education+
                       Age +
                       Gender +
                       Region +
                       Religion +
                       Immigration +
                       Abortion +
                       ForeignPolicy +
                       EconomicInequality +
                       COVID19 +
                       ViolentCrime +
                       HealthCare +
                       TheEconomy +
                       RacialandEthnicInequality +
                       ClimateChange +
                       Inflation +
                       GunPolicy +
                       SupremeCourtAppointments +
                       EconomicSituation +
                       FinancialSituation,
                     family=quasibinomial,
                     national.design.Climate.nov2022))
  
  
  
  
  regparty_m   <- margins(get(nam),design=national.design.Climate.nov2022)
  plot.df_party<-summary(regparty_m)
  
  plot.df_party <- as_tibble(plot.df_party)            %>% 
    left_join(temp, by = c("factor" = "name"))         %>% 
    separate(Cat, c("Cat_Ability", NA),remove = FALSE) %>% 
    mutate(lower = AME - 1.96*SE, upper = AME + 1.96*SE)
  
  nam <- paste0('reg_party_margins_', pp)
  assign(nam, plot.df_party %>% mutate(group = pp))
  
  
  
}

# Save Data
save(reg_party_margins_Democrat, 
     reg_party_Democrat, 
     reg_party_margins_Republican,
     reg_party_Republican, 
     reg_party_margins_Independent,
     reg_party_Independent, 
     file = "Data/logits_by_party_recode.RData")










