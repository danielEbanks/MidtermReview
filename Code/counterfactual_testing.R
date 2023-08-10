load("Data/CleanedSurveys.RData")

regression_2020 <-list()
regression_2022 <-list()



party_grid <- c("Independent","Democrat","Republican","Pooled")
idx <- 1
for (i in party_grid){
  print(i)
  if (i == "Pooled"){
    data_2020 <- droplevels(CaltechClimate_Nov2020)
    data_2022 <- droplevels(CaltechClimate_Nov2022)
  } else {
    data_2020 <- droplevels(CaltechClimate_Nov2020[CaltechClimate_Nov2020$PartyID == i,])
    data_2022 <- droplevels(CaltechClimate_Nov2022[CaltechClimate_Nov2022$PartyID == i,])
  }
  
  data_2020 <- data_2020 %>%select(vote.cong,COVID19,ClimateChange,GunPolicy,TheEconomy,EconomicInequality,ForeignPolicy,RacialandEthnicInequality,SupremeCourtAppointments,HealthCare ,ViolentCrime, Abortion, Gender ,Immigration, Age , Gender , Race , Education , Region,weight) %>%na.omit()
  data_2022 <- data_2022 %>%select(vote.cong,COVID19,ClimateChange,GunPolicy,TheEconomy,EconomicInequality,ForeignPolicy,RacialandEthnicInequality,SupremeCourtAppointments,HealthCare ,ViolentCrime, Abortion, Gender ,Immigration, Age , Gender , Race , Education , Region,weight) %>%na.omit()
  
  
  design_2020           <- svydesign(ids = ~ 0, data = data_2020, weights = data_2020$weight)
  regression_2020[[idx]]  <- svyglm(vote.cong ~ Immigration +
                                      Abortion +
                                      ForeignPolicy +
                                      EconomicInequality +
                                      COVID19 +
                                      ViolentCrime +
                                      HealthCare +
                                      TheEconomy +
                                      RacialandEthnicInequality +
                                      ClimateChange +
                                      GunPolicy +
                                      SupremeCourtAppointments + Age + Gender + Race + Education + 
                                      Region, design = design_2020, family = quasibinomial)
  if(i!="Republican"){
  margins_2020          <- summary(margins(regression_2020[[idx]], design = design_2020))[c(1,13,23), c("factor",'AME', 'SE')]}else{
  margins_2020          <- summary(margins(regression_2020[[idx]], design = design_2020))[c(1,13,23), c("factor",'AME', 'SE')]  
  }
  
  
  
  
  design_2022          <- svydesign(ids = ~ 0, data = data_2022, weights = data_2022$weight)
  regression_2022[[idx]] <- svyglm(vote.cong ~  Immigration +
                                     Abortion +
                                     ForeignPolicy +
                                     EconomicInequality +
                                     COVID19 +
                                     ViolentCrime +
                                     HealthCare +
                                     TheEconomy +
                                     RacialandEthnicInequality +
                                     ClimateChange +
                                     GunPolicy +
                                     SupremeCourtAppointments  + Age + Gender + Race + Education + 
                                     Region , design = design_2022, family = quasibinomial)
  if(i!="Republican"){
    margins_2022         <- summary(margins(regression_2022[[idx]], design = design_2022))[c(1,16,30), c("factor",'AME', 'SE')]}else{
    margins_2022         <- summary(margins(regression_2022[[idx]], design = design_2022))[c(1,14,27), c("factor",'AME', 'SE')]  
  }
  
  if (idx == 1){
    margin.reduced <- cbind(as.data.frame(rbind(c("Party" = i, "Year" = 2020), c("Party" = i, "Year" = 2022))),rbind(margins_2020,margins_2022))         
  } else {
    margin.reduced <- margin.reduced %>% add_row(cbind(as.data.frame(rbind(c("Party" = i, "Year" = 2020), c("Party" = i, "Year" = 2022))),rbind(margins_2020,margins_2022)) )
  }
  idx <- idx+1
}

margin.reduced<-margin.reduced %>%group_by(factor) %>% mutate(Year  = as.factor(as.numeric(Year)), 
                                          AME   = as.numeric(AME),
                                          SE = as.numeric(SE),
                                          lower = AME - 1.96*SE,
                                          upper = AME + 1.96*SE)




save(margin.reduced,regression_2020,regression_2022,file="Data/counterfactual_all_variables.RData")






