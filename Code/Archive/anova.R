## Load In Data
rm(list =ls())
library(haven)
library(survey)
library(tidyverse)
library(margins)
library(forcats)
library(stargazer)

apply_levels <- function(CaltechClimate_Nov2022){
  #demo levels
  CaltechClimate_Nov2022$Gender <- factor(CaltechClimate_Nov2022$Gender, levels = c( "Man" , "Woman" , "Non-Binary/Fluid",  "Prefer not to say"), labels = c( "Man" , "Woman" , "Non-Binary/Fluid",  "Prefer not to say"))
  CaltechClimate_Nov2022$Race <-  factor( CaltechClimate_Nov2022$Race, 
                                          levels = c("White","Black or African American" ,"Hispanic or Latino" ,
                                                     "Asian American" ,"American Indian/Native American",  "Arab, Middle Eastern, or North African",  
                                                     "Native Hawaiian" ,"Not Hawaiian, but other Pacific Islander"), 
                                          labels = c("White","Black or African American" ,"Hispanic or Latino" ,
                                                     "Asian American" ,"American Indian/Native American",  "Arab, Middle Eastern, or North African",  
                                                     "Native Hawaiian" ,"Not Hawaiian, but other Pacific Islander"))
  CaltechClimate_Nov2022$Age <- factor(CaltechClimate_Nov2022$Age,
                                       levels = c(   "65+" , "Under 30", "30-44",    "45-64"  ),
                                       labels = c(   "65+" , "Under 30", "30-44",    "45-64"  ))
  CaltechClimate_Nov2022$Religion <- factor(CaltechClimate_Nov2022$Religion,
                                            levels = c( "Protestant" ,"Catholic" ,"Jewish",  "Muslim","Some other religion", "No religion" ),
                                            labels = c( "Protestant" ,"Catholic" ,"Jewish",  "Muslim","Some other religion", "No religion" ))
  CaltechClimate_Nov2022$Region <- factor(CaltechClimate_Nov2022$Region,
                                          levels = c("Northeast", "Midwest",   "South" ,    "West"  ),
                                          labels = c("Northeast", "Midwest",   "South" ,    "West"  ))  
  CaltechClimate_Nov2022 <- CaltechClimate_Nov2022 %>% mutate(Education = as.factor(case_when(Education %in% c("High school graduate", "No HS", "Some college") ~ "No College",
                                                                                              Education %in% c( "2-year",  "4-year" ) ~ "College Graduate",
                                                                                              TRUE ~ "Post Grad"))) 
  CaltechClimate_Nov2022$Education <- factor(CaltechClimate_Nov2022$Education,
                                             levels = c("No College" , "College Graduate", "Post Grad"),
                                             labels = c("No College" , "College Graduate", "Post Grad"))
  
  CaltechClimate_Nov2022$PartyID <-factor(CaltechClimate_Nov2022$PartyID,
                                          labels = c( "Independent",  "Democrat",  "Other" ,  "Republican" ,     "Not sure"   ),
                                          levels = c( "Independent",  "Democrat",  "Other" ,  "Republican" ,     "Not sure"   ))
  
  
  #skill levels
  CaltechClimate_Nov2022$Abortion_ability <- factor(CaltechClimate_Nov2022$Abortion_ability, levels = c("Not sure" ,  "Democrat" ,  "Republican"), labels = c("Not sure" ,  "Democrat" ,  "Republican"))
  CaltechClimate_Nov2022$Crime_ability    <-  factor(CaltechClimate_Nov2022$Crime_ability, levels = c("Not sure" ,  "Democrat" ,  "Republican"), labels = c("Not sure" ,  "Democrat" ,  "Republican"))
  CaltechClimate_Nov2022$Climate_ability  <-  factor(CaltechClimate_Nov2022$Climate_ability, levels = c("Not sure" ,  "Democrat" ,  "Republican"), labels = c("Not sure" ,  "Democrat" ,  "Republican"))
  CaltechClimate_Nov2022$LawEnforcement_ability <-  factor(CaltechClimate_Nov2022$LawEnforcement_ability, levels = c("Not sure" ,  "Democrat" ,  "Republican"), labels = c("Not sure" ,  "Democrat" ,  "Republican"))
  CaltechClimate_Nov2022$Covid_ability          <-  factor(CaltechClimate_Nov2022$Covid_ability, levels = c("Not sure" ,  "Democrat" ,  "Republican"), labels = c("Not sure" ,  "Democrat" ,  "Republican"))
  CaltechClimate_Nov2022$Budget_ability         <-  factor(CaltechClimate_Nov2022$Budget_ability, levels = c("Not sure" ,  "Democrat" ,  "Republican"), labels = c("Not sure" ,  "Democrat" ,  "Republican"))
  CaltechClimate_Nov2022$GrowEconomy_ability <-  factor(CaltechClimate_Nov2022$GrowEconomy_ability, levels = c("Not sure" ,  "Democrat" ,  "Republican"), labels = c("Not sure" ,  "Democrat" ,  "Republican"))
  CaltechClimate_Nov2022$HealthCare_ability <-  factor(CaltechClimate_Nov2022$HealthCare_ability, levels = c("Not sure" ,  "Democrat" ,  "Republican"), labels = c("Not sure" ,  "Democrat" ,  "Republican"))
  CaltechClimate_Nov2022$ForeignPolicy_ability <-  factor(CaltechClimate_Nov2022$ForeignPolicy_ability, levels = c("Not sure" ,  "Democrat" ,  "Republican"), labels = c("Not sure" ,  "Democrat" ,  "Republican"))
  CaltechClimate_Nov2022$Inflation_ability <-  factor(CaltechClimate_Nov2022$Inflation_ability, levels = c("Not sure" ,  "Democrat" ,  "Republican"), labels = c("Not sure" ,  "Democrat" ,  "Republican"))
  
  #importance levels
  CaltechClimate_Nov2022$Immigration = fct_collapse(CaltechClimate_Nov2022$Immigration, "Not Important" = c("Not too important", "Not important at all"), "Important"  = c("Very important","Somewhat important"))
  CaltechClimate_Nov2022$Immigration <-  factor(CaltechClimate_Nov2022$Immigration, levels = c("Not Important",  "Important"), labels = c("Not Important",  "Important"))
  CaltechClimate_Nov2022$Abortion = fct_collapse(CaltechClimate_Nov2022$Abortion, "Not Important" = c("Not too important", "Not important at all"), "Important"  = c("Very important","Somewhat important"))
  CaltechClimate_Nov2022$Abortion <-  factor(CaltechClimate_Nov2022$Abortion, levels = c("Not Important",  "Important"), labels = c("Not Important",  "Important"))
  CaltechClimate_Nov2022$ForeignPolicy = fct_collapse(CaltechClimate_Nov2022$ForeignPolicy, "Not Important" = c("Not too important", "Not important at all"), "Important"  = c("Very important","Somewhat important"))
  CaltechClimate_Nov2022$ForeignPolicy <-  factor(CaltechClimate_Nov2022$ForeignPolicy, levels = c("Not Important",  "Important"), labels = c("Not Important",  "Important"))
  CaltechClimate_Nov2022$EconomicInequality = fct_collapse(CaltechClimate_Nov2022$EconomicInequality, "Not Important" = c("Not too important", "Not important at all"), "Important"  = c("Very important","Somewhat important"))
  CaltechClimate_Nov2022$EconomicInequality <-  factor(CaltechClimate_Nov2022$EconomicInequality, levels = c("Not Important",  "Important"), labels = c("Not Important",  "Important"))
  CaltechClimate_Nov2022$COVID19 = fct_collapse(CaltechClimate_Nov2022$COVID19, "Not Important" = c("Not too important", "Not important at all"), "Important"  = c("Very important","Somewhat important"))
  CaltechClimate_Nov2022$COVID19 <-  factor(CaltechClimate_Nov2022$COVID19, levels = c("Not Important",  "Important"), labels = c("Not Important",  "Important"))
  CaltechClimate_Nov2022$ViolentCrime = fct_collapse(CaltechClimate_Nov2022$ViolentCrime, "Not Important" = c("Not too important", "Not important at all"), "Important"  = c("Very important","Somewhat important"))
  CaltechClimate_Nov2022$ViolentCrime <-  factor(CaltechClimate_Nov2022$ViolentCrime, levels = c("Not Important",  "Important"), labels = c("Not Important",  "Important"))
  CaltechClimate_Nov2022$HealthCare = fct_collapse(CaltechClimate_Nov2022$HealthCare, "Not Important" = c("Not too important", "Not important at all"), "Important"  = c("Very important","Somewhat important"))
  CaltechClimate_Nov2022$HealthCare <-  factor(CaltechClimate_Nov2022$HealthCare, levels = c("Not Important",  "Important"), labels = c("Not Important",  "Important"))
  CaltechClimate_Nov2022$TheEconomy = fct_collapse(CaltechClimate_Nov2022$TheEconomy, "Not Important" = c("Not too important", "Not important at all"), "Important"  = c("Very important","Somewhat important"))
  CaltechClimate_Nov2022$TheEconomy <-  factor(CaltechClimate_Nov2022$TheEconomy, levels = c("Not Important",  "Important"), labels = c("Not Important",  "Important"))
  CaltechClimate_Nov2022$RacialandEthnicInequality = fct_collapse(CaltechClimate_Nov2022$RacialandEthnicInequality, "Not Important" = c("Not too important", "Not important at all"), "Important"  = c("Very important","Somewhat important"))
  CaltechClimate_Nov2022$RacialandEthnicInequality <-  factor(CaltechClimate_Nov2022$RacialandEthnicInequality, levels = c("Not Important",  "Important"), labels = c("Not Important",  "Important"))
  CaltechClimate_Nov2022$ClimateChange = fct_collapse(CaltechClimate_Nov2022$ClimateChange, "Not Important" = c("Not too important", "Not important at all"), "Important"  = c("Very important","Somewhat important"))
  CaltechClimate_Nov2022$ClimateChange <-  factor(CaltechClimate_Nov2022$ClimateChange,levels = c("Not Important",  "Important"), labels = c("Not Important",  "Important"))
  CaltechClimate_Nov2022$Inflation = fct_collapse(CaltechClimate_Nov2022$Inflation, "Not Important" = c("Not too important", "Not important at all"), "Important"  = c("Very important","Somewhat important"))
  CaltechClimate_Nov2022$Inflation <-  factor(CaltechClimate_Nov2022$Inflation, levels = c("Not Important",  "Important"), labels = c("Not Important",  "Important"))
  CaltechClimate_Nov2022$GunPolicy = fct_collapse(CaltechClimate_Nov2022$GunPolicy, "Not Important" = c("Not too important", "Not important at all"), "Important"  = c("Very important","Somewhat important"))
  CaltechClimate_Nov2022$GunPolicy <-  factor(CaltechClimate_Nov2022$GunPolicy, levels = c("Not Important",  "Important"), labels = c("Not Important",  "Important"))
  CaltechClimate_Nov2022$SupremeCourtAppointments = fct_collapse(CaltechClimate_Nov2022$SupremeCourtAppointments, "Not Important" = c("Not too important", "Not important at all"), "Important"  = c("Very important","Somewhat important"))
  CaltechClimate_Nov2022$SupremeCourtAppointments <-  factor(CaltechClimate_Nov2022$SupremeCourtAppointments, levels = c("Not Important",  "Important"), labels = c("Not Important",  "Important"))
  
  
  #Media attention 
  CaltechClimate_Nov2022$Television = fct_collapse(CaltechClimate_Nov2022$Television, "Sometimes" = c("Sometimes", "Rarely"))
  CaltechClimate_Nov2022$Television <-  factor(CaltechClimate_Nov2022$Television, levels = c( "Sometimes", "Often", "Never" ), labels = c( "Sometimes", "Often", "Never" ))
  CaltechClimate_Nov2022$Radio = fct_collapse(CaltechClimate_Nov2022$Radio, "Sometimes" = c("Sometimes", "Rarely"))
  CaltechClimate_Nov2022$Radio <-  factor(CaltechClimate_Nov2022$Radio, levels = c( "Sometimes", "Often", "Never" ), labels = c( "Sometimes", "Often", "Never" ))
  CaltechClimate_Nov2022$PrintPublications = fct_collapse(CaltechClimate_Nov2022$PrintPublications, "Sometimes" = c("Sometimes", "Rarely"))
  CaltechClimate_Nov2022$PrintPublications <-  factor(CaltechClimate_Nov2022$PrintPublications, levels = c( "Sometimes", "Often", "Never" ), labels = c( "Sometimes", "Often", "Never" ))
  CaltechClimate_Nov2022$Online = fct_collapse(CaltechClimate_Nov2022$Online, "Sometimes" = c("Sometimes", "Rarely"))
  CaltechClimate_Nov2022$Online <-  factor(CaltechClimate_Nov2022$Online, levels = c( "Sometimes", "Often", "Never" ), labels = c( "Sometimes", "Often", "Never" ))
  
  CaltechClimate_Nov2022$SocialMedia <- factor(CaltechClimate_Nov2022$SocialMedia, levels = c("No", "Yes", "Don't know"), labels = c("No", "Yes", "Don't know"))
  CaltechClimate_Nov2022$FollowGoverment <- factor(CaltechClimate_Nov2022$FollowGoverment, levels = c("Some of the time" ,"Most of the time",   "Only now and then", "Hardly at all" ,    "Don't know"  ),
                                                   labels = c("Some of the time" ,"Most of the time",   "Only now and then", "Hardly at all" ,    "Don't know"  ))
  #economic levels
  CaltechClimate_Nov2022$EconomicSituation <- factor( CaltechClimate_Nov2022$EconomicSituation, 
                                                      labels = c(  "Stayed the same", "Gotten better" ,  "Gotten worse"  ),
                                                      levels = c(  "Stayed the same", "Gotten better" ,  "Gotten worse"  ))
  CaltechClimate_Nov2022$FinancialSituation <-factor(CaltechClimate_Nov2022$FinancialSituation,
                                                     labels = c(  "The same", "Better off" ,  "Worse off"  ),
                                                     levels = c(  "The same", "Better off" ,  "Worse off"  ))
  
  return(CaltechClimate_Nov2022)
}

CaltechClimate_Nov2022 <- read_sav("Data/CalTech_November_2022.sav") 
CaltechClimate_Nov2022<-droplevels(CaltechClimate_Nov2022%>%filter(Q18 %in% c(1,2)) %>% 
                                     select(PartyID = pid3, 
                                            Television = news_1,
                                            Radio = news_2,
                                            PrintPublications = news_3,
                                            Online = news_4,
                                            SocialMedia = Q67,
                                            FollowGoverment = Q69,
                                            EconomicSituation  =  Q75 ,
                                            FinancialSituation = Q74,
                                            Immigration = Q31_1,
                                            Abortion = Q31_2,
                                            ForeignPolicy = Q31_3,
                                            EconomicInequality =  Q31_4 ,
                                            COVID19 = Q31_5 ,
                                            ViolentCrime = Q31_6,
                                            HealthCare = Q31_7,
                                            TheEconomy = Q31_8,
                                            RacialandEthnicInequality = Q31_9,
                                            ClimateChange = Q31_10 ,
                                            Inflation = Q31_11,
                                            GunPolicy = Q31_12,
                                            SupremeCourtAppointments =  Q31_13 ,
                                            Crime_ability=Q51_1,
                                            Climate_ability=Q51_2,
                                            Abortion_ability=Q51_3,
                                            LawEnforcement_ability=Q51_4,
                                            Covid_ability=Q51_5,
                                            Budget_ability=Q51_6,
                                            GrowEconomy_ability=Q51_7,
                                            HealthCare_ability=Q51_8,
                                            ForeignPolicy_ability=Q51_9,
                                            Inflation_ability=Q51_10,
                                            vote.cong = Q18,
                                            vote.sen =senvote_combined,
                                            vote.gov =govvote_combined,
                                            Race = race,
                                            Religion = Q85,
                                            Education = educ, 
                                            Age = age4,
                                            Region = region,
                                            Gender = gender3,
                                            weight) %>% 
                                     mutate(across(!weight, as_factor)))

CaltechClimate_Nov2022 <- CaltechClimate_Nov2022[-c(280,  723,  884, 1115 ),]

CaltechClimate_Nov2022 <- apply_levels(CaltechClimate_Nov2022)

national.design.Climate.nov2022 = svydesign(data = (CaltechClimate_Nov2022), weights = ~weight, id = ~1)


reg1_interaction     <- svyglm(formula = vote.cong~PartyID + 
                                 # Television +
                                 # Radio +
                                 # PrintPublications +
                                 # Online +
                                 # SocialMedia +
                                 FollowGoverment*PartyID +
                                 Crime_ability*PartyID+
                                 Climate_ability*PartyID+
                                 Abortion_ability*PartyID+
                                 LawEnforcement_ability*PartyID+
                                 Covid_ability*PartyID+
                                 Budget_ability*PartyID+
                                 GrowEconomy_ability*PartyID+
                                 HealthCare_ability*PartyID+
                                 ForeignPolicy_ability*PartyID+
                                 Inflation_ability*PartyID +
                                 Race*PartyID+
                                 Education*PartyID+
                                 Age*PartyID +
                                 Gender*PartyID +
                                 Region*PartyID +
                                 Religion*PartyID +
                                 Immigration*PartyID +
                                 Abortion*PartyID +
                                 ForeignPolicy*PartyID +
                                 EconomicInequality*PartyID +
                                 COVID19*PartyID +
                                 ViolentCrime*PartyID +
                                 HealthCare*PartyID +
                                 TheEconomy*PartyID +
                                 RacialandEthnicInequality*PartyID +
                                 ClimateChange*PartyID +
                                 Inflation*PartyID +
                                 GunPolicy*PartyID +
                                 SupremeCourtAppointments*PartyID +
                                 EconomicSituation*PartyID +
                                 FinancialSituation*PartyID,
                               family=quasibinomial,
                               national.design.Climate.nov2022)

reg1     <- svyglm(formula = vote.cong~PartyID + 
                     # Television +
                     # Radio +
                     # PrintPublications +
                     # Online +
                     # SocialMedia +
                     FollowGoverment +
                     Crime_ability+
                     Climate_ability+
                     Abortion_ability+
                     LawEnforcement_ability+
                     Covid_ability+
                     Budget_ability+
                     GrowEconomy_ability+
                     HealthCare_ability+
                     ForeignPolicy_ability+
                     Inflation_ability +
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
                   national.design.Climate.nov2022)

reg2_interaction     <- svyglm(formula = vote.cong~ 
                     Race*PartyID+
                     Region*PartyID + 
                     Religion*PartyID + 
                     Age*PartyID + 
                     Gender*PartyID + 
                     Education*PartyID + 
                     EconomicSituation*PartyID + 
                     FollowGoverment*PartyID + 
                     HealthCare_ability*PartyID + 
                     Inflation_ability*PartyID + 
                     Covid_ability*PartyID + 
                     Crime_ability*PartyID + 
                     Abortion_ability*PartyID + 
                     ClimateChange*PartyID + 
                     Abortion*PartyID + 
                     COVID19*PartyID+ 
                     EconomicInequality*PartyID + 
                     ForeignPolicy*PartyID + 
                     ViolentCrime*PartyID,
                   family=quasibinomial,
                   national.design.Climate.nov2022)

reg2     <- svyglm(formula = vote.cong~PartyID + 
                     Race+
                     Region + 
                     Religion + 
                     Age + 
                     Gender + 
                     Education + 
                     EconomicSituation + 
                     FollowGoverment + 
                     HealthCare_ability + 
                     Inflation_ability + 
                     Covid_ability + 
                     Crime_ability + 
                     Abortion_ability + 
                     ClimateChange + 
                     Abortion + 
                     COVID19 + 
                     EconomicInequality + 
                     ForeignPolicy + 
                     ViolentCrime,
                   family=quasibinomial,
                   national.design.Climate.nov2022)


reg3_interaction     <- svyglm(formula = vote.cong~ 
                     Race*PartyID+
                     Region*PartyID + 
                     Religion*PartyID + 
                     Age*PartyID + 
                     Gender*PartyID + 
                     Education*PartyID + 
                     EconomicSituation*PartyID + 
                     FollowGoverment*PartyID + 
                     HealthCare_ability*PartyID + 
                     Inflation_ability*PartyID + 
                     Covid_ability*PartyID + 
                     Crime_ability*PartyID + 
                     Abortion_ability*PartyID + 
                     Abortion*PartyID + 
                     EconomicInequality*PartyID + 
                     ForeignPolicy*PartyID + 
                     ViolentCrime*PartyID,
                   family=quasibinomial,
                   national.design.Climate.nov2022)



reg3     <- svyglm(formula = vote.cong~PartyID + 
                     Race+
                     Region + 
                     Religion + 
                     Age + 
                     Gender + 
                     Education + 
                     EconomicSituation + 
                     FollowGoverment + 
                     HealthCare_ability + 
                     Inflation_ability + 
                     Covid_ability + 
                     Crime_ability + 
                     Abortion_ability + 
                     Abortion + 
                     EconomicInequality + 
                     ForeignPolicy + 
                     ViolentCrime,
                   family=quasibinomial,
                   national.design.Climate.nov2022)


reg4_interaction     <- svyglm(formula = vote.cong~ 
                     Race*PartyID +
                     Region*PartyID + 
                     Religion*PartyID + 
                     Age*PartyID + 
                     Gender*PartyID + 
                     Education*PartyID + 
                     HealthCare_ability*PartyID+ 
                     Inflation_ability*PartyID + 
                     Abortion*PartyID + 
                     ForeignPolicy*PartyID,
                   family=quasibinomial,
                   national.design.Climate.nov2022)

reg4     <- svyglm(formula = vote.cong~PartyID + 
                     Race+
                     Region + 
                     Religion + 
                     Age + 
                     Gender + 
                     Education + 
                     HealthCare_ability + 
                     Inflation_ability + 
                     Abortion + 
                     ForeignPolicy,
                   family=quasibinomial,
                   national.design.Climate.nov2022)





regressions =  list(reg1, reg2, reg3, reg4,reg1_interaction, reg2_interaction, reg3_interaction, reg4_interaction )


anova_results <- matrix(0,nrow=8,ncol=8)
for (i in 1:8){
  for (j in (i+1):8){
    print(paste(i, "and", j, sep = " "))
    try(anova_results[i,j] <- anova(regressions[[i]], regressions[[j]])$p)
    
  }
}

xtable(as.data.frame(anova_results) %>% select(!V1), digits = -2)
