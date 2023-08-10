apply_levels <- function(survydf,nov20=F){
  #demo levels
  if(nov20==F){
  survydf$Gender <- factor(survydf$Gender, levels = c( "Man" , "Woman" , "Non-Binary/Fluid",  "Prefer not to say"), labels = c( "Man" , "Woman" , "Non-Binary/Fluid",  "Prefer not to say"))
  survydf$Race <-  factor( survydf$Race, 
                           levels = c("White","Black or African American" ,"Hispanic or Latino" ,
                                      "Asian American" ,"American Indian/Native American",  "Arab, Middle Eastern, or North African",  
                                      "Native Hawaiian" ,"Not Hawaiian, but other Pacific Islander"), 
                           labels = c("White","Black or African American" ,"Hispanic or Latino" ,
                                      "Asian American" ,"American Indian/Native American",  "Arab, Middle Eastern, or North African",  
                                      "Native Hawaiian" ,"Not Hawaiian, but other Pacific Islander"))
  survydf$Religion <- factor(survydf$Religion,
                             levels = c( "Protestant" ,"Catholic" ,"Jewish",  "Muslim","Some other religion", "No religion" ),
                             labels = c( "Protestant" ,"Catholic" ,"Jewish",  "Muslim","Some other religion", "No religion" ))
  }
  if(nov20==T){
    survydf$Gender <- factor(survydf$Gender, levels = c( "Male" , "Female" , "Non-Binary/Fluid",  "Prefer not to say"), labels = c( "Man" , "Woman" , "Non-Binary/Fluid",  "Prefer not to say"))
    survydf$Race <-  factor( survydf$Race, 
                             levels = c("White","Black" ,"Hispanic","Other" ), 
                             labels = c("White","Black or African American" ,"Hispanic or Latino" ,"Other"))
    }

  survydf$Age <- factor(survydf$Age,
                                       levels = c(   "65+" , "Under 30", "30-44",    "45-64"  ),
                                       labels = c(   "65+" , "Under 30", "30-44",    "45-64"  ))

  survydf$Region <- factor(survydf$Region,
                                          levels = c("Northeast", "Midwest",   "South" ,    "West"  ),
                                          labels = c("Northeast", "Midwest",   "South" ,    "West"  ))  
  survydf <- survydf %>% mutate(Education = as.factor(case_when(Education %in% c("High school graduate", "No HS", "Some college") ~ "No College",
                                                                                              Education %in% c( "2-year",  "4-year" ) ~ "College Graduate",
                                                                                              TRUE ~ "Post Grad"))) 
  survydf$Education <- factor(survydf$Education,
                                             levels = c("No College" , "College Graduate", "Post Grad"),
                                             labels = c("No College" , "College Graduate", "Post Grad"))
  
  survydf$PartyID <-factor(survydf$PartyID,
                                          labels = c( "Independent",  "Democrat",  "Other" ,  "Republican" ,     "Not sure"   ),
                                          levels = c( "Independent",  "Democrat",  "Other" ,  "Republican" ,     "Not sure"   ))
  
  if(nov20==F){
  #skill levels
  survydf$Abortion_ability <- factor(survydf$Abortion_ability, levels = c("Not sure" ,  "Democrat" ,  "Republican"), labels = c("Not sure" ,  "Democrat" ,  "Republican"))
  survydf$Crime_ability <-  factor(survydf$Crime_ability, levels = c("Not sure" ,  "Democrat" ,  "Republican"), labels = c("Not sure" ,  "Democrat" ,  "Republican"))
  survydf$Climate_ability <-  factor(survydf$Climate_ability, levels = c("Not sure" ,  "Democrat" ,  "Republican"), labels = c("Not sure" ,  "Democrat" ,  "Republican"))
  survydf$LawEnforcement_ability <-  factor(survydf$LawEnforcement_ability, levels = c("Not sure" ,  "Democrat" ,  "Republican"), labels = c("Not sure" ,  "Democrat" ,  "Republican"))
  survydf$Covid_ability <-  factor(survydf$Covid_ability, levels = c("Not sure" ,  "Democrat" ,  "Republican"), labels = c("Not sure" ,  "Democrat" ,  "Republican"))
  survydf$Budget_ability <-  factor(survydf$Budget_ability, levels = c("Not sure" ,  "Democrat" ,  "Republican"), labels = c("Not sure" ,  "Democrat" ,  "Republican"))
  survydf$GrowEconomy_ability <-  factor(survydf$GrowEconomy_ability, levels = c("Not sure" ,  "Democrat" ,  "Republican"), labels = c("Not sure" ,  "Democrat" ,  "Republican"))
  survydf$HealthCare_ability <-  factor(survydf$HealthCare_ability, levels = c("Not sure" ,  "Democrat" ,  "Republican"), labels = c("Not sure" ,  "Democrat" ,  "Republican"))
  survydf$ForeignPolicy_ability <-  factor(survydf$ForeignPolicy_ability, levels = c("Not sure" ,  "Democrat" ,  "Republican"), labels = c("Not sure" ,  "Democrat" ,  "Republican"))
  survydf$Inflation_ability <-  factor(survydf$Inflation_ability, levels = c("Not sure" ,  "Democrat" ,  "Republican"), labels = c("Not sure" ,  "Democrat" ,  "Republican"))
  }
  #importance levels
  survydf$Immigration = fct_collapse(survydf$Immigration, "Not Important" = c("Not too important", "Not important at all"), "Important"  = c("Very important","Somewhat important"))
  survydf$Immigration <-  factor(survydf$Immigration, levels = c("Not Important",  "Important"), labels = c("Not Important",  "Important"))
  survydf$Abortion = fct_collapse(survydf$Abortion, "Not Important" = c("Not too important", "Not important at all"), "Important"  = c("Very important","Somewhat important"))
  survydf$Abortion <-  factor(survydf$Abortion, levels = c("Not Important",  "Important"), labels = c("Not Important",  "Important"))
  survydf$ForeignPolicy = fct_collapse(survydf$ForeignPolicy, "Not Important" = c("Not too important", "Not important at all"), "Important"  = c("Very important","Somewhat important"))
  survydf$ForeignPolicy <-  factor(survydf$ForeignPolicy, levels = c("Not Important",  "Important"), labels = c("Not Important",  "Important"))
  survydf$EconomicInequality = fct_collapse(survydf$EconomicInequality, "Not Important" = c("Not too important", "Not important at all"), "Important"  = c("Very important","Somewhat important"))
  survydf$EconomicInequality <-  factor(survydf$EconomicInequality, levels = c("Not Important",  "Important"), labels = c("Not Important",  "Important"))
  survydf$COVID19 = fct_collapse(survydf$COVID19, "Not Important" = c("Not too important", "Not important at all"), "Important"  = c("Very important","Somewhat important"))
  survydf$COVID19 <-  factor(survydf$COVID19, levels = c("Not Important",  "Important"), labels = c("Not Important",  "Important"))
  survydf$ViolentCrime = fct_collapse(survydf$ViolentCrime, "Not Important" = c("Not too important", "Not important at all"), "Important"  = c("Very important","Somewhat important"))
  survydf$ViolentCrime <-  factor(survydf$ViolentCrime, levels = c("Not Important",  "Important"), labels = c("Not Important",  "Important"))
  survydf$HealthCare = fct_collapse(survydf$HealthCare, "Not Important" = c("Not too important", "Not important at all"), "Important"  = c("Very important","Somewhat important"))
  survydf$HealthCare <-  factor(survydf$HealthCare, levels = c("Not Important",  "Important"), labels = c("Not Important",  "Important"))
  survydf$TheEconomy = fct_collapse(survydf$TheEconomy, "Not Important" = c("Not too important", "Not important at all"), "Important"  = c("Very important","Somewhat important"))
  survydf$TheEconomy <-  factor(survydf$TheEconomy, levels = c("Not Important",  "Important"), labels = c("Not Important",  "Important"))
  survydf$RacialandEthnicInequality = fct_collapse(survydf$RacialandEthnicInequality, "Not Important" = c("Not too important", "Not important at all"), "Important"  = c("Very important","Somewhat important"))
  survydf$RacialandEthnicInequality <-  factor(survydf$RacialandEthnicInequality, levels = c("Not Important",  "Important"), labels = c("Not Important",  "Important"))
  survydf$ClimateChange = fct_collapse(survydf$ClimateChange, "Not Important" = c("Not too important", "Not important at all"), "Important"  = c("Very important","Somewhat important"))
  survydf$ClimateChange <-  factor(survydf$ClimateChange,levels = c("Not Important",  "Important"), labels = c("Not Important",  "Important"))
  if(nov20==F){
  survydf$Inflation = fct_collapse(survydf$Inflation, "Not Important" = c("Not too important", "Not important at all"), "Important"  = c("Very important","Somewhat important"))
  survydf$Inflation <-  factor(survydf$Inflation, levels = c("Not Important",  "Important"), labels = c("Not Important",  "Important"))
  }
  survydf$GunPolicy = fct_collapse(survydf$GunPolicy, "Not Important" = c("Not too important", "Not important at all"), "Important"  = c("Very important","Somewhat important"))
  survydf$GunPolicy <-  factor(survydf$GunPolicy, levels = c("Not Important",  "Important"), labels = c("Not Important",  "Important"))
  survydf$SupremeCourtAppointments = fct_collapse(survydf$SupremeCourtAppointments, "Not Important" = c("Not too important", "Not important at all"), "Important"  = c("Very important","Somewhat important"))
  survydf$SupremeCourtAppointments <-  factor(survydf$SupremeCourtAppointments, levels = c("Not Important",  "Important"), labels = c("Not Important",  "Important"))
  
  
  #Media attention 
  if(nov20==F){
  survydf$Television = fct_collapse(survydf$Television, "Sometimes" = c("Sometimes", "Rarely"))
  survydf$Television <-  factor(survydf$Television, levels = c( "Sometimes", "Often", "Never" ), labels = c( "Sometimes", "Often", "Never" ))
  survydf$Radio = fct_collapse(survydf$Radio, "Sometimes" = c("Sometimes", "Rarely"))
  survydf$Radio <-  factor(survydf$Radio, levels = c( "Sometimes", "Often", "Never" ), labels = c( "Sometimes", "Often", "Never" ))
  survydf$PrintPublications = fct_collapse(survydf$PrintPublications, "Sometimes" = c("Sometimes", "Rarely"))
  survydf$PrintPublications <-  factor(survydf$PrintPublications, levels = c( "Sometimes", "Often", "Never" ), labels = c( "Sometimes", "Often", "Never" ))
  survydf$Online = fct_collapse(survydf$Online, "Sometimes" = c("Sometimes", "Rarely"))
  survydf$Online <-  factor(survydf$Online, levels = c( "Sometimes", "Often", "Never" ), labels = c( "Sometimes", "Often", "Never" ))
  
  survydf$SocialMedia <- factor(survydf$SocialMedia, levels = c("No", "Yes", "Don't know"), labels = c("No", "Yes", "Don't know"))
  survydf$FollowGoverment <- factor(survydf$FollowGoverment, levels = c("Some of the time" ,"Most of the time",   "Only now and then", "Hardly at all" ,    "Don't know"  ),
                                                   labels = c("Some of the time" ,"Most of the time",   "Only now and then", "Hardly at all" ,    "Don't know"  ))
  #economic levels
 
  survydf$EconomicSituation <- factor( survydf$EconomicSituation, 
                                                      labels = c(  "Stayed the same", "Gotten better" ,  "Gotten worse"  ),
                                                      levels = c(  "Stayed the same", "Gotten better" ,  "Gotten worse"  ))
  survydf$FinancialSituation <-factor(survydf$FinancialSituation,
                                                     labels = c(  "The same", "Better off" ,  "Worse off"  ),
                                                     levels = c(  "The same", "Better off" ,  "Worse off"  ))
  }
  return(survydf)
}