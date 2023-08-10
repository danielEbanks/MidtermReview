## Load In Data
rm(list = ls())
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
  CaltechClimate_Nov2022$Crime_ability <-  factor(CaltechClimate_Nov2022$Crime_ability, levels = c("Not sure" ,  "Democrat" ,  "Republican"), labels = c("Not sure" ,  "Democrat" ,  "Republican"))
  CaltechClimate_Nov2022$Climate_ability <-  factor(CaltechClimate_Nov2022$Climate_ability, levels = c("Not sure" ,  "Democrat" ,  "Republican"), labels = c("Not sure" ,  "Democrat" ,  "Republican"))
  CaltechClimate_Nov2022$LawEnforcement_ability <-  factor(CaltechClimate_Nov2022$LawEnforcement_ability, levels = c("Not sure" ,  "Democrat" ,  "Republican"), labels = c("Not sure" ,  "Democrat" ,  "Republican"))
  CaltechClimate_Nov2022$Covid_ability <-  factor(CaltechClimate_Nov2022$Covid_ability, levels = c("Not sure" ,  "Democrat" ,  "Republican"), labels = c("Not sure" ,  "Democrat" ,  "Republican"))
  CaltechClimate_Nov2022$Budget_ability <-  factor(CaltechClimate_Nov2022$Budget_ability, levels = c("Not sure" ,  "Democrat" ,  "Republican"), labels = c("Not sure" ,  "Democrat" ,  "Republican"))
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

setwd('~/Dropbox/ClimateSurvey/')

CaltechClimate_Nov2022 <- read_sav("~/Dropbox/ClimateSurvey/Data/CalTech_November_2022.sav") 
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
  
demographics <- c("Race", "Religion", "Education", "Age", "Region", "Gender")

CaltechClimate_Nov2022 <- CaltechClimate_Nov2022[-c(280,  723,  884, 1115 ),]

CaltechClimate_Nov2022 <- apply_levels(CaltechClimate_Nov2022)

national.design.Climate.nov2022 = svydesign(data = (CaltechClimate_Nov2022), weights = ~weight, id = ~1)

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

reg1_m   <- margins(reg1,design=national.design.Climate.nov2022)
plot.df<-summary(reg1_m)
  
temp <- CaltechClimate_Nov2022 %>% pivot_longer(!c(vote.cong, weight), 
                                        names_to = "Cat",
                                        values_to = "type") %>% 
  select(Cat, type) %>% 
  group_by(Cat, type) %>% 
  summarise(n = n()) %>% 
  ungroup() %>% 
  select(Cat, type) %>% 
  unite("name", Cat:type, sep = "", remove= FALSE)

plot.df <- as_tibble(plot.df) %>% left_join(temp, by = c("factor" = "name")) %>% separate(Cat, c("Cat_Ability", NA),remove = FALSE)

g<-ggplot(data=plot.df %>% filter( Cat == "PartyID"),aes(x=AME,y=type))  +
  geom_pointrange(aes(xmin=lower,xmax=upper),show.legend = FALSE)+
  geom_vline(xintercept = 0,lty=2)+
  labs(y = "Party ID", x = "AME voting Republican") + 
  theme_classic()
pdf(paste0("Plots/CK_partyID_All.pdf"), height = 2, width = 6)
plot(g)
dev.off()

g<-ggplot(data=plot.df %>% filter(type %in% c("Republican", "Not sure", "Democrat"), Cat != "PartyID"),aes(x=AME,y=Cat_Ability, color = type))  +
  geom_pointrange(aes(xmin=lower,xmax=upper),show.legend = FALSE,position = position_dodge(width = 0.5))+
  geom_vline(xintercept = 0,lty=2)+
  scale_color_discrete(type= c("blue", "red" )) +
  labs(y = "Policy Area", x = "AME voting Republican") + 
  theme_classic()
pdf(paste0("Plots/CK_competence_All.pdf"), width = 8, height = 6)
plot(g)
dev.off()

g<-ggplot(data=plot.df %>% filter(Cat %in% demographics),aes(x=AME,y=type))  +
  geom_pointrange(aes(xmin=lower,xmax=upper),show.legend = FALSE)+
  geom_vline(xintercept = 0,lty=2)+
  facet_wrap(~Cat, scale = "free", nrow = 2) + 
  labs(y = "")+
  theme_classic()

pdf(paste0("Plots/CK_demo_All.pdf"), height = 6, width = 12)
plot(g)
dev.off()

g<-ggplot(data=plot.df %>% filter(type == "Important") ,aes(x=AME,y=Cat))  +
  geom_pointrange(aes(xmin=lower,xmax=upper),show.legend = FALSE)+
  geom_vline(xintercept = 0,lty=2)+
  facet_wrap(~type, scale = "free") +
  labs(y = "Policy Area", x = "AME voting Republican") +
  theme_classic()

pdf(paste0("Plots/CK_importance_All.pdf"), height = 4, width = 8)
plot(g)
dev.off()

g<-ggplot(data=plot.df %>% filter(Cat %in% c("EconomicSituation", 
                                             "FinancialSituation")),aes(x=AME,y=type))  +
  geom_pointrange(aes(xmin=lower,xmax=upper),show.legend = FALSE)+
  geom_vline(xintercept = 0,lty=2)+
  facet_wrap(~Cat, scale = "free") +
  labs(y = "", x = "AME voting Republican") +
  theme_classic()

pdf(paste0("Plots/CK_economics_All.pdf"), height = 4, width = 8)
plot(g)
dev.off()


################################################################################
############################### Smaller Regression   ###########################
################################################################################


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
reg3_m   <- margins(reg3,design=national.design.Climate.nov2022)
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



################################################################################
############################### Seperate Regression   ###########################
################################################################################

 
for (pp in c("Independent",  "Republican", "Democrat"  )){ #},  "Other" , "Not sure")){
  print(pp)
  nam <- paste0('reg_party_', pp)
  if (pp == "Democrat"){
    national.design.Climate.nov2022 = svydesign(data = droplevels(CaltechClimate_Nov2022 %>% filter(PartyID == pp, 
                                                                                                    !(Religion %in% c("Jewish", "Muslim", "No religion", "Don't know")),
                                                                                                    !(Gender %in% c("Non-Binary/Fluid", "Prefer not to say"))) %>%
                                                                    mutate(Race = as.factor(case_when(Race == "White" ~ "White", 
                                                                                                      Race == "Black or African American" ~ "Black",
                                                                                                      TRUE ~ "Other")))) , weights = ~weight, id = ~1)
    
    assign(nam, svyglm(formula = vote.cong~ 
                         Race+
                         Region + 
                         Religion + 
                         Age + 
                         Gender + 
                         Education + 
                         EconomicSituation + 
                         #FollowGoverment + 
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
                       national.design.Climate.nov2022))
  } else if (pp == "Republican") {
    national.design.Climate.nov2022 = svydesign(data = droplevels(CaltechClimate_Nov2022 %>% filter(PartyID == pp, 
                                                                                                    FollowGoverment != "Don't know") %>% 
                                                                    mutate(Race = as.factor(case_when(Race == "White" ~ "White", 
                                                                                                      Race == "Black or African American" ~ "Black",
                                                                                                      TRUE ~ "Other")))), weights = ~weight, id = ~1)
    
    assign(nam, svyglm(formula = vote.cong~ 
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
                       national.design.Climate.nov2022))
  }else {
    national.design.Climate.nov2022 = svydesign(data = droplevels(CaltechClimate_Nov2022 %>% filter(PartyID == pp) %>% 
                                                                    mutate(Race = as.factor(case_when(Race == "White" ~ "White", 
                                                                                                      Race == "Black or African American" ~ "Black",
                                                                                                      TRUE ~ "Other")))), weights = ~weight, id = ~1)
    
    assign(nam, svyglm(formula = vote.cong~ 
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
                       national.design.Climate.nov2022))
  }
 
  

  
  regparty_m   <- margins(get(nam),design=national.design.Climate.nov2022)
  plot.df_party<-summary(regparty_m)
  
  plot.df_party <- as_tibble(plot.df_party) %>% left_join(temp, by = c("factor" = "name"))%>% separate(Cat, c("Cat_Ability", NA),remove = FALSE) %>% 
    mutate(lower = AME - 1.96*SE, upper = AME + 1.96*SE)
  
  nam <- paste0('reg_party_margins_', pp)
  assign(nam, plot.df_party %>% mutate(group = pp))
  
  g<-ggplot(data=plot.df_party %>% filter(type %in% c("Republican", "Not sure", "Democrat"), Cat != "PartyID"),aes(x=AME,y=Cat_Ability, color = type))  +
    geom_pointrange(aes(xmin=lower,xmax=upper),show.legend = FALSE,position = position_dodge(width = 0.5))+
    geom_vline(xintercept = 0,lty=2)+
    scale_color_discrete(type= c("blue", "red" )) +
    labs(y = "Policy Area", x = "AME voting Republican") + 
    theme_classic()
  pdf(paste0("Plots/CK_competence_", pp, ".pdf"), width = 8, height = 6)
  plot(g)
  dev.off()
  
  g<-ggplot(data=plot.df_party %>% filter(Cat %in% demographics),aes(x=AME,y=type))  +
    geom_pointrange(aes(xmin=lower,xmax=upper),show.legend = FALSE) + 
    geom_vline(xintercept = 0,lty=2)+
    facet_wrap(~Cat, scale = "free", nrow = 2) + 
    labs(y = "")+
    theme_classic()
  
  pdf(paste0("Plots/CK_demo_", pp, ".pdf"), height = 6, width = 12)
  plot(g)
  dev.off()
  
  g<-ggplot(data=plot.df_party %>% filter(type == "Important") ,aes(x=AME,y=Cat))  +
    geom_pointrange(aes(xmin=lower,xmax=upper),show.legend = FALSE)+
    geom_vline(xintercept = 0,lty=2)+
    facet_wrap(~type, scale = "free") +
    labs(y = "Policy Area", x = "AME voting Republican") +
    theme_classic()
  
  pdf(paste0("Plots/CK_importance_", pp, ".pdf"), height = 4, width = 8)
  plot(g)
  dev.off()
  

  
}


save(reg_party_margins_Democrat, 
     reg_party_Democrat, 
     reg_party_margins_Republican,
     reg_party_Republican, 
     reg_party_margins_Independent,
     reg_party_Independent, 
     reg3, 
     reg3_m,
     CaltechClimate_Nov2022,
     file = "Data/regressions.RData")
################################################################################
############################### Figures   ######################################
################################################################################
load("Data/regressions.RData")

national.design.Climate.nov2022 = svydesign(data = droplevels(CaltechClimate_Nov2022), weights = ~weight, id = ~1)

plot.df_comb<-summary(reg3_m)
plot.df_comb <- as_tibble(plot.df_comb) %>% left_join(temp, by = c("factor" = "name"))%>% separate(Cat, c("Cat_Ability", NA),remove = FALSE) %>% 
  mutate(lower = AME - 1.96*SE, upper = AME + 1.96*SE) %>% mutate(group = "Pooled") %>% 
  add_row(reg_party_margins_Democrat) %>% 
  add_row(reg_party_margins_Republican) %>% 
  add_row(reg_party_margins_Independent) 



g<-ggplot(data=plot.df_comb %>% filter(type == "Important") ,aes(x=AME,y=Cat, color = group))  +
  geom_pointrange(aes(xmin=lower,xmax=upper),position = position_dodge(width = 0.5))+
  geom_vline(xintercept = 0,lty=2)+
  labs(y = "Policy Area", x = "AME voting Republican") +
  scale_color_discrete(type= c("#0A08B7", "#8008AD", "black", "#C62D0D" )) + 
  theme_classic() +
  theme(legend.position = "bottom") 

pdf(paste0("Plots/CK_importance_comparison.pdf"), height = 4, width = 8)
plot(g)
dev.off()

g <- ggplot(as_tibble(svytable(~ PartyID + vote.cong , national.design.Climate.nov2022)) %>% 
         group_by(PartyID) %>% 
         mutate(N = sum(n)) %>%
         filter(PartyID %in%  c("Independent",  "Republican", "Democrat")),
       aes(y = n/N*100, 
           x = PartyID,
           fill = vote.cong,
           label = round(n/N*100, digits = 1))) + geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_discrete(type = c("#0A08B7", "#C62D0D" )) + 
  labs(x = "Party ID", y = "Percent Voting") + 
  geom_text(position = position_dodge(width = .9), vjust = -0.5) + 
  theme_classic() + 
  ylim(0,120)+
  scale_y_continuous(breaks = c(20,40,60,80,100))+
  guides(fill=guide_legend(title="House Vote Choice"))+
  theme(legend.position = "bottom")

pdf(paste0("Plots/CK_partyID_voting.pdf"), height = 5, width = 4)
plot(g)
dev.off()

g <- ggplot(as_tibble(svytable(~ PartyID + vote.cong +Immigration +
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
                                 SupremeCourtAppointments, national.design.Climate.nov2022)) %>% 
              group_by(PartyID) %>% 
              mutate(N = sum(n)) %>%
              filter(PartyID %in%  c("Independent",  "Republican", "Democrat")),
            aes(y = n/N*100, 
                x = PartyID,
                fill = vote.cong,
                label = round(n/N*100, digits = 1))) + geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_discrete(type = c("#0A08B7", "#C62D0D" )) + 
  labs(x = "Party ID", y = "Percent Voting") + 
  geom_text(position = position_dodge(width = .9), vjust = -0.5) + 
  theme_classic() + 
  theme(legend.position = "none")

pdf(paste0("Plots/CK_partyID_voting.pdf"), height = 4, width = 4)
plot(g)
dev.off()
t <-as_tibble(svytable(~ PartyID + vote.cong +Immigration +
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
                         SupremeCourtAppointments, national.design.Climate.nov2022)) %>% 
  pivot_longer(cols = !c(PartyID, vote.cong, n), names_to = "Issue", values_to = "Type") %>% 
  group_by(PartyID, vote.cong, Issue, Type) %>% summarise(n = sum(n)) %>% 
  ungroup() %>% 
  group_by(PartyID, Issue, Type) %>% mutate(N = sum(n)) %>% 
  pivot_wider(names_from = vote.cong, values_from = n) %>% 
  filter(PartyID %in% c("Democrat", "Republican", "Independent"), Issue != "SupremeCourtAppointments")


g <- ggplot(t, aes(color =PartyID , y = `Democratic candidate`/N, x = Type, size = N)) + 
  scale_color_discrete(type = c("#0A08B7", "#8008AD","#C62D0D" )) + 
  geom_point() + 
  facet_wrap(~Issue) +
  ylim(-0.25, 1.25) + 
  theme_bw()+
  theme(legend.position = "bottom") +
  labs(y = "Percent vote for Democratic Candidate", x = "")+
  guides(size = "none")

pdf(paste0("Plots/CK_partyID_by_Issue.pdf"), height = 4, width = 8)
plot(g)
dev.off()

t <-as_tibble(svytable(~ PartyID + vote.cong +EconomicSituation + FinancialSituation 
                       , national.design.Climate.nov2022)) %>% 
  pivot_longer(cols = !c(PartyID, vote.cong, n), names_to = "Issue", values_to = "Type") %>% 
  group_by(PartyID, vote.cong, Issue, Type) %>% summarise(n = sum(n)) %>% 
  ungroup() %>% 
  group_by(PartyID, Issue, Type) %>% mutate(N = sum(n)) %>% 
  pivot_wider(names_from = vote.cong, values_from = n) %>% 
  filter(PartyID %in% c("Democrat", "Republican", "Independent"))


g <- ggplot(t, aes(color =PartyID , y = `Democratic candidate`/N, x = Type, size = N)) + 
  scale_color_discrete(type = c("#0A08B7", "#466D1D","#C62D0D" )) + 
  geom_point() + 
  facet_wrap(~Issue, scales = "free_x") +
  theme_bw()+
  labs(y = "Percent vote for Democratic Candidate", x = "")+
  guides(size = "none") 

pdf(paste0("Plots/CK_partyID_by_economics.pdf"), height = 4, width = 8)
plot(g)
dev.off()

g <- ggplot(as_tibble(svytable(~ PartyID + Abortion +vote.cong, national.design.Climate.nov2022)) %>% 
              group_by(PartyID, Abortion) %>% 
              mutate(N = sum(n)) %>%
              filter(PartyID %in%  c("Independent",  "Republican", "Democrat")),
            aes(y = n/N*100, 
                x = interaction(Abortion),
                fill = vote.cong,
                label = round(N))) + geom_bar(stat = "identity", position = "stack") + 
  scale_fill_discrete(type = c("#0A08B7", "#C62D0D" )) + 
  geom_text( mapping = aes(y = 105)) + 
  facet_wrap(~PartyID)+
  labs(x = "Abortion", y = "Percent Voting") + 
  theme_classic() + 
  theme(legend.position = "none")

pdf(paste0("Plots/CK_Abortion.pdf"), height = 3, width = 6)
plot(g)
dev.off()


g <- ggplot(as_tibble(svytable(~ PartyID + ViolentCrime +vote.cong, national.design.Climate.nov2022)) %>% 
              group_by(PartyID, ViolentCrime) %>% 
              mutate(N = sum(n)) %>%
              filter(PartyID %in%  c("Independent",  "Republican", "Democrat")),
            aes(y = n/N*100, 
                x = interaction(ViolentCrime),
                fill = vote.cong,
                label = round(N))) + geom_bar(stat = "identity", position = "stack") + 
  scale_fill_discrete(type = c("#0A08B7", "#C62D0D" )) + 
  geom_text( mapping = aes(y = 105)) + 
  facet_wrap(~PartyID)+
  labs(x = "Violent Crime", y = "Percent Voting") + 
  theme_classic() + 
  theme(legend.position = "none")

pdf(paste0("Plots/CK_ViolentCrime.pdf"), height = 3, width = 6)
plot(g)
dev.off()



g <- ggplot(as_tibble(svytable(~ PartyID + EconomicInequality +vote.cong, national.design.Climate.nov2022)) %>% 
              group_by(PartyID, EconomicInequality) %>% 
              mutate(N = sum(n)) %>%
              filter(PartyID %in%  c("Independent",  "Republican", "Democrat")),
            aes(y = n/N*100, 
                x = interaction(EconomicInequality),
                fill = vote.cong,
                label = round(N))) + geom_bar(stat = "identity", position = "stack") + 
  scale_fill_discrete(type = c("#0A08B7", "#C62D0D" )) + 
  geom_text( mapping = aes(y = 105)) + 
  facet_wrap(~PartyID)+
  labs(x = "Economic Inequality", y = "Percent Voting") + 
  theme_classic() + 
  theme(legend.position = "none")

pdf(paste0("Plots/CK_EconomicInequality.pdf"), height = 3, width = 6)
plot(g)
dev.off()


g <- ggplot(as_tibble(svytable(~ PartyID + EconomicSituation +vote.cong, national.design.Climate.nov2022)) %>% 
              group_by(PartyID, EconomicSituation) %>% 
              mutate(N = sum(n)) %>%
              filter(PartyID %in%  c("Independent",  "Republican", "Democrat")),
            aes(y = n/N*100, 
                x = interaction(EconomicSituation),
                fill = vote.cong,
                label = round(N))) + geom_bar(stat = "identity", position = "stack") + 
  scale_fill_discrete(type = c("#0A08B7", "#C62D0D" )) + 
  geom_text( mapping = aes(y = 105)) + 
  facet_wrap(~PartyID)+
  labs(x = "Economic Situation", y = "Percent Voting") + 
  theme_classic() + 
  theme(legend.position = "none")

pdf(paste0("Plots/CK_EconomicSituation.pdf"), height = 3, width = 6)
plot(g)
dev.off()


g <- ggplot(as_tibble(svytable(~ PartyID + FinancialSituation +vote.cong, national.design.Climate.nov2022)) %>% 
              group_by(PartyID, FinancialSituation) %>% 
              mutate(N = sum(n)) %>%
              filter(PartyID %in%  c("Independent",  "Republican", "Democrat")),
            aes(y = n/N*100, 
                x = interaction(FinancialSituation),
                fill = vote.cong,
                label = round(N))) + geom_bar(stat = "identity", position = "stack") + 
  scale_fill_discrete(type = c("#0A08B7", "#C62D0D" )) + 
  geom_text( mapping = aes(y = 105)) + 
  facet_wrap(~PartyID)+
  labs(x = "Financial Situation", y = "Percent Voting") + 
  theme_classic() + 
  theme(legend.position = "none")

pdf(paste0("Plots/CK_FinancialSituation.pdf"), height = 3, width = 6)
plot(g)
dev.off()



################################################################################
############################### Generating Regression Tables   ##################
################################################################################


national.design.Climate.nov2022 = svydesign(data = (CaltechClimate_Nov2022), weights = ~weight, id = ~1)

reg1_m   <- margins(reg1,design=national.design.Climate.nov2022)
plot.df<-summary(reg1_m)

val <- list(plot.df[,'AME'])[[1]]
names(val) <- plot.df[,'factor']
s_1 <- list(plot.df[,'SE'])[[1]]
names(s_1) <- plot.df[,'factor']
reg1$coefficients <- val

reg2_m   <- margins(reg2,design=national.design.Climate.nov2022)
plot.df_2<-summary(reg2_m)

val <- list(plot.df_2[,'AME'])[[1]]
names(val) <- plot.df_2[,'factor']
s_2 <- list(plot.df_2[,'SE'])[[1]]
names(s_2) <- plot.df_2[,'factor']
reg2$coefficients <- val


reg3_m   <- margins(reg3,design=national.design.Climate.nov2022)
plot.df_3<-summary(reg3_m)

val <- list(plot.df_3[,'AME'])[[1]]
names(val) <- plot.df_3[,'factor']
s_3 <- list(plot.df_3[,'SE'])[[1]]
names(s_3) <- plot.df_3[,'factor']
reg3$coefficients <- val


reg4_m   <- margins(reg4,design=national.design.Climate.nov2022)
plot.df_4<-summary(reg4_m)

val <- list(plot.df_4[,'AME'])[[1]]
names(val) <- plot.df_4[,'factor']
s_4 <- list(plot.df_4[,'SE'])[[1]]
names(s_4) <- plot.df_4[,'factor']
reg4$coefficients <- val

national.design.Climate.nov2022 = svydesign(data = droplevels(CaltechClimate_Nov2022 %>% filter(PartyID == "Independent")), weights = ~weight, id = ~1)

reg_party_Independent_m   <- margins(reg_party_Independent,design=national.design.Climate.nov2022)
plot.df_3I<-summary(reg_party_Independent_m)

val <- list(plot.df_3I[,'AME'])[[1]]
names(val) <- plot.df_3I[,'factor']
s_3I <- list(plot.df_3I[,'SE'])[[1]]
names(s_3I) <- plot.df_3I[,'factor']
reg_party_Independent$coefficients <- val

national.design.Climate.nov2022 = svydesign(data = droplevels(CaltechClimate_Nov2022 %>% filter(PartyID == "Republican")), weights = ~weight, id = ~1)


reg_party_Republican_m   <- margins(reg_party_Republican,,design=national.design.Climate.nov2022)
plot.df_3R<-summary(reg_party_Republican_m)

val <- list(plot.df_3R[,'AME'])[[1]]
names(val) <- plot.df_3R[,'factor']
s_3R <- list(plot.df_3R[,'SE'])[[1]]
names(s_3R) <- plot.df_3R[,'factor']
reg_party_Republican$coefficients <- val

national.design.Climate.nov2022 = svydesign(data = droplevels(CaltechClimate_Nov2022 %>% filter(PartyID == "Democrat")), weights = ~weight, id = ~1)

reg_party_Democrat_m   <- margins(reg_party_Democrat,design=national.design.Climate.nov2022)
plot.df_3D<-summary(reg_party_Democrat_m)

val <- list(plot.df_3D[,'AME'])[[1]] 
names(val) <- plot.df_3D[,'factor']
s_3D <- list(plot.df_3D[,'SE'])[[1]]
names(s_3D) <- plot.df_3D[,'factor']
reg_party_Democrat$coefficients <- val


stargazer(reg1, reg2, reg3, reg4, report=('vc*s'), se = list(s_1, s_2, s_3, s_4))

stargazer(reg3,reg_party_Republican, reg_party_Democrat, reg_party_Independent, report=('vc*s'), se = list(s_3, s_3R, s_3D, s_3I))

