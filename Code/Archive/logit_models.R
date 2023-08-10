## Load In Data

library(haven)
library(survey)
library(tidyverse)
library(margins)



labels <- c("Abortion","Age 30-44","Age  45-64","Age 65+", "Budget Reduction",
           "Climate Change","Covid","Education - Some College","Education - College Grad",
           "Education - Postgrad","Foriegn Policy","Growing the Economy","Health Care",
           "Inflation","Law Enforcement","Black","Hispanic","Other","Terrorism")
color.grid <- c("blue","red")
name.grid <- c("Democratic Party","Republican Party")
for(pp in 1:2){
CaltechClimate_Nov2022 <- read_sav("Data/CalTech_November_2022.sav")
CaltechClimate_Nov2022<-CaltechClimate_Nov2022%>%filter(!pid3 %in% c(1,2)) %>% mutate(terror=as.numeric(Q51_1==pp),
                                  climate=as.numeric(Q51_2==pp),
                                  abortion=as.numeric(Q51_3==pp),
                                  law.enf=as.numeric(Q51_4==pp),
                                  covid=as.numeric(Q51_5==pp),
                                  budget=as.numeric(Q51_6==pp),
                                  grow.econ=as.numeric(Q51_7==pp),
                                  health.care=as.numeric(Q51_8==pp),
                                  foreign.policy=as.numeric(Q51_9==pp),
                                  inflation=as.numeric(Q51_10==pp),
                                  vote.cong =as.numeric(Q18==pp),
                                  vote.sen =as.numeric(senvote_combined==pp),
                                  vote.gov =as.numeric(govvote_combined==pp))
national.design.climate.nov2022 = svydesign(data = (CaltechClimate_Nov2022), weights = ~weight, id = ~1)

pdf(paste0("Plots/issue.party.trust_ind_",pp,".pdf"))
reg1     <- svyglm(formula = vote.cong~terror+climate+abortion+law.enf+covid+
                     budget+grow.econ+health.care+foreign.policy+inflation+factor(race4)+factor(educ4)+factor(age4)
                   ,family=quasibinomial ,national.design.climate.nov2022)
reg1_m   <- margins(reg1,design=national.design.climate.nov2022)
plot.df<-summary(reg1_m)%>%mutate(labels=labels)
g<-ggplot(data=plot.df,aes(x=AME,y=reorder(labels, (AME))))  +
  geom_pointrange(aes(xmin=lower,xmax=upper),colour=color.grid[pp],show.legend = FALSE)+
  theme_bw()+
  geom_vline(xintercept = 0,lty=2)+
  ggtitle("Congress")+
  ylab("")+
  xlab(paste0("AME on Probability for voting the\n",name.grid[pp]))
plot(g)


reg1     <- svyglm(formula = vote.sen~terror+climate+abortion+law.enf+covid+
                     budget+grow.econ+health.care+foreign.policy+inflation+factor(race4)+factor(educ4)+factor(age4)
                   ,family=quasibinomial ,national.design.climate.nov2022)
reg1_m   <- margins(reg1,design=national.design.climate.nov2022)
plot.df<-summary(reg1_m)%>%mutate(labels=labels)
g<-ggplot(data=plot.df,aes(x=AME,y=reorder(labels, (AME))))  +
  geom_pointrange(aes(xmin=lower,xmax=upper),colour=color.grid[pp],show.legend = FALSE)+
  theme_bw()+
  geom_vline(xintercept = 0,lty=2)+
  ggtitle("Senator")+
  ylab("")+
  xlab(paste0("AME on Probability for voting the\n",name.grid[pp]))
plot(g)

reg1     <- svyglm(formula = vote.gov~terror+climate+abortion+law.enf+covid+
                     budget+grow.econ+health.care+foreign.policy+inflation+factor(race4)+factor(educ4)+factor(age4)
                   ,family=quasibinomial ,national.design.climate.nov2022)
reg1_m   <- margins(reg1,design=national.design.climate.nov2022)
plot.df<-summary(reg1_m)%>%mutate(labels=labels)
g<-ggplot(data=plot.df,aes(x=AME,y=reorder(labels, (AME))))  +
  geom_pointrange(aes(xmin=lower,xmax=upper),colour=color.grid[pp],show.legend = FALSE)+
  theme_bw()+
  geom_vline(xintercept = 0,lty=2)+
  ggtitle("Governor")+
  ylab("")+
  xlab(paste0("AME on Probability for voting the\n",name.grid[pp]))
plot(g)
dev.off()
}

labels <-c("Abortion",
  "Age 30-44",
  "Age 45-64",
  "Age 65+",
  "Climate Change",
  "COVID",
  "Crime",
  "Economy",
  "Economic Inequality",
  "Education - Some College",
  "Education - College Grad",
  "Education - Postgrad",
  "Foreign Policy",
  "Gun Policy",
  "Health Care",
  "Immigration",
  "Inflation",
  "National Economic Perception",
  "Personal Economic Perception",
  "Racial and Ethnic Inequality",
  "Black",
  "Hispanic",
  "Other",
  "Supreme Court Appointments")
color.grid <- c("blue","red")
name.grid  <- c("Democratic Party","Republican Party")
party_grid <- 1:3
party_label <- c("Democratic","Republican","Independent")

for(pp in 1:2){
 for(i in 1:length(party_grid)){
  print(pp)
  #Load in Data 
  CaltechClimate_Nov2022 <- read_sav("Data/CalTech_November_2022.sav")
  
  #Filter on party
  CaltechClimate_Nov2022 <- CaltechClimate_Nov2022               %>%
                            filter(pid3 %in% c(party_grid[i])) %>% 
                            mutate(immigration   = as.numeric(Q31_1 %in% c(1,2)),
                                  abortion       = as.numeric(Q31_2 %in% c(1,2)),
                                  foreign.policy = as.numeric(Q31_3 %in% c(1,2)),
                                  econ.ineq = as.numeric(Q31_4      %in% c(1,2)),
                                  covid = as.numeric(Q31_5 %in% c(1,2)),
                                  crime = as.numeric(Q31_6 %in% c(1,2)),
                                  health.care = as.numeric(Q31_7 %in% c(1,2)),
                                  econ      = as.numeric(Q31_8   %in% c(1,2)),
                                  race.ineq = as.numeric(Q31_9   %in% c(1,2)),
                                  climate   = as.numeric(Q31_10  %in% c(1,2)),
                                  inflation = as.numeric(Q31_11  %in% c(1,2)),
                                  guns      = as.numeric(Q31_12  %in% c(1,2)),
                                  supreme.court  = as.numeric(Q31_13 %in% c(1,2)),
                                  personal_sit   = as.numeric(Q74 %in% c(1,2)), 
                                  national_sit   = as.numeric(Q75 %in% c(1,2)),                         
                                  vote.cong = as.numeric(Q18==pp),
                                  vote.sen  = as.numeric(senvote_combined==pp),
                                  vote.gov  = as.numeric(govvote_combined==pp))
  
  national.design.climate.nov2022 = svydesign(data    = CaltechClimate_Nov2022, 
                                              weights = ~weight, 
                                              id      = ~1)
  
  
  # Fit Model among each partisan group for vote choice
  
  reg1     <- svyglm(formula = vote.cong~immigration+
                       abortion       +
                       foreign.policy +
                       econ.ineq      +
                       covid          +
                       crime          +
                       health.care +
                       econ        +
                       race.ineq   +
                       climate   +
                       inflation +
                       guns      + 
                       supreme.court +
                       personal_sit  +
                       national_sit  +
                       factor(race4) +
                       factor(educ4) +
                       factor(age4)
                     ,family=quasibinomial ,national.design.climate.nov2022)
  
  #Compute AME's
  
  reg1_m   <- margins(reg1,
                      design=national.design.climate.nov2022)
  
  #Create dataframe to plot AMEs
  plot.df  <- summary(reg1_m)%>%
              mutate(labels_fixed=labels)
  
  #Create pointrange plot for each coef.
  g<-ggplot(data=plot.df,aes(x= AME,
                             y= reorder(labels_fixed, AME)))  +
    geom_pointrange(aes(xmin = lower,
                        xmax = upper),
                    colour=color.grid[pp],show.legend = FALSE) +
    theme_bw()+
    geom_vline(xintercept = 0,
               lty        = 2)+
    ggtitle("Congress")+
    ylab("")+
    xlab(paste0("AME on Probability for voting the\n",name.grid[pp]))
  
  pdf(paste0("Plots/",party_label[i],"_issue.importance_ind_",pp,".pdf"))
  plot(g)
  dev.off()
  
  }
}

