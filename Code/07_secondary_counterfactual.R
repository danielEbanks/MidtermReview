
# Load in Libraries.

library(haven)
library(survey)
library(tidyverse)
library(margins)
library(forcats)
library(stargazer)
library(aod)
library(xgboost)
library(gtsummary)
library(xtable)
library(data.table)
library(ggmosaic)

# load("Data/logits_by_party.RData")
load("Data/CleanedSurveys.RData")
load("Data/counterfactual.RData")

regression_2022_abortion = regression_2022
regression_2022_violentcrime = regression_2022
for (i in c(1,2,3,4)){
  regression_2022_abortion[[i]]$coefficients['AbortionImportant'] = regression_2020[[i]]$coefficients['AbortionImportant']
  regression_2022_violentcrime[[i]]$coefficients['ViolentCrimeImportant'] = regression_2020[[i]]$coefficients['ViolentCrimeImportant']
}


data = droplevels(CaltechClimate_Nov2022 %>% filter(PartyID == "Republican") %>% 
          mutate(pred = as.numeric(predict(regression_2022[[3]], type = "response")))) %>% 
  add_row(droplevels(CaltechClimate_Nov2022 %>% filter(PartyID == "Democrat") %>%
          mutate(pred = as.numeric(predict(regression_2022[[2]], type = "response"))))) %>% 
  add_row(droplevels(CaltechClimate_Nov2022 %>% filter(PartyID == "Independent") %>%
            mutate(pred = as.numeric(predict(regression_2022[[1]], type = "response")))))%>% 
    mutate(vote = case_when(vote.cong == "Republican candidate" ~ 1,
                            TRUE ~ 0))

data <- data %>% 
  mutate(pred_abortion = c(as.numeric(predict(regression_2022[[3]], 
                                              type = "response", 
                                              newdata = droplevels(data %>% 
                                                filter(PartyID == "Republican") %>%  mutate(Abortion = "Not Important")))),
                           as.numeric(predict(regression_2022[[2]], 
                                              type = "response", 
                                              newdata = data %>% 
                                                filter(PartyID == "Democrat") %>%  mutate(Abortion = "Not Important"))),
                           as.numeric(predict(regression_2022[[1]], 
                                              type = "response", 
                                              newdata = data %>% 
                                                filter(PartyID == "Independent") %>%  mutate(Abortion = "Not Important")))),
         pred_violentcrime = c(as.numeric(predict(regression_2022[[3]], 
                                         type = "response", 
                                         newdata = data %>% 
                                           filter(PartyID == "Republican") %>%  mutate(ViolentCrime = "Not Important"))),
                      as.numeric(predict(regression_2022[[2]], 
                                         type = "response", 
                                         newdata = data %>% 
                                           filter(PartyID == "Democrat") %>%  mutate(ViolentCrime = "Not Important"))),
                      as.numeric(predict(regression_2022[[1]], 
                                         type = "response", 
                                         newdata = data %>% 
                                           filter(PartyID == "Independent") %>%  mutate(ViolentCrime = "Not Important")))))



data %>% summarise(out = sum(weight*vote)/sum(weight), 
                                         pred = sum(weight*pred)/sum(weight),
                                         pred_abortion = sum(weight*pred_abortion)/sum(weight),
                                         pred_violentcrime = sum(weight*pred_violentcrime)/sum(weight))



data <- data %>% 
  mutate(pred_abortion_2020 = c(as.numeric(predict(regression_2022_abortion[[3]], 
                                                   type = "response", 
                                                   newdata = data %>% 
                                                     filter(PartyID == "Republican"))),
                                as.numeric(predict(regression_2022_abortion[[2]], 
                                                   type = "response", 
                                                   newdata = data %>% 
                                                     filter(PartyID == "Democrat"))),
                                as.numeric(predict(regression_2022_abortion[[1]], 
                                                   type = "response", 
                                                   newdata = data %>% 
                                                     filter(PartyID == "Independent")))),
         pred_violentcrime_2020 = c(as.numeric(predict(regression_2022_violentcrime[[3]], 
                                                       type = "response", 
                                                       newdata = data %>% 
                                                         filter(PartyID == "Republican"))),
                               as.numeric(predict(regression_2022_violentcrime[[2]], 
                                                  type = "response", 
                                                  newdata = data %>% 
                                                    filter(PartyID == "Democrat"))),
                               as.numeric(predict(regression_2022_violentcrime[[1]], 
                                                  type = "response", 
                                                  newdata = data %>% 
                                                    filter(PartyID == "Independent")))))

data %>% summarise(out = sum(weight*vote)/sum(weight), 
                   pred = sum(weight*pred)/sum(weight),
                   pred_abortion = sum(weight*pred_abortion)/sum(weight),
                   pred_violentcrime = sum(weight*pred_violentcrime)/sum(weight),
                   pred_abortion_2020 = sum(weight*pred_abortion_2020)/sum(weight),
                   pred_violentcrime_2020 = sum(weight*pred_violentcrime_2020)/sum(weight))


