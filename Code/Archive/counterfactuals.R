## Load In Data
library(haven)
library(survey)
library(tidyverse)
library(margins)
library(forcats)
library(stargazer)

load("Data/regressions.RData")

proportions <- CaltechClimate_Nov2022 %>% group_by(PartyID) %>% 
  summarise(w = sum(weight)) %>% 
  filter(PartyID %in% c("Democrat", "Republican", "Independent")) %>% 
  ungroup() %>% mutate(N = sum(w), p = w/N)
national.design.Climate.nov2022 = svydesign(data = (CaltechClimate_Nov2022), weights = ~weight, id = ~1)

full <- as_tibble(predict(reg3, type = "response", se.fit = TRUE)) %>% 
  mutate(lower = response - 1.96*SE, upper = response + 1.96*SE, w = national.design.Climate.nov2022$variables$weight)


national.design.Climate.nov2022_D = svydesign(data = droplevels(CaltechClimate_Nov2022 %>% filter(PartyID == "Democrat", 
                                                                                                !(Religion %in% c("Jewish", "Muslim", "No religion", "Don't know")),
                                                                                                !(Gender %in% c("Non-Binary/Fluid", "Prefer not to say"))) %>%
                                                                mutate(Race = as.factor(case_when(Race == "White" ~ "White", 
                                                                                                  Race == "Black or African American" ~ "Black",
                                                                                                  TRUE ~ "Other")))) , weights = ~weight, id = ~1)
national.design.Climate.nov2022_R = svydesign(data = droplevels(CaltechClimate_Nov2022 %>% filter(PartyID == "Republican" ,
                                                                                                FollowGoverment != "Don't know") %>% 
                                                                mutate(Race = as.factor(case_when(Race == "White" ~ "White", 
                                                                                                  Race == "Black or African American" ~ "Black",
                                                                                                  TRUE ~ "Other")))), weights = ~weight, id = ~1)
national.design.Climate.nov2022_I  = svydesign(data = droplevels(CaltechClimate_Nov2022 %>% filter(PartyID == "Independent") %>% 
                                                                                                    mutate(Race = as.factor(case_when(Race == "White" ~ "White", 
                                                                                                                                      Race == "Black or African American" ~ "Black",
                                                                                                                                      TRUE ~ "Other")))), weights = ~weight, id = ~1)
survey_values <- as_tibble(svymean(~vote.cong, national.design.Climate.nov2022))
survey_values_D <- as_tibble(svymean(~vote.cong, national.design.Climate.nov2022_D))
survey_values_R <- as_tibble(svymean(~vote.cong, national.design.Climate.nov2022_R))
survey_values_I <- as_tibble(svymean(~vote.cong, national.design.Climate.nov2022_I))
D <- as_tibble(predict(reg_party_Democrat, type = "response")) %>% 
  mutate(lower = response - 1.96*SE, upper = response + 1.96*SE,  w = national.design.Climate.nov2022_D$variables$weight)
R <- as_tibble(predict(reg_party_Republican, type = "response")) %>% 
            mutate(lower = response - 1.96*SE, upper = response + 1.96*SE,  w = national.design.Climate.nov2022_R$variables$weight)
I <- as_tibble(predict(reg_party_Independent, type = "response")) %>% 
            mutate(lower = response - 1.96*SE, upper = response + 1.96*SE,  w = national.design.Climate.nov2022_I$variables$weight)

pooled <- D %>% mutate(party= "Democrat") %>% 
  add_row(R %>% mutate(party = "Republican")) %>% 
  add_row(I %>% mutate(party = "Independent")) %>% 
  left_join(proportions %>% select(PartyID, p), by = c("party" = "PartyID"))%>% 
  group_by(party) %>%
  mutate(w = w/sum(w)*p) %>% ungroup()

ggplot() + 
  geom_histogram(full, mapping = aes(x = response, weight = w, y = (..count..)/sum(..count..)), fill = "grey", alpha = 0.5) + 
  geom_pointrange(full %>% summarise(response = weighted.mean(response,w), lower = weighted.mean(lower, w), upper = weighted.mean(upper, w)), mapping = aes(xmin = lower, xmax = upper, x = response, y = .3),show.legend = FALSE)+
  geom_histogram(pooled, mapping = aes(x = response, weight = w, y = (..count..)/sum(..count..), fill = party), alpha = 0.5, position = "stack") + 
  geom_pointrange(pooled %>% group_by(party) %>% summarise(response = weighted.mean(response,w), lower = weighted.mean(lower, w), upper = weighted.mean(upper, w)), mapping = aes(xmin = lower, xmax = upper, x = response, y = .275, color = party),show.legend = FALSE)+
  geom_pointrange(pooled %>%  summarise(response = weighted.mean(response,w), lower = weighted.mean(lower, w), upper = weighted.mean(upper, w)), mapping = aes(xmin = lower, xmax = upper, x = response, y = .28725),color = "black", show.legend = FALSE)+
  geom_vline(xintercept =  survey_values[2,1] %>% pull())+
  geom_vline(xintercept = (survey_values[2,1] + 1.96*survey_values[2,2])%>% pull(), linetype = "dashed") +
  geom_vline(xintercept = (survey_values[2,1] - 1.96*survey_values[2,2])%>% pull(), linetype = "dashed") +
  geom_vline(xintercept =  survey_values_D[2,1] %>% pull(), color= "#0A08B7")+
  geom_vline(xintercept = (survey_values_D[2,1] + 1.96*survey_values_D[2,2])%>% pull(), linetype = "dashed", color= "#0A08B7") +
  geom_vline(xintercept = (survey_values_D[2,1] - 1.96*survey_values_D[2,2])%>% pull(), linetype = "dashed", color= "#0A08B7") +
  geom_vline(xintercept =  survey_values_R[2,1] %>% pull(), color= "#C62D0D")+
  geom_vline(xintercept = (survey_values_R[2,1] + 1.96*survey_values_R[2,2])%>% pull(), linetype = "dashed", color= "#C62D0D") +
  geom_vline(xintercept = (survey_values_R[2,1] - 1.96*survey_values_R[2,2])%>% pull(), linetype = "dashed", color= "#C62D0D") +
  geom_vline(xintercept =  survey_values_I[2,1] %>% pull(), color= "#8008AD")+
  geom_vline(xintercept = (survey_values_I[2,1] + 1.96*survey_values_I[2,2])%>% pull(), linetype = "dashed", color= "#8008AD") +
  geom_vline(xintercept = (survey_values_I[2,1] - 1.96*survey_values_I[2,2])%>% pull(), linetype = "dashed", color= "#8008AD") +
  scale_color_discrete(type= c("#0A08B7", "#8008AD", "#C62D0D" )) + 
  scale_fill_discrete(type= c("#0A08B7", "#8008AD",  "#C62D0D" )) + 
  theme_classic() + 
  labs(x = "Probability of voting Republican", y = "Weighted percent of respondants")


CaltechClimate_Nov2022 <- CaltechClimate_Nov2022 %>% select(vote.cong, PartyID, 
                                                              Race,
                                                              Region, 
                                                              Religion, 
                                                              Age, 
                                                              Gender, 
                                                              Education, 
                                                              EconomicSituation, 
                                                              FollowGoverment, 
                                                              HealthCare_ability, 
                                                              Inflation_ability, 
                                                              Covid_ability, 
                                                              Crime_ability, 
                                                              Abortion_ability, 
                                                              ClimateChange, 
                                                              Abortion, 
                                                              COVID19, 
                                                              EconomicInequality, 
                                                              ForeignPolicy, 
                                                              ViolentCrime)

test1 <- tibble("PartyID" = c("Democrat","Democrat", "Republican", "Republican", "Independent", "Independent", "Democrat","Democrat", "Republican", "Republican", "Independent", "Independent"), 
                "Race" = "White",
                "Region" = "Northeast", 
                "Religion" = "Protestant", 
                "Age" = "30-44", 
                "Gender" = "Woman",
                "Education" = "College Graduate", 
                "EconomicSituation" = "Stayed the same", 
                "FollowGoverment" = "Some of the time", 
                "HealthCare_ability" = "Democrat", 
                "Inflation_ability" = "Republican", 
                "Covid_ability" = "Democrat", 
                "Crime_ability" = "Republican", 
                "Abortion_ability" = c("Democrat","Democrat", "Democrat", "Democrat", "Democrat", "Democrat", "Republican", "Republican", "Republican", "Republican", "Republican", "Republican"), 
                "ClimateChange" = "Important", 
                "Abortion" = c("Important", "Not Important","Important", "Not Important","Important", "Not Important","Important", "Not Important","Important", "Not Important","Important", "Not Important"), 
                "COVID19" = "Important", 
                "EconomicInequality" = "Important", 
                "ForeignPolicy" = "Important", 
                "ViolentCrime" = "Important")
pred1 <- cbind(predict(reg3, newdata = test1, type = "response"), test1) %>% 
  pivot_wider(names_from = "Abortion", values_from = c("response", "SE"))


g <- ggplot(pred1, aes(xmin = response_Important + 1.96*SE_Important  - `response_Not Important` - 1.96*`SE_Not Important`,
                  xmax = response_Important - 1.96*SE_Important - `response_Not Important` + 1.96*`SE_Not Important`,
                  x = response_Important - `response_Not Important`, 
                  y = interaction(PartyID),
                  color = PartyID, label = Abortion_ability)) + 
  facet_wrap(~Abortion_ability, nrow = 1) +
  geom_pointrange() + 
  scale_color_discrete(type= c("#0A08B7", "#8008AD", "#C62D0D" )) +
  labs(x = "First Differences on viewing Abortion as important", y = "") +  
  geom_vline(xintercept = 0,lty=2)+
  theme_classic()+
  theme(legend.position = "none")


pdf(paste0("Plots/CK_abortion_FD.pdf"), height = 2, width = 6)
plot(g)
dev.off()


prediction <- tibble("response" = numeric(), "SE" = numeric(), prob = numeric(), party = character())
for (p in seq(0,1,.1)){
  out <- c()
  pred_temp_I <- tibble("response" = numeric(), "SE" = numeric())
  pred_temp_D <- tibble("response" = numeric(), "SE" = numeric())
  pred_temp_R <- tibble("response" = numeric(), "SE" = numeric())
  for (t in 1:10){
  ind_abortion <- CaltechClimate_Nov2022 %>% mutate(prob = rbinom(1871, 1, p),
                                                          Abortion = factor(case_when((PartyID == "Independent" & prob == 1) ~ "Important",
                                                                               (PartyID == "Independent" & prob == 0) ~ "Not Important",
                                                                               TRUE ~ as.character(Abortion))))
  rep_abortion <- CaltechClimate_Nov2022 %>% mutate(prob = rbinom(1871, 1, p),
                                                    Abortion = factor(case_when((PartyID == "Republican" & prob == 1) ~ "Important",
                                                                                (PartyID == "Republican" & prob == 0) ~ "Not Important",
                                                                                TRUE ~ as.character(Abortion))))
  dem_abortion <- CaltechClimate_Nov2022 %>% mutate(prob = rbinom(1871, 1, p),
                                                    Abortion = factor(case_when((PartyID == "Democrat" & prob == 1) ~ "Important",
                                                                                (PartyID == "Democrat" & prob == 0) ~ "Not Important",
                                                                                TRUE ~ as.character(Abortion))))
  pred_temp_I <- pred_temp_I %>% add_row(cbind(predict(reg3, newdata = ind_abortion, type = "response"), ind_abortion) %>%
    summarise(response = weighted.mean(response, weight), SE = weighted.mean(SE, weight))) 
  pred_temp_D <- pred_temp_D %>% add_row(cbind(predict(reg3, newdata = dem_abortion, type = "response"), ind_abortion) %>%
                                           summarise(response = weighted.mean(response, weight), SE = weighted.mean(SE, weight))) 
  pred_temp_R <- pred_temp_R %>% add_row(cbind(predict(reg3, newdata = rep_abortion, type = "response"), ind_abortion) %>%
                                           summarise(response = weighted.mean(response, weight), SE = weighted.mean(SE, weight))) 
  }
  pred_temp_I <- pred_temp_I %>% summarise(response = mean(response), SE = mean(SE))
  pred_temp_D <- pred_temp_D %>% summarise(response = mean(response), SE = mean(SE))
  pred_temp_R <- pred_temp_R %>% summarise(response = mean(response), SE = mean(SE))
  prediction <- prediction %>% 
    add_row(pred_temp_I %>% mutate(prob = p, party = "Independent"))%>% 
    add_row(pred_temp_D %>% mutate(prob = p, party = "Democrat"))%>% 
    add_row(pred_temp_R %>% mutate(prob = p, party = "Republican"))
  
}

ggplot() + 
  geom_pointrange(prediction, mapping = aes(x = prob, ymin = response - 1.96*SE, ymax = response + 1.96*SE, y = response, color = party)) +
  facet_wrap(~party)+
  geom_pointrange(full %>% summarise(response = weighted.mean(response,w), lower = weighted.mean(lower, w), upper = weighted.mean(upper, w)),
                  mapping = aes(ymin = lower, ymax = upper, y = response, x = 0.5),show.legend = FALSE)
  






prediction <- tibble("response" = numeric(), "SE" = numeric(), prob = numeric())
for (p in c(seq(-10,10,0.5), -1.301985)){
  out <- c()
  pred_temp <- tibble("response" = numeric(), "SE" = numeric())
  for (t in 1:100){
    reg_temp <- reg3
    reg_temp$coefficients["AbortionImportant"] = p
    pred_temp <- pred_temp %>% add_row(cbind(predict(reg_temp,  type = "response"), CaltechClimate_Nov2022) %>%
                                             summarise(response = weighted.mean(response, weight), SE = weighted.mean(SE, weight))) 
  }
  pred_temp <- pred_temp %>% summarise(response = mean(response), SE = mean(SE))

  prediction <- prediction %>% 
    add_row(pred_temp %>% mutate(prob = p))
  
}

ggplot() + 
  geom_pointrange(prediction, mapping = aes(x = prob, ymin = response - 1.96*SE, ymax = response + 1.96*SE, y = response), color = "grey", alpha = 0.5) +
  geom_pointrange(prediction %>% filter(p == -1.301985 ), mapping = aes(x = prob, ymin = response - 1.96*SE, ymax = response + 1.96*SE, y = response), color = "black") + 
  geom_vline(xintercept =  -1.301985, lty = 2) +
  labs(x = "Coefficient on Abortion being important", y = "Predicted vote share of Republicans")







