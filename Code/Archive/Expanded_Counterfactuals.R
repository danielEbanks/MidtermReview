library("haven")
library("survey")
library("tidyverse")
library("xgboost")
library('margins')
library('gtsummary')
library('xtable')

Survey_2020 <- read_sav("Data/CALTECH_Post_Election_Survey_2020.sav")

Survey_2020 <- Survey_2020[c('nat20_congvote', 'Q105b', 'age4', 'gender', 'race4', 'educ4', 'region', 'pid3', 'marstat', 'weight')]

Survey_2020 <- Survey_2020[Survey_2020$nat20_congvote %in% 1:2,]

Survey_2020 <- na.omit(Survey_2020)

Survey_2020$Vote.Republican <- ifelse(Survey_2020$nat20_congvote == 2, 1, 0)

Survey_2020$Abortion.Important <- ifelse(Survey_2020$Q105b %in% 1:2, 1, 0) 

Survey_2020$age4 <- as.factor(Survey_2020$age4)

Survey_2020$gender <- as.factor(Survey_2020$gender)

Survey_2020$race4 <- as.factor(Survey_2020$race4)

Survey_2020$educ4 <- as.factor(Survey_2020$educ4)

Survey_2020$pid3 <- as.factor(Survey_2020$pid3)

Survey_2020$region <- as.factor(Survey_2020$region)

Survey_2020$marstat <- as.factor(Survey_2020$marstat)

Survey_2022 <- read_sav("Data/CalTech_November_2022.sav")

Survey_2022 <- Survey_2022[c('Q18', 'Q31_2', 'age4', 'gender3', 'race4', 'educ4', 'region', 'pid3', 'marstat', 'weight')]

Survey_2022 <- na.omit(Survey_2022)

Survey_2022 <- Survey_2022[Survey_2022$Q18 %in% 1:2,]

Survey_2022$Vote.Republican <- ifelse(Survey_2022$Q18 == 2, 1, 0)

Survey_2022$Abortion.Important <- ifelse(Survey_2022$Q31_2 %in% 1:2, 1, 0) 

Survey_2022$age4 <- as.factor(Survey_2022$age4)

Survey_2022$gender3 <- as.factor(Survey_2022$gender3)

Survey_2022$race4 <- as.factor(Survey_2022$race4)

Survey_2022$educ4 <- as.factor(Survey_2022$educ4)

Survey_2022$pid3 <- as.factor(Survey_2022$pid3)

Survey_2022$region <- as.factor(Survey_2022$region)

Survey_2022$marstat <- as.factor(Survey_2022$marstat)


# Vote by Party ID
scale2 <- function(x, na.rm = FALSE) (as.character(as.numeric(x)*100))
out_raw <- as_tibble(tbl_svysummary(data = svydesign(data = droplevels(Survey_2020 %>% 
                                                                select(pid3, Q105b, weight) %>% 
                                                                mutate(across(!weight, as_factor))), 
                                            weights = ~weight,id = ~1)
                           ,
                           by = "Q105b",
                           percent = "row", 
                           label = list(pid3 ~ "Party ID"), 
                           missing = "no",
                           statistic=list(all_continuous() ~ "{mean} ({p25}, {p75})", 
                                          all_categorical() ~ "{p}")))

out <- out_raw[c(2:6),]  %>% mutate("Year" = 2020, "Type" = "mean") 
colnames(out) <- c("PartyID", "Very important", "Somewhat important", "Not too important", "Not important at all", "Year", "Type")

out_raw <- as_tibble(tbl_svysummary(data = svydesign(data = droplevels(Survey_2020 %>% 
                                                                         select(pid3, Q105b, weight) %>% 
                                                                         mutate(across(!weight, as_factor))), 
                                                     weights = ~weight,id = ~1)
                                    ,
                                    by = "Q105b",
                                    percent = "row", 
                                    label = list(pid3 ~ "Party ID"), 
                                    missing = "no",
                                    statistic=list(all_continuous() ~ "{mean} ({p25}, {p75})", 
                                                   all_categorical() ~ "{p.std.error}")))

out_raw <- out_raw[c(2:6),] %>% mutate_all(scale2 ) %>% mutate("Year" = 2020, "Type" = "std_error") 
colnames(out_raw) <- c("PartyID", "Very important", "Somewhat important", "Not too important", "Not important at all", "Year", "Type")

out <- out %>% add_row(out_raw %>% mutate(PartyID = as.character(as.numeric(PartyID)/100)))


out_raw <- as_tibble(tbl_svysummary(data = svydesign(data = droplevels(Survey_2022 %>% 
                                                                  select(pid3, Q31_2, weight) %>% 
                                                                  mutate(across(!weight, as_factor))), 
                                              weights = ~weight,id = ~1)
                             ,
                             by = "Q31_2",
                             percent = "row", 
                             label = list(pid3 ~ "Party ID"), 
                             missing = "no",
                             statistic=list(all_continuous() ~ "{mean} ({p25}, {p75})", 
                                            all_categorical() ~ "{p}")))

out_raw <- out_raw[c(2:6),] %>%  mutate("Year" = 2022, "Type" = "mean") 
colnames(out_raw) <- c("PartyID", "Very important", "Somewhat important", "Not too important", "Not important at all", "Year", "Type")

out <- out %>% add_row(out_raw)

out_raw <- as_tibble(tbl_svysummary(data = svydesign(data = droplevels(Survey_2022 %>% 
                                                                         select(pid3, Q31_2, weight) %>% 
                                                                         mutate(across(!weight, as_factor))), 
                                                     weights = ~weight,id = ~1)
                                    ,
                                    by = "Q31_2",
                                    percent = "row", 
                                    label = list(pid3 ~ "Party ID"), 
                                    missing = "no",
                                    statistic=list(all_continuous() ~ "{mean} ({p25}, {p75})", 
                                                   all_categorical() ~ "{p.std.error}")))

out_raw <- out_raw[c(2:6),] %>% mutate_all(scale2 ) %>% mutate("Year" = 2022, "Type" = "std_error") 
colnames(out_raw) <- c("PartyID", "Very important", "Somewhat important", "Not too important", "Not important at all", "Year", "Type")

out <- out %>%
  add_row(out_raw %>% mutate(PartyID = as.character(as.numeric(PartyID)/100))) 



out_raw <- as_tibble(tbl_svysummary(data = svydesign(data = droplevels(Survey_2020 %>% mutate(pid3 = 10) %>% 
                                                                         select(pid3, Q105b, weight) %>% 
                                                                         mutate(across(!weight, as_factor))), 
                                                     weights = ~weight,id = ~1)
                                    ,
                                    by = "Q105b",
                                    percent = "row", 
                                    label = list(pid3 ~ "Party ID"), 
                                    missing = "no",
                                    statistic=list(all_continuous() ~ "{mean} ({p25}, {p75})", 
                                                   all_categorical() ~ "{p}")))

out_raw <- out_raw[2,]  %>% mutate("Year" = 2020, "Type" = "mean") 
colnames(out_raw) <- c("PartyID", "Very important", "Somewhat important", "Not too important", "Not important at all", "Year", "Type")

out <- out %>% add_row(out_raw)

out_raw <- as_tibble(tbl_svysummary(data = svydesign(data = droplevels(Survey_2020 %>%  mutate(pid3 = 10) %>% 
                                                                         select(pid3, Q105b, weight) %>% 
                                                                         mutate(across(!weight, as_factor))), 
                                                     weights = ~weight,id = ~1)
                                    ,
                                    by = "Q105b",
                                    percent = "row", 
                                    label = list(pid3 ~ "Party ID"), 
                                    missing = "no",
                                    statistic=list(all_continuous() ~ "{mean} ({p25}, {p75})", 
                                                   all_categorical() ~ "{p.std.error}")))

out_raw <- out_raw[2,] %>% mutate_all(scale2 ) %>% mutate("Year" = 2020, "Type" = "std_error") 
colnames(out_raw) <- c("PartyID", "Very important", "Somewhat important", "Not too important", "Not important at all", "Year", "Type")

out <- out %>% add_row(out_raw %>% mutate(PartyID = as.character(as.numeric(PartyID)/100)))


out_raw <- as_tibble(tbl_svysummary(data = svydesign(data = droplevels(Survey_2022 %>%  mutate(pid3 = 10) %>% 
                                                                         select(pid3, Q31_2, weight) %>% 
                                                                         mutate(across(!weight, as_factor))), 
                                                     weights = ~weight,id = ~1)
                                    ,
                                    by = "Q31_2",
                                    percent = "row", 
                                    label = list(pid3 ~ "Party ID"), 
                                    missing = "no",
                                    statistic=list(all_continuous() ~ "{mean} ({p25}, {p75})", 
                                                   all_categorical() ~ "{p}")))

out_raw <- out_raw[2,] %>%  mutate("Year" = 2022, "Type" = "mean") 
colnames(out_raw) <- c("PartyID", "Very important", "Somewhat important", "Not too important", "Not important at all", "Year", "Type")

out <- out %>% add_row(out_raw)

out_raw <- as_tibble(tbl_svysummary(data = svydesign(data = droplevels(Survey_2022 %>%  mutate(pid3 = 10) %>% 
                                                                         select(pid3, Q31_2, weight) %>% 
                                                                         mutate(across(!weight, as_factor))), 
                                                     weights = ~weight,id = ~1)
                                    ,
                                    by = "Q31_2",
                                    percent = "row", 
                                    label = list(pid3 ~ "Party ID"), 
                                    missing = "no",
                                    statistic=list(all_continuous() ~ "{mean} ({p25}, {p75})", 
                                                   all_categorical() ~ "{p.std.error}")))

out_raw <- out_raw[2,] %>% mutate_all(scale2 ) %>% mutate("Year" = 2022, "Type" = "std_error") 
colnames(out_raw) <- c("PartyID", "Very important", "Somewhat important", "Not too important", "Not important at all", "Year", "Type")

out <- out %>%
  add_row(out_raw %>% mutate(PartyID = as.character(as.numeric(PartyID)/100))) 


out <- out %>% 
  pivot_wider(names_from = Type, values_from = c(`Very important`, `Somewhat important`, `Not too important`, `Not important at all`)) %>% 
  arrange(PartyID)

print(xtable(out), include.rownames = FALSE)

for (i in 1:4){
################################################################################

if (i == 4){
  data_2020 <- droplevels(Survey_2020)
  data_2022 <- droplevels(Survey_2022)
} else {
  data_2020 <- droplevels(Survey_2020[Survey_2020$pid3 == i,])
  data_2022 <- droplevels(Survey_2022[Survey_2022$pid3 == i,])
}


design_2020 <- svydesign(ids = ~ 0, data = data_2020, weights = data_2020$weight)

regression_2020 <- svyglm(Vote.Republican ~ Abortion.Important + age4 + gender + race4 + educ4 + region + marstat, design = design_2020, family = binomial)

################################################################################

################################################################################



design_2022 <- svydesign(ids = ~ 0, data = data_2022, weights = data_2022$weight)

regression_2022 <- svyglm(Vote.Republican ~ Abortion.Important + age4 + gender3 + race4 + educ4 + region + marstat, design = design_2022, family = binomial)

################################################################################

margins_2020 <- summary(margins(regression_2020, design = design_2020))[1, c('AME', 'lower', 'upper')]

margins_2022 <- summary(margins(regression_2022, design = design_2022))[1, c('AME', 'lower', 'upper')]

if (i == 1){
  margins <- as.data.frame(rbind(c("Party" = i, "Year" = 2020, margins_2020), c("Party" = i, "Year" = 2022, margins_2022)))
} else {
  margins <- margins %>% add_row(as.data.frame(rbind(c("Party" = i,"Year" = 2020, margins_2020), c("Party" = i,"Year" = 2022, margins_2022))))
}
}

margins$Year <- as.factor(as.numeric(margins$Year))

margins$AME <- as.numeric(margins$AME)

margins$lower <- as.numeric(margins$lower)

margins$upper <- as.numeric(margins$upper)


ggplot(data = margins, 
       mapping = aes(y = Year, 
                     x = AME * 100, 
                     xmin = lower * 100, 
                     xmax = upper * 100,
                     color = as.character(Party), 
                     label = round(AME * 100, digits = 1))) +
  geom_pointrange(position = position_dodge(0.5), size = 0.125) +
  geom_vline(xintercept = 0, linetype = 'dashed') +
  labs(x = "AME (%)") +
  scale_color_discrete(type= c("#0A08B7", "#C62D0D", "#8008AD","black"))+
  theme_classic() +
  theme(legend.position = "none")

ggsave("~/Dropbox/ClimateSurvey/Plots/Counterfactual.pdf", height = 2, width = 5, dpi = 300)








