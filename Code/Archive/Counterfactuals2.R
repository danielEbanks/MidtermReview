library("haven")
library("survey")
library("tidyverse")
library("xgboost")
library('margins')
library(gtsummary)

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


################################################################################

Survey_2020 <- Survey_2020[Survey_2020$pid3 == 3,]

design_2020 <- svydesign(ids = ~ 0, data = Survey_2020, weights = Survey_2020$weight)

regression_2020 <- svyglm(Vote.Republican ~ Abortion.Important + age4 + gender + race4 + educ4 + region + marstat, design = design_2020, family = binomial)

################################################################################

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


################################################################################

Survey_2022 <- Survey_2022[Survey_2022$pid3 == 3,]

design_2022 <- svydesign(ids = ~ 0, data = Survey_2022, weights = Survey_2022$weight)

regression_2022 <- svyglm(Vote.Republican ~ Abortion.Important + age4 + gender3 + race4 + educ4 + region + marstat, design = design_2022, family = binomial)

################################################################################

margins_2020 <- summary(margins(regression_2020, design = design_2020))[1, c('AME', 'lower', 'upper')]

margins_2022 <- summary(margins(regression_2022, design = design_2022))[1, c('AME', 'lower', 'upper')]

margins <- as.data.frame(rbind(c("Year" = 2020, margins_2020), c("Year" = 2022, margins_2022)))

margins$Year <- as.factor(as.numeric(margins$Year))

margins$AME <- as.numeric(margins$AME)

margins$lower <- as.numeric(margins$lower)

margins$upper <- as.numeric(margins$upper)

ggplot(data = margins, mapping = aes(y = Year, x = AME * 100, xmin = lower * 100, xmax = upper * 100, label = round(AME * 100, digits = 1))) +
  geom_point() +
  geom_text(vjust = -1) +
  geom_pointrange() +
  geom_vline(xintercept = 0, linetype = 'dashed') +
  labs(x = "AME (%)") +
  theme_classic()

ggsave("~/Dropbox/ClimateSurvey/Plots/Counterfactual.pdf", height = 2.5, width = 6, dpi = 300)




