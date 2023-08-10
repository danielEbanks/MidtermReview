

## Recode

print("Recoded Attempt.")
pooled.reg     <- svyglm(formula = vote.cong~PartyID_recode + 
                           #FollowGoverment +
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

#Calculate AMEs for pooled logit
print("Calculate AMEs")
pooled.reg.m   <- margins(pooled.reg,design=national.design.Climate.nov2022)

#Save AMEs of pooled logit in data frame for plotting
pooled.plot.df    <- summary(pooled.reg.m) 

temp <- CaltechClimate_Nov2022 %>%
  pivot_longer(!c(vote.cong, weight),
               names_to  = "Cat",
               values_to = "type") %>%
  select(Cat, type)   %>%
  group_by(Cat, type) %>%
  summarise(n = n())  %>%
  ungroup()           %>%
  select(Cat, type)   %>%
  unite("name", Cat:type, sep = "", remove= FALSE)

pooled.plot.df <- as_tibble(pooled.plot.df)            %>% 
  left_join(temp, by = c("factor" = "name"))         %>% 
  separate(Cat, c("Cat_Ability", NA),remove = FALSE) %>% 
  mutate(lower = AME - 1.96*SE, upper = AME + 1.96*SE)


save(pooled.plot.df,pooled.reg,file="Data/pooled_regression_recode.RData")
