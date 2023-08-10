# Load Data for Tables

load("Data/wald.test.RData")
load("Data/counterfactual.RData")






#Abortion Comparison Tables
party_grid <- c("Independent","Democrat","Republican","Pooled")




# Wald Test
wald.test.statistic <- wald.test.statistic %>%arrange(Variable)
print("Create Table E.13")
print(xtable(wald.test.statistic[,c(2,1)],
       caption = "We report Wald statistics testing whether party-level interactions are jointly 0 in a pooled model. Large test statistics suggest we can reject the null that coefficients desegregated in the pooled are jointly 0. We instead report the  Average Marginal Effects from logit specifications dis-aggregated by party ID",
       label="tab: waldtests",
       align=c("l","l","c")),
       include.rownames=FALSE,file="Tables/Table_Appendix_Wald.tex")
