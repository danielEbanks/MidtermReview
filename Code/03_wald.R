pooled_party_interaction_reg   <- svyglm(formula = vote.cong~PartyID + 

                                 #FollowGoverment*PartyID +
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




var.names <- gsub("Important","",names(coef(pooled_party_interaction_reg)[seq(204,254,3)]))
var.names <- gsub("^[^_]*:", "",var.names)
var.names <- gsub("EconomicS","Economic S",var.names)
var.names <- gsub("tionG","tion: G",var.names)
var.names <- gsub("tionB","tion: B",var.names)
var.names <- gsub("tionW","tion: W",var.names)
var.names <- gsub("cialS","cial S",var.names)

idx<-1
wald.test.statistic <-list()
for(i in seq(204,254,3)){
wald.test.statistic[[idx]] <- data.frame(wald.statistic=wald.test(Sigma = vcov(pooled_party_interaction_reg), 
          b     = coef(pooled_party_interaction_reg), 
          Terms = i:(i+2))$result$chi2[1],Variable = var.names[idx])
idx<-1+idx
}


wald.test.statistic<-rbindlist(wald.test.statistic)
save(pooled_party_interaction_reg,wald.test.statistic,file="Data/wald.test.RData")

