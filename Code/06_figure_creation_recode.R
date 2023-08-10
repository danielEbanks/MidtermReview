

## Create final plot inputs

print("Load Plot Data")
HistoricalHouse <- read_csv("Data/HistoricalHouse.csv")


load("Data/CleanedSurveys.RData")     # From 00
load("Data/pooled_regression_recode.RData")  # From 01
load("Data/logits_by_party_recode.RData")    # From 02
load("Data/counterfactual_all_variables_recode.RData")     # From 04

# Data Manip
print("Create Final Plot Inputs")

national.design.Climate.nov2022 = svydesign(data = droplevels(CaltechClimate_Nov2022), weights = ~weight, id = ~1)


temp <- CaltechClimate_Nov2022 %>%
  pivot_longer(!c(vote.cong, weight),
               names_to  = "Cat",
               values_to = "type") %>%
  select(Cat, type)   %>%
  group_by(Cat, type) %>%
  dplyr::summarise(n = n())  %>%
  ungroup()           %>%
  select(Cat, type)   %>%
  unite("name", Cat:type, sep = "", remove= FALSE)

plot.df_comb<-summary(pooled.plot.df)
plot.df_comb <- pooled.plot.df %>% 
  mutate(group = "Pooled") %>% 
  add_row(reg_party_margins_Democrat) %>% 
  add_row(reg_party_margins_Republican) %>% 
  add_row(reg_party_margins_Independent) 

## Figure 1




## Figure 2
print("figure 2 recode")

g <- ggplot(as_tibble(svytable(~ PartyID_recode + vote.cong , national.design.Climate.nov2022)) %>% 
              group_by(PartyID_recode) %>% 
              mutate(N = sum(n)) %>%
              filter(PartyID_recode %in%  c("Independent",  "Republican", "Democrat")),
            aes(y = n/N*100, 
                x = PartyID_recode,
                fill = vote.cong,
                label = round(n/N*100, digits = 1))) + geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_discrete(type = c("#0A08B7", "#C62D0D" )) + 
  labs(x = "Party ID", y = "Percent Voting") + 
  geom_text(position = position_dodge(width = .9), vjust = -0.5) + 
  theme_classic() + 
  ylim(0,120)+
  scale_y_continuous(breaks = c(20,40,60,80,100))+
  guides(fill=guide_legend(title="Congressional Vote"))+
  theme(legend.position = "bottom")


pdf(paste0("Plots/figure2_partyID_recode_voting.pdf"), height = 6, width = 5)
plot(g)
dev.off()




print("Begin Figure 3 Recode")

Financial <- ggplot(data = CaltechClimate_Nov2022%>%filter(PartyID_recode %in% c("Independent","Democrat","Republican")) ) +
  facet_wrap(vars(PartyID_recode), scales = 'free_x') +
  geom_mosaic(aes(x = product(vote.cong, FinancialSituation), fill = vote.cong, weight = weight)) +
  theme_classic() +
  scale_fill_discrete(type = c('#0A08B7', '#C62D0D')) + 
  labs(x = 'Financial Situation', y = '') + 
  theme(legend.position = 'none', axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), axis.text.y = element_blank(), axis.ticks.y = element_blank()) 

plot(Financial)

ggsave('Plots/figure3_Financial_recode.pdf', width = 6.5, height = 2.5, dpi = 1200)

Economic <- ggplot(data =CaltechClimate_Nov2022%>%filter(PartyID_recode %in% c("Independent","Democrat","Republican"))) +
  facet_wrap(vars(PartyID_recode), scales = 'free_x') +
  geom_mosaic(aes(x = product(vote.cong, EconomicSituation), fill = vote.cong, weight = weight)) +
  theme_classic() +
  scale_fill_discrete(type = c('#0A08B7', '#C62D0D')) + 
  labs(x = 'Economic Situation', y = '', fill = 'Congressonal Vote') + 
  theme(legend.position = 'bottom', axis.text.x = element_text(angle = 45, vjust = 1, hjust=1), axis.text.y = element_blank(), axis.ticks.y = element_blank()) 

plot(Economic)

ggsave('Plots/figure3_Economic_recode.pdf', width = 6.5, height = 3.25, dpi = 1200)



## Figure 4 



print("Begin Figure 4 Recode")

t <-as_tibble(svytable(~ PartyID_recode + vote.cong +Immigration +
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
  rename("Foreign Policy"      = ForeignPolicy,
         "Economic Inequality" = EconomicInequality,
         "COVID-19" = COVID19,
         "Violent Crime"=ViolentCrime ,
         "Health Care"= HealthCare ,
         "The Economy"=TheEconomy ,
         "Racial and Ethnic Inequality"=RacialandEthnicInequality ,
         "Climate Change"=ClimateChange ,
         "Gun Policy"=GunPolicy ,
  )%>%
  pivot_longer(cols = !c(PartyID_recode, vote.cong, n), names_to = "Issue", values_to = "Type") %>% 
  group_by(PartyID_recode, vote.cong, Issue, Type) %>% dplyr::summarise(n = sum(n)) %>% 
  ungroup() %>% 
  group_by(PartyID_recode, Issue, Type) %>% mutate(N = sum(n)) %>% 
  pivot_wider(names_from = vote.cong, values_from = n) %>% 
  filter(PartyID_recode %in% c("Democrat", "Republican", "Independent"), 
         Issue != "SupremeCourtAppointments") 



g <- ggplot(t, aes(color =PartyID_recode , y = `Democratic candidate`/N, x = Type, size = N)) + 
  scale_color_discrete(type = c("#0A08B7", "#8008AD","#C62D0D" )) + 
  geom_point() + 
  facet_wrap(~Issue) +
  ylim(-0.25, 1.25) + 
  theme_bw()+
  theme(legend.position = "bottom") +
  labs(y = "Percent vote for Democratic Candidate", x = "",color=" Party ID")+
  guides(size = "none")

pdf(paste0("Plots/figure4_partyID_by_Issue_recode.pdf"), height = 4, width = 8)
plot(g)
dev.off()






print("Begin Figure 7 Recode")


g<-ggplot(data=reg_party_margins_Independent      %>% 
            filter(type == "Important")           %>%
            mutate(Cat = fct_reorder(Cat, (AME))) %>%
            arrange(AME),
          aes(x=AME*100,y=Cat))  +
  geom_pointrange(aes(xmin=lower*100,xmax=upper*100),show.legend = FALSE, color = "#8008AD")+
  geom_vline(xintercept = 0,lty=2)+
  facet_wrap(~type, scale = "free") +
  labs(y = "Policy Area", x = "AME voting Republican") +
  theme_classic() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) 

pdf(paste0("Plots/figure7_importance_Independents_recode.pdf"), height = 4, width = 8)
plot(g)
dev.off()

## Figure 8



print("Begin Figure 8 Recode")
margin.reduced <- margin.reduced %>%
  filter(factor %in% c("AbortionImportant","ViolentCrimeImportant", "ImmigrationImportant"), 
         Party != "Pooled") %>%
   mutate(factor = as.factor(factor)) 
ggplot(data = margin.reduced, 
       mapping = aes(y = factor, 
                     x = AME * 100, 
                     xmin = lower * 100, 
                     xmax = upper * 100,
                     color = as.character(Party), 
                     lty = Year,
                     label = round(AME * 100, digits = 1))) +
  annotate("rect", xmin = -Inf, xmax = Inf, ymin = 2.5, ymax = Inf,
           alpha = .1,fill = "blue")+
  geom_pointrange(position = position_dodge(0.5), size = 0.125) +
  geom_vline(xintercept = 0, linetype = 'dashed') +
  scale_y_discrete(labels = c("Violent Crime", "Immigration", "Abortion"),
                   limits=rev(levels(margin.reduced$factor))) +
  labs(x = "AME (%)", y = "") +
  guides(color=guide_legend(title="Party ID"), linetype = guide_legend(title = "Election Year"))+
  scale_color_discrete(type= c("#0A08B7", "#8008AD", "#C62D0D"))+
  scale_linetype_manual(values=c("twodash", "solid"))+
  theme_classic() +
  theme(legend.position = "bottom", legend.box="vertical",
        legend.margin=margin(),
        axis.text.y = element_text(angle = 90, vjust = 1, hjust=0.5))

ggsave("Plots/figure8_Counterfactual_recode.pdf", height = 6, width = 5.5, dpi = 300)


# ggplot(data = margin.reduced %>%filter(factor=="ViolentCrimeImportant", Party != "Pooled"), 
#        mapping = aes(y = Year, 
#                      x = AME * 100, 
#                      xmin = lower * 100, 
#                      xmax = upper * 100,
#                      color = as.character(Party), 
#                      label = round(AME * 100, digits = 1))) +
#   geom_pointrange(position = position_dodge(0.5), size = 0.125) +
#   geom_vline(xintercept = 0, linetype = 'dashed') +
#   labs(x = "", y = "") +
#   scale_color_discrete(type= c("#0A08B7", "#8008AD","black", "#C62D0D"))+
#   theme_classic() +
#   theme(legend.position = "none")
# 
# ggsave("Plots/figure8_Counterfactual_crime.pdf", height = 2, width = 5, dpi = 300)
# 
# 
# ggplot(data = margin.reduced %>%filter(factor=="ImmigrationImportant", Party != "Pooled"), 
#        mapping = aes(y = Year, 
#                      x = AME * 100, 
#                      xmin = lower * 100, 
#                      xmax = upper * 100,
#                      color = as.character(Party), 
#                      label = round(AME * 100, digits = 1))) +
#   geom_pointrange(position = position_dodge(0.5), size = 0.125) +
#   geom_vline(xintercept = 0, linetype = 'dashed') +
#   labs(x = "", y = "") +
#   scale_color_discrete(type= c("#0A08B7", "#8008AD","black", "#C62D0D"))+
#   theme_classic() +
#   theme(legend.position = "none")
# 
# ggsave("Plots/figure8_Counterfactual_immigration.pdf", height = 2, width = 5, dpi = 300)


