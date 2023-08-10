library(readr)
library(tidyverse)
library(ggplot2)




## 

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
  theme(legend.position = "none")

pdf(paste0("Plots/CK_partyID_voting.pdf"), height = 4, width = 4)
plot(g)
dev.off()

##

print("Begin Figure 3")
g <- ggplot(as_tibble(svytable(~ vote.cong +state_type+PartyID, national.design.Climate.nov2022)) %>% 
              group_by(state_type,PartyID) %>% 
              mutate(N = sum(n)) %>%
              filter(PartyID %in%  c("Independent",  "Republican", "Democrat")),
            aes(y = n/N*100, 
                x = state_type,
                fill = vote.cong,
                label = round(n/N*100, digits = 1))) + geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_discrete(type = c("#0A08B7", "#C62D0D" )) + 
  labs(x = "State Type", y = "Percent Voting") + 
  geom_text(position = position_dodge(width = .9), vjust = -0.5) + 
  theme_classic() + 
  theme(legend.position = "none")+
  facet_grid(~PartyID)

pdf(paste0("Plots/partyID_voting_politicalgeography_22.pdf"), height = 4, width = 4)
plot(g)
dev.off()


print("Begin Figure 3")
g <- ggplot(as_tibble(svytable(~ vote.cong+state_type +PartyID, national.design.Climate.nov2020)) %>% 
              group_by(state_type,PartyID) %>% 
              mutate(N = sum(n))%>%
              filter(PartyID %in%  c("Independent",  "Republican", "Democrat")),
            aes(y = n/N*100, 
                x = state_type,
                fill = vote.cong,
                label = round(n/N*100, digits = 1))) + geom_bar(stat = "identity", position = "dodge") + 
  scale_fill_discrete(type = c("#0A08B7", "#C62D0D" )) + 
  labs(x = "State Type", y = "Percent Voting") + 
  geom_text(position = position_dodge(width = .9), vjust = -0.5) + 
  theme_classic() + 
  theme(legend.position = "none")+
  facet_grid(~PartyID)

pdf(paste0("Plots/partyID_voting_politicalgeography_20.pdf"), height = 4, width = 4)
plot(g)
dev.off()
