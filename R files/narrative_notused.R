library(tidyverse)
library(ggplot2)
library(usmap)
library(janitor)
library(usmap)
library(data.table)
library(geofacet) ## map-shaped grid of ggplots
library(statebins)

setwd("~/Desktop/Gov 1347/Blog/R files")


actual_results <- read_csv("../Data/popvote_bystate_1948-2020.csv")
results_2020 <- actual_results %>% filter(year == 2020)
results_2020$state <- c(state.abb, 'DC')[match(results_2020$state, c(state.name, 'District of Columbia'))]
results_2020 <- results_2020 %>% mutate(biden_pv = D/total, 
                                        trump_pv = R/total, 
                                        winner =case_when(biden_pv > trump_pv ~ "Biden",
                                                          trump_pv > biden_pv ~ "Trump",
                                                          biden_pv == trump_pv ~ "Tie"))

senate_results <- read_csv("../NarrativeData/senate_county_candidate.csv")
pres_results <- read_csv("../NarrativeData/president_county_candidate.csv")
pres_votes <- read_csv("../NarrativeData/president_county.csv")
senate_votes <- read_csv("../NarrativeData/senate_county.csv")

pick_cand <- senate_results %>% filter(party == "DEM") %>%
  group_by(state, candidate, county) %>%
  summarise_at(vars(total_votes), funs(mean)) %>% 
  ungroup() %>%
  group_by(state, candidate) %>%
  summarise_at(vars(total_votes), funs(sum)) %>%
  filter(total_votes > 50000)

cand_list <- unique(pick_cand$candidate)
  

senate_df <- senate_results %>% filter(party == "DEM", candidate %in% cand_list) %>% 
  left_join(senate_votes, by =c("state" = "state", "county" = "county")) %>% 
  mutate(sen_vote_share = total_votes.x / current_votes)  %>% drop_na()%>% 
  group_by(state, county, candidate) %>%
  summarise_at(vars(sen_vote_share), funs(mean(., na.rm=TRUE))) 

pres_df <- pres_results %>% filter(party == "DEM") %>% 
  left_join(pres_votes, by = c("state" = "state", "county" = "county")) %>%
  mutate(pres_vote_share = total_votes.x / current_votes) %>% filter(percent > 95) %>%
  distinct(county, state, .keep_all = TRUE)

comp_senate_df <- senate_df %>% select(county, state, sen_vote_share) %>%
  left_join(pres_df %>% select(county, state, pres_vote_share), 
             by = c("state" = "state", "county" = "county"))

sen_county <- lm(sen_vote_share ~ pres_vote_share, data = comp_senate_df)
summary(sen_county)

plot_df <- comp_senate_df %>% 
  filter(state != "Delaware") %>% 
  filter(state != "Massachusetts") %>%
  drop_na()

ggplot(plot_df , aes  (x = pres_vote_share, y = sen_vote_share, label = state)) + 
  geom_point() + 
  facet_wrap(facets = state ~.) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "grey"),
        legend.key = element_rect(fill="transparent", colour=NA),
        legend.position = "none") + 
  labs(x = "Biden Presidential Vote Share", 
       y="Dempcrat Senate Vote Share", 
       title = "Presidential vs Senate Democratic Vote Share by County")




