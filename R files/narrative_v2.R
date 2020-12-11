library(tidyverse)
library(ggplot2)
library(usmap)
library(janitor)
library(usmap)
library(data.table)
library(geofacet) ## map-shaped grid of ggplots
library(statebins)

setwd("~/Desktop/Gov 1347/Blog/R files")

demo_df <- read_csv("../Data/demographic_data_209.csv") 
old_voting_df <- read_csv("../Data/countypres_2000-2016.csv") %>% filter(year == 2016)

voting_df <- read_csv("../Data/county_results_2020.csv") %>% row_to_names(row_number = 1) 

old_voting_wide <- old_voting_df %>% filter(party == "democrat") %>%
  left_join(old_voting_df %>% filter(party == "republican"), 
            by = c("state" =  "state", "FIPS" = "FIPS", "county" = "county")) %>% 
  select(state, FIPS, candidatevotes.x, candidatevotes.y, totalvotes.x) %>%
  mutate(dem_percent = candidatevotes.x / (candidatevotes.x + candidatevotes.y), 
         rep_percent = candidatevotes.y/(candidatevotes.x + candidatevotes.y))
colnames(old_voting_wide) <- c("state", "fips", "dem_votes", "rep_votes", 
                               "total_votes", "dem_percent", "rep_percent")
old_voting_wide$fips <- as.character(old_voting_wide$fips)

voting_df$totalvote <- as.numeric(as.character(voting_df$totalvote))
voting_df$vote1 <- as.numeric(as.character(voting_df$vote1))
voting_df$vote2 <- as.numeric(as.character(voting_df$vote2))

voting_df <- voting_df %>% select(fips, totalvote, vote1, vote2) %>% 
  mutate(dem_percent = vote1/(vote1+vote2), rep_percent = vote2/(vote1+vote2))

demo_df$fips <- as.character(demo_df$fips)
demo_df <- demo_df %>% select(fips, median_age, female_percentage, 
                              population, life_expectancy, pct_none, pct_hs, pct_bachelors, 
                              pct_black, pct_asian, pct_hispanic, pct_non_hispanic_white, 
                              median_household_income, pct_rural, pop_density)

full_df <- voting_df %>% left_join(old_voting_wide, by = c("fips" = "fips")) %>%
  select(fips, dem_percent.x, rep_percent.x, dem_percent.y, rep_percent.y, state)
colnames(full_df) <- c("fips", "dem_2020_share", "rep_2020_share", "dem_2016_share", "rep_2016_share", "state")
full_df <- full_df %>% left_join(demo_df, by = c("fips" = "fips"))

full_df <- full_df %>% mutate(dem_change = dem_2020_share - dem_2016_share, 
                              rep_change = rep_2020_share - rep_2016_share)

ggplot(full_df, aes(x = pop_density, y = dem_change, label = state), alpha = 0.5) +
  geom_point() +
  scale_x_continuous(trans='log10') +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "grey"),
        legend.key = element_rect(fill="transparent", colour=NA),
        legend.position = "none") +
  labs(x = "Log(Population Density) in Log(People/Sq Mile)", 
       y="Change in Democrat Vote Share from 2016 to 2020", 
       title = "Change in Democrat Vote Share by Population Density") +
  geom_smooth(se = FALSE, method = "lm")

ggplot(full_df, aes(x = median_household_income, y = dem_change, label = state), alpha = 0.5) +
  geom_point() +
  scale_x_continuous(trans="log10")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "grey"),
        legend.key = element_rect(fill="transparent", colour=NA),
        legend.position = "none") +
  labs(x = "Log(Median Household Income)", 
       y="Change in Democrat Vote Share from 2016 to 2020", 
       title = "Change in Democrat Vote Share by Income") +
  geom_smooth(se = FALSE, method = "lm")
  
ggplot(full_df, aes(x = pct_rural, y = dem_change, label = state), alpha = 0.5) +
  geom_point() +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "grey"),
        legend.key = element_rect(fill="transparent", colour=NA),
        legend.position = "none") +
  labs(x = "Percent Rural", 
       y="Change in Democrat Vote Share from 2016 to 2020", 
       title = "Change in Democrat Vote Share by Percent Rural") +
  geom_smooth(se = FALSE, method = "lm")

ggplot(full_df, aes(x = median_household_income, y = pop_density, label = state), alpha = 0.5) +
  geom_point() +
  scale_x_continuous(trans="log10")+
  scale_y_continuous(trans="log10")+
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "grey"),
        legend.key = element_rect(fill="transparent", colour=NA),
        legend.position = "none") +
  labs(x = "Log(Median Household Income)", 
       y="Log(Population Density) in Log(People/Sq Mile)", 
       title = "Household Income Against Population Density") +
  geom_smooth(se = FALSE, method = "lm")

pct_rural_lm <- lm(dem_change ~ pct_rural, data = full_df)
pop_density_lm <- lm(dem_change ~ log10(pop_density), data = full_df)
income_lm <- lm(dem_change ~ log10(median_household_income), data = full_df)
summary(pct_rural_lm)

pop_density_lm$coefficients[2]

all_three_lm <- lm(dem_change ~ pct_rural + log10(median_household_income)+log10(pop_density), data = full_df)
summary(all_three_lm)
