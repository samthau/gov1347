## install via `install.packages("name")`
library(tidyverse)
library(ggplot2)
library(usmap)

## set working directory here
setwd("~/Desktop/Gov 1347/Post1")

## read in state pop vote
pvstate_df <- read_csv("popvote_bystate_1948-2016.csv")
pvstate_df$full <- pvstate_df$state

## shapefile of states from `usmap` library
## note: `usmap` merges this internally, but other packages may not!
states_map <- usmap::us_map()
unique(states_map$abbr)



## map: win-margins
pv_margins_map <- pvstate_df %>%
  filter(year >= 1972) %>%
  mutate(win_margin = (R_pv2p-D_pv2p))

## Find max vote margins for both parties
pv_margins_map[which.max(pv_margins_map$win_margin),]
pv_margins_map[which.min(pv_margins_map$win_margin),]

## map grid of vote margin
plot_usmap(data = pv_margins_map, regions = "states", values = "win_margin") +
  facet_wrap(facets = year ~.) + ## specify a grid by year
  scale_fill_gradient2(
    high = "red", 
    mid = "white",
    low = "blue", 
    breaks = c(-75,-50,-25,0,25,50,75), 
    limits = c(-75,75),
    name = "win margin"
  ) +
  theme_void() +
  theme(strip.text = element_text(size = 12),
        aspect.ratio=1)


## map grid of wins
pv_map_grid <- pvstate_df %>%
  filter(year >= 1976) %>%
  mutate(winner = ifelse(R > D, "republican", "democrat"))

plot_usmap(data = pv_map_grid, regions = "states", values = "winner", color = "white") +
  facet_wrap(facets = year ~.) + ## specify a grid by year
  scale_fill_manual(values = c("blue", "red"), name = "PV winner") +
  theme_void() +
  theme(strip.text = element_text(size = 12),
        aspect.ratio=1)


## tipping point states
#find missing state level data
missing_states <- pvstate_df[rowSums(is.na(pvstate_df)) > 0,]
missing_states

#read in electoral college data,  clean
ec_values <- read.csv("ElectoralCollegePost1948_adj.csv")
ec_values <- ec_values[colSums(!is.na(ec_values)) > 0]
ec_values <- na.omit(ec_values)
names(ec_values)[names(ec_values) == "State"] <- "state"
names(ec_values)[names(ec_values) == "Year"] <- "year"

# clean popular vote by state data
pvstate_df[is.na(pvstate_df)] <- 0
pvstate_df <- pvstate_df %>%
  mutate(winner = ifelse(R > D, "republican", "democrat")) %>%
  mutate(win_margin = (R_pv2p-D_pv2p))

# A few rows are lost from 2020 entries and 
# entries from states that didn't exist at the time of the election
tipping_df <- pvstate_df %>% left_join(ec_values, by=c("state","year"))

#tipping point calculations
year_list <- c(1948,1952,1956,1960,1964,1968,1972,1976,1980,1984,1988,1992,1996,2000,2004,2008,2012,2016)
state_list <- seq(from=1, to=length(year_list), by=1)
threshold_list <- aggregate(tipping_df$EC, by=list(Category=tipping_df$year), FUN=sum) /2 +1

for(i in seq(from=1, to=length(year_list),by=1)){
  
  temp_year <- year_list[i]
  threshold <- threshold_list[i,2]
  
  dem_wins <- tipping_df %>% filter(winner == 'democrat') %>% filter(year == temp_year)
  dem_wins <- arrange(dem_wins, win_margin)
  dem_wins <- dem_wins %>% mutate(cumsum = cumsum(EC))
  rep_wins <- tipping_df %>% filter(winner == 'republican') %>% filter(year == temp_year)
  rep_wins <- arrange(rep_wins, -win_margin)
  rep_wins <- rep_wins %>% mutate(cumsum = cumsum(EC))
  
  if (dem_wins[nrow(dem_wins),ncol(dem_wins)]>rep_wins[nrow(rep_wins),ncol(rep_wins)]){
    past_tip <- dem_wins %>% filter(cumsum >= threshold)
  } else {
    past_tip <- rep_wins %>% filter(cumsum >= threshold)
  }
  
  out_tip <- past_tip[1,]
  state_list[i] <- out_tip$state
}


display <- as.data.frame(cbind(state_list,year_list))
names(display)[names(display) == "state_list"] <- "Tipping Point State"
names(display)[names(display) == "year_list"] <- "Election"
display

