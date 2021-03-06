# import libraries
library(tidyverse)
library(ggplot2)
library(usmap)
library(janitor)
library(usmap)
library(data.table)
library(geofacet) ## map-shaped grid of ggplots
library(betareg)


# set wd
setwd("~/Desktop/Gov 1347/Blog/R files")

# read in all data
nat_popvote_df <- read_csv("../Data/popvote_1948-2016.csv")
nat_poll_df    <- read_csv("../Data/pollavg_1968-2016.csv")

state_popvote_df <- read_csv("../Data/popvote_bystate_1948-2016.csv")
state_poll_df <- read_csv("../Data/pollavg_bystate_1968-2016.csv")

nat_econ <- read_csv("../Data/econ.csv")
state_rdi <- read_csv("../Data/state_rdi.csv")

vep_df <- read_csv("../Data/vep_1980-2016.csv")

# clean data
#build state df with econ, polls, and 
weeks <- 4
poll_df <- state_poll_df %>% filter(weeks_left >= weeks, weeks_left<25) %>% 
  select(year, state, party, candidate_name, avg_poll) %>% 
  group_by(year, state, candidate_name, party) %>%
  summarize(avg_poll = mean(avg_poll))

#build economic df at state level from rdi and national gdp
econ_df <- state_rdi[,1:2]
colnames(econ_df) <- c("state","rdi_q2")
econ_df$rdi_q2 <- as.numeric(econ_df$rdi_q2)

for(i in seq(from = 18, to = 290, by = 16)){
  temp <- state_rdi[,c(1,i)]
  colnames(temp) <- c("state","rdi_q2")
  temp$rdi_q2 <- as.numeric(temp$rdi_q2)
  econ_df <- rbind(econ_df, temp)
}

#add years and national gdp growth
year_list <- seq(from = 1948, to = 2020, by=4)
year_append <- rep(year_list[1],51)
nat_econ_sub <- nat_econ %>% filter(year %in% year_list, quarter == 2) %>% 
  select(year, GDP_growth_qt)
gdp_append <- as.numeric(rep(nat_econ_sub[1,2],51))

for(i in seq(from = 2, to=length(year_list), by =1)){
  new_year = rep(year_list[i], 51)
  new_gdp = as.numeric(rep(nat_econ_sub[i,2], 51))
  year_append = append(year_append, new_year)
  gdp_append = append(gdp_append, new_gdp)
}

#append year and national gdp to the econ df
econ_df$year <- year_append
econ_df$gdp <- gdp_append


## Merge datasets and clean
inc_data <- nat_popvote_df %>% select("year","party","winner", "incumbent","incumbent_party")

reg_df <- poll_df %>% left_join(econ_df, by=c("state"="state","year"="year")) %>% 
  left_join(state_popvote_df, by= c("state"="state", "year"="year")) %>%
  left_join(inc_data, 
            by  = c("year"="year","party" = "party" ))

reg_df$period <- ifelse(reg_df$year %in% 1952:1972, "1952-1976",
                        ifelse(reg_df$year %in% 1972:1992, "1972-1992", "1996-2020"))

reg_df <- reg_df %>% mutate("d_pv"=100*D/total) %>% mutate("r_pv"=100*R/total)
reg_df <- reg_df %>% 
  mutate(inc_party = case_when((party=="democrat" & incumbent_party==TRUE) ~ "democrat", 
                               (party=="democrat" & incumbent_party==FALSE) ~ "republican",
                               (party=="republican" & incumbent_party==TRUE) ~ "republican",
                               (party=="republican" & incumbent_party==FALSE) ~ "democrat")) %>%
  mutate(chl_party = case_when((party=="democrat" & incumbent_party==FALSE) ~ "democrat", 
                               (party=="democrat" & incumbent_party==TRUE) ~ "republican",
                               (party=="republican" & incumbent_party==FALSE) ~ "republican",
                               (party=="republican" & incumbent_party==TRUE) ~ "democrat"))

reg_df <- reg_df %>% 
  mutate(inc_pv = case_when((inc_party=="republican") ~ R, 
                            (inc_party=="democrat") ~ D)) %>% 
  mutate(chl_pv = case_when(chl_party == "democrat" ~ D, 
                            chl_party == "republican" ~ R))

reg_df <- reg_df %>% mutate(incumbent_win = case_when(inc_party == "republican" & r_pv > d_pv ~ 1,
                                                      inc_party == "republican" & r_pv < d_pv ~ 0, 
                                                      inc_party == "democrat" & r_pv < d_pv ~ 1,
                                                      inc_party == "democrat" & r_pv > d_pv ~ 0))

reg_df <- reg_df %>% left_join(vep_df, by = c("state" = "state", "year" = "year"))

reg_df <- reg_df %>% filter(year > 1979)

reg_df <- na.omit(reg_df)


## logit regression for probability
inc_log <- glm(cbind(inc_pv, VEP-inc_pv) ~ rdi_q2 + gdp + state + avg_poll, 
               data = reg_df %>% filter(year < 2020, incumbent_party == TRUE), family=binomial)

chl_log <- glm(cbind(chl_pv, VEP-chl_pv) ~ rdi_q2 + gdp + state + avg_poll + period, 
               data = reg_df %>% filter(year < 2020, incumbent_party == FALSE), family=binomial)
summary(inc_log)
summary(chl_log)

# predictions for 2020
## 2020 predictions
poll_2020_df <- read_csv("../Data/presidential_poll_averages_2020.csv")
elec_col <- read_csv("../Data/ElectoralCollegePost1948_adj.csv") %>% filter(Year==2020)
remove_empty(elec_col, which = c("cols"), quiet = TRUE)


elxnday_2020 <- as.Date("11/3/2020", "%m/%d/%Y")
dnc_2020 <- as.Date("8/20/2020", "%m/%d/%Y")
rnc_2020 <- as.Date("8/27/2020", "%m/%d/%Y")

colnames(poll_2020_df) <- c("year","state","poll_date","candidate_name","avg_support","avg_support_adj")

poll_2020_df <- poll_2020_df %>%
  mutate(party = case_when(candidate_name == "Donald Trump" ~ "republican",
                           candidate_name == "Joseph R. Biden Jr." ~ "democrat"),
         poll_date = as.Date(poll_date, "%m/%d/%Y")) %>%
  mutate(days_left = round(difftime(elxnday_2020, poll_date, unit="days")),
         weeks_left = round(difftime(elxnday_2020, poll_date, unit="weeks")),
         before_convention = case_when(poll_date < dnc_2020 & party == "democrat" ~ TRUE,
                                       poll_date < rnc_2020 & party == "republican" ~ TRUE,
                                       TRUE ~ FALSE)) %>% filter(!is.na(party)) 

poll_2020_clean <- poll_2020_df %>% filter(weeks_left < 25, weeks_left >=weeks) %>% 
  group_by(state, candidate_name, party) %>%
  summarize(avg_poll = mean(avg_support)) %>% filter(state != "National") %>% 
  select(state, candidate_name, avg_poll, party) %>% mutate(period = "1996-2020") %>%
  mutate(incumbent_party = case_when(candidate_name == "Joseph R. Biden Jr." ~ FALSE, 
                                     candidate_name == "Donald Trump" ~ TRUE)) %>%
  filter(state != "NE-1", state != "NE-2", state != "ME-1", state != "ME-2")

gdp <- rep(-0.0949, 102)
poll_2020_clean$gdp <- gdp
poll_2020_clean$avg_poll <- as.numeric(poll_2020_clean$avg_poll)

pred_data_df <- poll_2020_clean %>% left_join(state_rdi[,c(1,290)], c("state" = "State"))
colnames(pred_data_df) <- c("state", "candidate_name","avg_poll","party", "period", "incumbent_party","gdp", "rdi_q2")

pred_data_df <- pred_data_df %>% left_join(vep_df %>% filter(year == 2016), by = c("state"="state"))

# make state predictions and label them
trump_df <- pred_data_df %>% filter(incumbent_party == TRUE)
biden_df <- pred_data_df %>% filter(incumbent_party == FALSE)

pred_prob_trump <- as.data.frame(predict(inc_log, newdata = trump_df, type = "response"))
pred_prob_trump$state <- trump_df$state
pred_prob_trump <- as.data.frame(pred_prob_trump)

pred_prob_biden <- as.data.frame(predict(chl_log, newdata = biden_df, type = "response"))
pred_prob_biden$state <- biden_df$state
pred_prob_biden <- as.data.frame(pred_prob_biden)

sim_df <- pred_prob_trump %>% left_join(pred_prob_biden, by = c("state"= "state")) %>% 
  left_join(elec_col[ , colSums(is.na(elec_col)) == 0], by = c("state" = "State")) %>% left_join(vep_df %>% filter(year == 2016),
                                                               by = c("state"="state"))

colnames(sim_df) <- c("trump_p", "state", "biden_p", "year", "EC", "temp", "VEP", "VAP")
sim_df <- sim_df %>% select(state, trump_p, biden_p, EC, VEP)


results_df <- data.frame(state = character(0), trump_v = numeric(0), 
                           biden_v = numeric(0), EC = numeric(0)  , sim = numeric(0))

count = 1000
for(st in sim_df$state){
    temp_df <- sim_df %>% filter(state == st)
    trump_vote <- rbinom(n=count, size = ceiling(temp_df$VEP), prob = temp_df$trump_p)
    biden_vote <- rbinom(n=count, size = ceiling(temp_df$VEP), prob = temp_df$biden_p)

    res_st <- cbind.data.frame(st, trump_vote, biden_vote, rep(temp_df$EC, count), seq(from = 1, to = count, by = 1))
    colnames(res_st) = c("state", "trump_v", "biden_v", "EC","sim")

    results_df <- rbind(results_df, res_st)
}
  
results_df <- results_df %>% mutate(win = case_when(trump_v > biden_v ~ "Trump",
                                                      trump_v < biden_v ~ "Biden",
                                                      trump_v == biden_v ~ "Tie"))

ec_results <- aggregate(results_df$EC, results_df %>% select(win, sim), FUN = sum)
biden_votes <- aggregate(results_df$biden_v, results_df %>% select(sim), FUN = sum)
trump_votes <- aggregate(results_df$trump_v, results_df %>% select(sim), FUN = sum)

find_tip <- function(df){
  dem_wins <- df %>% filter(win == 'Biden') 
  dem_wins <- arrange(dem_wins, biden_v - trump_v)
  dem_wins <- dem_wins %>% mutate(cumsum = cumsum(EC))
  rep_wins <- df %>% filter(win == 'Trump') 
  rep_wins <- arrange(rep_wins, trump_v - biden_v)
  rep_wins <- rep_wins %>% mutate(cumsum = cumsum(EC))
  
  if (dem_wins[nrow(dem_wins),ncol(dem_wins)]>rep_wins[nrow(rep_wins),ncol(rep_wins)]){
    past_tip <- dem_wins %>% filter(cumsum >= 270)
  } else {
    past_tip <- rep_wins %>% filter(cumsum >= 270)
  }
  
  out_tip <- past_tip[1,]
  tip <- out_tip$state
  
  return(tip)
}

tipping_point <- results_df %>% 
  group_by(sim) %>%
  do(data.frame(val=find_tip(.)))


results_df <- results_df %>% mutate(margin = trump_v - biden_v)

ggplot(results_df) +
  geom_histogram(aes(margin), bins = 10) +
  facet_geo(~ state,  label = "name", scales = "free") + 
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) + 
  scale_x_continuous(labels = function(x) format(x, scientific = TRUE)) + 
  xlab("Vote Margin") + ylab("Frequency")


df <- gather(results_df %>% filter(state == "Pennsylvania"), 
             key = Vote, value = sim, c("trump_v", "biden_v"))



