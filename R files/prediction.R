library(tidyverse)
library(ggplot2)
library(usmap)
library(janitor)
library(usmap)
library(data.table)
library(geofacet) ## map-shaped grid of ggplots
library(statebins)
library(reshape2)
library(apcluster)
library(MASS)
library(stabledist)
library(lubridate)




#set wd
setwd("~/Desktop/Gov 1347/Blog/R files")

# read in all data
nat_popvote_df <- read_csv("../Data/popvote_1948-2016.csv")
nat_poll_df    <- read_csv("../Data/pollavg_1968-2016.csv")

state_popvote_df <- read_csv("../Data/popvote_bystate_1948-2016.csv") %>%
  mutate(D_pv = D/total, R_pv = R/total, vote_margin = abs(D_pv - R_pv))

state_poll_df <- read_csv("../Data/pollavg_bystate_1968-2016.csv") %>% 
  filter(days_left <= 21) %>%
  group_by(year, state, candidate_name, party) %>%
  dplyr::summarize(avg_poll = mean(avg_poll)/100) %>% 
  dplyr::select(year, state, party, avg_poll)

state_poll_df$state <- c(state.abb, 'DC')[match(state_poll_df$state, c(state.name, 'District of Columbia'))]
state_popvote_df$state <- c(state.abb, 'DC')[match(state_popvote_df$state, c(state.name, 'District of Columbia'))]

nat_econ <- read_csv("../Data/econ.csv")
state_rdi <- read_csv("../Data/state_rdi.csv")
vep_df <- read_csv("../Data/vep_1980-2016.csv")
vep_df$state <- c(state.abb, 'DC')[match(vep_df$state, c(state.name, 'District of Columbia'))]


demo <- read_csv("../Data/demographic_1990-2018.csv") %>% dplyr::select(-total)

cost <- read.delim("../Data/covi_1996-2016.txt", header = TRUE, sep = "|", stringsAsFactors = FALSE)
colnames(cost) <- c("state", "1996", "2000", "2004", "2008", "2012", "2016")
cost_df <- melt(cost, 
                id.vars = c("state"), 
                value.name = "year")
colnames(cost_df) <- c("state", "year", "cost")
dc_cost <- cbind(rep('DC', 6), seq(from = 1996, to = 2016, by = 4), rep(0,6))
colnames(dc_cost) <- c("state", "year", "cost")
cost_df <- rbind(cost_df, dc_cost)
cost_df$year <- as.numeric(as.character(cost_df$year))

wide_polls <- state_poll_df %>% filter(party == "republican") %>% 
  left_join(state_poll_df %>% filter(party == "democrat"), by = c("state", "year"))

colnames(wide_polls) <- c("r_name","year", "state", "rep", "rep_poll","d_name", "dem", "dem_poll")

wide_polls <- wide_polls %>% mutate(poll_margin = abs(rep_poll - dem_poll)) %>%
  dplyr::select("state", "year", "poll_margin")

turnout_votes <- state_popvote_df %>% dplyr::select("state", "year", "total", "vote_margin") %>%
  left_join(wide_polls %>% dplyr::select("state", "year", "poll_margin"))

turnout_df <- turnout_votes %>%
  left_join(demo, by = c("state" = "state", "year" = "year")) %>%
  left_join(cost_df, by = c("state" = "state", "year" = "year")) %>%
  left_join(vep_df, by = c("state" = "state", "year" = "year"))

## Load 2020 polling data
poll_2020_df <- read_csv("../Data/presidential_poll_averages_2020.csv")

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

poll_2020_clean <- poll_2020_df %>% filter(days_left <= 21) %>% 
  group_by(state, candidate_name, party) %>%
  summarize(avg_poll = mean(avg_support_adj)/100) %>% filter(state != "National") %>% 
  dplyr::select(state, candidate_name, avg_poll, party) %>%
  filter(state != "NE-1", state != "NE-2", state != "ME-1", state != "ME-2")

wide_poll_2020 <- poll_2020_clean %>% filter(party == "democrat") %>% 
  left_join(poll_2020_clean %>% filter(party == "republican"), 
            by = c("state" = "state"))
colnames(wide_poll_2020) <- c("state", "biden", "dem_poll", "dem", "trump", "rep_poll", "rep")  
wide_poll_2020$state <- c(state.abb, 'DC')[match(wide_poll_2020$state, c(state.name, 'District of Columbia'))]
poll_margin_2020 <- wide_poll_2020 %>% mutate(poll_margin = rep_poll - dem_poll) %>% 
  dplyr::select(state, poll_margin)

turnout_2020 <- poll_margin_2020 %>% 
  left_join(demo %>% filter(year == 2018), by = c("state"= "state")) %>% 
  dplyr::select(poll_margin, Asian, Black, Hispanic, Indigenous,
         White, Female, Male, age20, age3045, age4565, age65) %>% 
  add_column(vote_margin = NA) %>% add_column(cost = NA) %>% add_column(total = NA) %>% 
  add_column(VEP = NA) %>% add_column(VAP = NA)
turnout_2020$year <- rep(2020, 51)


## scale predictors
num_pred <- c("vote_margin", "Asian", "Black", "Hispanic", "Indigenous", "White", 
              "Female", "Male", "age20", "age3045", "age4565", "age65",
              "last_vote_margin", "last_turnout")

turnout_scaled <- rbind(turnout_df, turnout_2020)
turnout_scaled <- turnout_scaled %>% filter(year > 1984) %>% 
  group_by(state) %>%
  mutate(last_vote_margin = dplyr::lag(vote_margin, k = 1, default = NA)) %>%
  mutate(last_turnout = dplyr::lag(total, k = 1, default = NA)) %>%
  ungroup() %>%
  dplyr::select(state,VEP,total, year, poll_margin, num_pred) %>% group_by(year) %>% mutate_at(num_pred, scale) %>% 
  ungroup() %>% mutate_at(vars(VEP), funs(round(.))) %>% ungroup() %>% filter(year > 1990)


##turnout model
turnout_model <- glm(total ~ last_vote_margin + poll_margin +Black + Hispanic + 
                       Asian + White + Male + age20 + age3045 + age4565 +
                       last_turnout + state, 
                     data = turnout_scaled %>% filter(year < 2020), family = "poisson")
summary(turnout_model)

validate_turnout <- function(yr){
  temp_df <- turnout_scaled %>% filter(year != yr, year < 2020)
  
  if(yr != 2016){
    pred_df <- turnout_scaled %>% filter(year == yr)
  }else{
    pred_df <- turnout_scaled %>% filter(year == yr, state != "DC")
  }
  
  temp_model <- glm(total ~ last_vote_margin + poll_margin + Black + Hispanic + 
                         Asian + White + Male + age20 + age3045 + age4565 +
                         last_turnout + last_vote_margin + state, data = temp_df, family = "poisson")
  predicted_turnout_pct <- predict(temp_model, pred_df, type = "response")
  predicted_turnout <- predicted_turnout_pct * pred_df$VEP

  err <- sum((predicted_turnout - pred_df$total)**2)
  err
}

year_list <- seq(from = 1992, to = 2016, by = 4)
turnout_error <- sapply(year_list, validate_turnout)

## predict 2020 turnout and get variances
turnout_pred_df <- turnout_scaled %>% filter(year == 2020)
turnout_2020_pred <- as.data.frame(predict(turnout_model, turnout_pred_df, type = "response"))
turnout_2020_pred$state <- turnout_pred_df$state
colnames(turnout_2020_pred) <- c("turnout", "state")


## build econ data and attatch it to the turnout dataframe
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
  dplyr::select(year, GDP_growth_qt)
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
econ_df$state <- c(state.abb, 'DC')[match(econ_df$state, c(state.name, 'District of Columbia'))]


## Merge datasets and clean
poll_data <- nat_popvote_df %>% dplyr::select("year","party","winner", "incumbent","incumbent_party")


reg_df <- state_poll_df %>% left_join(econ_df, by=c("state"="state","year"="year")) %>% 
  left_join(state_popvote_df, by= c("state"="state", "year"="year")) %>%
  left_join(poll_data, 
            by  = c("year"="year","party" = "party" ))


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
  mutate(inc_pv = case_when((inc_party=="republican") ~ R_pv2p, 
                            (inc_party=="democrat") ~ D_pv2p)) %>% 
  mutate(chl_pv = case_when(chl_party == "democrat" ~ D_pv2p, 
                            chl_party == "republican" ~ R_pv2p))

reg_df <- reg_df %>% mutate(incumbent_win = case_when(inc_party == "republican" & R_pv2p > D_pv2p ~ 1,
                                                      inc_party == "republican" & R_pv2p < D_pv2p ~ 0, 
                                                      inc_party == "democrat" & R_pv2p < D_pv2p ~ 1,
                                                      inc_party == "democrat" & R_pv2p > D_pv2p ~ 0))

reg_df <- reg_df %>% 
  mutate(inc_votes = case_when((inc_party=="republican") ~ R, 
                               (inc_party=="democrat") ~ D)) %>% 
  mutate(chl_votes = case_when(chl_party == "democrat" ~ D, 
                               chl_party == "republican" ~ R))

reg_df <- reg_df %>% left_join(vep_df, by = c("state" = "state", "year" = "year")) 
reg_df <- reg_df %>% left_join(demo, by = c("state" = "state", "year" = "year"))
reg_df <- na.omit(reg_df)
reg_df <- reg_df %>% mutate_at(vars(VEP), funs(round(.)))


gdp_2020 <- rep(-0.0949, 102)
poll_2020_clean$gdp <- gdp_2020
pred_df <- poll_2020_clean %>% left_join(state_rdi[,c(1,290)], c("state" = "State"))
pred_df$state <- c(state.abb, 'DC')[match(poll_2020_clean$state, c(state.name, 'District of Columbia'))]

pred_df <- pred_df %>% 
  left_join(demo %>% filter(year == 2018), by = c("state" = "state")) %>% 
  left_join(vep_df %>% filter(year == 2016), by = c("state" = "state")) %>% 
  dplyr::select(-year.x) %>% dplyr::select(-year.y) 

colnames(pred_df)[6] <- "rdi_q2"

pred_df <- pred_df %>% add_column(year = rep(2020, 102), total = NA, D = NA, R = NA, R_pv2p = NA,
                                  D_pv2p= NA, D_pv = NA, R_pv = NA, vote_margin = NA, winner = NA,
                                  inc_pv = NA, chl_pv = NA, incumbent_win = NA, inc_votes = NA, 
                                  chl_votes = NA) %>% 
  mutate(incumbent = case_when(party == "republican" ~ TRUE, 
                               party == "democrat" ~ FALSE)) %>%
  mutate(incumbent_party = incumbent, inc_party = incumbent, chl_party = !incumbent) 

reg_df$inc_party <- as.logical(reg_df$inc_party)
reg_df$chl_party <- as.logical(reg_df$chl_party)

vote_num <- c("rdi_q2", "Asian",
              "Black", "Hispanic", "Indigenous", "White", "Female", "Male", "age20",
              "age3045", "age4565", "age65")

full_votes <- rbind(reg_df, pred_df) %>% dplyr::select(-incumbent,-inc_party, -chl_party) %>%
  mutate(inc_party = incumbent_party, chl_party = !incumbent_party) %>% 
  group_by(year) %>%
  mutate_at(vote_num, scale) %>% ungroup() %>%
  left_join(turnout_scaled %>% dplyr::select(state, year, last_vote_margin), by = c("state" = "state", "year" = "year"))


## build vote share model
inc_vote_model <- glm(cbind(inc_votes, total - inc_votes) ~ rdi_q2 + gdp + state + 
                        avg_poll + Black + Hispanic + Asian + White + Male + 
                        age20 + age3045 + age4565 + last_vote_margin + party, 
                        data = full_votes %>% filter(year < 2020, incumbent_party == TRUE), 
                      family = binomial) 

chl_vote_model <- glm(cbind(chl_votes, total - chl_votes) ~ rdi_q2 + gdp + state + 
                        avg_poll + Black + Hispanic + Asian + White + Male + 
                        age20 + age3045 + age4565 + last_vote_margin + party, 
                      data = full_votes %>% filter(year < 2020, incumbent_party == FALSE), 
                      family = binomial)

summary(inc_vote_model)
summary(chl_vote_model)

## classification accuracy by year
year_list <- seq(from = 1992, to = 2016, by = 4)

vote_share_check <- function(yr){
  temp_inc_df <- full_votes %>% filter(year != yr, year < 2020, incumbent_party == TRUE)
  temp_chl_df <- full_votes %>% filter(year != yr, year < 2020, incumbent_party == FALSE)
  
  if(yr != 2016){
    pred_inc_df <- full_votes %>% filter(year == yr, incumbent_party == TRUE)
    pred_chl_df <- full_votes %>% filter(year == yr, incumbent_party == FALSE)
  }else{
    pred_inc_df <- full_votes %>% filter(year == yr, incumbent_party == TRUE, state != "DC")
    pred_chl_df <- full_votes %>% filter(year == yr, incumbent_party == FALSE, state != "DC")  
  }
  
  temp_inc_vote_model <- glm(cbind(inc_votes, total - inc_votes) ~ rdi_q2 + gdp + state + 
                          avg_poll + Black + Hispanic + Asian + White + Male + 
                            age20 + age3045 + age4565 +last_vote_margin + party, 
                        data = temp_inc_df, family = binomial)
  
  temp_chl_vote_model <- glm(cbind(chl_votes, total - chl_votes) ~ rdi_q2 + gdp + state + 
                          avg_poll + Black + Hispanic + Asian + White + Male + 
                            age20 + age3045 + age4565 + last_vote_margin + party, 
                        data = temp_chl_df, 
                        family = binomial)
  
  temp_pred_inc <- predict(inc_vote_model, pred_inc_df)
  temp_pred_chl <- predict(chl_vote_model, pred_chl_df)

  states_correct <- (temp_pred_inc > temp_pred_chl) == (pred_inc_df$inc_pv > pred_inc_df$chl_pv)
  frac_right <- sum(states_correct)/length(temp_pred_chl)
  
  c(yr, frac_right)
  
}

outsamp_votes <- lapply(year_list, vote_share_check)
view(outsamp_votes)


## get variance for the 2020 polls
poll_2020_var <- poll_2020_df %>% filter(days_left <= 21) %>% 
  group_by(state, candidate_name, party) %>%
  summarize(poll_var = sd(avg_support_adj/100)) %>% filter(state != "National") %>% 
  dplyr::select(state, candidate_name, poll_var, party) %>%
  filter(state != "NE-1", state != "NE-2", state != "ME-1", state != "ME-2") %>% ungroup() %>%
  dplyr::select(state, poll_var, party)

poll_2020_var$state <- c(state.abb, 'DC')[match(poll_2020_var$state, c(state.name, 'District of Columbia'))]

## make predictions for inc and chl models to get means
trump_df <- full_votes %>% filter(year == 2020, incumbent_party == TRUE)
biden_df <- full_votes %>% filter(year == 2020, incumbent_party == FALSE)

trump_pred <- as.data.frame(predict(inc_vote_model, trump_df, type = "response"))
trump_pred$state <- trump_df$state
colnames(trump_pred) <- c("vote_share", "state")

biden_pred <- as.data.frame(predict(chl_vote_model, biden_df, type = "response"))
biden_pred$state <- biden_df$state
colnames(biden_pred) <- c("vote_share", "state")

## load in atlernate variances
pres_polls <- read.csv("../Data/president_polls.csv") %>% 
  mutate(state = state.abb[match(state, state.name)],
         party = case_when(candidate_name == "Donald Trump" ~ "republican",
                           candidate_name == "Joseph R. Biden Jr." ~ "democrat"),
         end_date = mdy(end_date),
         poll_date = as.Date(end_date, "%m/%d/%Y"),
         election_date = mdy(election_date),
         days_left = round(difftime(election_date, poll_date, unit="days")),
         weeks_left = round(difftime(election_date, poll_date, unit="weeks"))) %>%
  filter(!is.na(party))

poll_var_edited <- pres_polls %>% filter(days_left <= 21) %>%
  group_by(state, party) %>% summarize(poll_var = sd(pct/100))
na.omit(poll_var_edited)

poll_var_dc <- poll_2020_var %>% filter(state == "DC")
poll_var_full <- rbind(poll_var_edited, poll_var_dc)


## attach variances
trump_pred_df <- data.frame(trump_pred) %>% left_join(poll_var_full %>% filter(party == "republican"), 
                                          by = c("state" = "state"))

biden_pred_df <- data.frame(biden_pred) %>% left_join(poll_var_full %>% filter(party == "democrat"), 
                                                      by = c("state" = "state"))


## correlation matricies
vote_pred <- c("rdi_q2", "gdp", "avg_poll", "Black", "Hispanic", "Asian", "White", "Male", 
               "age20", "age3045", "age4565", "last_vote_margin")

biden_correlation <- corSimMat(biden_df %>% dplyr::select(vote_pred))
trump_correlation <- corSimMat(trump_df %>% dplyr::select(vote_pred))

## covariance matricies
cor2cov <- function(R,S){
  diag(S) %*% R %*% diag(S)
}

biden_cov <- cor2cov(biden_correlation, biden_pred_df$poll_var) 
trump_cov <- cor2cov(trump_correlation, trump_pred_df$poll_var) 


##get mean and variance for uniform polling error
nat_popvote_clean <- nat_popvote_df %>% filter(party == "republican") %>%
  dplyr::select(year, pv) %>%
  left_join(nat_popvote_df %>% filter(party == "democrat") %>%
              dplyr::select(year,pv), by = c("year" = "year"))
colnames(nat_popvote_clean) <- c("year", "rpv", "dpv")

nat_poll_clean <- nat_poll_df %>% filter(days_left <= 7) %>% 
  dplyr::select(year, party, avg_support) %>% group_by(year, party) %>%
  summarize(avg_support = mean(avg_support))

nat_poll_clean2 <- nat_poll_clean %>% 
  filter(party == "republican") %>%
  dplyr::select(year, avg_support) %>%
  left_join(nat_poll_clean %>% 
              filter(party == "democrat") %>%
              dplyr::select(year, avg_support), by = c("year" = "year"))
colnames(nat_poll_clean2) <- c("year", "r_poll", "d_poll")

nat_var <- nat_popvote_clean %>%
  dplyr::select(year, rpv, dpv) %>%
  left_join(nat_poll_clean2 %>% 
              dplyr::select(year, r_poll, d_poll), by = c("year" = "year")) %>% 
  mutate(rep_error = (rpv - r_poll)/100, dem_error = (dpv - d_poll)/100) 
nat_var <- na.omit(nat_var)

nat_mean_rep <- mean(nat_var$rep_error)
nat_mean_dem <- mean(nat_var$dem_error)
nat_var_rep <- var(nat_var$rep_error)
nat_var_dem <- var(nat_var$dem_error)
party_cov <- cov(nat_var$rep_error, nat_var$dem_error)

cov_mat <- rbind(c(nat_mean_rep, party_cov),c(party_cov, nat_mean_dem))

## Draws for turnout and vote share
set.seed(1347)
count <- 10000
trump_sims <- as.data.frame(t(mvrnorm(n = count, trump_pred_df$vote_share, trump_cov)))
trump_sims$state <- trump_pred_df$state
biden_sims <- as.data.frame(t(mvrnorm(n = count, biden_pred_df$vote_share, biden_cov)))
biden_sims$state <- biden_pred_df$state
nat_error_biden <- rstable(count, alpha = 2, beta = 0, gamma = sqrt(0.5*nat_var_dem), 
                           delta = nat_mean_dem)
nat_error_trump <- rstable(count, alpha = 2, beta = 0, gamma = sqrt(0.5*nat_var_rep), 
                           delta = nat_mean_rep)

turnout_sims <- NULL
for(st in turnout_pred_df$state){
  temp_mean <- turnout_2020_pred %>% filter(state == st) %>% dplyr::select(turnout)
  output <- cbind.data.frame(st, rpois(count, as.numeric(temp_mean)) , seq(from = 1, to = count, by = 1))
  
  turnout_sims <- rbind(turnout_sims, output)
}
colnames(turnout_sims) <- c("state", "turnout", "sim")


## consolidate to results
elec_col <- read_csv("../Data/ElectoralCollegePost1948_adj.csv") %>% filter(Year==2020) %>% 
  dplyr::select(State, EC)
elec_col <- remove_empty(elec_col, which = c("cols"), quiet = TRUE)
elec_col$State <- c(state.abb, 'DC')[match(elec_col$State, c(state.name, 'District of Columbia'))]

results_df <- NULL

for(st in turnout_2020_pred$state){
  p_biden <- t(biden_sims %>% filter(state == st) %>% dplyr::select(-state) + nat_error_biden)
  p_trump <- t(trump_sims %>% filter(state == st) %>% dplyr::select(-state) + nat_error_trump)
  s_turnout <- t(turnout_sims %>% filter(state == st) %>% dplyr::select(-state,-sim))
  
  v_trump <- rbinom(n = count, size = s_turnout, prob = p_trump)
  v_biden <- rbinom(n = count, size = s_turnout, prob = p_biden)
  
  ec_val <- elec_col %>% filter(State == st) %>% dplyr::select(EC)
  
  res_st <- cbind.data.frame(st, v_trump, v_biden, 
                             ec_val, seq(from = 1, to = count, by = 1))
  colnames(res_st) <- c("state", "trump_v", "biden_v", "EC", "sim")
  
  results_df <- rbind(results_df, res_st)
}



results_df <- results_df %>% mutate(win = case_when(trump_v > biden_v ~ "Trump",
                                                    trump_v < biden_v ~ "Biden",
                                                    trump_v == biden_v ~ "Tie"))

write.csv(results_df, "../Data/prediction_highvar.csv")

ec_results <- aggregate(results_df$EC, results_df %>% dplyr::select(win, sim), FUN = sum)
biden_pv <- aggregate(results_df$biden_v, results_df %>% dplyr::select(sim), FUN = sum)
trump_pv <- aggregate(results_df$trump_v, results_df %>% dplyr::select(sim), FUN = sum)


## ec plot
biden_ec_plot <- ggplot(ec_results %>% filter(win == "Biden"), aes(x = x, fill = win), alpha = 0.75 ) +
  geom_density(alpha =0.25, color = "dodgerblue2") +
  geom_histogram(bins = max(ec_results$x) - min(ec_results$x), aes(y = ..density..),position = 'identity') + 
  labs(x="Electoral College Results", y="Frequency", color = "Legend") + ggtitle("Predicted Electoral College Results") + 
  scale_fill_manual(values=c("dodgerblue2")) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "grey"),
        legend.key = element_rect(fill="transparent", colour=NA),
        legend.position = "none")+
  scale_color_manual(values=c("deepskyblue")) + 
  labs(title = "Biden Electoral College Votes from 10000 Simulations") + xlim(0,538)

trump_ec_plot <- ggplot(ec_results %>% filter(win == "Trump"), aes(x = x, fill = win), alpha = 0.75 ) +
  geom_density(alpha =0.25, color = "firebrick2") +
  geom_histogram(bins = max(ec_results$x) - min(ec_results$x), aes(y = ..density..),position = 'identity') + 
  labs(x="Electoral College Results", y="Frequency", color = "Legend") + ggtitle("Predicted Electoral College Results") + 
  scale_fill_manual(values=c("firebrick2")) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "grey"),
        legend.key = element_rect(fill="transparent", colour=NA),
        legend.position = "none")+
  scale_color_manual(values=c("firebrick")) + 
  labs(title = "Trump Electoral College Votes from 10000 Simulations") + xlim(0,538)

ec_plot <- gridExtra::grid.arrange(trump_ec_plot, biden_ec_plot)


## popular vote plot
pv_df <- biden_pv %>% left_join(trump_pv, by = c("sim" = "sim"))
colnames(pv_df) <- c("sim", "biden_votes", "trump_votes")
pv_df <- pv_df %>% mutate(biden_pv = biden_votes/(biden_votes+trump_votes), 
                          trump_pv = trump_votes/(biden_votes+trump_votes))

biden_pv_plot <- ggplot(pv_df %>% dplyr::select(biden_pv), aes(x = biden_pv), alpha = 0.75) +
  geom_histogram(bins = 100, 
                 aes(y = ..density..),
                 position = 'identity', 
                 color = "dodgerblue2",
                 fill = "deepskyblue") + 
  labs(x="Popular Vote Results", y="Frequency", color = "Legend") + ggtitle("Predicted Popular Vote Results") + 
  scale_fill_manual(values=c("dodgerblue2")) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "grey"),
        legend.key = element_rect(fill="transparent", colour=NA),
        legend.position = "none")+
  labs(title = "Biden Popular Vote Percent from 10000 Simulations") + xlim(0,1)

trump_pv_plot <- ggplot(pv_df %>% dplyr::select(trump_pv), aes(x = trump_pv), alpha = 0.75) +
  geom_histogram(bins = 100, 
                 aes(y = ..density..),
                 position = 'identity', 
                 color = "firebrick",
                 fill = "firebrick2") + 
  labs(x="Popular Vote Results", y="Frequency", color = "Legend") + ggtitle("Predicted Popular Vote Results") + 
  scale_fill_manual(values=c("dodgerblue2")) + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "grey"),
        legend.key = element_rect(fill="transparent", colour=NA),
        legend.position = "none")+
  labs(title = "Trump Popular Vote Percent from 10000 Simulations") + xlim(0,1)

pv_plot <- gridExtra::grid.arrange(trump_pv_plot, biden_pv_plot, ncol = 2)

## state by state results
avg_popvote_state <- results_df %>% mutate(trump_pv = trump_v/(trump_v+biden_v),
                                           biden_pv = biden_v/(trump_v+biden_v)) %>%
  group_by(state) %>%
  summarize(trump_pv = mean(trump_pv), biden_pv = mean(biden_pv)) %>%
  mutate(win = case_when(trump_pv > biden_pv ~ "Trump",
                         biden_pv > trump_pv ~ "Biden"))

avg_popvote_table <- avg_popvote_state %>% dplyr::select(state, trump_pv, biden_pv)
colnames(avg_popvote_table) <- c("State", "Trump Popular Vote", "Biden Popular Vote")

pop_vote_plot <- avg_popvote_state %>% 
  ggplot(aes(state = state, fill = fct_relevel(win, "Trump", "Biden"))) +
  geom_statebins() +
  theme_statebins() +
  labs(title = "2020 Presidential Election Based on Average Popular Vote Share",
       subtitle = "From 10000 Simulations",
       fill = "") +
  scale_fill_manual(values=c("#619CFF", "#CCCCCC", "#F8766D"), breaks = c("Biden", "N/A", "Trump"))

avg_popvote_state <- avg_popvote_state %>% mutate(margin = trump_pv - biden_pv) 

plot_usmap(data = avg_popvote_state, regions = "states", values = "margin") +
  scale_fill_gradient2(
    high = "firebrick2", 
    mid = "white",
    low = "dodgerblue2", 
    breaks = c(-0.50,0,0.50), 
    limits = c(-0.50,0.50),
    name = "Trump Popular Vote Margin"
  ) +
  theme_void() +
  theme(strip.text = element_text(size = 12),
        aspect.ratio=1, legend.position = "bottom") + labs(title = "2020 Presidential Election Based on Average Popular Vote Share",
                               subtitle = "From 10000 Simulations",
                               fill = "") 
ggsave("avg_pop_vote_map.png")


sstate_count <- results_df %>% group_by(state) %>% count(win)

ec_wins <- ec_results %>% spread(win, x) %>% mutate(winner = case_when(Biden > Trump ~ "Biden", 
                                                                       Trump > Biden ~ "Trump", 
                                                                       Biden == Trump ~ "Tie"))

state_pv <- results_df %>% 
  mutate(biden_pv= biden_v/(biden_v+trump_v), trump_pv = trump_v/(biden_v+trump_v)) 

biden_pv_clean <- cbind.data.frame(state_pv %>% dplyr::select(state, sim, biden_pv), rep("Biden", count) )
trump_pv_clean <- cbind.data.frame(state_pv %>% dplyr::select(state, sim, trump_pv), rep("Trump", count) )

colnames(biden_pv_clean) <- c("state", "sim", "popvote", "candidate")
colnames(trump_pv_clean) <- c("state", "sim", "popvote", "candidate")

state_pv_clean <- rbind.data.frame(biden_pv_clean, trump_pv_clean)

battle_states <- c("AZ", "FL", "GA", "IA", "ME", "MI", "MN", "NC", "NE", "NH","NV", "OH", 
                   "PA", "TX","SC", "WI")

state_pv_clean %>% filter(state %in% battle_states) %>%ggplot(aes( x = popvote, fill = candidate, color = candidate)) +
  facet_wrap(facets = state ~.) +
  geom_density(alpha = 0.5) + 
  labs(title = "Density of Simulated Vote Share in Battleground States",
                        subtitle = "From 10000 Simulations",
                        fill = "",
       x = "Popular Vote Share", y = "Frequency") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "grey"),
        legend.key = element_rect(fill="transparent", colour=NA)) + 
  scale_fill_manual(name = "Candidate Vote Share",values=c("dodgerblue2", "firebrick2"), labels = c("Biden", "Trump")) + 
  scale_color_manual(name = "Candidate Vote Share",values = c("deepskyblue", "firebrick"),labels = c("Biden", "Trump")) + xlim(0,1)
ggsave("battleground.png")


victors <- ec_wins %>% group_by(winner) %>% count()


