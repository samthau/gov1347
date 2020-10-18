# import libraries
library(tidyverse)
library(ggplot2)
library(usmap)
library(janitor)
library(usmap)
library(data.table)
library(ebbr)
library(gamlss)

setwd("~/Desktop/Gov 1347/Blog/R files")

nat_popvote_df <- read_csv("../Data/popvote_1948-2016.csv")
nat_poll_df    <- read_csv("../Data/pollavg_1968-2016.csv")

state_popvote_df <- read_csv("../Data/popvote_bystate_1948-2016.csv")
state_poll_df <- read_csv("../Data/pollavg_bystate_1968-2016.csv")

nat_econ <- read_csv("../Data/econ.csv")
state_rdi <- read_csv("../Data/state_rdi.csv")

demo <- read_csv("../Data/demographic_1990-2018.csv")
vep_df <- read_csv("../Data/vep_1980-2016.csv")


## clean data
weeks <- 2
poll_df <- state_poll_df %>% filter(weeks_left >= weeks, weeks_left<4) %>% 
  dplyr::select(year, state, party, candidate_name, avg_poll) %>% 
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


## Merge datasets and clean
inc_data <- nat_popvote_df %>% dplyr::select("year","party","winner", "incumbent","incumbent_party")

reg_df <- poll_df %>% left_join(econ_df, by=c("state"="state","year"="year")) %>% 
  left_join(state_popvote_df, by= c("state"="state", "year"="year")) %>%
  left_join(inc_data, 
            by  = c("year"="year","party" = "party" ))

reg_df$period <- ifelse(reg_df$year %in% 1992:1996, "1992-1996",
                        ifelse(reg_df$year %in% 2000:2004, "2000-2004",
                               ifelse(reg_df$year %in% 2008:2012, "2008-2012",
                                      ifelse(reg_df$year %in% 2016-2020, "2016-2020", "NA"))))

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
reg_df$state <- c(state.abb, 'DC')[match(reg_df$state, c(state.name, 'District of Columbia'))]

reg_df <- reg_df %>% left_join(demo, by = c("state" = "state","year" = "year" ))
reg_df <- reg_df %>% filter(year > 1990)

reg_df <- na.omit(reg_df)

inc_df <- reg_df %>% filter(incumbent_party == TRUE)
chl_df <- reg_df %>% filter(incumbent_party == FALSE)

inc_eb <- gamlss(cbind(inc_pv, floor(VEP)-inc_pv) ~ avg_poll + state + period + rdi_q2 + gdp,
               data = inc_df,
               family = BB(mu.link = "logit"))

chl_eb <- gamlss(cbind(chl_pv, floor(VEP)-chl_pv) ~ state + avg_poll + period + rdi_q2 + gdp,
                 data = chl_df,
                 family = BB(mu.link = "logit"))

## fit 2020 data, using weighted average of previous two elections as data
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

poll_2020_clean <- poll_2020_df %>% filter(weeks_left < 5, weeks_left >=weeks) %>% 
  group_by(state, candidate_name, party) %>%
  summarize(avg_poll = mean(avg_support)) %>% filter(state != "National") %>% 
  dplyr::select(state, candidate_name, avg_poll, party) %>% mutate(period = "2016-2020") %>%
  mutate(incumbent_party = case_when(candidate_name == "Joseph R. Biden Jr." ~ FALSE, 
                                     candidate_name == "Donald Trump" ~ TRUE)) %>%
  filter(state != "NE-1", state != "NE-2", state != "ME-1", state != "ME-2")

gdp <- rep(-0.0949, 102)
poll_2020_clean$gdp <- gdp
poll_2020_clean$avg_poll <- as.numeric(poll_2020_clean$avg_poll)

pred_data_df <- poll_2020_clean %>% left_join(state_rdi[,c(1,290)], c("state" = "State"))
colnames(pred_data_df) <- c("state", "candidate_name","avg_poll","party", "period", "incumbent_party","gdp", "rdi_q2")
pred_data_df <- pred_data_df %>% left_join(vep_df %>% filter(year == 2016), by = c("state"="state"))
pred_data_df$state <- c(state.abb, 'DC')[match(pred_data_df$state, c(state.name, 'District of Columbia'))]


## get R and D numbers from past two years, weighted differently to serve as update
guess_rd <- state_popvote_df %>%dplyr::select(D, R, year, state) %>% filter(year > 2010)

v100 <-  guess_rd %>%filter(year == 2016) %>% dplyr::select(state, R, D)
v85 <- 0.85*(guess_rd %>%filter(year == 2016) %>% dplyr::select(R, D)) + 0.15*(guess_rd %>%filter(year == 2012) %>% dplyr::select(R, D))
v75 <- 0.75*(guess_rd %>%filter(year == 2016) %>% dplyr::select(R, D)) + 0.25*(guess_rd %>%filter(year == 2012) %>% dplyr::select(R, D))
v60 <- 0.6*(guess_rd %>%filter(year == 2016) %>% dplyr::select(R, D)) + 0.4*(guess_rd %>%filter(year == 2012) %>% dplyr::select(R, D))

app <- cbind.data.frame(v100, v85, v75, v60)
colnames(app) <- c("state", "R100", "D100", "R85", "D85", "R75", "D75", "R60", "D60")
app$state <- c(state.abb, 'DC')[match(app$state, c(state.name, 'District of Columbia'))]


## combine and split data
pred_data_df <- pred_data_df %>% left_join(app, by = c("state" = "state"))

trump_data <- pred_data_df %>% 
  filter(candidate_name == "Donald Trump") %>% ungroup()%>%
  dplyr::select("state", "D100", "VEP", "avg_poll", "period", "rdi_q2", "gdp")

colnames(trump_data) <- c("state", "inc_pv", "VEP", "avg_poll", "period", "rdi_q2","gdp")

biden_data <- pred_data_df %>% 
  filter(candidate_name == "Joseph R. Biden Jr.") %>% ungroup()%>%
  dplyr::select("state", "R100", "VEP", "avg_poll", "period",  "rdi_q2", "gdp")

colnames(biden_data) <- c("state", "chl_pv", "VEP", "avg_poll", "period", "rdi_q2", "gdp")


## do predictions for 100
trump_posterior_100 <- trump_data %>%
  mutate(mu = predict(inc_eb, what = "mu", newdata = trump_data),
         sigma = predict(chl_eb, what = "sigma", newdata = trump_data, type = "response"),
         alpha0 = mu / sigma,
         beta0 = (1 - mu) / sigma,
         alpha1 = alpha0 + inc_pv,
         beta1 = beta0 + VEP - inc_pv,
         new_eb = alpha1 / (alpha1 + beta1))

biden_posterior_100 <- biden_data %>%
  mutate(mu = predict(inc_eb, what = "mu", newdata = biden_data),
         sigma = predict(chl_eb, what = "sigma", newdata = biden_data, type = "response"),
         alpha0 = mu / sigma,
         beta0 = (1 - mu) / sigma,
         alpha1 = alpha0 + chl_pv,
         beta1 = beta0 + VEP - chl_pv,
         new_eb = alpha1 / (alpha1 + beta1))

elec <- elec_col
elec <- remove_empty(elec, which = c("cols"), quiet = TRUE)
elec$State <- c(state.abb, 'DC')[match(elec$State, c(state.name, 'District of Columbia'))]

election100 <- trump_posterior_100 %>% left_join(biden_posterior_100, by = c("state" = "state")) %>%
  dplyr::select("state", "new_eb.x", "new_eb.y") %>% mutate(winner = case_when(new_eb.x > new_eb.y ~ "Trump", 
                                                                               new_eb.x < new_eb.y ~ "Biden")) %>%
  left_join(elec, by = c("state" = "State"))

## do predictions for 85-15
trump_data <- pred_data_df %>% 
  filter(candidate_name == "Donald Trump") %>% ungroup()%>%
  dplyr::select("state", "D85", "VEP", "avg_poll", "period", "rdi_q2", "gdp")

colnames(trump_data) <- c("state", "inc_pv", "VEP", "avg_poll", "period", "rdi_q2","gdp")

biden_data <- pred_data_df %>% 
  filter(candidate_name == "Joseph R. Biden Jr.") %>% ungroup()%>%
  dplyr::select("state", "R85", "VEP", "avg_poll", "period",  "rdi_q2", "gdp")

colnames(biden_data) <- c("state", "chl_pv", "VEP", "avg_poll", "period", "rdi_q2", "gdp")

trump_posterior_85 <- trump_data %>%
  mutate(mu = predict(inc_eb, what = "mu", newdata = trump_data),
         sigma = predict(chl_eb, what = "sigma", newdata = trump_data, type = "response"),
         alpha0 = mu / sigma,
         beta0 = (1 - mu) / sigma,
         alpha1 = alpha0 + inc_pv,
         beta1 = beta0 + VEP - inc_pv,
         new_eb = alpha1 / (alpha1 + beta1))

biden_posterior_85 <- biden_data %>%
  mutate(mu = predict(inc_eb, what = "mu", newdata = biden_data),
         sigma = predict(chl_eb, what = "sigma", newdata = biden_data, type = "response"),
         alpha0 = mu / sigma,
         beta0 = (1 - mu) / sigma,
         alpha1 = alpha0 + chl_pv,
         beta1 = beta0 + VEP - chl_pv,
         new_eb = alpha1 / (alpha1 + beta1))

election85 <- trump_posterior_85 %>% left_join(biden_posterior_85, by = c("state" = "state")) %>%
  dplyr::select("state", "new_eb.x", "new_eb.y") %>% mutate(winner = case_when(new_eb.x > new_eb.y ~ "Trump", 
                                                                               new_eb.x < new_eb.y ~ "Biden")) %>%
  left_join(elec, by = c("state" = "State"))

## do predictions for 75-25
trump_data <- pred_data_df %>% 
  filter(candidate_name == "Donald Trump") %>% ungroup()%>%
  dplyr::select("state", "D75", "VEP", "avg_poll", "period", "rdi_q2", "gdp")

colnames(trump_data) <- c("state", "inc_pv", "VEP", "avg_poll", "period", "rdi_q2","gdp")

biden_data <- pred_data_df %>% 
  filter(candidate_name == "Joseph R. Biden Jr.") %>% ungroup()%>%
  dplyr::select("state", "R75", "VEP", "avg_poll", "period",  "rdi_q2", "gdp")

colnames(biden_data) <- c("state", "chl_pv", "VEP", "avg_poll", "period", "rdi_q2", "gdp")

trump_posterior_75 <- trump_data %>%
  mutate(mu = predict(inc_eb, what = "mu", newdata = trump_data),
         sigma = predict(chl_eb, what = "sigma", newdata = trump_data, type = "response"),
         alpha0 = mu / sigma,
         beta0 = (1 - mu) / sigma,
         alpha1 = alpha0 + inc_pv,
         beta1 = beta0 + VEP - inc_pv,
         new_eb = alpha1 / (alpha1 + beta1))

biden_posterior_75 <- biden_data %>%
  mutate(mu = predict(inc_eb, what = "mu", newdata = biden_data),
         sigma = predict(chl_eb, what = "sigma", newdata = biden_data, type = "response"),
         alpha0 = mu / sigma,
         beta0 = (1 - mu) / sigma,
         alpha1 = alpha0 + chl_pv,
         beta1 = beta0 + VEP - chl_pv,
         new_eb = alpha1 / (alpha1 + beta1))

election75 <- trump_posterior_75 %>% left_join(biden_posterior_75, by = c("state" = "state")) %>%
  dplyr::select("state", "new_eb.x", "new_eb.y") %>% mutate(winner = case_when(new_eb.x > new_eb.y ~ "Trump", 
                                                                               new_eb.x < new_eb.y ~ "Biden")) %>%
  left_join(elec, by = c("state" = "State"))

## do predictions for 60-40
trump_data <- pred_data_df %>% 
  filter(candidate_name == "Donald Trump") %>% ungroup()%>%
  dplyr::select("state", "D60", "VEP", "avg_poll", "period", "rdi_q2", "gdp")

colnames(trump_data) <- c("state", "inc_pv", "VEP", "avg_poll", "period", "rdi_q2","gdp")

biden_data <- pred_data_df %>% 
  filter(candidate_name == "Joseph R. Biden Jr.") %>% ungroup()%>%
  dplyr::select("state", "R60", "VEP", "avg_poll", "period",  "rdi_q2", "gdp")

colnames(biden_data) <- c("state", "chl_pv", "VEP", "avg_poll", "period", "rdi_q2", "gdp")

trump_posterior_60 <- trump_data %>%
  mutate(mu = predict(inc_eb, what = "mu", newdata = trump_data),
         sigma = predict(chl_eb, what = "sigma", newdata = trump_data, type = "response"),
         alpha0 = mu / sigma,
         beta0 = (1 - mu) / sigma,
         alpha1 = alpha0 + inc_pv,
         beta1 = beta0 + VEP - inc_pv,
         new_eb = alpha1 / (alpha1 + beta1))

biden_posterior_60 <- biden_data %>%
  mutate(mu = predict(inc_eb, what = "mu", newdata = biden_data),
         sigma = predict(chl_eb, what = "sigma", newdata = biden_data, type = "response"),
         alpha0 = mu / sigma,
         beta0 = (1 - mu) / sigma,
         alpha1 = alpha0 + chl_pv,
         beta1 = beta0 + VEP - chl_pv,
         new_eb = alpha1 / (alpha1 + beta1))

election60 <- trump_posterior_60 %>% left_join(biden_posterior_60, by = c("state" = "state")) %>%
  dplyr::select("state", "new_eb.x", "new_eb.y") %>% mutate(winner = case_when(new_eb.x > new_eb.y ~ "Trump", 
                                                                               new_eb.x < new_eb.y ~ "Biden")) %>%
  left_join(elec, by = c("state" = "State"))

## information based on polls
pred_data_df <- pred_data_df %>% mutate(voters = R100+D100) %>% mutate(Rp = floor(voters*avg_poll/100),
                                                                       Dp = floor(voters*avg_poll/100))

trump_data <- pred_data_df %>% 
  filter(candidate_name == "Donald Trump") %>% ungroup()%>%
  dplyr::select("state", "Rp", "VEP", "avg_poll", "period", "rdi_q2", "gdp")

colnames(trump_data) <- c("state", "inc_pv", "VEP", "avg_poll", "period", "rdi_q2","gdp")

biden_data <- pred_data_df %>% 
  filter(candidate_name == "Joseph R. Biden Jr.") %>% ungroup()%>%
  dplyr::select("state", "Dp", "VEP", "avg_poll", "period",  "rdi_q2", "gdp")

colnames(biden_data) <- c("state", "chl_pv", "VEP", "avg_poll", "period", "rdi_q2", "gdp")

trump_posterior_p <- trump_data %>%
  mutate(mu = predict(inc_eb, what = "mu", newdata = trump_data, type = "response"),
         sigma = predict(chl_eb, what = "sigma", newdata = trump_data, type = "response"),
         alpha0 = mu / sigma,
         beta0 = (1 - mu) / sigma,
         alpha1 = alpha0 + inc_pv,
         beta1 = beta0 + VEP - inc_pv,
         new_eb = alpha1 / (alpha1 + beta1))

biden_posterior_p <- biden_data %>%
  mutate(mu = predict(inc_eb, what = "mu", newdata = biden_data, type = "response"),
         sigma = predict(chl_eb, what = "sigma", newdata = biden_data, type = "response"),
         alpha0 = mu / sigma,
         beta0 = (1 - mu) / sigma,
         alpha1 = alpha0 + chl_pv,
         beta1 = beta0 + VEP - chl_pv,
         new_eb = alpha1 / (alpha1 + beta1))

election_p <- trump_posterior_p %>% left_join(biden_posterior_p, by = c("state" = "state")) %>%
  dplyr::select("state", "new_eb.x", "new_eb.y") %>% mutate(winner = case_when(new_eb.x > new_eb.y ~ "Trump", 
                                                                               new_eb.x < new_eb.y ~ "Biden")) %>%
  left_join(elec, by = c("state" = "State"))



## get EC results
ec100 <- aggregate(election100$EC, election100 %>% dplyr::select(winner), FUN = sum)
ec85 <- aggregate(election85$EC, election85 %>% dplyr::select(winner), FUN = sum)
ec75 <- aggregate(election75$EC, election75 %>% dplyr::select(winner), FUN = sum)
ec60 <- aggregate(election60$EC, election60 %>% dplyr::select(winner), FUN = sum)
ecp <- aggregate(election_p$EC, election_p %>% dplyr::select(winner), FUN = sum)

## after picking an outcome, do A/B testing
ab_test <- function(alpha_a, beta_a,
                     alpha_b, beta_b) {
  u1 <- alpha_a / (alpha_a + beta_a)
  u2 <- alpha_b / (alpha_b + beta_b)
  var1 <- alpha_a * beta_a / ((alpha_a + beta_a) ^ 2 * (alpha_a + beta_a + 1))
  var2 <- alpha_b * beta_b / ((alpha_b + beta_b) ^ 2 * (alpha_b + beta_b + 1))
  pnorm(0, u2 - u1, sqrt(var1 + var2))
}

election100 <- election100 %>% mutate(ab_val = 
                                        ab_test(trump_posterior_100$alpha1, trump_posterior_100$beta1,
                                                biden_posterior_100$alpha1, biden_posterior_100$beta1))

election85 <- election85 %>% mutate(ab_val = 
                                        ab_test(trump_posterior_85$alpha1, trump_posterior_85$beta1,
                                                biden_posterior_85$alpha1, biden_posterior_85$beta1))

election_p <- election_p %>% mutate(ab_val = 
                                      ab_test(trump_posterior_p$alpha1, trump_posterior_p$beta1,
                                              biden_posterior_p$alpha1, biden_posterior_p$beta1))

election_p <- election_p %>% mutate(ab_val = 
                                      round(ab_test(trump_posterior_p$alpha1,trump_posterior_p$beta1,
                                                    biden_posterior_p$alpha1, biden_posterior_p$beta1), digits = 3))

## make maps
plot_usmap(data = election_p, regions = "states", values = "winner", labels = TRUE) +
  scale_fill_manual(values = c("blue", "red"), 
                    name = "Projected winner based on 2020 poll update") +
  theme_void() + 
  labs(title =  "Electoral Map Predicted from Poll Update") +
  theme(plot.title = element_text(face="bold"))

plot_usmap(data = election100, regions = "states", values = "winner", labels = TRUE) +
  scale_fill_manual(values = c("blue", "red"), 
                    name = "Projected winner based on full 2016 update") +
  theme_void() + 
  labs(title =  "Electoral Map Predicted from Full 2016 update") +
  theme(plot.title = element_text(face="bold"))

plot_usmap(data = election75, regions = "states", values = "winner", labels = TRUE) +
  scale_fill_manual(values = c("blue", "red"), 
                    name = "Projected winner based on 75% 2016, 25% 2012 update") +
  theme_void() + 
  labs(title =  "Electoral Map Predicted from Ensemble Update") +
  theme(plot.title = element_text(face="bold"))



##
count = 1
for(st in election_p$state){
  temp_df <- election_p %>% filter(state == st)
  
  trump_df <- trump_posterior_p %>% filter(state == st)
  biden_df <- biden_posterior_p %>% filter(state == st)
  
  trump_vote <- rBB(count, trump_df$mu, trump_df$sigma, trump_df$VEP)
  biden_vote <- rBB(count, biden_df$mu, biden_df$sigma, biden_df$VEP)
  
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

