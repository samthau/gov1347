# import libraries
library(tidyverse)
library(ggplot2)
library(usmap)
library(janitor)
library(usmap)

# set wd
setwd("~/Desktop/Gov 1347/Blog/R files")

# read in all data
nat_popvote_df <- read_csv("../Data/popvote_1948-2016.csv")
nat_poll_df    <- read_csv("../Data/pollavg_1968-2016.csv")

state_popvote_df <- read_csv("../Data/popvote_bystate_1948-2016.csv")
state_poll_df <- read_csv("../Data/pollavg_bystate_1968-2016.csv")

nat_econ <- read_csv("../Data/econ.csv")
state_rdi <- read_csv("../Data/state_rdi.csv")

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
  mutate(inc_pv = case_when((inc_party=="republican") ~ R_pv2p, 
                            (inc_party=="democrat") ~ D_pv2p)) %>% 
  mutate(chl_pv = case_when(chl_party == "democrat" ~ D_pv2p, 
                            chl_party == "republican" ~ R_pv2p))

reg_df <- reg_df %>% mutate(incumbent_win = case_when(inc_party == "republican" & r_pv > d_pv ~ 1,
                                                      inc_party == "republican" & r_pv < d_pv ~ 0, 
                                                      inc_party == "democrat" & r_pv < d_pv ~ 1,
                                                      inc_party == "democrat" & r_pv > d_pv ~ 0))

reg_df <- na.omit(reg_df)


## modified polls plus as comparison
inc_plus_lm <- lm(inc_pv ~ rdi_q2 + gdp + state + avg_poll, data = reg_df %>% filter(year < 2020, incumbent_party == TRUE))
summary(inc_plus_lm)

chl_plus_lm <- lm(chl_pv ~ rdi_q2 + gdp + state + avg_poll + period, data = reg_df %>% filter(year < 2020, incumbent_party == FALSE))
summary(chl_plus_lm)

mean(abs(inc_plus_lm$residuals))
mean(abs(chl_plus_lm$residuals))

## logit regression for incumbent vote share
inc_log <- glm(incumbent_win ~ rdi_q2 + gdp + state + avg_poll, 
               data = reg_df %>% filter(year < 2020, incumbent_party == TRUE), family=binomial)
summary(inc_log)

## out of sample comparisons
# run out of sample fit to see how often states are called correctly
year_list <- seq(from = 1972, to = 2016, by = 4)
state_list <- unique(reg_df$state)


check_out1 <- function(year, state){
  #true vote shares
  true_inc <- reg_df$inc_pv[reg_df$year == year & reg_df$state == state & reg_df$incumbent_party == TRUE]
  true_chl <- reg_df$chl_pv[reg_df$year == year & reg_df$state == state & reg_df$incumbent_party  == FALSE]
  
  if (length(true_inc)>0){
    
    #dataframes excluding the row (incumbency is backwards as everything is negated)
    inc_df <- reg_df %>% filter(!(incumbent_party == FALSE & year == year & state == state))
    chl_df <- reg_df %>% filter(!(incumbent_party == TRUE & year == year & state == state))
    
    #prediction dataframes
    inc_p <- reg_df[reg_df$year == year & reg_df$state == state & reg_df$incumbent_party == TRUE,]
    chl_p <- reg_df[reg_df$year == year & reg_df$state == state & reg_df$incumbent_party == FALSE,]
    
    ##plus model out-of-sample prediction
    mod_plus_inc_ <- lm(inc_pv ~ avg_poll + rdi_q2 + gdp + state, data = inc_df)
    mod_plus_chl_ <- lm(chl_pv ~ avg_poll + rdi_q2 + gdp + state, data = chl_df)
    pred_plus_inc <- predict(mod_plus_inc_, inc_p)
    pred_plus_chl <- predict(mod_plus_chl_, chl_p)
    
    ## logit regression for the incumbent
    inc_log <- glm(incumbent_win ~ rdi_q2 + gdp + state + avg_poll + period, data = inc_df, family=binomial)    
    pred_inc_log <- 100*predict(inc_log, inc_p, type = "response")
    
    c(year, state,
      plus_margin_error = (pred_plus_inc-pred_plus_chl) - (true_inc-true_chl),
      plus_winner_correct = (pred_plus_inc > pred_plus_chl) == (true_inc > true_chl),
      log_winner_correct = (pred_inc_log > 50) == inc_p$incumbent_win
    )
  }else{
    c(NA, NA, NA, NA, NA)
  }
}

outsamp_dflist <- sapply(year_list, function(year){sapply(state_list, function(state) check_out1(year,state))})

outsamp_df <- cbind("year", "state","plus_margin_error",
                    "plus_winner_correct","log_winner_correct")

for(j in seq(from = 1, to = 12, by =1)){
  for(i in seq(from = 1, to = 251, by = 5)){
    start <- i
    stop <- i+4
    temp <- outsamp_dflist[start:stop, j]
    outsamp_df <- rbind(outsamp_df, temp)
  }
}

outsamp_df <- as.data.frame(outsamp_df)  
rownames(outsamp_df) <- NULL
outsamp_df <- outsamp_df %>% row_to_names(row_number=1)
outsamp_df_clean <- na.omit(outsamp_df)

plus_w <- length(outsamp_df_clean$plus_winner_correct[outsamp_df_clean$plus_winner_correct== TRUE])/length(outsamp_df_clean$plus_winner_correct)
log_w <- length(outsamp_df_clean$log_winner_correct[outsamp_df_clean$log_winner_correct== TRUE])/length(outsamp_df_clean$log_winner_correct)

outsamp_df_clean <- outsamp_df_clean %>% mutate(plus_idx = case_when(plus_winner_correct == TRUE ~ 1, plus_winner_correct == FALSE ~ 0)) %>%
  mutate(log_idx = case_when(log_winner_correct == TRUE  ~ 1, log_winner_correct == FALSE  ~ 0))

ln_log_cor <- cor(outsamp_df_clean$plus_idx, outsamp_df_clean$log_idx)


## ensemble model check
check_out2 <- function(year, state){
  #true vote shares
  true_inc <- reg_df$inc_pv[reg_df$year == year & reg_df$state == state & reg_df$incumbent_party == TRUE]

  if (length(true_inc)>0){
    
    #dataframes excluding the row (incumbency is backwards as everything is negated)
    inc_df <- reg_df %>% filter(!(incumbent_party == FALSE & year == year & state == state))

    #prediction dataframes
    inc_p <- reg_df[reg_df$year == year & reg_df$state == state & reg_df$incumbent_party == TRUE,]

    ##plus model out-of-sample prediction
    mod_plus_inc_ <- lm(inc_pv ~ avg_poll + rdi_q2 + gdp + state, data = inc_df)
    pred_plus_inc <- predict(mod_plus_inc_, inc_p)

    ## logit regression for the incumbent
    inc_log <- glm(incumbent_win ~ rdi_q2 + gdp + state + avg_poll + period, data = inc_df, family=binomial)    
    pred_inc_log <- 100*predict(inc_log, inc_p, type = "response")
    
    ## ensemble model out of sample check
    pred_ens <- plus_w/(plus_w+log_w)*pred_plus_inc + log_w/(plus_w+log_w)*pred_inc_log
    
    c(year, state,
      plus_winner_correct = (pred_plus_inc > 50) == inc_p$incumbent_win,
      log_winner_correct = (pred_inc_log > 50) == inc_p$incumbent_win,
      ensemble_winner_correct = (pred_ens > 50) == inc_p$incumbent_win
    )
  }else{
    c(NA, NA, NA, NA, NA)
  }
}

outsamp_ens_list <- sapply(year_list, function(year){sapply(state_list, function(state) check_out2(year,state))})

outsamp_ens_df <- cbind("year", "state",
                    "plus_winner_correct","log_winner_correct","ens_winner_correct")

for(j in seq(from = 1, to = 12, by =1)){
  for(i in seq(from = 1, to = 251, by = 5)){
    start <- i
    stop <- i+4
    temp <- outsamp_dflist[start:stop, j]
    outsamp_ens_df <- rbind(outsamp_ens_df, temp)
  }
}

outsamp_ens_df <- as.data.frame(outsamp_ens_df)  
rownames(outsamp_ens_df) <- NULL
outsamp_ens_df <- outsamp_ens_df %>% row_to_names(row_number=1)
outsamp_ens <- na.omit(outsamp_ens_df)

length(outsamp_ens$ens_winner_correct[outsamp_ens$ens_winner_correct== TRUE])/length(outsamp_ens$ens_winner_correct)



## 2020 predictions
poll_2020_df <- read_csv("../Data/presidential_poll_averages_2020.csv")
elec_col <- read_csv("../Data/ElectoralCollegePost1948_adj.csv") %>% filter(Year> 1968)

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

pred_plus_inc <- predict(inc_plus_lm, pred_data_df %>% filter(incumbent_party == TRUE))
pred_log_inc <- 100*predict(inc_log, pred_data_df %>% filter(incumbent_party == TRUE), type = "response")
pred_ens_inc <- plus_w/(plus_w+log_w)*pred_plus_inc + log_w/(plus_w+log_w)*pred_log_inc

pred_df <- cbind.data.frame(unique(pred_data_df$state), pred_plus_inc, pred_log_inc, 
                            pred_ens_inc)

pred_win <- cbind.data.frame(unique(pred_data_df$state), 
                             case_when(pred_df$pred_plus_inc > 50 ~ "Trump",
                                       pred_df$pred_plus_inc < 50 ~ "Biden"),
                             case_when(pred_df$pred_log_inc > 50 ~ "Trump",
                                       pred_df$pred_log_inc < 50 ~ "Biden"),
                             case_when(pred_df$pred_ens_inc > 50 ~ "Trump",
                                       pred_df$pred_ens_inc < 50 ~ "Biden"))

colnames(pred_win) <- c("State", "polls plus", "logit", "ensemble")
pred_win_ec <- pred_win %>% left_join(elec_col %>% filter(Year==2020), by = ("State"="State"))
colnames(pred_win_ec) <- c("state", "polls", "logit", "ensemble","year","EC")
pred_win_ec <- remove_empty(pred_win_ec, which = c("cols"), quiet = TRUE)

plus_ec <- aggregate(pred_win_ec$EC, by=list(pred_win_ec$polls), FUN=sum)
logit_ec <- aggregate(pred_win_ec$EC, by=list(pred_win_ec$logit), FUN=sum)
ens_ec <- aggregate(pred_win_ec$EC, by=list(pred_win_ec$ensemble), FUN=sum)

plot_usmap(data = pred_win_ec, regions = "states", values = "polls") +
  scale_fill_manual(values = c("blue", "red"), 
                    name = "Projected winner by polls plus model") +
  theme_void()

plot_usmap(data = pred_win_ec, regions = "states", values = "logit") +
  scale_fill_manual(values = c("blue", "red"), 
                    name = "Projected winner by logistic regression model") +
  theme_void()

plot_usmap(data = pred_win_ec, regions = "states", values = "ensemble") +
  scale_fill_manual(values = c("blue", "red"), 
                    name = "Projected winner by ensemble model") +
  theme_void()







