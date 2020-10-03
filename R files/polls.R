# import libraries
library(tidyverse)
library(ggplot2)
library(usmap)
library(janitor)
library(usmap)

# set wd
setwd("~/Desktop/Gov 1347/Elections/Posts")

# read in all data
nat_popvote_df <- read_csv("../Data/popvote_1948-2016.csv")
nat_poll_df    <- read_csv("../Data/pollavg_1968-2016.csv")

state_popvote_df <- read_csv("../Data/popvote_bystate_1948-2016.csv")
state_poll_df <- read_csv("../Data/pollavg_bystate_1968-2016.csv")

nat_econ <- read_csv("../Data/econ.csv")
state_rdi <- read_csv("../Data/state_rdi.csv")

#build state df with econ, polls, and 
weeks <- 5
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
  
reg_df$period <- ifelse(reg_df$year %in% 1972:1984, "1972-1984",
                               ifelse(reg_df$year %in% 1988:1996, "1992-2000",
                                      ifelse(reg_df$year %in% 2000:2012, "2000-2012", 
                                             "2016-2020")))

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
  mutate(inc_pv = case_when((inc_party=="republican") ~ r_pv, 
                              (inc_party=="democrat") ~ d_pv)) %>% 
  mutate(chl_pv = case_when(chl_party == "democrat" ~ d_pv, 
                            chl_party == "republican" ~ r_pv))

reg_df <- na.omit(reg_df)

##fundamentals model
inc_fund_lm <- lm(inc_pv ~ rdi_q2 + gdp + period + state, data = reg_df %>% filter(year < 2020, incumbent_party == TRUE))
summary(inc_fund_lm)

chl_fund_lm <- lm(chl_pv ~ rdi_q2 + gdp + period + state, data = reg_df %>% filter(year < 2020, incumbent_party == FALSE))
summary(chl_fund_lm)

## polls model
inc_polls_lm <- lm(inc_pv ~ avg_poll, data = reg_df %>% filter(year < 2020, incumbent_party == TRUE))
summary(inc_polls_lm)

chl_polls_lm <- lm(chl_pv ~ avg_poll, data = reg_df %>% filter(year < 2020, incumbent_party == FALSE))
summary(chl_polls_lm)

## polls and fundamentals model
inc_plus_lm <- lm(inc_pv ~ rdi_q2 + gdp + period + state + avg_poll, data = reg_df %>% filter(year < 2020, incumbent_party == TRUE))
summary(inc_plus_lm)

chl_plus_lm <- lm(chl_pv ~ rdi_q2 + gdp + period + state + avg_poll, data = reg_df %>% filter(year < 2020, incumbent_party == FALSE))
summary(chl_plus_lm)

## in sample fit
mean(abs(inc_fund_lm$residuals))
mean(abs(chl_fund_lm$residuals))

mean(abs(inc_polls_lm$residuals))
mean(abs(chl_polls_lm$residuals))

mean(abs(inc_plus_lm$residuals))
mean(abs(chl_plus_lm$residuals))

## out of sample fit and comparisons to actual winners for ensemble weighting
# load in and join electoral college values
elec_col <- read_csv("../Data/ElectoralCollegePost1948_adj.csv") %>% filter(Year> 1968)
elec_col <- elec_col[,1:3]
reg_df <- reg_df %>% left_join(elec_col, by = c("year" = "Year", "state" = "State"))

# run out of sample fit to see how often states are called correctly
year_list <- seq(from = 1972, to = 2016, by = 4)
state_list <- unique(reg_df$state)

check_out <- function(year, state){
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
    
    #fundamental model out-of-sample prediction
    mod_econ_inc_ <- lm(inc_pv ~ rdi_q2 + gdp + period + state, data = inc_df )
    mod_econ_chl_ <- lm(chl_pv ~ rdi_q2 + gdp + period + state, data = chl_df )
    pred_econ_inc <- predict(mod_econ_inc_, inc_p)
    pred_econ_chl <- predict(mod_econ_chl_, chl_p)
  
    ##poll model out-of-sample prediction
    mod_poll_inc_ <- lm(inc_pv ~ avg_poll, data = inc_df)
    mod_poll_chl_ <- lm(chl_pv ~ avg_poll, data = chl_df)
    pred_poll_inc <- predict(mod_poll_inc_, inc_p)
    pred_poll_chl <- predict(mod_poll_chl_, chl_p)
    
    ##plus model out-of-sample prediction
    mod_plus_inc_ <- lm(inc_pv ~ avg_poll + rdi_q2 + gdp + period + state, data = inc_df)
    mod_plus_chl_ <- lm(chl_pv ~ avg_poll + rdi_q2 + gdp + period + state, data = chl_df)
    pred_plus_inc <- predict(mod_plus_inc_, inc_p)
    pred_plus_chl <- predict(mod_plus_chl_, chl_p)
    
    c(year, state,
                     econ_margin_error = (pred_econ_inc-pred_econ_chl) - (true_inc-true_chl),
                     poll_margin_error = (pred_poll_inc-pred_poll_chl) - (true_inc-true_chl),
                     plus_margin_error = (pred_plus_inc-pred_plus_chl) - (true_inc-true_chl),
                     econ_winner_correct = (pred_econ_inc > pred_econ_chl) == (true_inc > true_chl),
                     poll_winner_correct = (pred_poll_inc > pred_poll_chl) == (true_inc > true_chl),
                     plus_winner_correct = (pred_plus_inc > pred_plus_chl) == (true_inc > true_chl)
    )
  }else{
    c(NA, NA, NA, NA, NA, NA, NA, NA)
  }
}

outsamp_dflist <- sapply(year_list, function(year){sapply(state_list, function(state) check_out(year,state))})

outsamp_df <- cbind("year", "state","fund_margin_error",
                "polls_margin_error","plus_margin_error","fund_correct","polls_correct","plus_correct")

for(j in seq(from = 1, to = 12, by =1)){
  for(i in seq(from = 1, to = 402, by = 8)){
    start <- i
    stop <- i+7
    temp <- outsamp_dflist[start:stop, j]
    outsamp_df <- rbind(outsamp_df, temp)
  }
}

outsamp_df <- as.data.frame(outsamp_df)
rownames(outsamp_df) <- NULL
outsamp_df <- outsamp_df %>% row_to_names(row_number=1)
outsamp_df_clean <- na.omit(outsamp_df)

outsamp_df_clean$fund_margin_error <- as.numeric(as.character(outsamp_df_clean$fund_margin_error))
outsamp_df_clean$polls_margin_error <- as.numeric(as.character(outsamp_df_clean$polls_margin_error))
outsamp_df_clean$plus_margin_error <- as.numeric(as.character(outsamp_df_clean$plus_margin_error))

colMeans(abs(outsamp_df_clean[,3:5]))

length(outsamp_df_clean$fund_correct[outsamp_df_clean$fund_correct== TRUE])/length(outsamp_df_clean$fund_correct)
length(outsamp_df_clean$polls_correct[outsamp_df_clean$polls_correct== TRUE])/length(outsamp_df_clean$polls_correct)
length(outsamp_df_clean$plus_correct[outsamp_df_clean$plus_correct== TRUE])/length(outsamp_df_clean$plus_correct)


## 2020 predictions
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

poll_2020_clean <- poll_2020_df %>% filter(weeks_left < 50, weeks_left >=weeks) %>% 
  group_by(state, candidate_name, party) %>%
  summarize(avg_poll = mean(avg_support)) %>% filter(state != "National") %>% 
  select(state, candidate_name, avg_poll, party) %>% mutate(period = "2016-2020") %>%
  mutate(incumbent_party = case_when(candidate_name == "Joseph R. Biden Jr." ~ FALSE, 
                                     candidate_name == "Donald Trump" ~ TRUE)) %>%
  filter(state != "NE-1", state != "NE-2", state != "ME-1", state != "ME-2")

NE_df <- colnames(poll_2020_clean)
NE_df <- rbind.data.frame(NE_df, c("Nebraska", "Donald Trump", 57.9, "republican","2016-2020",TRUE))
NE_df <- NE_df %>% row_to_names(row_number=1)
temp <- cbind("Nebraska", "Joseph R. Biden Jr.", 40.3, "democrat","2016-2020",FALSE)
temp <- as.data.frame(temp)
colnames(temp) <- colnames(NE_df)
NE_df <- rbind(NE_df, temp)

DC_df <- colnames(poll_2020_clean)
DC_df <- rbind.data.frame(DC_df, c("District of Columbia", "Donald Trump", 5.6, "republican","2016-2020",TRUE))
DC_df <- DC_df %>% row_to_names(row_number=1)
temp <- cbind("District of Columbia", "Joseph R. Biden Jr.", 91.0, "democrat","2016-2020",FALSE)
temp <- as.data.frame(temp)
colnames(temp) <- colnames(DC_df)
DC_df <- rbind(DC_df, temp)

IL_df <- colnames(poll_2020_clean)
IL_df <- rbind.data.frame(IL_df, c("Illinois", "Donald Trump", 38, "republican","2016-2020",TRUE))
IL_df <- IL_df %>% row_to_names(row_number=1)
temp <- cbind("Illinois", "Joseph R. Biden Jr.", 60.5, "democrat","2016-2020",FALSE)
temp <- as.data.frame(temp)
colnames(temp) <- colnames(IL_df)
IL_df <- rbind(IL_df, temp)

RI_df <- colnames(poll_2020_clean)
RI_df <- rbind.data.frame(RI_df, c("Rhode Island", "Donald Trump", 36.5, "republican","2016-2020",TRUE))
RI_df <- RI_df %>% row_to_names(row_number=1)
temp <- cbind("Rhode Island", "Joseph R. Biden Jr.", 62.1, "democrat","2016-2020",FALSE)
temp <- as.data.frame(temp)
colnames(temp) <- colnames(RI_df)
RI_df <- rbind(RI_df, temp)

SD_df <- colnames(poll_2020_clean)
SD_df <- rbind.data.frame(SD_df, c("South Dakota", "Donald Trump", 58.8, "republican","2016-2020",TRUE))
SD_df <- SD_df %>% row_to_names(row_number=1)
temp <- cbind("South Dakota", "Joseph R. Biden Jr.", 39.7, "democrat","2016-2020",FALSE)
temp <- as.data.frame(temp)
colnames(temp) <- colnames(SD_df)
SD_df <- rbind(SD_df, temp)

WY_df <- colnames(poll_2020_clean)
WY_df <- rbind.data.frame(WY_df, c("Wyoming", "Donald Trump", 68.0, "republican","2016-2020",TRUE))
WY_df <- WY_df %>% row_to_names(row_number=1)
temp <- cbind("Wyoming", "Joseph R. Biden Jr.", 28.7, "democrat","2016-2020",FALSE)
temp <- as.data.frame(temp)
colnames(temp) <- colnames(WY_df)
WY_df <- rbind(WY_df, temp)

poll_2020_clean <- as.data.frame(poll_2020_clean)


poll_2020_clean <- rbind(poll_2020_clean, NE_df)
poll_2020_clean <- rbind(poll_2020_clean, DC_df)
poll_2020_clean <- rbind(poll_2020_clean, IL_df)
poll_2020_clean <- rbind(poll_2020_clean, SD_df)
poll_2020_clean <- rbind(poll_2020_clean, WY_df)
poll_2020_clean <- rbind(poll_2020_clean, RI_df)

gdp <- rep(-0.0949, 102)
poll_2020_clean$gdp <- gdp
poll_2020_clean$avg_poll <- as.numeric(poll_2020_clean$avg_poll)

pred_data_df <- poll_2020_clean %>% left_join(state_rdi[,c(1,290)], c("state" = "State"))
colnames(pred_data_df) <- c("state", "candidate_name","avg_poll","party", "period", "incumbent_party","gdp", "rdi_q2")

pred_fund_inc <- predict(inc_fund_lm, pred_data_df %>% filter(incumbent_party == TRUE))
pred_fund_chl <- predict(chl_fund_lm, pred_data_df %>% filter(incumbent_party == FALSE))
pred_polls_inc <- predict(inc_polls_lm, pred_data_df %>% filter(incumbent_party == TRUE))
pred_polls_chl <- predict(chl_polls_lm, pred_data_df %>% filter(incumbent_party == FALSE))
pred_plus_inc <- predict(inc_plus_lm, pred_data_df %>% filter(incumbent_party == TRUE))
pred_plus_chl <- predict(chl_plus_lm, pred_data_df %>% filter(incumbent_party == FALSE))

pred_df <- cbind.data.frame(unique(pred_data_df$state), pred_fund_inc, pred_fund_chl, 
                 pred_polls_inc, pred_polls_chl, 
                 pred_plus_inc, pred_plus_chl)

pred_win <- cbind.data.frame(unique(pred_data_df$state), 
                             case_when(pred_df$pred_fund_inc > pred_df$pred_fund_chl ~ "Trump",
                                       pred_df$pred_fund_inc < pred_df$pred_fund_chl ~ "Biden"),
                             case_when(pred_df$pred_polls_inc > pred_df$pred_polls_chl ~ "Trump",
                                       pred_df$pred_polls_inc < pred_df$pred_polls_chl ~ "Biden"),
                             case_when(pred_df$pred_plus_inc > pred_df$pred_plus_chl ~ "Trump",
                                       pred_df$pred_plus_inc < pred_df$pred_plus_chl ~ "Biden"))

colnames(pred_win) <- c("State", "fundamentals", "polls", "plus")
pred_win_ec <- pred_win %>% left_join(elec_col %>% filter(Year==2020), by = ("State"="State"))
colnames(pred_win_ec) <- c("state", "fundamentals", "polls", "plus","year","EC")

## electoral college
fund_ec <- aggregate(pred_win_ec$EC, by=list(fundamental=pred_win_ec$fundamentals), FUN=sum)
polls_ec <- aggregate(pred_win_ec$EC, by=list(fundamental=pred_win_ec$polls), FUN=sum)
plus_ec <- aggregate(pred_win_ec$EC, by=list(fundamental=pred_win_ec$plus), FUN=sum)

## plots
plot_usmap(data = pred_win_ec, regions = "states", values = "fundamentals") +
  scale_fill_manual(values = c("blue", "red"), 
                    name = "Projected winner by fundamentals model") +
  theme_void()

plot_usmap(data = pred_win_ec, regions = "states", values = "polls") +
  scale_fill_manual(values = c("blue", "red"), 
                    name = "Projected winner by fundamentals model") +
  theme_void()

plot_usmap(data = pred_win_ec, regions = "states", values = "plus") +
  scale_fill_manual(values = c("blue", "red"), 
                    name = "Projected winner by fundamentals model") +
  theme_void()








