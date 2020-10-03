#install packages
install.packages("plm")
install.packages("sem")

# import libraries
library(tidyverse)
library(ggplot2)
library(usmap)
library(plm)
library(sem)
library(data.table)

# set working directory
setwd("~/Desktop/Gov 1347/Elections/Posts")

## read in state pop vote
pvstate_df <- read_csv("../Data/popvote_bystate_1948-2016.csv")

## shapefile of states from `usmap` library
## note: `usmap` merges this internally, but other packages may not!
states_map <- usmap::us_map()
unique(states_map$abbr)

## read in  economic data
us_econ <- read_csv("../Data/econ.csv")
state_econ <- read_csv("../Data/state_econ.csv")

##set election cycles
years <- seq(from=1960, to=2016, by=4)

## build data frame for national level regression
nat_econ_df <- na.omit(us_econ[c("year","quarter","GDP_growth_qt","RDI_growth","unemployment")])
nat_econ_df <- nat_econ_df %>% filter(year %in% years, quarter<3)
setDT(nat_econ_df)   # coerce to data.table
nat_econ_df <- dcast(nat_econ_df, year ~ quarter, value.var = c("GDP_growth_qt", "RDI_growth", "unemployment"))

pv_nat_col <- pv_nat %>% filter(incumbent_party == TRUE, year>=1960) %>% select(year, winner, pv2p)
nat_econ_df$vote_margin <- pv_nat_col$pv2p
nat_econ_df$RDI_growth_1 <- 100*nat_econ_df$RDI_growth_1
nat_econ_df$RDI_growth_2 <- 100*nat_econ_df$RDI_growth_2

nat.linreg <- lm(vote_margin~GDP_growth_qt_1+GDP_growth_qt_2+RDI_growth_1+RDI_growth_2, nat_econ_df)
summary(nat.linreg)


##state by state regressions
#reshape state RDI growth
state_econ_df <- state_econ[,1:3]
colnames(state_econ_df) <- c("state","Q1","Q2")

for(i in seq(from=4, to=64, by=4)){
  start <- i
  stop <- 1+i
  temp <- state_econ[,c(1,start:stop)]
  colnames(temp) <-  c("state","Q1","Q2")
  state_econ_df <- rbind(state_econ_df, temp)
}

year_col=rep(1952, 51)
years = seq(from=1952,to=2016, by=4)

us_econ_clean = us_econ %>% filter(year %in% years, quarter<3)
gdp_1 = us_econ_clean %>% filter(quarter==1)
gdp_1 = gdp_1$GDP_growth_qt
gdp_2 = us_econ_clean %>% filter(quarter==2)
gdp_2 = gdp_2$GDP_growth_qt

gdp_append_1 = rep(gdp_1[1],51)
gdp_append_2 = rep(gdp_2[1],51)

for(i in seq(from=2, to=length(years),by=1)){
  new_year = rep(years[i],51)
  g_1 = rep(gdp_1[i],51)
  g_2 = rep(gdp_2[i],51)
  
  year_col= append(year_col, new_year)
  gdp_append_1 = append(gdp_append_1,g_1)  
  gdp_append_2 = append(gdp_append_2,g_2)
}

state_econ_df$year = year_col
state_econ_df$gdp_1 = gdp_append_1
state_econ_df$gdp_2 = gdp_append_2

## create state by state regression dataframe by appending econ data to incumbent vote share
pv_nat <- read_csv("../Data/popvote_1948-2016.csv")
incumbent_party <- pv_nat %>% select(party,year, incumbent_party) %>%  filter(incumbent_party==TRUE)

state_reg_df <- state_econ_df
state_reg_df$vote_share <- NA


for (i in seq(from=1, to=nrow(state_reg_df), by=1)){
    y <- state_reg_df$year[i]
    inc <- incumbent_party$party[which(incumbent_party$year == y)]
    
    if (inc == "republican"){
        share <- pvstate_df %>% filter(year == y, state== state_reg_df$state[i]) %>% select(R_pv2p)
    } else {
        share <- pvstate_df %>% filter(year == y, state== state_reg_df$state[i]) %>% select(D_pv2p)
    }
  
    state_reg_df$vote_share[i] <- as.numeric(share)
}

state_reg <- na.omit(state_reg_df)
state_reg <- state_reg %>% filter(year>1959)

fixed_effects <- lm(vote_share ~ Q1+Q2+gdp_1+gdp_2+state, state_reg)
summary(fixed_effects)
hist(residuals(fixed_effects))

econ_2020 <- state_econ[,c(1,70)]
colnames(econ_2020) <- c("state","Q1")
econ_2020$Q2 <- rep(6.41,51)
econ_2020$gdp_1 <- rep(0.31926,51)
econ_2020$gdp_2 <- rep(-9.54111,51)

prediction <- as.data.frame(predict(fixed_effects, econ_2020))
prediction$state <- econ_2020$state
colnames(prediction) <- c("inc_vote_share", "state")
prediction$margin <- 2*prediction$inc_vote_share-100
prediction <- prediction %>% mutate(winner = ifelse(margin>0, "Trump","Biden"))

plot_usmap(data = prediction, regions = "states", values = "winner") +
  scale_fill_manual(values = c("red","blue"), name = "Predicted State Winner") +
  theme_void() + labs(title="Predicted state winners using fixed effects regression")



## state by state regression
output <- transpose(as.data.frame(c(NA, NA)))
colnames(output) <- c("vote_share","state")

for(i in seq(from=1, to=51, by=1)){
  temp_state <- unique(state_reg$state)[i]
  temp_df <- state_reg %>% filter(state==temp_state)
  temp_model <- lm(vote_share~Q1+Q2+gdp_1+gdp_2, temp_df)
  
  out_val <- as.data.frame(predict(temp_model, econ_2020[which(econ_2020$state == temp_state),]))
  out_val$state <-temp_state
  colnames(out_val) <- c("vote_share","state")
  output <- rbind(output,out_val)
}

output <- na.omit(output)
output <- output %>% mutate(vote_margin = 2*vote_share-100)
output <- output %>% mutate(winner = if_else(vote_margin>0, "Trump", "Biden"))
plot_usmap(data = output, regions = "states", values = "winner") +
  scale_fill_manual(values = c("blue","red"), name = "Predicted State Winner") +
  theme_void() + labs(title="Predicted state winners based on state level regressions")

## electoral college calculations
ec <- read_csv("../Data/ElectoralCollegePost1948.csv")
ec <- ec[,c(1,21)]
colnames(ec) <- c("state","EC")

fixed_effects_ec <- prediction %>% left_join(ec, "state")
indep_ec <- output %>% left_join(ec, "state")

fixed_effects_biden <- fixed_effects_ec %>% filter(winner=="Biden")
fixed_effects_biden <- sum(fixed_effects_biden$EC)

fixed_effects_trump <- fixed_effects_ec %>% filter(winner=="Trump")
fixed_effects_trump <- sum(fixed_effects_trump$EC)


indep_biden <- indep_ec %>% filter(winner=="Biden")
indep_biden <- sum(indep_biden$EC)

indep_trump <- indep_ec %>% filter(winner=="Trump")
indep_trump <- sum(indep_trump$EC)










## not used, garbage fit
## version with unemployemnent instead of personal income
local <- read_csv("../Data/local.csv")
years <- seq(from=1976, to=2016, by=4)
names(local)[names(local) == "State and area"] <- "state"
local <- local %>% filter(state %in% unique(state_reg$state), Month < 12,  Year %in% years)
unemployed <- local %>% group_by(Year, state) %>% summarise_at(vars(Unemployed_prce),              
                                                        list(name = mean))   
unemployed_reg <- unemployed
unemployed_reg$vote_share <- NA

for (i in seq(from=1, to=nrow(unemployed_reg), by=1)){
  y <- unemployed_reg$Year[i]
  inc <- incumbent_party$party[which(incumbent_party$year == y)]
  
  if (inc == "republican"){
    share <- pvstate_df %>% filter(year == y, state== unemployed_reg$state[i]) %>% select(R_pv2p)
  } else {
    share <- pvstate_df %>% filter(year == y, state== unemployed_reg$state[i]) %>% select(D_pv2p)
  }
  
  unemployed_reg$vote_share[i] <- as.numeric(share)
}

unemployed_reg <- na.omit(unemployed_reg)

unemploy <- lm(vote_share~name, unemployed_reg)
summary(unemploy)





##
# import libraries
library(tidyverse)
library(ggplot2)
library(usmap)
library(plm)
library(data.table)
library(knitr)
library(pander)
library(broom)
library(tibble)
library(dplyr)
library(tidyr)
library(purrr)


## read in state pop vote
pvstate_df <- read_csv("../Data/popvote_bystate_1948-2016.csv")
pv_nat <- read_csv("../Data/popvote_1948-2016.csv")

## shapefile of states from `usmap` library
## note: `usmap` merges this internally, but other packages may not!
states_map <- usmap::us_map()
unique(states_map$abbr)

## read in  economic data
us_econ <- read_csv("../Data/econ.csv")
state_econ <- read_csv("../Data/state_econ.csv")

##set election cycles
years <- seq(from=1960, to=2016, by=4)

## build data frame for national level regression
nat_econ_df <- na.omit(us_econ[c("year","quarter","GDP_growth_qt","RDI_growth","unemployment")])
nat_econ_df <- nat_econ_df %>% filter(year %in% years, quarter<3)
setDT(nat_econ_df)   # coerce to data.table
nat_econ_df <- dcast(nat_econ_df, year ~ quarter, value.var = c("GDP_growth_qt", "RDI_growth", "unemployment"))

pv_nat_col <- pv_nat %>% filter(incumbent_party == TRUE, year>=1960) %>% select(year, winner, pv2p)
nat_econ_df$vote_margin <- pv_nat_col$pv2p
nat_econ_df$RDI_growth_1 <- 100*nat_econ_df$RDI_growth_1
nat_econ_df$RDI_growth_2 <- 100*nat_econ_df$RDI_growth_2

nat.linreg <- lm(vote_margin~1+GDP_growth_qt_1+GDP_growth_qt_2+RDI_growth_1+RDI_growth_2, nat_econ_df) 
pred_df <- transpose(as.data.frame(c(0.00635502610617094, 0.0972422967358656,0.31926,-9.54111)))
colnames(pred_df) <- c("RDI_growth_1","RDI_growth_2","GDP_growth_qt_1","GDP_growth_qt_2")

pred_out <- predict(nat.linreg, pred_df)



