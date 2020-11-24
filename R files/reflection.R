library(tidyverse)
library(ggplot2)
library(usmap)
library(janitor)
library(usmap)
library(data.table)
library(geofacet) ## map-shaped grid of ggplots
library(statebins)

setwd("~/Desktop/Gov 1347/Blog/R files")

results_df <- read.csv("../Data/prediction.csv")

ec_results <- aggregate(results_df$EC, results_df %>% dplyr::select(win, sim), FUN = sum)
biden_pv <- aggregate(results_df$biden_v, results_df %>% dplyr::select(sim), FUN = sum)
trump_pv <- aggregate(results_df$trump_v, results_df %>% dplyr::select(sim), FUN = sum)

avg_popvote_state <- results_df %>% mutate(trump_pv = trump_v/(trump_v+biden_v),
                                           biden_pv = biden_v/(trump_v+biden_v)) %>%
  group_by(state) %>%
  summarize(trump_pv = mean(trump_pv), biden_pv = mean(biden_pv)) %>%
  mutate(win = case_when(trump_pv > biden_pv ~ "Trump",
                         biden_pv > trump_pv ~ "Biden"))

avg_popvote_table <- avg_popvote_state %>% dplyr::select(state, trump_pv, biden_pv)
colnames(avg_popvote_table) <- c("State", "Trump Popular Vote", "Biden Popular Vote")


ec_wins <- ec_results %>% spread(win, x) %>% mutate(winner = case_when(Biden > Trump ~ "Biden", 
                                                                       Trump > Biden ~ "Trump", 
                                                                       Biden == Trump ~ "Tie"))

victors <- ec_wins %>% group_by(winner) %>% count()

actual_results <- read_csv("../Data/popvote_bystate_1948-2020.csv")
results_2020 <- actual_results %>% filter(year == 2020)
results_2020$state <- c(state.abb, 'DC')[match(results_2020$state, c(state.name, 'District of Columbia'))]
results_2020 <- results_2020 %>% mutate(biden_pv = D/total, 
                                        trump_pv = R/total, 
                                        winner =case_when(biden_pv > trump_pv ~ "Biden",
                                                          trump_pv > biden_pv ~ "Trump",
                                                          biden_pv == trump_pv ~ "Tie"))


biden_df <- results_2020 %>% dplyr::select(state, biden_pv) %>% 
  left_join(avg_popvote_state %>% dplyr::select(state,biden_pv), by = c("state" = "state"))
colnames(biden_df) <- c("state", "actual", "predicted")

trump_df <- results_2020 %>% dplyr::select(state, trump_pv) %>% 
  left_join(avg_popvote_state %>% dplyr::select(state,trump_pv), by = c("state" = "state"))
colnames(trump_df) <- c("state", "actual", "predicted")



biden_state_vote <- ggplot(biden_df, aes  (x = predicted, y = actual, label = state)) + 
  geom_text() + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") + 
  geom_smooth(method = "lm", se = FALSE, color = "deepskyblue") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "grey"),
        legend.key = element_rect(fill="transparent", colour=NA),
        legend.position = "none") +
  labs(x = "Predicted Biden Vote Share", y="Actual Biden Vote Share", title = "Predicted vs Actual Biden Vote Shares")

trump_state_vote <- ggplot(trump_df, aes  (x = predicted, y = actual, label = state)) + 
  geom_text() + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") + 
  geom_smooth(method = "lm", se = FALSE, color = "firebrick") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "grey"),
        legend.key = element_rect(fill="transparent", colour=NA),
        legend.position = "none") + 
  labs(x = "Predicted Trump Vote Share", y="Actual Trump Vote Share", title = "Predicted vs Actual Trump Vote Shares")


state_votes <- gridExtra::grid.arrange(biden_state_vote, trump_state_vote, ncol = 2)

biden_df <- biden_df %>% mutate(margin = predicted - actual)
trump_df <- trump_df %>% mutate(margin = predicted - actual)

biden_margin_map <- plot_usmap(data = biden_df, regions = "states", values = "margin") +
  scale_fill_gradient2(
    high = "firebrick2", 
    mid = "white",
    low = "dodgerblue2", 
    breaks = c(-0.15,0,0.15), 
    limits = c(-0.15,0.15),
    name = "Biden Margin"
  ) +
  theme_void() +
  theme(strip.text = element_text(size = 12),
        aspect.ratio=1, legend.position = "bottom") + 
  labs(title = "Biden Vote Margins, Predicted Minus Actual", fill = "") 

trump_margin_map <- plot_usmap(data = trump_df, regions = "states", values = "margin") +
  scale_fill_gradient2(
    high = "dodgerblue2", 
    mid = "white",
    low = "firebrick2", 
    breaks = c(-0.15,0,0.15), 
    limits = c(-0.15,0.15),
    name = "Trump Margin"
  ) +
  theme_void() +
  theme(strip.text = element_text(size = 12),
        aspect.ratio=1, legend.position = "bottom") + 
  labs(title = "Trump Vote Margins, Predicted Minus Actual", fill = "") 

state_votes <- gridExtra::grid.arrange(biden_margin_map, trump_margin_map, ncol = 2)





pred_map <- avg_popvote_state %>% 
  ggplot(aes(state = state, fill = fct_relevel(win, "Trump", "Biden"))) +
  geom_statebins() +
  theme_statebins() +
  labs(title = "2020 Presidential Election Based on Average Popular Vote Share",
       subtitle = "From 10000 Simulations",
       fill = "") +
  scale_fill_manual(values=c("#619CFF", "#CCCCCC", "#F8766D"), breaks = c("Biden", "N/A", "Trump"))


real_map <- results_2020 %>% 
  ggplot(aes(state = state, fill = fct_relevel(winner, "Trump", "Biden"))) +
  geom_statebins() +
  theme_statebins() +
  labs(title = "Actual Outcomes of the 2020 Election",
       fill = "") +
  scale_fill_manual(values=c("#619CFF", "#CCCCCC", "#F8766D"), breaks = c("Biden", "N/A", "Trump"))





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
  labs(title = "Biden Electoral College Votes from 10000 Simulations") + xlim(0,538) + 
  geom_vline(linetype = "dashed", xintercept = 306)


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
  labs(title = "Trump Electoral College Votes from 10000 Simulations") + xlim(0,538) + 
  geom_vline(linetype = "dashed", xintercept = 232)

ec_plot <- gridExtra::grid.arrange(trump_ec_plot, biden_ec_plot)



check_results <-  results_df %>% 
  left_join(results_2020 %>% dplyr::select(state, winner), by = c("state" = "state")) %>%
  mutate(correct = case_when(win == winner ~ 1, win != winner ~ 0))

class_accuracy <- aggregate(check_results$correct, by=list(sim=check_results$sim), FUN=sum) 

accuracy_hist <- ggplot(class_accuracy, aes(x = x), alpha = 0.75) +
  geom_histogram(bins = max(class_accuracy$x) - min(class_accuracy$x), 
                 aes(y = ..density..),position = 'identity', , fill = "forestgreen") + 
  labs(x="Number of States Predicted Correctly", y="Frequency") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "grey"),
        legend.key = element_rect(fill="transparent", colour=NA),
        legend.position = "none")+
  labs(title = "Classification Accuracy For Each Simulation") 
accuracy_hist






## sims
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
turnout_pred_df <- turnout_scaled %>% filter(year == 2020)
turnout_2020_pred <- as.data.frame(predict(turnout_model, turnout_pred_df, type = "response"))
turnout_2020_pred$state <- turnout_pred_df$state
colnames(turnout_2020_pred) <- c("turnout", "state")

turnout_check <- turnout_2020_pred %>% 
  left_join(results_2020 %>% dplyr::select(state, total), by = ("state" = "state")) %>% 
  mutate(margin = turnout - total)

turnout_plot <- ggplot(turnout_check, aes  (x = turnout, y = total, label = state)) + 
  geom_text() + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") + 
  geom_smooth(method = "lm", se = FALSE, color = "black") + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(),
        panel.background = element_blank(), 
        axis.line = element_line(colour = "grey"),
        legend.key = element_rect(fill="transparent", colour=NA),
        legend.position = "none") + 
  labs(x = "Predicted Turnout", y="Actual Turnout", title = "Predicted vs Actual Turnout") +
  scale_x_continuous(trans='log10') +
  scale_y_continuous(trans='log10')
turnout_plot


turnout_RMSE = sqrt(sum((turnout_check$turnout - turnout_check$total) ** 2)/51)
biden_RMSE = sqrt(sum((biden_df$margin)**2)/51)
trump_RMSE = sqrt(sum((trump_df$margin)**2)/51)

state_results <- results_df %>% group_by(state) %>% count(win)
state_ratios <- state_results %>% filter(win == "Trump") %>% 
  full_join(state_results %>% filter(win == "Biden"), by = c("state" = "state"))
colnames(state_ratios) = c("state", "trump", "trump_wins", "biden", "biden_wins")
state_ratios <- state_ratios %>% dplyr::select(state, trump_wins, biden_wins) %>% 
  replace(is.na(.),0) %>%
  mutate(trump_win_frac = trump_wins/10000, biden_win_frac = biden_wins/10000) %>%
  left_join(results_2020 %>% dplyr::select(state, winner), by = c("state" = "state")) %>%
  mutate(win_dummy = case_when(winner == "Trump" ~ 1, winner == "Biden" ~ 0))

brier = sum((state_ratios$trump_win_frac - state_ratios$win_dummy) ** 2)/51

stat_list <- c("Classification Accuracy","Turnout RMSE", "Biden RMSE", "Trump RMSE", "Brier Score")
val <- c(46/51, turnout_RMSE, biden_RMSE, trump_RMSE, brier)

model_stats <- cbind.data.frame(stat_list, val)

brier_df <- state_ratios %>% dplyr::select(state, trump_win_frac, win_dummy) %>%
  mutate(brier_contribution = (trump_win_frac - win_dummy)**2)


pv_df <- biden_pv %>% left_join(trump_pv, by = c("sim" = "sim"))
colnames(pv_df) <- c("sim", "biden_votes", "trump_votes")
pv_df <- pv_df %>% mutate(biden_pv = biden_votes/(biden_votes+trump_votes), 
                          trump_pv = trump_votes/(biden_votes+trump_votes))

mean(pv_df$biden_pv)
mean(pv_df$trump_pv)
