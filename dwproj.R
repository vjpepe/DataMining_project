# installing packages
install.packages('tidyverse')
install.packages('dummies')
install.packages('caTools')
install.packages('data.table')
install.packages('roll')
install.packages('imputeTS')

# load libraries
library(tidyverse)
library(dummies)
library(caTools)
library(purrr)
library(data.table)
library(roll)
library(party)
library(imputeTS)
library(tidymodels)

# read
data_url = "https://raw.githubusercontent.com/zzhangusf/Predicting-Fantasy-Football-Points-Using-Machine-Learning/master/data/aggregated_2015.csv"
df = read.csv(data_url)

# sort by playerID, weeks
df = df %>%
  arrange(playerID, weeks, Team, Pos)
tail(df)

# Feature Engineering


######### Game Char indicators


## boolean for home or away game

df = df %>%
  mutate(home = ifelse(h.a == 'h', TRUE, FALSE))


rolling_average = function(dframe, window) {
  x = runmean(dframe, window)
  x = roll_mean(dframe, width = window, min_obs = 1)
  return(shift(x, 1))
  #return(x)
}

new_df <- df %>%
  group_by(Oppt, Pos) %>%
  arrange(weeks) %>%
  mutate(pref.fd.defense = rolling_average(FD.points, 1)) %>%
  mutate(roll_avg_def = rolling_average(FD.points, 4))

new_df <- new_df %>%
  group_by(playerID) %>%
  arrange(weeks) %>%
  mutate(FD.prev.points = rolling_average(FD.points, 1)) %>%
  mutate(roll_avg_FD.pts = rolling_average(FD.points, 4))


# was just testing to see if our data nested correctly

# test <- df_test %>%
#   select(name, playerID, Oppt, Pos, weeks, FD.points, pref.fd.defense, roll_avg_def, FD.prev.points, roll_avg_FD.pts) %>%
#   group_by(Oppt, Pos) %>%
#   arrange(weeks) %>%
#   nest()


# create separate dfs for each position and standardize each of them

# Quarterbacks
quarterback <- new_df %>%
  filter(Pos == "QB") %>%
  .[,c(1, 3:10, 14:27, 66, 68:71)] %>%
  na_replace(0)

qb_standard <- quarterback[, c(2, 8:28)]

qb_standard[, 2:22] <- scale(quarterback[, 8:28], center = T, scale = T)

# Split into test and train
qb_split <- qb_standard %>% 
  initial_split()

qb.train <- training(qb_split)
qb.test <- testing(qb_split)


# Running backs
runningback <- new_df %>%
  filter(Pos == "RB") %>%
  .[, c(1, 3:10, 21:44, 66, 68:71)] %>% 
  na_replace(0)

rb_standard <- runningback[, c(2, 8:38)]

rb_standard[, 2:32] <- scale(runningback[, 8:38], center = T, scale = T)

# Split into test and train
rb_split <- rb_standard %>% 
  initial_split()

rb.train <- training(rb_split)
rb.test <- testing(rb_split)


## Wide Recievers
wide_receiver <- new_df %>%
  filter(Pos == "WR") %>%
  .[, c(1, 3:10, 21:44, 66, 68:71)] %>% 
  na_replace(0)

wr_standard <- wide_receiver[, c(2, 8:38)]

wr_standard[, 2:32] <- scale(wide_receiver[, 8:38], center = T, scale = T)

# Split into test and train
wr_split <- wr_standard %>% 
  initial_split()

wr.train <- training(wr_split)
wr.test <- testing(wr_split)


# Tight Ends
tight_end <- new_df %>%
  filter(Pos == "TE") %>%
  .[, c(1, 3:10, 28:34, 66, 68:71)] %>% 
  na_replace(0)

te_standard <- tight_end[, c(2, 8:21)]

te_standard[, 2:15] <- scale(tight_end[, 8:21], center = T, scale = T)

# Split into test and train
te_split <- te_standard %>% 
  initial_split()

te.train <- training(te_split)
te.test <- testing(te_split)


# Kicker
kicker <- new_df %>%
  filter(Pos == "PK") %>%
  .[, c(1, 3:10, 45:53, 66, 68:71)] %>% 
  na_replace(0)

pk_standard <- kicker[, c(2, 8:23)]

pk_standard[, 2:17] <- scale(kicker[, 8:23], center = T, scale = T)

# Split into test and train
pk_split <- pk_standard %>% 
  initial_split()

pk.train <- training(pk_split)
pk.test <- testing(pk_split)


# fit the random forest
q1 <- cforest(FD.points~., data=quarterback, control=cforest_unbiased(mtry=2,ntree=50))
# get variable importance, based on mean decrease in accuracy
varimp(q1)

# fit the random forest
q2 <- cforest(FD.points~., data=runningback, control=cforest_unbiased(mtry=2,ntree=50))
# get variable importance, based on mean decrease in accuracy
varimp(q2)

# fit the random forest
q3 <- cforest(FD.points~., data=wide_receiver, control=cforest_unbiased(mtry=2,ntree=50))
# get variable importance, based on mean decrease in accuracy
varimp(q3)


# fit the random forest
q4 <- cforest(FD.points~., data=tight_end, control=cforest_unbiased(mtry=2,ntree=50))
# get variable importance, based on mean decrease in accuracy
varimp(q4)


# fit the random forest
q5 <- cforest(FD.points~., data=kicker, control=cforest_unbiased(mtry=2,ntree=50))
# get variable importance, based on mean decrease in accuracy
varimp(q5)






