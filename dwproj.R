# installing packages
install.packages('dplyr')
install.packages('dummies')
install.packages('caTools')
install.packages('data.table')
install.packages('roll')

# load libraries
library(dplyr)
library(dummies)
library(caTools)
library(purrr)
library(data.table)
library(roll)
library(party)

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

df_test <- df %>%
  group_by(Oppt, Pos) %>%
  arrange(weeks) %>%
  mutate(pref.fd.defense = rolling_average(FD.points, 1)) %>%
  mutate(roll_avg_def = rolling_average(FD.points, 4))

df_test <- df_test %>%
  group_by(playerID) %>%
  arrange(weeks) %>%
  mutate(FD.prev.points = rolling_average(FD.points, 1)) %>%
  mutate(roll_avg_FD.pts = rolling_average(FD.points, 4))

test <- df_test %>%
  select(name, playerID, Oppt, Pos, weeks, FD.points, pref.fd.defense, roll_avg_def, FD.prev.points, roll_avg_FD.pts) %>%
  group_by(Oppt, Pos) %>%
  arrange(weeks) %>%
  nest()

quarterback <- df_test %>%
  filter(Pos == "QB") %>%
  .[,c(1, 3:10, 14:27, 66, 68:71)]

runningback <- df_test %>%
  filter(Pos == "RB") %>%
  .[, c(1, 3:10, 21:44, 66, 68:71)]

wide_receiver <- df_test %>%
  filter(Pos == "WR") %>%
  .[, c(1, 3:10, 21:44, 66, 68:71)]

tight_end <- df_test %>%
  filter(Pos == "TE") %>%
  .[, c(1, 3:10, 28:34, 66, 68:71)]

kicker <- df_test %>%
  filter(Pos == "PK") %>%
  .[, c(1, 3:10, 45:53, 66, 68:71)]



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






