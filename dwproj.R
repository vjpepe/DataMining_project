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

# read
data_url = "https://raw.githubusercontent.com/zzhangusf/Predicting-Fantasy-Football-Points-Using-Machine-Learning/master/data/aggregated_2015.csv"
df = read.csv(data_url)

# sort by playerID, weeks
df = df %>% 
  arrange(playerID, weeks)

tail(df)

# Feature Engineering

######### Game Char indicators

## boolean for home or away game
df = df %>% 
  mutate(home = ifelse(h.a == 'h', TRUE, FALSE))

# creating dummies for all opponent teams
oppts = dummy(df$Oppt, sep = ".")

# helper teams data frame
teams = data.frame()
# list of all teams
team_list = c('ARI', 'ATL', 'BAL', 'BUF', 'CAR', 'CHI', 'CIN', 'CLE', 'DAL', 'DEN', 'DET',
              'GB', 'HOU', 'IND', 'JAC', 'KC', 'MIA', 'MIN', 'NE', 'NO', 'NYG', 'NYJ',
              'OAK', 'PHI', 'PIT', 'SD', 'SEA', 'SF', 'STL', 'TB', 'TEN', 'WAS')

tail(df)

# looping through each row and setting 1 to the team
for(i in 1:length(df$Team)) {
  temp = ifelse(team_list == df$Team[i], 1, 0)
  teams = rbind(teams, temp)
}

#idx = 1:range(length(df$Team))
rownames(teams) = 1:range(length(df$Team))
colnames(teams) = team_list

# combining df + oppts + teams
df = cbind(df, oppts, teams)

# list of all features created so far
features = c('home', colnames(oppts), team_list)

#### random testing
testign = df %>% 
  select(playerID, weeks, name, Oppt.NYG, ATL, BAL) %>% 
  filter(name == 'K.Cousins')

####### rolling average function
rolling_average = function(dframe, window) {
  x = runmean(dframe, window)
  x = roll_mean(dframe, width = window, min_obs = 1)
  return(shift(x, 1))
  #return(x)
}

# test = df[ ,c(1,2,9,13)]

test2 = df %>% 
  group_by(playerID) %>% 
  mutate(test_val2 = rolling_average(FD.points, 4))

testing = test2 %>% 
  select(weeks, playerID, FD.points, test_val2)  %>% 
  filter(playerID == '00-0031407')


##### branch testing #####






