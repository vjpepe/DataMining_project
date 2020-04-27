# installing packages
install.packages('tidyverse')
install.packages('dummies')
install.packages('caTools')
install.packages('data.table')
install.packages('roll')
install.packages('imputeTS')
install.packages('tidymodels')
install.packages('earth')

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
library(earth)

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

qb_standard <- quarterback

qb_standard[, 8:28] <- scale(quarterback[, 8:28], center = T, scale = T)

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

rb_standard <- runningback

rb_standard[, 8:38] <- scale(runningback[, 8:38], center = T, scale = T)


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

wr_standard <- wide_receiver

wr_standard[, 8:38] <- scale(wide_receiver[, 8:38], center = T, scale = T)

wr_standard <- wr_standard[-c(16, 26, 28)]

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

te_standard <- tight_end

te_standard[, 8:21] <- scale(tight_end[, 8:21], center = T, scale = T)

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

pk_standard <- kicker

pk_standard[, 8:23] <- scale(kicker[, 8:23], center = T, scale = T)

# Split into test and train
pk_split <- pk_standard %>% 
  initial_split()

pk.train <- training(pk_split)
pk.test <- testing(pk_split)


# build model
(q1.train <- earth(FD.points~., data = qb.train))

# estimate variable importance
(ev1 <- evimp)

# Removing features not needed based on variable importance
qb.train <- qb.train[ -c(1,3:5,7) ]
qb.test <- qb.test[ -c(1,3:5,7) ]


# build model
(q2.train <- earth(FD.points~., data=rb.train))

# estimate variable importance
(ev2 <- evimp)


# Removing features not needed based on variable importance
rb.train <- rb.train[ -c(1,3:5,7) ]
rb.test <- rb.test[ -c(1,3:5,7) ]

# build model (not running)
(q3.train <- earth(FD.points~., data = wr.train))

# estimate variable importance
(ev3 <- evimp)


# Removing features not needed based on variable importance
wr.train <- wr.train[ -c(1,3:5,7) ]
wr.test <- wr.test[ -c(1,3:5,7) ]

# build model
(q4.train <- earth(FD.points~., data=te.train))

# estimate variable importance
(ev4 <- evimp)


# Removing features not needed based on variable importance
te.train <- te.train[ -c(1,3:5,7) ]
te.test <- te.test[ -c(1,3:5,7) ]


# build model
(q5.train <- earth(FD.points~., data=pk.train))

# estimate variable importance
(ev5 <- evimp)

# Removing features not needed based on variable importance
pk.train <- pk.train[ -c(1,3:5,7) ]
pk.test <- pk.test[ -c(1,3:5,7) ]

set.seed(1234)

#### ------------------------------------------------------------------------------------------------
#### QUARTERBACKS

## Linear Model
linfit <- lm(FD.points ~ ., data = qb.train)
summary(linfit)

lm_spec <- linear_reg() %>% 
  set_engine(engine = "lm")

lm_fit <- lm_spec %>% 
  fit(FD.points ~ ., data = qb.train)


## Random Forest
rfspec <- rand_forest(mode = "regression") %>% 
  set_engine("ranger")

rf_fit <- rfspec %>% 
  fit(FD.points ~., data = qb.train)

## Ridge Regression
ridge_spec <- linear_reg(penalty = .10, mixture = 0) %>% # mixture = 0 meaning no L1 penalty 
  set_mode("regression") %>% 
  set_engine("glmnet") 


ridge_fit <- ridge_spec %>% 
  fit(FD.points ~ ., data = qb.train)

results_qb_train <- lm_fit %>% 
  predict(new_data = qb.train) %>% 
  mutate(truth = qb.train$FD.points, 
         model = "lm") %>% 
  bind_rows(rf_fit %>% 
              predict(new_data = qb.train) %>% 
              mutate(truth = qb.train$FD.points, 
                     model = "rf")) %>%
  bind_rows(ridge_fit %>% 
              predict(new_data = qb.train) %>% 
              mutate(truth = qb.train$FD.points, 
                     model = "Ridge"))

results_qb_test <- lm_fit %>% 
  predict(new_data = qb.test) %>% 
  mutate(truth = qb.test$FD.points, 
         model = "lm") %>% 
  bind_rows(rf_fit %>% 
              predict(new_data = qb.test) %>% 
              mutate(truth = qb.test$FD.points, 
                     model = "rf")) %>%
  bind_rows(ridge_fit %>% 
              predict(new_data = qb.test) %>% 
              mutate(truth = qb.test$FD.points, 
                     model = "Ridge"))

results_qb_train %>% 
  group_by(model) %>% 
  rmse(truth = truth, estimate = .pred)

results_qb_test %>% 
  group_by(model) %>% 
  rmse(truth = truth, estimate = .pred)

results_qb_test %>% 
  mutate(train = "testing") %>% 
  bind_rows(results_qb_train %>% 
              mutate(train = "training")) %>% 
  ggplot(aes(truth, .pred, color = model)) + 
  geom_abline(lty = 2, color = "grey80", size = 1.5) + 
  geom_point(alpha = .5) + 
  facet_wrap(~train) +
  ggtitle("Quarterback RMSE By Model")

#### ------------------------------------------------------------------------------------------------
#### Running Backs

## Linear Model
linfit_rb <- lm(FD.points ~ ., data = rb.train)
summary(linfit_rb)

lm_spec <- linear_reg() %>% 
  set_engine(engine = "lm")

lm_fit_rb <- lm_spec %>% 
  fit(FD.points ~ ., data = rb.train)


## Random Forest
rfspec_rb <- rand_forest(mode = "regression") %>% 
  set_engine("ranger")

rf_fit_rb <- rfspec_rb %>% 
  fit(FD.points ~., data = rb.train)

## Ridge Regression
ridge_spec_rb <- linear_reg(penalty = .10, mixture = 0) %>% # mixture = 0 meaning no L1 penalty 
  set_mode("regression") %>% 
  set_engine("glmnet") 


ridge_fit_rb <- ridge_spec_rb %>% 
  fit(FD.points ~ ., data = rb.train)

results_rb_train <- lm_fit_rb %>% 
  predict(new_data = rb.train) %>% 
  mutate(truth = rb.train$FD.points, 
         model = "lm") %>% 
  bind_rows(rf_fit_rb %>% 
              predict(new_data = rb.train) %>% 
              mutate(truth = rb.train$FD.points, 
                     model = "rf")) %>%
  bind_rows(ridge_fit_rb %>% 
              predict(new_data = rb.train) %>% 
              mutate(truth = rb.train$FD.points, 
                     model = "Ridge"))

results_rb_test <- lm_fit_rb %>% 
  predict(new_data = rb.test) %>% 
  mutate(truth = rb.test$FD.points, 
         model = "lm") %>% 
  bind_rows(rf_fit_rb %>% 
              predict(new_data = rb.test) %>% 
              mutate(truth = rb.test$FD.points, 
                     model = "rf")) %>%
  bind_rows(ridge_fit_rb %>% 
              predict(new_data = rb.test) %>% 
              mutate(truth = rb.test$FD.points, 
                     model = "Ridge"))

results_rb_train %>% 
  group_by(model) %>% 
  rmse(truth = truth, estimate = .pred)

results_rb_test %>% 
  group_by(model) %>% 
  rmse(truth = truth, estimate = .pred)

results_rb_test %>% 
  mutate(train = "testing") %>% 
  bind_rows(results_rb_train %>% 
              mutate(train = "training")) %>% 
  ggplot(aes(truth, .pred, color = model)) + 
  geom_abline(lty = 2, color = "grey80", size = 1.5) + 
  geom_point(alpha = .5) + 
  facet_wrap(~train) + 
  ggtitle("Running Back RMSE By Model")

#### ------------------------------------------------------------------------------------------------
#### Wide Receivers

## Linear Model
linfit_wr <- lm(FD.points ~ ., data = wr.train)
summary(linfit_wr)

lm_spec_wr <- linear_reg() %>% 
  set_engine(engine = "lm")

lm_fit_wr <- lm_spec_wr %>% 
  fit(FD.points ~ ., data = wr.train)


## Random Forest
rfspec_wr <- rand_forest(mode = "regression") %>% 
  set_engine("ranger")

rf_fit_wr <- rfspec_wr %>% 
  fit(FD.points ~., data = wr.train)

## Ridge Regression
ridge_spec_wr <- linear_reg(penalty = .10, mixture = 0) %>% # mixture = 0 meaning no L1 penalty 
  set_mode("regression") %>% 
  set_engine("glmnet") 


ridge_fit_wr <- ridge_spec_wr %>% 
  fit(FD.points ~ ., data = wr.train)

results_wr_train <- lm_fit_wr %>% 
  predict(new_data = wr.train) %>% 
  mutate(truth = wr.train$FD.points, 
         model = "lm") %>% 
  bind_rows(rf_fit_wr %>% 
              predict(new_data = wr.train) %>% 
              mutate(truth = wr.train$FD.points, 
                     model = "rf")) %>%
  bind_rows(ridge_fit_wr %>% 
              predict(new_data = wr.train) %>% 
              mutate(truth = wr.train$FD.points, 
                     model = "Ridge"))


results_wr_test <- lm_fit_wr %>% 
  predict(new_data = wr.test) %>% 
  mutate(truth = wr.test$FD.points, 
         model = "lm") %>% 
  bind_rows(rf_fit_wr %>% 
              predict(new_data = rb.test) %>% 
              mutate(truth = rb.test$FD.points, 
                     model = "rf")) %>%
  bind_rows(ridge_fit_wr %>% 
              predict(new_data = wr.test) %>% 
              mutate(truth = wr.test$FD.points, 
                     model = "Ridge"))

results_wr_train %>% 
  group_by(model) %>% 
  rmse(truth = truth, estimate = .pred)

results_wr_test %>% 
  group_by(model) %>% 
  rmse(truth = truth, estimate = .pred)

results_wr_test %>% 
  mutate(train = "testing") %>% 
  bind_rows(results_wr_train %>% 
              mutate(train = "training")) %>% 
  ggplot(aes(truth, .pred, color = model)) + 
  geom_abline(lty = 2, color = "grey80", size = 1.5) + 
  geom_point(alpha = .5) + 
  facet_wrap(~train) + 
  ggtitle("Wide Receiver RMSE By Model")

#### ------------------------------------------------------------------------------------------------
#### Tight Ends
## Linear Model
linfit_te <- lm(FD.points ~ ., data = te.train)
summary(linfit_te)

lm_spec <- linear_reg() %>% 
  set_engine(engine = "lm")

lm_fit_te <- lm_spec %>% 
  fit(FD.points ~ ., data = te.train)


## Random Forest
rfspec_te <- rand_forest(mode = "regression") %>% 
  set_engine("ranger")

rf_fit_te <- rfspec_te %>% 
  fit(FD.points ~., data = te.train)

## Ridge Regression
ridge_spec_te <- linear_reg(penalty = .10, mixture = 0) %>% # mixture = 0 meaning no L1 penalty 
  set_mode("regression") %>% 
  set_engine("glmnet") 


ridge_fit_te <- ridge_spec_te %>% 
  fit(FD.points ~ ., data = te.train)

results_te_train <- lm_fit_te %>% 
  predict(new_data = te.train) %>% 
  mutate(truth = te.train$FD.points, 
         model = "lm") %>% 
  bind_rows(rf_fit_te %>% 
              predict(new_data = te.train) %>% 
              mutate(truth = te.train$FD.points, 
                     model = "rf")) %>%
  bind_rows(ridge_fit_te %>% 
              predict(new_data = te.train) %>% 
              mutate(truth = te.train$FD.points, 
                     model = "Ridge"))

results_te_test <- lm_fit_te %>% 
  predict(new_data = te.test) %>% 
  mutate(truth = te.test$FD.points, 
         model = "lm") %>% 
  bind_rows(rf_fit_te %>% 
              predict(new_data = te.test) %>% 
              mutate(truth = te.test$FD.points, 
                     model = "rf")) %>%
  bind_rows(ridge_fit_te %>% 
              predict(new_data = te.test) %>% 
              mutate(truth = te.test$FD.points, 
                     model = "Ridge"))

results_te_train %>% 
  group_by(model) %>% 
  rmse(truth = truth, estimate = .pred)

results_te_test %>% 
  group_by(model) %>% 
  rmse(truth = truth, estimate = .pred)

## GRAPH OF RMSE FOR TIGHT ENDS
results_te_test %>% 
  mutate(train = "testing") %>% 
  bind_rows(results_te_train %>% 
              mutate(train = "training")) %>% 
  ggplot(aes(truth, .pred, color = model)) + 
  geom_abline(lty = 2, color = "grey80", size = 1.5) + 
  geom_point(alpha = .5) + 
  facet_wrap(~train) + 
  ggtitle("Tight Ends RMSE By Model")

#### ------------------------------------------------------------------------------------------------
#### Kickers 

linfit_pk <- lm(FD.points ~ ., data = pk.train)
summary(linfit_pk)

lm_spec <- linear_reg() %>% 
  set_engine(engine = "lm")

lm_fit_pk <- lm_spec %>% 
  fit(FD.points ~ ., data = pk.train)


## Random Forest
rfspec_pk <- rand_forest(mode = "regression") %>% 
  set_engine("ranger")

rf_fit_pk <- rfspec_pk %>% 
  fit(FD.points ~., data = pk.train)

## Ridge Regression
ridge_spec_pk <- linear_reg(penalty = .10, mixture = 0) %>% # mixture = 0 meaning no L1 penalty 
  set_mode("regression") %>% 
  set_engine("glmnet") 


ridge_fit_pk <- ridge_spec_pk %>% 
  fit(FD.points ~ ., data = pk.train)

results_pk_train <- lm_fit_pk %>% 
  predict(new_data = pk.train) %>% 
  mutate(truth = pk.train$FD.points, 
         model = "lm") %>% 
  bind_rows(rf_fit_pk %>% 
              predict(new_data = pk.train) %>% 
              mutate(truth = pk.train$FD.points, 
                     model = "rf")) %>%
  bind_rows(ridge_fit_pk %>% 
              predict(new_data = pk.train) %>% 
              mutate(truth = pk.train$FD.points, 
                     model = "Ridge"))

results_pk_test <- lm_fit_pk %>% 
  predict(new_data = pk.test) %>% 
  mutate(truth = pk.test$FD.points, 
         model = "lm") %>% 
  bind_rows(rf_fit_pk %>% 
              predict(new_data = pk.test) %>% 
              mutate(truth = pk.test$FD.points, 
                     model = "rf")) %>%
  bind_rows(ridge_fit_pk %>% 
              predict(new_data = pk.test) %>% 
              mutate(truth = pk.test$FD.points, 
                     model = "Ridge"))

results_pk_train %>% 
  group_by(model) %>% 
  rmse(truth = truth, estimate = .pred)

results_pk_test %>% 
  group_by(model) %>% 
  rmse(truth = truth, estimate = .pred)

## GRAPH OF RMSE FOR TIGHT ENDS
results_pk_test %>% 
  mutate(train = "testing") %>% 
  bind_rows(results_pk_train %>% 
              mutate(train = "training")) %>% 
  ggplot(aes(truth, .pred, color = model)) + 
  geom_abline(lty = 2, color = "grey80", size = 1.5) + 
  geom_point(alpha = .5) + 
  facet_wrap(~train) + 
  ggtitle("Place Kickers RMSE By Model")