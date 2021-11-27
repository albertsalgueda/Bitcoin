#THIS IS THE MAIN DECISION-MAKINNG STRATEGY: Should provide the predicted price 
# + the % deviation between market price and prediction. 
#API: -get_predicted_price() -get_deviation() -get_confidence() 
#based on probabilistic analysiss

#PREREQUISITES
library(plumber)
library(Rbitcoin)
library(readr)
library( tidyverse )
library (mosaic)
library(ggformula)
library(ggplot2)
library(prophet)
library(tidymodels)
library(modelr)
library(lubridate)
library(dplyr)
library(zoo)
library(NeuralNetTools)
library( parsnipExtra )
library(rpart)
#SCRIPT TO READ AND TRANSFORM HISTORICAL DATA

df <- read_csv("Bitcoin Cleaned Data - Cleaned Data.csv", 
               col_types = cols(Date = col_date(format = "%m/%d/%Y"), 
                                `Market Price ($)` = col_number(), 
                                `Av. Electricity Price ($/KWh)` = col_number(), 
                                `Block Rewards (BTC)` = col_number(), 
                                `Transaction Fees (USD)` = col_number(), 
                                Difficulty = col_number(), `Hash rate TH/s` = col_number(), 
                                `Efficency ( TH/kW)` = col_number(), 
                                `Efficency ( J/TH )` = col_number(), 
                                `Google Trends` = col_number(), `Unique Wallets` = col_number()))
View(df)



###SCRIPT TO BUILD THE COST OF PRODUCTION 

df <- df %>% 
  mutate(efficency = `Efficency ( J/TH )`/1000)

#IMPUTE EFFICIENCY -> Use 90 day Moving Average 
#TODO -> improve code (join it together)

df <- df %>% 
  dplyr::mutate(efficiency_cleaned = zoo::rollmean(efficency, k = 90, fill = NA))

df <- df %>% 
  mutate(network_hashrate = 60*60*24*`Hash rate TH/s`)

df <- df %>% 
  mutate( btc_day = (`Block Rewards (BTC)`*network_hashrate)/Difficulty)

df <- df %>% 
  mutate(Eday = df$`Av. Electricity Price ($/KWh)`*24*efficiency_cleaned)

df <- df %>% 
  mutate(total_cost = Eday*1000)

df <- df %>% 
  mutate(ProductionCost = total_cost/btc_day)

df <- df %>% 
  rename(Price=`Market Price ($)`) %>%
  rename(Trends=`Google Trends`)
#visualize results
df %>% 
  filter(year(Date)>2012) %>% 
  ggplot( aes(x=Date, y=ProductionCost)) +
  geom_line(color="#69b3a2") 
#compute the ratio
df <- df %>% 
  mutate(ratio=ProductionCost/Price)
#visualize results
df %>% 
  filter(year(Date)>2012) %>% 
  ggplot( aes(x=Date, y=ratio)) +
  geom_line(color="#69b3a2") 
#create a column with


###SCRIPT TO ADD (and treat) CURRENT MARKET DATA 

current_price= market.api.process('bitstamp',c('BTC','USD'),'ticker') #gets price in btcusd
Sys.sleep(10) #sleep between API call
current_price = subset(current_price,select=c("timestamp","last"))
current_price$timestamp <- format(as.Date(current_price$timestamp), "%d/%m/%Y")
current_price <- current_price %>% 
  rename(Date=timestamp,'Market Price ($)'=last)
###SCRIPT TO JOIN DATASETS ( doing on simpliest_forecast)
merge(df,current_price) #error


#TODO: Create the simpliest_forecast ( done)

#CREATE A RNN TO PREDICT BTC PRODUCTION PRICE


library( rnn )
df <- df %>% 
  filter(year(Date)>2014) 

view(df)

Xb <- int2bin( df$ProductionCost, length=16 )

Y <- int2bin( df$Price, length=16 )

X <- array( Xb, dim=c(dim(Xb),1) )

modelRnn <- trainr(Y=Y, X=X,
                   learningrate = 0.05, #how much the model changes, how quickly can lear
                   hidden_dim = 20, #dimensions or hidden layers
                   numepochs = 500 )

dfRnnErrors <- data.frame(
  epoch=1:ncol(modelRnn$error), 
  errors=colMeans(modelRnn$error) )

#visualize the errors
gf_line( dfRnnErrors, errors ~ epoch )

#add predictions
df$predRnnB <- predictr( modelRnn, X )

#convert to integer
df$predRnn <- bin2int( df$predRnnB )

#visualize the predictions
gf_line( df, predRnn ~ Date )

#calculate the error
df$ErrorRnn <- df$predRnn - df$nextClose

#asses the model
metrics( df, nextClose, predRnn )
metrics( tail( df, 10), nextClose, predRnn )
summary( df$ErrorRnn )

gf_point( df, predRnn~Date, color = "blue" )











##ALL APPROACHES BELOW ARE 4 EXPERIMENTAL PURPOSES ONLY
""
#DOING:
# neural network prediction ( error when fitting)
set.seed( 28 )
split <- initial_split( df, prop = 0.5 )
dfTrain <- training( split )
dfTest <- testing( split )

df <- na.omit(df)
df$network_hashrate

the_recipe <-
  recipe( df ) %>%
  update_role( Price, new_role = "outcome" ) %>%
  update_role( Trends, new_role = 'predictor') #error when I add  derived metrics

mdlNeural <- mlp( hidden_units = c(10),epochs=2000) %>% 
  set_engine("keras") %>%
  set_args( lifesign="full", threshold=1) %>% #the lower the threshold the better 
  set_mode("regression")

the_worklfow <-
  workflow() %>% 
  add_recipe(the_recipe) %>% 
  add_model(mdlNeural)

the_fit<-fit(the_worklfow,dfTrain)

#add predictions
dfTrain <- dfTrain %>%
  add_predictions( the_fit, var="predNeural1",type="numeric") 

#asses the model
metrics( dfTrain, Price, predNeural1 )


#Tune the model
library(keras)
mdlNeural2 <- optimizer_rmsprop(
  learning_rate = 0.001,
  rho = 0.9,
  epsilon = NULL,
  decay = 0,
  clipnorm = NULL,
  clipvalue = NULL
)



#TRY ANOTHER MODEL

the_recipe2 <-
  recipe( df ) %>%
  update_role( Price, new_role = "outcome" ) %>%
  update_role( ProductionCost,new_role = 'predictor') %>% 
  step_naomit(all_predictors())

rf_defaults <- rand_forest(mode = "regression")


the_worklfow2 <-
  workflow() %>% 
  add_recipe(the_recipe2) %>% 
  add_model(rf_defaults)

the_fit2<-fit(the_worklfow2,dfTrain)

dfTrain <- dfTrain %>%
  add_predictions( the_fit2, var="predForest",type="numeric") %>% 
  mutate(forest = predForest$pred_numeric,forest=NULL )


library(tsibble) # Tidy Temporal Data Frames and Tools
library(feasts) # Feature Extraction and Statistics for Time Series
library(tsibbledata) # Diverse Datasets for 'tsibble'

df %>% glimpse()

df1 <-  df %>%
  filter(year(Date) == 2014) %>%
  mutate(Price = scale(Price), ProductionCost = scale(ProductionCost)) %>%
  pivot_longer(-Date, names_to = "variable")
