#THIS IS THE MAIN DECISION-MAKINNG STRATEGY: Should provide the predicted price 
# + the % deviation between market price and prediction. 
#API: -get_predicted_price() -get_deviation() -get_confidence() #based on probabilistic analysis
  
#PREREQUISITES
library(Rbitcoin)
library(sys)
library(readr)
library( tidyverse )
library (mosaic)
library(ggformula)
library(ggplot2)
library( lubridate )
library(prophet)
library(tidymodels)
library(modelr)
library(lubridate)
library(readxl)
library(dplyr)
library(zoo)
library( remotes )
library(NeuralNetTools)
library( parsnipExtra )
add_neuralnet_engine()

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


df <- df%>% 
  filter(year(Date)>2013) 
df <- df %>% 
  rename(df, `Market Price ($)` = MarketPrice )

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


#visualize results
df %>% 
  filter(year(Date)>2012) %>% 
  ggplot( aes(x=Date, y=ProductionCost)) +
  geom_line(color="#69b3a2") 
#compute the ratio
df <- df %>% 
  mutate(ratio=ProductionCost/`Market Price ($)`)
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


#TODO: build a model using a lag ( LEAD AND LAG) dplyr::



#TODO: Create the simpliest_forecast ( done)


#DOING:
# neural network prediction ( error when fitting)
set.seed( 28 )
split <- initial_split( df, prop = 0.5 )
dfTrain <- training( split )
dfTest <- testing( split )

the_recipe <-
  recipe( df ) %>%
  update_role( 'Market Price ($)', new_role = "outcome" ) %>%
  update_role( 'Av. Electricity Price ($/KWh)',new_role = 'predictor') 

mdlNeural <- mlp( hidden_units = c(4,4,4) ) %>% #set up 3 layers of 4 nodes
  set_engine("neuralnet") %>%
  set_args( lifesign="full", threshold=1856987 ) %>% #the lower the threshold the better 
  set_mode("regression")

the_worklfow <-
  workflow() %>% 
  add_recipe(the_recipe) %>% 
  add_model(mdlNeural)

the_fit <- fit(the_worklfow,df)
#error

n <- neuralnet('Market Price ($)' ~ ProductionCost,df,hidden=1)
#error


#Tune the model








