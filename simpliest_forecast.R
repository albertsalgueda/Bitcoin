#start simple and build...
#time-series prediction using prophet

library("prophet")

market_price <- read_csv("market-price - market-price.csv", 
                          col_types = cols(ds = col_date(format = "%d/%m/%Y"), 
                           y = col_number()))

market_price <- market_price %>% 
  filter(year(ds)>2016) 
market_price$ds <- format(as.Date(market_price$ds), "%d/%m/%Y")
  
#Update and transform data using the API
current_price= market.api.process('bitstamp',c('BTC','USD'),'ticker')
current_price = subset(current_price,select=c("timestamp","last"))
current_price$timestamp <- format(as.Date(current_price$timestamp), "%d/%m/%Y")
current_price <- current_price %>% 
  rename(ds=timestamp,y=last)
view(current_price)

#merge ( i did it all... but still does not merge properly)
merge(market_price,current_price) #error
view(market_price)

#fit the model using prophet
Model1 <- prophet(market_price)
#create future database
Future1 <- make_future_dataframe(Model1,periods = 365)
tail(Future1)
#Forecasting
forecast1 <- predict(Model1,Future1)
tail(forecast1)
#plot the model
dyplot.prophet(Model1,forecast1)
#plot model estimates
prophet_plot_components(Model1,forecast1)


##HOW CAN I COMPUTE THE HISTORICAL RATIO? #error
ratio2 = forecast1$yhat/market_price$y

##API ANSWER
#test the API with this simple forecast




