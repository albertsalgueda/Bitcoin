#univariable time-series prediction using prophet

library("prophet")

market_price <- read_csv("market_price.csv", 
                         col_types = cols(ds = col_date(format = "%m/%d/%Y"), 
                                          y = col_number()))
market_price <- market_price %>% 
  filter(year(ds)>2016) 
market_price$y <- as.numeric(market_price$y)

#Update and transform data using the API
#while (TRUE) {
  current_price= market.api.process('bitstamp',c('BTC','USD'),'ticker')
  current_price = subset(current_price,select=c("timestamp","last"))
  current_price$timestamp <- format(as.Date(current_price$timestamp), "%Y-%m-%d")
  current_price <- current_price %>% 
    rename(ds=timestamp,y=last)
  current_price$y=as.numeric(current_price$y)
#}

#merge ( i did it all... but still does not merge properly)
market_price <- rbind(market_price,current_price)
view(market_price)
#fit the model using prophet
Model1 <- prophet(market_price)
#create future database
Future1 <- make_future_dataframe(Model1,periods = 100)
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
save(Model1,file="prophet.RData")







