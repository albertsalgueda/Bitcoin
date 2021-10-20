library(Rbitcoin)

df_1 <-current_price1

for(i in 1:100){
  current_price1= market.api.process('bitstamp',c('BTC','USD'),'ticker')
  Sys.sleep(10)
  current_price1 = subset(current_price1,select=c("timestamp","last"))
  current_price1 <- current_price1 %>% 
    rename(ds=timestamp,y=last)
  current_price1$y=as.numeric(current_price1$y)
  df_1 <- rbind(df_1,current_price1)
  #Model2 <- prophet(df_1)
  #Future2 <- make_future_dataframe(Model2,periods = 1)
  #forecast2 <- predict(Model1,Future2)
  #dyplot.prophet(Model2,forecast2)
  #tail(forecast1)
  print(ggplot(df_1, aes(x=ds, y=y) +
    geom_line(color="#69b3a2"))) 
}
df_1 %>% 
  ggplot( aes(x=ds, y=y)) +
  geom_line(color="#69b3a2") 
dyplot.prophet(Model2,forecast2)
view(df_1)
