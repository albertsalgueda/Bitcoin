library(plumber)
#r <- plumb("API.R")
#r$run(port=8000)
#curl --data "x=5&y=3" http://localhost:8000/function 

#* @apiTitle Bitcoin Prediction
#* @apiDescription AI-based prediction using neural networks
#* Insert the number of days you want to get the prediction
#* @get /model
function(x){
  load("prophet.RData")
  Future1 <- make_future_dataframe(Model1,periods = as.numeric(x))
  forecast1 <- predict(Model1,Future1)
  tail(forecast1,n=as.numeric(x))
}




