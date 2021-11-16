#use neural networks and bunch of random variables to predict bitcoin price

btc <- read_csv("btc.csv", col_types = cols(date = col_date(format = "%Y-%m-%d"), 
                                            AdrBalUSD100Cnt = col_number()))

btc <- btc %>%
  filter( ! is.na( PriceUSD ) ) %>%
  filter( ! is.na( HashRate ) ) 

set.seed( 28 )
split <- initial_split( btc, prop = 0.5 )
btcTrain <- training( split )
btcTest <- testing( split )


rec1<-
  recipe( btc ) %>%
  update_role( PriceUSD, new_role = "outcome" ) %>%
  update_role( AdrActCnt, AdrBal1in100KCnt, AdrBal1in100MCnt,AdrBal1in10BCnt,AdrBal1in10KCnt, AdrBalCnt, AdrBalNtv0.001Cnt, AdrBalNtv0.1Cnt, AdrBalNtv100Cnt, AdrBalNtv1MCnt, AdrBalNtv1KCnt, BlkWghtMean,BlkCnt, CapAct1yrUSD, CapMVRVCur, CapMVRVFF, CapMrktCurUSD, CapRealUSD, DiffMean, FeeTotNtv, FeeTotUSD, HashRate,IssContNtv,IssTotUSD, RevHashNtv, RevHashRateUSD, SER, SplyAct90d,SplyActEver, SplyCur,TxCnt,IssContPctDay, new_role = "predictor") %>%
  step_naomit( all_predictors(), all_outcomes() ) %>%
  step_normalize( all_numeric_predictors() ) %>% 
  step_dummy(all_nominal(), -all_outcomes()) %>%
  step_zv(all_predictors())

mdlNeural <- mlp( hidden_units = c(4,4,4) ) %>% #set up 3 layers of 4 nodes
  set_engine("nnet") %>%
  set_args( lifesign="full", threshold=1856987 ) %>% #the lower the threshold the better 
  set_mode("regression")

wflowNeural1 <-
  workflow()%>%
  add_model(mdlNeural)%>% 
  add_recipe(rec1)

fitNeural <- fit( wflowNeural1, btc )
#Warning message:
#Algorithm did not converge in 1 of 1 repetition(s) within the stepmax-
#> SOLVED by increasing the threshold
plotnet( fitNeural$fit$fit$fit, max_sp=T )
neuralweights( fitNeural$fit$fit$fit, struct=struct ) 

btc <- btc %>%
  add_predictions(fitNeural,var="predNeural1",type="numeric")%>%
  mutate(predNeural1_pred_numeric=predNeural1$.pred_class,
         predNeural1 = NULL)

#how to make projections ?


