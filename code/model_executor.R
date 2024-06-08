model_executor <- function(quotes_bitcoin, log_transformed_training, arima_model, nnar_model, is_preloaded)	{
  
  training_size <- length(log_transformed_training)
  testing_size <- length(quotes_bitcoin) - training_size
  overall_size <- training_size + testing_size
  
  if (is_preloaded && training_size==FIRST_TRAINING_SIZE) {
    e <- new.env(parent = emptyenv())
    load(PATH_1966_DAYS_FORECASTED_DF, envir = e)
    data_frame_1966 <- e[[DF_1966_NAME]]
    return (data_frame_1966)
  }
  
  if (is_preloaded && training_size==SECOND_TRAINING_SIZE) {
    ee <- new.env(parent = emptyenv())
    load(PATH_466_DAYS_FORECASTED_DF, envir = ee)
    data_frame_466 <- ee[[DF_466_NAME]]
    return (data_frame_466)
  }
  
  # Create a vector to store the forecasts
  forecasts_without_reestimation_arima <- numeric(overall_size)
  forecasts_with_reestimation_arima <- numeric(overall_size)
  forecasts_without_reestimation_nnar <- numeric(overall_size)
  forecasts_with_reestimation_nnar <- numeric(overall_size)
  for(k in 1:training_size) { # 500 or 2000
    forecasts_without_reestimation_arima[k] <- as.double(log_transformed_training$Close[k])
    forecasts_with_reestimation_arima[k] <- as.double(log_transformed_training$Close[k])
    forecasts_without_reestimation_nnar[k] <- as.double(log_transformed_training$Close[k])
    forecasts_with_reestimation_nnar[k] <- as.double(log_transformed_training$Close[k])
  }
  
  
  # Forecasting step-by-step without re-estimation and with re-estimation
  for (i in 1:testing_size) {
    print(i)
    # Forecast the next period using the already fitted model
    if (i==1) {
      arima_model_500_without <- Arima(log_transformed_training$Close, order=arima_model)
      arima_model_500_with <- auto.arima(log_transformed_training$Close)
      nnar_model_500_without <- nnetar(log_transformed_training$Close, p=nnar_model[1], P=nnar_model[2], size=2)
      nnar_model_500_with <- nnetar(log_transformed_training$Close)
    } else {
      arima_model_500_without <- Arima(ts(rbind(log_transformed_training,log(quotes_bitcoin[training_size:training_size+(i-1)]))), order = arima_model)
      arima_model_500_with <- auto.arima(rbind(log_transformed_training,log(quotes_bitcoin[training_size:training_size+(i-1)]))$Close)
      nnar_model_500_without <- nnetar(ts(rbind(log_transformed_training,log(quotes_bitcoin[training_size:(training_size+(i-1))]))), p=nnar_model[1], P=nnar_model[2], size=2)
      nnar_model_500_with <- nnetar(ts(rbind(log_transformed_training,log(quotes_bitcoin[training_size:(training_size+(i-1))]))))
    }
    
    temp_result <- forecast(arima_model_500_without, h=1)
    forecasts_without_reestimation_arima[training_size+i] <- temp_result$mean[1]
    temp_result <- forecast(arima_model_500_with, h=1)
    forecasts_with_reestimation_arima[training_size+i] <- temp_result$mean[1]
    temp_result <- forecast(nnar_model_500_without, h=1)
    forecasts_without_reestimation_nnar[training_size+i] <- temp_result$mean[1]
    temp_result <- forecast(nnar_model_500_with, h=1)
    forecasts_with_reestimation_nnar[training_size+i] <- temp_result$mean[1]
    
  }
  
  
  accuracy(forecasts_without_reestimation_arima, log(quotes_bitcoin))
  accuracy(forecasts_with_reestimation_arima, log(quotes_bitcoin))
  
  accuracy(forecasts_without_reestimation_nnar, log(quotes_bitcoin))
  accuracy(forecasts_with_reestimation_nnar, log(quotes_bitcoin))
  
  df_forecasted <- data.frame(date=index(quotes_bitcoin), 
                                        arima_without_reestimation = forecasts_without_reestimation_arima, 
                                        arima_with_reestimation = forecasts_with_reestimation_arima,
                                        nnar_without_reestimation = forecasts_without_reestimation_nnar,
                                        nnar_with_reestimation = forecasts_with_reestimation_nnar,
                                        close=log(quotes_bitcoin$Close))
  
  return(df_forecasted)
}	
