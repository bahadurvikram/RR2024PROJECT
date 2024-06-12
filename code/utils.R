#common function to prepare time series stationarity test results set data
stationary_test_data <- function(results_adf, title, sample_duration, adf_result, pp_result)	{
  rbind(results_adf, data.frame(Data=title, Training_Sample=sample_duration,
                                ADF_Test=paste(sprintf("%.3f", unname(adf_result$statistic)),"(",sprintf("%.3f", adf_result$p.value),")"),
                                PP_Test=paste(sprintf("%.3f", unname(pp_result$statistic)),"(",sprintf("%.3f", pp_result$p.value),")")))
  
}
#RMSE = accuracy(model)[2]
#MAPE= accuracy(model)[5]
#MAE = accuracy(model)[6]

#common function to prepare performance matrix result set data
performance_data <- function(forecast_perf, title, sample_duration, model_var)	{
  rbind(forecast_perf, data.frame(Forecast_Model=title, Training_Sample=sample_duration,
                                RMSE=sprintf("%.3f", model_var[2]),
                                MAPE=sprintf("%.3f", model_var[5]),
                                MAE=sprintf("%.3f", model_var[3])))
}

#common function to prepare comparison of models data
performance_compared_data <- function(perf_compared, title, model_var)	{
  rbind(perf_compared, data.frame(Models_Compared=title,
                                  DM_Statistics=sprintf("%.3f", unname(model_var$statistic)),
                                  p_Value=format(unname(model_var$p.value), scientific = TRUE)))
}