stationary_test_data <- function(results_adf, title, sample_duration, adf_result, pp_result)	{
  rbind(results_adf, data.frame(Data=title, Training_Sample=sample_duration,
                                ADF_Test=paste(sprintf("%.3f", unname(adf_result$statistic)),"(",sprintf("%.3f", adf_result$p.value),")"),
                                PP_Test=paste(sprintf("%.3f", unname(pp_result$statistic)),"(",sprintf("%.3f", pp_result$p.value),")")))
  
}