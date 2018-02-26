library(quantmod)
library(PerformanceAnalytics)
library(jsonlite)
library(dplyr)
library(sqldf)
library(xlsx)

options(scipen=999)

etf_database <- fromJSON("etf_database.json",flatten = TRUE)
etf_flow_database <- fromJSON("etf_flow_database.json",flatten = TRUE)
rownames(etf_database) <- etf_database[,3]
etf_database[,3] <- NULL
rownames(etf_flow_database) <- etf_flow_database[,1]
etf_flow_database[,1] <- NULL 
database_symbols <- data.frame(rownames(etf_database))
flow_symbols <- data.frame(rownames(etf_flow_database))

etf_database_symbols_not_in_flow_database <- sqldf('SELECT * FROM database_symbols EXCEPT SELECT * FROM flow_symbols')

regression_result <- data.frame(matrix(ncol = 4, nrow = 0))
colnames(regression_result) <- c("symbol", "intercept_estimate","intercept_p_value","r_squared") 

for(working_symbol in rownames(etf_database)){
  out <- tryCatch({
    quote_env <- new.env() 
    
    #working_symbol <- 'QQQ'#rownames(etf_database)[106]
    quantmod::getSymbols(Symbols = working_symbol, src = "yahoo", env=quote_env, from = as.Date("1993-01-01"), to = as.Date("2017-12-06"), adjust = TRUE)
    
    symbol_daily_close <- quote_env[[working_symbol]][,6]
    
    symbol_flow <-xts(as.numeric(etf_flow_database[working_symbol,][[1]][,2]),
                      order.by = as.Date(etf_flow_database[working_symbol,][[1]][,1]))
    
    start_date <- max(index(symbol_flow[1]), index(symbol_daily_close[1]))
    end_date <- min(index(tail(symbol_flow, n=1)), index(tail(symbol_daily_close, n=1))) 
    
    symbol_flow <- symbol_flow[paste(start_date, "/" , end_date, sep ="")]
    symbol_daily_close <- symbol_daily_close[paste(start_date, "/", end_date, sep ="")]
    
    symbol_monthly_return <- monthlyReturn(symbol_daily_close)
    symbol_flow_monthly <- apply.monthly(symbol_flow, FUN = sum) 
    
    symbol_monthly_return <- cbind(coredata(symbol_monthly_return),
                                   as.numeric(format(index(symbol_monthly_return), "%Y%m")))
    
    colnames(symbol_monthly_return) <- c('return', 'month')
    
    symbol_flow_monthly <- cbind(coredata(symbol_flow_monthly), as.numeric(format(index(symbol_flow_monthly),"%Y%m")))
    symbol_flow_monthly <- cbind(diff(symbol_flow_monthly[,1])/symbol_flow_monthly[-nrow(symbol_flow_monthly),1],na.trim(lead(symbol_flow_monthly[,2])))
    colnames(symbol_flow_monthly) <- c('flow','month')
    
    merged_return_flow <- merge(symbol_monthly_return,symbol_flow_monthly, by = "month")
    
    #next month's return vs this month's new flow
    merged_return_flow <- transform(merged_return_flow, return =c(merged_return_flow$return[-
                                                                                              1],NA))
    merged_return_flow <- merged_return_flow[which(!is.nan(merged_return_flow$flow) &
                                                     !is.infinite(merged_return_flow$flow) & !is.na(merged_return_flow$return)),]
    merged_return_flow <- merged_return_flow[which(merged_return_flow$flow > -5 & merged_return_flow$flow < 5 & merged_return_flow$flow !=-1),]
    
    model <- lm(formula = merged_return_flow$return ~ merged_return_flow$flow, data = merged_return_flow)
    
    model_summary <- summary(model) 
    
    #scatter.smooth(x=merged_return_flow$flow, y=merged_return_flow$return, main="Monthly return vs flow")
    
    regression_result[nrow(regression_result) + 1,] <- list(working_symbol,
                                                            model_summary[['coefficients']][2,1],model_summary[['coefficients']][2,4],
                                                            model_summary[['r.squared']])
  },
  error = function(e) regression_result[nrow(regression_result) + 1,] <- list(working_symbol, NA, NA, NA)) 
}
