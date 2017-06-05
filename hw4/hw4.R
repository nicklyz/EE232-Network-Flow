library(igraph)
library(xts)
setwd("./Documents/EE 232E/EE232-Network-Flow/hw4/")

# Question 1

read_stock_info = function(csv_name) {
  return(read.csv(paste("data", csv_name, sep="/")))
}

get_time_series_from_stock_info = function(csv_name) {
  # Get the stock info from a csv file
  stock_info = read_stock_info(csv_name)
  # Convert into a time series
  return(xts(stock_info[,-1], order.by = as.Date(stock_info$Date)))
}

# Computes r_i(t)
r_t = function(time_series) {
  closing_prices = time_series$Close
  shifted_closing_prices = lag(time_series$Close, 1)
  return((log(closing_prices) - log(shifted_closing_prices))[-1])
}

# Computes the cross correlation coefficient
cross_correlation_coefficient = function(time_series1, time_series2) {
  r1_t = r_t(time_series1)
  r2_t = r_t(time_series2)
  
  top = mean(r1_t * r2_t) - mean(r1_t) * mean(r2_t)
  bot = (mean(r1_t^2) - mean(r1_t)^2) * (mean(r2_t^2) - mean(r2_t)^2)
  return(top / sqrt(bot))
}

all_csv_file_names = list.files(paste("data", sep="/"), pattern="*.csv")
all_time_series = list()
for (csv_file_name in all_csv_file_names) {
  all_time_series[[csv_file_name]] = get_time_series_from_stock_info(csv_file_name)
}

cross_correlation_coefficient_matrix = matrix(0, nrow = length(all_csv_file_names), ncol = length(all_csv_file_names))
for (csv_file_name1 in all_csv_file_names) {
  
}
