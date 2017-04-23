# Part 1: Factorial Function

library(purrr)
library(microbenchmark)
library(tidyr)
library(magrittr)
library(dplyr)

# The factorial_loop() function computes the factorial of an integer using looping.

factorial_loop <- function(n) {
  if(n < 0) {
    NaN
  } else if(n == 0 | n == 1) {
    1
  } else {
    result <- 1
    for(i in 1:n){
      result <- result * i
    }
    result
  }
}

# The factorial_reduce() function computes the factorial of an integer using the reduce() function in the purrr package.

factorial_reduce <- function(n) {
  if(n < 0) {
    NaN
  } else if(n == 0 | n == 1) {
    1
  } else {
    reduce(1:n, function(x, y) {
      as.numeric(x) * as.numeric(y)
    })
  }
}

# The factorial_func() function computes the factorial of an integer using recursion

factorial_func <- function(n) {
  if(n < 0) {
    NaN
  } else if(n == 0 | n == 1) {
    1
  } else {
    n * factorial_func(n - 1)
  }
}

# The factorial_mem() function computes the factorial of an integer using memoization

# First, initialize the factorial table with NA for 1 < n <= 1000
factorial_tbl <- c(1,rep(NA, 999))

factorial_mem <- function(n) {
  if(n < 0) {
    NaN
  } else if(n == 0) {
    1
  } else if(!is.na(factorial_tbl[n])) {
    factorial_tbl[n]
  } else {
    factorial_tbl[n] <<- n * factorial_func(n - 1)
    factorial_tbl[n]
  }
}

# Store the maximum factorial integer value in n_max.
# The program will calculate factorial values from 1! to n_max!
n_max <- 100

# Use microbenchmark to calculate factorials from 1 to n_max (using looping and the factorial_loop function), 
# 100 times for each factorial value, and time the results
# The median time (in nanoseconds) for each factorial calculation is stored in the factorial_loop_data dataframe

factorial_loop_data <- map(1:n_max, function(x){ microbenchmark(factorial_loop(x))$time})
names(factorial_loop_data) <- as.character(1:n_max)
factorial_loop_data <- as.data.frame(factorial_loop_data) 
colnames(factorial_loop_data) <- 1:n_max

factorial_loop_data %<>%
  gather(num, time) %>%
  group_by(num) %>%
  summarise(med_time = median(time))
factorial_loop_data$num <- as.integer(factorial_loop_data$num)

# Use microbenchmark to calculate factorials from 1 to n_max (using reduce and the factorial_reduce function),
# 100 times for each factorial value, and time the results
# The median time (in nanoseconds) for each factorial calculation is stored in the factorial_reduce_data dataframe

factorial_reduce_data <- map(1:n_max, function(x){ microbenchmark(factorial_reduce(x))$time})
names(factorial_reduce_data) <- as.character(1:n_max)
factorial_reduce_data <- as.data.frame(factorial_reduce_data)
colnames(factorial_reduce_data) <- 1:n_max

factorial_reduce_data %<>%
  gather(num, time) %>%
  group_by(num) %>%
  summarise(med_time = median(time))
factorial_reduce_data$num <- as.integer(factorial_reduce_data$num)

# Use microbenchmark to calculate factorials from 1 to n_max (using recursion and the factorial_func function),
# 100 times for each factorial value, and time the results
# The median time (in nanoseconds) for each factorial calculation is stored in the factorial_func_data dataframe

factorial_func_data <- map(1:n_max, function(x){ microbenchmark(factorial_func(x))$time})
names(factorial_func_data) <- as.character(1:n_max)
factorial_func_data <- as.data.frame(factorial_func_data)
colnames(factorial_func_data) <- 1:n_max

factorial_func_data %<>%
  gather(num, time) %>%
  group_by(num) %>%
  summarise(med_time = median(time))
factorial_func_data$num <- as.integer(factorial_func_data$num)

# Use microbenchmark to calculate factorials from 1 to n_max (using memoization and the factorial_mem function),
# 100 times for each factorial value, and time the results
# The median time (in nanoseconds) for each factorial calculation is stored in the factorial_mem_data dataframe

factorial_tbl <- c(1,rep(NA, 999))
factorial_mem_data <- map(1:n_max, function(x){ microbenchmark(factorial_mem(x))$time})
names(factorial_mem_data) <- as.character(1:n_max)
factorial_mem_data <- as.data.frame(factorial_mem_data)
colnames(factorial_mem_data) <- 1:n_max

factorial_mem_data %<>%
  gather(num, time) %>%
  group_by(num) %>%
  summarise(med_time = median(time))
factorial_mem_data$num <- as.integer(factorial_mem_data$num)

# Combine the timing results from the four calculation methods into a single dataframe in tidy format
factorial_timing_results <- full_join(factorial_loop_data, factorial_reduce_data, 
                                      by = "num", suffix = c(".loop",".reduce"))
factorial_timing_results <- full_join(factorial_timing_results, factorial_func_data, 
                                      by = "num", suffix = c(".reduce",".recursion"))
factorial_timing_results <- full_join(factorial_timing_results, factorial_mem_data, 
                                      by = "num", suffix = c(".recursion",".memoization"))
factorial_timing_results <- factorial_timing_results[order(factorial_timing_results$num),]

# Print the timing results in pretty format using knitr::kable
# All Median Time values are in nanoseconds

# Note that for very small n (n=1) the reduce() function is fastest, 
# looping and recursion are equal and second fastest and memoization is slowest
# For n > 1 the memoization method is fastest, 
# the loop method is second fastest, recursion is third fastest and the reduce function is the slowest

options(tibble.print_max = Inf)
colnames(factorial_timing_results) <- c("Factorial Number",
                                        "Median Time (Loop)", 
                                        "Median Time (Reduce)", 
                                        "Median Time (Recursion)", 
                                        "Median Time (Memoization)")
knitr::kable(factorial_timing_results)



# The following code will plot the timing results 
# Note: the plot will not appear in the txt output file.  It is plotted in the plot window in RStudio.

plot(factorial_timing_results$"Factorial Number", 
     factorial_timing_results$"Median Time (Reduce)", 
     xlab = "Factorial Number", ylab = "Median Time (nanoseconds)",
     pch = 18, bty = "n", xaxt = "n", yaxt = "n")
axis(1, at = 1:n_max)
axis(2, at = seq(0, 300000, by = 25000))
points(factorial_timing_results$"Factorial Number" + 0.01, 
       factorial_timing_results$"Median Time (Loop)", col = "blue", pch = 18)
points(factorial_timing_results$"Factorial Number" + 0.02, 
       factorial_timing_results$"Median Time (Recursion)", col = "red", pch = 18)
points(factorial_timing_results$"Factorial Number" + 0.03, 
       factorial_timing_results$"Median Time (Memoization)", col = "green", pch = 18)

legend(1, 150000, c("Reduce","Loop","Recursion","Memoization"), pch = 18,
       col = c("black","blue","red","green"), bty = "n", cex = 1, y.intersp = 1.5)

