rm(list = ls())

library(tidyverse)
library(R2MLwiN)
library(ddpcr)

options(stringsAsFactors = FALSE,
        scipen = 999,
        # MLwiN_path option must be set to the directory that contains the MLwiN executable file
        MLwiN_path = 'C:/Program Files/MLwiN v3.05')

data <- read_rds("data_for_analysis.rds") %>%
  arrange(., year) 

# set window widths; use odd positive integers for ease of plotting values on the middle year
x.vec <- c(3, 5, 7, 9)

for (x in x.vec) {
  
  # store row indices for last row of each year
  years <- as.integer(data$year) %>%
    unique() 
  
  indices <- as.integer(years) * 0
  indices[length(indices)] <- nrow(data)
  count = 1
  
  for (i in 1:(nrow(data)-1)) {
    if (data$year[i] != data$year[i+1]) {
      indices[count] = i
      count = count + 1
    }
  }
  
  # compute number of time periods to analyze
  num.iter = years[length(years)] - years[1] + 2 - x
  num.iter = ifelse(num.iter <= 0, print("Not enough years."), num.iter)
  gap = (x-1)/2
  center.years = years[(1+gap):(count-gap)] # for x-axis
  
  # to store results
  results <- data.frame(matrix(ncol = num.iter, nrow = 4))
  
  # main computation loop
  start.ind = 1
  for (i in 1:num.iter) {
    
    # to track progress
    print(paste0("x = ", x, ", center year = ", center.years[i]))
    
    end.ind = indices[i+x-1]
    # extract window
    data.window <- data %>%
      slice(., start.ind:end.ind)
    
    # to eliminate extreme outliers, winsorize top and bottom 1 percent of observations for each year
    # find the 1st and 99th percentiles for performance for each year, then drop observations that lie outside of those boundaries
    p.1 <- quantile(data.window$performance, probs = 0.01)
    p.99 <- quantile(data.window$performance, probs = 0.99)
    
    data.window <- mutate(data.window, performance = ifelse(performance < p.1, p.1, ifelse(performance > p.99, p.99, performance)))
    
    # implement McGahan and Porter's (1997) screening procedure to drop businesses that appear for only one year and businesses that are the only ones in an industry-year
    # group by gvkey-SICS1, then drop all with only one year (drop firm-inds that only appear once; i.e., only operate for one year)
    # group by year-SICS1, then drop all with only one gvkey (drop ind-years that only appear once; i.e., only have one player) 
    
    # also implement Bowman and Helfat's (2001) recommendation to drop single-business firms
    # group by year-gvkey, then drop all with only one SICS1 (drop firm-years that only appear once; i.e., only operate in one industry)
    
    old_length = nrow(data.window)
    
    data.window <- mutate(data.window, year_gvkey = paste0(year, gvkey), gvkey_ind = paste0(gvkey, SICS1), year_ind = paste0(year, SICS1)) %>%
      mutate(., drop_year_gvkey = as.integer(!(duplicated(year_gvkey) | duplicated(year_gvkey, fromLast = TRUE))),
              drop_gvkey_ind = as.integer(!(duplicated(gvkey_ind) | duplicated(gvkey_ind, fromLast = TRUE))),
              drop_year_ind = as.integer(!(duplicated(year_ind) | duplicated(year_ind, fromLast = TRUE)))) %>%
      mutate(., drop_final = ifelse((drop_year_gvkey == 1 | drop_gvkey_ind == 1 | drop_year_ind == 1), 1, 0)) %>%
      filter(., drop_final == 0)
     
    new_length = nrow(data.window)
     
    # repeat until no more new drop_year_gvkey, drop_gvkey_ind, and drop_year_ind
    while (old_length != new_length) {
     old_length = new_length
    
     data.window <- mutate(data.window, year_gvkey = paste0(year, gvkey), gvkey_ind = paste0(gvkey, SICS1), year_ind = paste0(year, SICS1)) %>%
       mutate(., drop_year_gvkey = as.integer(!(duplicated(year_gvkey) | duplicated(year_gvkey, fromLast = TRUE))),
              drop_gvkey_ind = as.integer(!(duplicated(gvkey_ind) | duplicated(gvkey_ind, fromLast = TRUE))),
              drop_year_ind = as.integer(!(duplicated(year_ind) | duplicated(year_ind, fromLast = TRUE))),
              # drop rows that have a non-numeric performance value
              drop_performance = as.integer(is.na(performance) | is.infinite(performance))) %>%
       mutate(., drop_final = ifelse((drop_year_gvkey == 1 | drop_gvkey_ind == 1 | drop_year_ind == 1 | drop_performance == 1), 1, 0)) %>%
       filter(., drop_final == 0)
    
     new_length = nrow(data.window)
    }
    
    # compute de-meaned performance
    data.window <- mutate(data.window, performance_d = performance - mean(performance))
    
    # EstM = 1 specifies MCMC estimation 
    # xc = TRUE indicates cross-classification
    model <- runMLwiN(performance_d ~ 1 + (1 | SICS1) + (1 | gvkey) + (1 | gvkey_ind) + (1 | ID), 
                      estoptions = list(xc = TRUE, 
                                        EstM = 1, 
                                        sort.force = TRUE,
                                        notation = 'class', 
                                        debugmode = FALSE,
                                        mcmcMeth = list(iterations = 500000, burnin = 50000)
                                        ),
                      data = data.window,
                      stdout = NULL)
    
    sigma <- unname(coef(model))[-1]
    sigma_norm <- sigma / sum(sigma)
    results[,i] = sigma_norm
    
    print(sigma_norm)
    
    start.ind = indices[i]+1
  }
  
  # save results
  saveRDS(results, paste0("MLwiN_", x, "yearwindow.rds"))
  
}



