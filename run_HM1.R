rm(list = ls())

library(tidyverse)

options(stringsAsFactors = FALSE,
        scipen = 999)

data <- read_rds("data_for_analysis.rds") %>%
  mutate(., NAICSS1 = as.character(NAICSS1), naics = as.character(naics), year = as.character(year), performance = 100*performance) %>%
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
  
  # to store sigma outcomes
  results <- data.frame(matrix(ncol = num.iter, nrow = 4))
  # to store z calculations
  z.vec <- matrix(0, ncol = 1, nrow = 4)
  # to store H calculations
  h.mat <- matrix(0, ncol = 4, nrow = 4)
  
  # main computation loop
  start.ind = 1
  for (i in 1:num.iter) {
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
    
    # compute l.vec, which contains (in order) l_alpha, l_beta, l_phi, and N (i.e., l_epsilon)
    l.vec <- mapply(n_distinct, list(data.window$SICS1, data.window$gvkey, data.window$gvkey_ind, data.window$ID))
    
    # implement henderson's method 1
    h.mat[c(1, 3, 4), 1] = l.vec[4]
    h.mat[c(2, 3, 4), 2] = l.vec[4]
    h.mat[c(3, 4), 3] = l.vec[4]
    h.mat[4, 4] = l.vec[4]
    
    h.mat[1, 4] = l.vec[1]
    h.mat[2, 4] = l.vec[2]
    h.mat[3, 4] = l.vec[3]
    
    i.. <- data.window %>%
      group_by(., SICS1) %>%
      summarise(., n = n(), r = sum(performance_d))
    
    .k. <- data.window %>%
      group_by(., gvkey) %>%
      summarise(., n = n(), r = sum(performance_d))
    
    ..t <- data.window %>%
      group_by(., year) %>%
      summarise(., n = n(), r = sum(performance_d))
    
    i.t <- data.window %>%
      group_by(., year_ind) %>%
      summarise(., n = n(), r = sum(performance_d))
    
    ik. <- data.window %>%
      group_by(., gvkey_ind) %>%
      summarise(., n = n(), r = sum(performance_d))
    
    z.vec[1, 1] = sum(i..$r * i..$r / i..$n)
    z.vec[2, 1] = sum(.k.$r * .k.$r / .k.$n)
    z.vec[3, 1] = sum(ik.$r * ik.$r / ik.$n)
    z.vec[4, 1] = sum(data.window$performance_d * data.window$performance_d)
    
    nik._ni.. <- data.window %>%
      group_by(., SICS1, gvkey) %>%
      summarise(., n = n()) %>%
      mutate(., n2 = n*n) %>%
      group_by(., SICS1) %>%
      summarise(., sum = sum(n2)) %>%
      mutate(., div = sum / i..$n)
    
    ni.t_ni.. <- data.window %>%
      group_by(., SICS1, year) %>%
      summarise(., n = n()) %>%
      mutate(., n2 = n*n) %>%
      group_by(., SICS1) %>%
      summarise(., sum = sum(n2)) %>%
      mutate(., div = sum / i..$n)
    
    nik._n.k. <- data.window %>%
      group_by(., gvkey, SICS1) %>%
      summarise(., n = n()) %>%
      mutate(., n2 = n*n) %>%
      group_by(., gvkey) %>%
      summarise(., sum = sum(n2)) %>%
      mutate(., div = sum / .k.$n)
    
    n.kt_n.k. <- data.window %>%
      group_by(., gvkey, year) %>%
      summarise(., n = n()) %>%
      mutate(., n2 = n*n) %>%
      group_by(., gvkey) %>%
      summarise(., sum = sum(n2)) %>%
      mutate(., div = sum / .k.$n)
    
    ni.t_n..t <- data.window %>%
      group_by(., year, SICS1) %>%
      summarise(., n = n()) %>%
      mutate(., n2 = n*n) %>%
      group_by(., year) %>%
      summarise(., sum = sum(n2)) %>%
      mutate(., div = sum / ..t$n)
    
    n.kt_n..t <- data.window %>%
      group_by(., year, gvkey) %>%
      summarise(., n = n()) %>%
      mutate(., n2 = n*n) %>%
      group_by(., year) %>%
      summarise(., sum = sum(n2)) %>%
      mutate(., div = sum / ..t$n)
    
    h.mat[1, c(2, 3)] = sum(nik._ni..$div)
    h.mat[2, c(1, 3)] = sum(nik._n.k.$div)
    
    sigma <- solve(h.mat) %*% z.vec
    sigma_norm <- sigma / sum(sigma) 
    results[,i] = sigma_norm
    
    start.ind = indices[i]+1
  }
  
  # save results
  saveRDS(results, paste0("HM1_", x, "yearwindow.rds"))
  
}

