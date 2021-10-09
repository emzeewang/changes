rm(list = ls())

library(tidyverse)

options(stringsAsFactors = FALSE,
        scipen = 999)

# save the Segments data file in the same directory as this R file under the name "raw_data.csv"
data.original <- read_csv("raw_data.csv") %>%
  mutate(., year = floor(srcdate / 10000), corp.ind = paste0(gvkey, " ", SICS1))

is.largest <- function(x) as.integer(seq_along(x) == which.max(x))

data.screened <- data.original %>%
  # reduce data to necessary columns
  select(., c(gvkey, stype, sid, ias, ops, sales, datadate, srcdate, NAICSS1, SICS1, snms, conm, cusip, cik, sic, naics, year, corp.ind)) %>%
  # drop missing value rows, drop STSEG rows
  filter(., !is.na(ias), !is.na(ops), !is.na(SICS1), stype != "STSEG") %>%
  # drop exact duplicates 
  unique() %>%
  # pad sids
  mutate(., sid = str_pad(sid, 2, side = "left", "0"), keep = 0, year = floor(srcdate / 10000)) %>%
  # drop restated data in ID group by keeping only largest datadate for each srcdate
  mutate(., ID = paste0(gvkey, stype, sid, srcdate), stype_sid = paste0(stype, sid)) %>%
  transform(., keep = ave(datadate, ID, FUN = is.largest)) %>%
  filter(., keep == 1) %>%
  # pad industry codes 
  mutate(., SICS1 = str_pad(SICS1, 4, side = "left", "0")) %>%
  # initial screening, following McGahan and Porter (1997)
  # drop rows with restricted industries
  filter(., !((as.integer(SICS1) >= 6000) & (as.integer(SICS1) < 7000)), !(as.integer(SICS1) >= 8880)) %>%
  # drop rows with either sales < 10M or assets < 10M (or both)
  filter(., ((sales >= 10) & (ias >= 10))) %>%
  # prepare to merge rows into businesses 
  mutate(., newID = paste0(gvkey, stype, srcdate, SICS1), ias_sum = ias, ops_sum = ops, sales_sum = sales, count = 1, keep = 0) %>%
  arrange(., newID) 

# sum ias, ops, and sales within rows to merge, mark row to keep
for (i in 2:nrow(data.screened)) {
  if (data.screened$newID[i] == data.screened$newID[i-1]) {
    data.screened$ias_sum[i] = data.screened$ias[i] + data.screened$ias_sum[i-1]
    data.screened$ops_sum[i] = data.screened$ops[i] + data.screened$ops_sum[i-1]
    data.screened$sales_sum[i] = data.screened$sales[i] + data.screened$sales_sum[i-1]
    data.screened$count[i] = data.screened$count[i-1] + 1
  }
}

# merge rows that have the same newID, as defined above, to create unique business-year rows
data.screened <- transform(data.screened, keep = ave(count, newID, FUN = is.largest)) %>%
  filter(., keep == 1) %>%
  arrange(., gvkey, srcdate, SICS1, stype) %>%
  mutate(., dupe = 0)

# identify potential duplicates listed under different segment types
# specifically, identify rows that have a different stype than the row above, but have the same ias_sum, ops_sum, and sales_sum 
for (i in 2:nrow(data.screened)) {
  if ((data.screened$stype[i] != data.screened$stype[i-1]) & (data.screened$ias_sum[i] == data.screened$ias_sum[i-1]) & (data.screened$ops_sum[i] == data.screened$ops_sum[i-1]) & (data.screened$sales_sum[i] == data.screened$sales_sum[i-1])) {
    data.screened$dupe[i] = 1
  }
}

# drop potential duplicates and delete extra variables
data.screened <- filter(data.screened, dupe == 0) %>%
  select(., -c(sid, ias, ops, sales, datadate, count, dupe, keep))

# identify more potential duplicates by marking rows that have the same newID2, as defined below (i.e., different stype but same business-year ID)
data.screened <- mutate(data.screened, newID2 = paste(gvkey, year, SICS1), posdupe = 0, dupe = 0) %>%
  arrange(., newID2)

for (i in 2:nrow(data.screened)) {
  if (data.screened$newID2[i] == data.screened$newID2[i-1]) {
    data.screened$posdupe[i] = 1
    data.screened$posdupe[i-1] = 1
    if ((data.screened$ias_sum[i] == data.screened$ias_sum[i-1]) & (data.screened$ops_sum[i] == data.screened$ops_sum[i-1]) & (data.screened$sales_sum[i] == data.screened$sales_sum[i-1])) {
      data.screened$dupe[i] = 1
    }
  }
}

# drop more potential duplicates
data.screened <- filter(data.screened, dupe == 0) %>%
  mutate(., del.seg = 0)

# identify and drop GEOSEG rows that are likely duplicates of BUSSEG or OPSEG rows
for (i in which(data.screened$posdupe == 1)) {
  if ((data.screened$stype[i] == "GEOSEG")) {
    data.screened$del.seg[i] = 1
  }
}

data.screened <- filter(data.screened, del.seg == 0) %>%
  mutate(., posdupe = 0) %>%
  arrange(., newID2)

# check again for potential duplicates
for (i in 2:nrow(data.screened)) {
  if (data.screened$newID2[i] == data.screened$newID2[i-1]) {
    data.screened$posdupe[i] = 1
    data.screened$posdupe[i-1] = 1
    if ((data.screened$ias_sum[i] == data.screened$ias_sum[i-1]) & (data.screened$ops_sum[i] == data.screened$ops_sum[i-1]) & (data.screened$sales_sum[i] == data.screened$sales_sum[i-1])) {
      data.screened$dupe[i] = 1
    }
  }
}

data.screened <- filter(data.screened, dupe == 0)

# mark the latest srcdate to keep for any remaining potential duplicates with the same newID3, as defined below
data.screened <- arrange(data.screened, desc(posdupe)) %>%
  mutate(., keep.date = 0, newID3 = paste0(gvkey, stype, SICS1, snms, year))

for (i in which(data.screened$posdupe == 1)) {
  if ((data.screened$newID3[i] != data.screened$newID3[i+1])) {
    data.screened$keep.date[i] = 1
  }
}

data.screened <- filter(data.screened, posdupe == 0 | keep.date == 1) 

# check again for potential duplicates
data.screened <- mutate(data.screened, posdupe = 0) %>%
  arrange(., newID2)

for (i in 2:nrow(data.screened)) {
  if (data.screened$newID2[i] == data.screened$newID2[i-1]) {
    data.screened$posdupe[i] = 1
    data.screened$posdupe[i-1] = 1
    if ((data.screened$ias_sum[i] == data.screened$ias_sum[i-1]) & (data.screened$ops_sum[i] == data.screened$ops_sum[i-1]) & (data.screened$sales_sum[i] == data.screened$sales_sum[i-1])) {
      data.screened$dupe[i] = 1
    }
  }
}

data.screened <- arrange(data.screened, desc(posdupe))

# change & to "and" and ignore case in newID3 comparison to identify remaining potential duplicates
for (i in which(data.screened$posdupe == 1)) {
  data.screened$newID3[i] = str_to_lower(str_replace(data.screened$newID3[i], "&", "and"))
}

data.screened <- arrange(data.screened, desc(posdupe)) %>%
  mutate(., keep.date = 0)

for (i in which(data.screened$posdupe == 1)) {
  if ((data.screened$newID3[i] != data.screened$newID3[i+1])) {
    data.screened$keep.date[i] = 1
  }
}

data.screened <- filter(data.screened, posdupe == 0 | keep.date == 1) 

# at this point, very few unresolved overlaps with different srcdates should remain for the same reporting year 
# keep the latest srcdate per newID3 rather than drop all of these unresolved overlaps (choosing the other option makes very little difference)

data.screened <- arrange(data.screened, desc(posdupe)) %>%
  mutate(., keep.date = 0)

for (i in which(data.screened$posdupe == 1)) {
  if ((data.screened$newID2[i] != data.screened$newID2[i+1])) {
    data.screened$keep.date[i] = 1
  }
}

data.screened <- filter(data.screened, posdupe == 0 | keep.date == 1) 

# rearrange and check for potential duplicates one last time
data.screened <- mutate(data.screened, posdupe = 0) %>%
  arrange(., newID2)

for (i in 2:nrow(data.screened)) {
  if (data.screened$newID2[i] == data.screened$newID2[i-1]) {
    data.screened$posdupe[i] = 1
    data.screened$posdupe[i-1] = 1
    if ((data.screened$ias_sum[i] == data.screened$ias_sum[i-1]) & (data.screened$ops_sum[i] == data.screened$ops_sum[i-1]) & (data.screened$sales_sum[i] == data.screened$sales_sum[i-1])) {
      data.screened$dupe[i] = 1
    }
  }
}

data.screened <- arrange(data.screened, desc(posdupe))

# reduce data to necessary columns
data.screened <- select(data.screened, c(gvkey, stype, SICS1, NAICSS1, conm, cusip, cik, sic, naics, ias_sum, ops_sum, sales_sum, year))

# re-title columns and compute performance in new column
data.final <- mutate(data.screened, ID = paste0(gvkey, year, SICS1), ind.year = paste0(SICS1, year), corp.ind = paste(gvkey, SICS1), performance = ops_sum/ias_sum, dupe = 0) %>%
  arrange(., ID)

# check that each gvkey-SICS1-year (i.e., business-year) combination is unique
for (i in 2:nrow(data.final)) {
  if (data.final$ID[i] == data.final$ID[i-1]) {
    data.final$dupe[i] = 1
  }
}

# if no more duplicates are marked, save the current screening progress
if (sum(data.final$dupe) == 0){
  print("Ready for next steps!")
  saveRDS(data.final, "data_final.rds") # use this file to avoid going through the above screening process again
} else {
  print("Check for duplicates.")
}

### get base summary stats ###

# for each year, retrieve number of firms, number of industries, number of segments 

summary.stats <- group_by(data.final, year) %>%
  summarise(., num_firms = n_distinct(gvkey), num_inds = n_distinct(SICS1), num_segs = n_distinct(corp.ind))

# summary stats show that 1976, 1977, and 2020 have significantly fewer observations than other years, so drop those
# this restricts the study to the 42 years from 1978 to 2019

data.final <- filter(data.final, year != 1976, year != 1977, year != 2020)

# SFAS 131 correction -- identify gvkeys of firms that operated exactly one business segment in 1997
one.seg.97 <- filter(data.final, year == 1997) %>%
  group_by(., gvkey) %>%
  summarise(., n = n()) %>%
  filter(., n == 1) %>%
  select(., gvkey)

# drop single business firms in 1997 and their observations after 1997
data.final.reduced <- filter(data.final, !((gvkey %in% one.seg.97$gvkey) & (year >= 1997))) %>%
  # as a precautionary measure, drop rows that have a non-numeric performance value
  filter(., !((is.na(performance)) | (is.infinite(performance))))

# reduce data to columns necessary for analysis and convert performance to a percent measure
data.for.analysis <- select(data.final.reduced, -c(ind.year, corp.ind)) %>%
  mutate(., NAICSS1 = as.character(NAICSS1), naics = as.character(naics), year = as.character(year), performance = 100*performance)

# save the final screened sample for analysis
saveRDS(data.for.analysis, "data_for_analysis.rds")

