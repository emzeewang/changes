# How-to
Download .R files.

To replicate the outputs used to produce Figures 2 through 9 in [insert paper name here]:
1. Download raw data from https://wrds-www.wharton.upenn.edu/pages/get-data/compustat-capital-iq-standard-poors/compustat/historical-segments-daily/historical-segments/ and name the file "raw_data.csv".
2. Place "raw_data.csv" in the same folder as the .R files. 
3. For outputs with 4-digit SIC code industry definition (1978 - 2019), run clean_raw_data.R. Then ... 
   - Run run_HM1.R for VCA method outputs. 
   - Place MLwiN executable in 'C:/Program Files/MLwiN v3.05' then run run_R2MLwiN.R for HLM method outputs.
4. For outputs with 6-digit NAICS code industry definition (1997 - 2019), run clean_raw_data_naics.R. Then ... 
   - Run run_HM1_naics.R for VCA method outputs. 
   - Place MLwiN executable in 'C:/Program Files/MLwiN v3.05' then run run_R2MLwiN_naics.R for HLM method outputs.
