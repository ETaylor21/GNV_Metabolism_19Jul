library(fasstr)
library(streamMetabolizer)
library(tidyverse)
library (devtools)
library(StreamPULSE)
library(dplyr)
library(lubridate)
library(bayesplot)
library(driftR)

#####HATCHET CREEK#####

#Pre-Calibration# 
#Start Date: 2/13/19
#Start Time: 11:15 EST/ 16:15 UTC
#End Date: 2/14/19
#End Time: 11:20 EST/ 16:20 UTC

#Post-Calibration# 
#Start Date: 8/15/19
#Start Time: 16:20 EST/ 21:20 UTC
#End Date: 8/19/19
#End Time: 9:30 EST/ 14:30 UTC

##Step 1##
#calculate the average calibration values for the pre- and post-calibration bubbling events

HAT_precal = read_csv('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/Metabolism/Metab Test 2/Calibration_Files/HAT_20190213_20190214_SN20472627.csv', col_types = cols(
  DateTime_UTC = col_datetime(format = "%y-%m-%d %H:%M:%S"), 
  DOconc_mgL = col_double(),
  Temp_C = col_double()))

bubble_start_DO = HAT_precal %>% 
  filter(DateTime_UTC >= as_datetime('2019-02-13 16:19:06') & DateTime_UTC <=  as_datetime('2019-02-14 16:19:06')) %>% 
  summarise(mean(DOconc_mgL), mean(Temp_C))

bubble_start_DO

#Record Bubble Start Avg DO: 8.82 mg/L
#Record Bubble Start Avg Temp: 20.5 C
#DO look-up value: 9.0 mg/L


HAT_postcal = read_csv('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/Metabolism/Metab Test 2/Calibration_Files/HAT_Eliza_20190814_20190819_SN20472627.csv', col_types = cols(
  DateTime_UTC = col_datetime(format = "%y-%m-%d %H:%M:%S"), 
  DOconc_mgL = col_double(),
  Temp_C = col_double()))

bubble_end_DO = HAT_postcal %>% 
  filter(DateTime_UTC >= as_datetime('2019-08-15 21:20:06') & DateTime_UTC <=  as_datetime('2019-08-19 14:30:06')) %>% 
  summarise(mean(DOconc_mgL), mean(Temp_C))

bubble_end_DO

#Record Bubble End Avg DO: 8.62 mg/L
#Record Bubble End Avg Temp: 21.5 C
#DO look-up value: 8.8 mg/L


data = read_csv('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/Metabolism/Metab Test 2/Calibration_Files/HAT_20190214_20190814_SN20472627_summary.csv', col_types = cols(
  Date = col_date(format = "%y-%m-%d"),
  Time_UTC = col_time(format = "%H:%M:%S"), 
  DOconc_mgL = col_double(),
  Temp_C = col_double()))

data_clean = dr_factor(data, corrFactor = corrFac, dateVar = Date, timeVar = Time_UTC, keepDateTime = TRUE) %>%
  dr_correctOne(sourceVar = DOconc_mgL, cleanVar = DO_Corr, calVal = 8.62, calStd = 8.8, factorVar = corrFac)

data_clean

write_csv(data_clean, path = "C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/Metabolism/Metab Test 2/Calibration_Files/HAT_20190214_20190814_SN20472627_summary_clean.csv", 
          na = 'NA', col_names = TRUE)


data2 = read_csv('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/Metabolism/Metab Test 2/Calibration_Files/HAT_sensordata_15min.csv', col_types = cols(
  Date = col_date(format = "%y-%m-%d"),
  Time_UTC = col_time(format = "%H:%M:%S"), 
  DOconc_mgL = col_double()))

data_clean2 = dr_factor(data2, corrFactor = corrFac, dateVar = Date, timeVar = Time_UTC, keepDateTime = TRUE) %>%
  dr_correctOne(sourceVar = DOconc_mgL, cleanVar = DO_Corr, calVal = 8.62, calStd = 8.8, factorVar = corrFac)

data_clean2

write_csv(data_clean2, path = "C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/Metabolism/Metab Test 2/Calibration_Files/HAT_20190214_20190814_SN20472627_summary_clean2_15min.csv", 
          na = 'NA', col_names = TRUE)


#####POSSUM CREEK#####

#Pre-Calibration# 
#Start Date: 2/13/19
#Start Time: 11:15 EST/ 16:15 UTC
#End Date: 2/14/19
#End Time: 11:20 EST/ 16:20 UTC

#Post-Calibration# 
#Start Date: 8/15/19
#Start Time: 16:20 EST/ 21:20 UTC
#End Date: 8/19/19
#End Time: 9:30 EST/ 14:30 UTC

##Step 1##
#calculate the average calibration values for the pre- and post-calibration bubbling events

POS_precal = read_csv('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/Metabolism/Metab Test 2/Calibration_Files/POSNW16_20190213_20190214_SN20472624.csv', col_types = cols(
  DateTime_UTC = col_datetime(format = "%y-%m-%d %H:%M:%S"), 
  DOconc_mgL = col_double(),
  Temp_C = col_double()))

bubble_start_DO = POS_precal %>% 
  filter(DateTime_UTC >= as_datetime('2019-02-13 16:19:06') & DateTime_UTC <=  as_datetime('2019-02-14 16:19:06')) %>% 
  summarise(mean(DOconc_mgL), mean(Temp_C))

bubble_start_DO

#Record Bubble Start Avg DO: 9.03 mg/L
#Record Bubble Start Avg Temp: 20.4 C
#DO look-up value: 9.02 mg/L


POS_postcal = read_csv('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/Metabolism/Metab Test 2/Calibration_Files/POS_Jane_20190618_20190819_SN20472624.csv', col_types = cols(
  DateTime_UTC = col_datetime(format = "%y-%m-%d %H:%M:%S"), 
  DOconc_mgL = col_double(),
  Temp_C = col_double()))

bubble_end_DO = POS_postcal %>% 
  filter(DateTime_UTC >= as_datetime('2019-08-15 21:20:06') & DateTime_UTC <=  as_datetime('2019-08-19 14:30:06')) %>% 
  summarise(mean(DOconc_mgL), mean(Temp_C))

bubble_end_DO

#Record Bubble End Avg DO: 8.72 mg/L
#Record Bubble End Avg Temp: 21.4 C
#DO look-up value: 8.85 mg/L

###This would be the better method to process data in that I woud run the drift correction first and then upload to StreamPULSE for data cleaning###
data = read_csv('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/Metabolism/Metab Test 2/Calibration_Files/POS_20190214_20190814_SN20472627_summary.csv', col_types = cols(
  Date = col_date(format = "%y-%m-%d"),
  Time_UTC = col_time(format = "%H:%M:%S"), 
  DOconc_mgL = col_double(),
  Temp_C = col_double()))

data_clean = dr_factor(data, corrFactor = corrFac, dateVar = Date, timeVar = Time_UTC, keepDateTime = TRUE) %>%
  dr_correctOne(sourceVar = DOconc_mgL, cleanVar = DO_Corr, calVal = 8.62, calStd = 8.8, factorVar = corrFac)

data_clean

write_csv(data_clean, path = "C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/Metabolism/Metab Test 2/Calibration_Files/POS_20190214_20190814_SN20472627_summary_clean.csv", 
          na = 'NA', col_names = TRUE)


data2 = read_csv('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/Metabolism/Metab Test 2/Calibration_Files/POS_sensordata_15min.csv', col_types = cols(
  Date = col_date(format = "%y-%m-%d"),
  Time_UTC = col_time(format = "%H:%M:%S"), 
  DOconc_mgL = col_double()))

data_clean2 = dr_factor(data2, corrFactor = corrFac, dateVar = Date, timeVar = Time_UTC, keepDateTime = TRUE) %>%
  dr_correctOne(sourceVar = DOconc_mgL, cleanVar = DO_Corr, calVal = 8.72, calStd = 8.85, factorVar = corrFac)

data_clean2

write_csv(data_clean2, path = "C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/Metabolism/Metab Test 2/Calibration_Files/POS_20190214_20190814_SN20472627_summary_clean2_15min.csv", 
          na = 'NA', col_names = TRUE)


#####HOGDN#####

#Pre-Calibration# 
#Start Date: 2/13/19
#Start Time: 11:15 EST/ 16:15 UTC
#End Date: 2/14/19
#End Time: 11:20 EST/ 16:20 UTC

#Post-Calibration# 
#Start Date: 8/15/19
#Start Time: 16:20 EST/ 21:20 UTC
#End Date: 8/19/19
#End Time: 9:30 EST/ 14:30 UTC


##Step 1##
#calculate the average calibration values for the pre- and post-calibration bubbling events

HOGDN_precal = read_csv('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/Metabolism/Metab Test 2/Calibration_Files/HOGDN_20190213_20190214_SN20472628.csv', col_types = cols(
  DateTime_UTC = col_datetime(format = "%y-%m-%d %H:%M:%S"), 
  DOconc_mgL = col_double(),
  Temp_C = col_double()))

bubble_start_DO = HOGDN_precal %>% 
  filter(DateTime_UTC >= as_datetime('2019-02-13 16:19:06') & DateTime_UTC <=  as_datetime('2019-02-14 16:19:06')) %>% 
  summarise(mean(DOconc_mgL), mean(Temp_C))

bubble_start_DO

#Record Bubble Start Avg DO: 8.89 mg/L
#Record Bubble Start Avg Temp: 20.4 C
#DO look-up value: 9.02 mg/L


HOGDN_postcal = read_csv('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/Metabolism/Metab Test 2/Calibration_Files/HOGDN_Ellen_20190814_20190819_SN20472628.csv', col_types = cols(
  DateTime_UTC = col_datetime(format = "%y-%m-%d %H:%M:%S"), 
  DOconc_mgL = col_double(),
  Temp_C = col_double()))

bubble_end_DO = HOGDN_postcal %>% 
  filter(DateTime_UTC >= as_datetime('2019-08-15 21:20:06') & DateTime_UTC <=  as_datetime('2019-08-19 14:30:06')) %>% 
  summarise(mean(DOconc_mgL), mean(Temp_C))

bubble_end_DO

#Record Bubble End Avg DO: 8.52 mg/L
#Record Bubble End Avg Temp: 21.4 C
#DO look-up value: 8.85 mg/L

###This would be the better method to process data in that I woud run the drift correction first and then upload to StreamPULSE for data cleaning###
data = read_csv('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/Metabolism/Metab Test 2/Calibration_Files/HOGDN_20190214_20190814_SN20472627_summary.csv', col_types = cols(
  Date = col_date(format = "%y-%m-%d"),
  Time_UTC = col_time(format = "%H:%M:%S"), 
  DOconc_mgL = col_double(),
  Temp_C = col_double()))

data_clean = dr_factor(data, corrFactor = corrFac, dateVar = Date, timeVar = Time_UTC, keepDateTime = TRUE) %>%
  dr_correctOne(sourceVar = DOconc_mgL, cleanVar = DO_Corr, calVal = 8.52, calStd = 8.85, factorVar = corrFac)

data_clean

write_csv(data_clean, path = "C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/Metabolism/Metab Test 2/Calibration_Files/HOGDN_20190214_20190814_SN20472627_summary_clean.csv", 
          na = 'NA', col_names = TRUE)


data2 = read_csv('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/Metabolism/Metab Test 2/Calibration_Files/HOGDN_sensordata_15min.csv', col_types = cols(
  Date = col_date(format = "%y-%m-%d"),
  Time_UTC = col_time(format = "%H:%M:%S"), 
  DOconc_mgL = col_double()))

data_clean2 = dr_factor(data2, corrFactor = corrFac, dateVar = Date, timeVar = Time_UTC, keepDateTime = TRUE) %>%
  dr_correctOne(sourceVar = DOconc_mgL, cleanVar = DO_Corr, calVal = 8.52, calStd = 8.85, factorVar = corrFac)

data_clean2

write_csv(data_clean2, path = "C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/Metabolism/Metab Test 2/Calibration_Files/HOGDN_20190214_20190814_SN20472627_summary_clean2_15min.csv", 
          na = 'NA', col_names = TRUE)


#####HOGNW16th#####

#Pre-Calibration# 
#Start Date: 04/09/19
#Start Time: 12:20 EST/ 17:20 UTC
#End Date: 04/10/19
#End Time: 13:25 EST/ 18:20 UTC

#Post-Calibration# 
#Start Date: 8/15/19
#Start Time: 16:20 EST/ 21:20 UTC
#End Date: 8/19/19
#End Time: 9:30 EST/ 14:30 UTC

HOGNW16th_precal = read_csv('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/Metabolism/Metab Test 2/Calibration_Files/HOGNW16th_20190409_20190410_SN20472623.csv', col_types = cols(
  DateTime_UTC = col_datetime(format = "%y-%m-%d %H:%M:%S"), 
  DOconc_mgL = col_double(),
  Temp_C = col_double()))

bubble_start_DO = HOGNW16th_precal %>% 
  filter(DateTime_UTC >= as_datetime('2019-04-09 21:21:46') & DateTime_UTC <=  as_datetime('2019-04-10 18:21:46')) %>% 
  summarise(mean(DOconc_mgL), mean(Temp_C))

bubble_start_DO

#Record Bubble Start Avg DO: 8.61 mg/L
#Record Bubble Start Avg Temp: 21.4 C
#DO look-up value: 8.85 mg/L


HOGNW16th_postcal = read_csv('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/Metabolism/Metab Test 2/Calibration_Files/HOGUP_Maya_20190814_20190819_SN20472623.csv', col_types = cols(
  DateTime_UTC = col_datetime(format = "%y-%m-%d %H:%M:%S"), 
  DOconc_mgL = col_double(),
  Temp_C = col_double()))

bubble_end_DO = HOGNW16th_postcal %>% 
  filter(DateTime_UTC >= as_datetime('2019-08-15 21:20:06') & DateTime_UTC <=  as_datetime('2019-08-19 14:30:06')) %>% 
  summarise(mean(DOconc_mgL), mean(Temp_C))

bubble_end_DO

#Record Bubble End Avg DO: 8.72 mg/L
#Record Bubble End Avg Temp: 21.4 C
#DO look-up value: 8.85 mg/L

###This would be the better method to process data in that I woud run the drift correction first and then upload to StreamPULSE for data cleaning###
data = read_csv('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/Metabolism/Metab Test 2/Calibration_Files/HOGNW16th_20190214_20190814_SN20472627_summary.csv', col_types = cols(
  Date = col_date(format = "%y-%m-%d"),
  Time_UTC = col_time(format = "%H:%M:%S"), 
  DOconc_mgL = col_double(),
  Temp_C = col_double()))

data_clean = dr_factor(data, corrFactor = corrFac, dateVar = Date, timeVar = Time_UTC, keepDateTime = TRUE) %>%
  dr_correctOne(sourceVar = DOconc_mgL, cleanVar = DO_Corr, calVal = 8.72, calStd = 8.85, factorVar = corrFac)

data_clean

write_csv(data_clean, path = "C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/Metabolism/Metab Test 2/Calibration_Files/HOGNW16th_20190214_20190814_SN20472627_summary_clean.csv", 
          na = 'NA', col_names = TRUE)


data2 = read_csv('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/Metabolism/Metab Test 2/Calibration_Files/HOGUP_sensordata_15min.csv', col_types = cols(
  Date = col_date(format = "%y-%m-%d"),
  Time_UTC = col_time(format = "%H:%M:%S"), 
  DOconc_mgL = col_double()))

data_clean2 = dr_factor(data2, corrFactor = corrFac, dateVar = Date, timeVar = Time_UTC, keepDateTime = TRUE) %>%
  dr_correctOne(sourceVar = DOconc_mgL, cleanVar = DO_Corr, calVal = 8.72, calStd = 8.85, factorVar = corrFac)

data_clean2

write_csv(data_clean2, path = "C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/Metabolism/Metab Test 2/Calibration_Files/HOGNW16th_20190214_20190814_SN20472627_summary_clean2_15min.csv", 
          na = 'NA', col_names = TRUE)



#####SWEETWATER BRANCH#####

#Pre-Calibration# 
#Start Date: 04/09/19
#Start Time: 12:20 EST/ 17:20 UTC
#End Date: 04/10/19
#End Time: 13:25 EST/ 18:20 UTC

#Post-Calibration# 
#Start Date: 8/15/19
#Start Time: 16:20 EST/ 21:20 UTC
#End Date: 8/19/19
#End Time: 9:30 EST/ 14:30 UTC



#####TUMBLIN CREEK#####

#Pre-Calibration# 
#Start Date: 2/13/19
#Start Time: 11:15 EST/ 16:15 UTC
#End Date: 2/14/19
#End Time: 11:20 EST/ 16:20 UTC

#Post-Calibration# 
#Start Date: 6/19/19
#Start Time: 14:48 EST/ 18:48 UTC
#End Date: 6/21/19
#End Time: 13:14 EST/ 17:14 UTC


TUM_precal = read_csv('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/Metabolism/Metab Test 2/Calibration_Files/TUM441_20190213_20190214_SN20472625.csv', col_types = cols(
  DateTime_UTC = col_datetime(format = "%y-%m-%d %H:%M:%S"), 
  DOconc_mgL = col_double(),
  Temp_C = col_double()))

bubble_start_DO = TUM_precal %>% 
  filter(DateTime_UTC >= as_datetime('2019-02-13 16:19:06') & DateTime_UTC <=  as_datetime('2019-02-14 16:19:06')) %>% 
  summarise(mean(DOconc_mgL), mean(Temp_C))

bubble_start_DO

#Record Bubble Start Avg DO: 8.61 mg/L
#Record Bubble Start Avg Temp: 21.4 C
#DO look-up value: 8.85 mg/L


TUM_postcal = read_csv('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/Metabolism/Metab Test 2/Calibration_Files/TUM441_20190424_20190710_SN20472625.csv', col_types = cols(
  DateTime_UTC = col_datetime(format = "%y-%m-%d %H:%M:%S"), 
  DOconc_mgL = col_double(),
  Temp_C = col_double()))

bubble_end_DO = TUM_postcal %>% 
  filter(DateTime_UTC >= as_datetime('2019-06-19 18:20:06') & DateTime_UTC <=  as_datetime('2019-06-21 17:30:06')) %>% 
  summarise(mean(DOconc_mgL), mean(Temp_C))

bubble_end_DO

#Record Bubble End Avg DO: 8.72 mg/L
#Record Bubble End Avg Temp: 21.4 C
#DO look-up value: 8.85 mg/L

###This would be the better method to process data in that I woud run the drift correction first and then upload to StreamPULSE for data cleaning###
#data = read_csv('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/Metabolism/Metab Test 2/Calibration_Files/HOGNW16th_20190214_20190814_SN20472627_summary.csv', col_types = cols(
  #Date = col_date(format = "%y-%m-%d"),
  #Time_UTC = col_time(format = "%H:%M:%S"), 
  #DOconc_mgL = col_double(),
  #Temp_C = col_double()))

#data_clean = dr_factor(data, corrFactor = corrFac, dateVar = Date, timeVar = Time_UTC, keepDateTime = TRUE) %>%
  #dr_correctOne(sourceVar = DOconc_mgL, cleanVar = DO_Corr, calVal = 8.72, calStd = 8.85, factorVar = corrFac)

#data_clean

#write_csv(data_clean, path = "C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/Metabolism/Metab Test 2/Calibration_Files/HOGNW16th_20190214_20190814_SN20472627_summary_clean.csv", 
          #na = 'NA', col_names = TRUE)


data2 = read_csv('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/Metabolism/Metab Test 2/Calibration_Files/HOGUP_sensordata_15min.csv', col_types = cols(
  Date = col_date(format = "%y-%m-%d"),
  Time_UTC = col_time(format = "%H:%M:%S"), 
  DOconc_mgL = col_double()))

data_clean2 = dr_factor(data2, corrFactor = corrFac, dateVar = Date, timeVar = Time_UTC, keepDateTime = TRUE) %>%
  dr_correctOne(sourceVar = DOconc_mgL, cleanVar = DO_Corr, calVal = 8.72, calStd = 8.85, factorVar = corrFac)

data_clean2

write_csv(data_clean2, path = "C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/Metabolism/Metab Test 2/Calibration_Files/HOGNW16th_20190214_20190814_SN20472627_summary_clean2_15min.csv", 
          na = 'NA', col_names = TRUE)


