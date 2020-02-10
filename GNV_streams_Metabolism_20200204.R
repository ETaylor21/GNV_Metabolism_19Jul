#####GENERAL INFO ABOUT SITES######
#HATCHET sensor reported out of water from 3/22/2019 to 4/11 when it was fixed 
#LONGITUDES: 
#HAT = -82.22
#SWB and SWBUP = -82.32
#HOGNW16 (UP) = -82.35
#HOGDN = -82.39
#POSNW16 = -82.36
#TUM441 = -82.34


#install.packages( 
# c("LakeMetabolizer","unitted","dplyr","lazyeval","lubridate","magrittr",
#   "tidyr","chron","dygraphs","ggplot2","RCurl","rstan","XML","xts"),
#  repos=c("https://owi.usgs.gov/R","https://cran.rstudio.com"))
#devtools::install_github("USGS-R/streamMetabolizer")
#devtools::find_rtools()

#install.packages("remotes")
#remotes::install_github("bcgov/fasstr")

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
HAT = read_csv('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/Metabolism/Metab Test 2/FL_HAT_sensorData.csv', col_types = cols(
  DateTime_UTC = col_datetime(format = "%y-%m-%d %H:%M:%S"), 
  region = col_character(),
  site = col_character(),
  variable = col_character(),
  value = col_double(),
  flagtype = col_character(),
  flagcomment = col_character()))

HATDischarge = read_csv('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/StreamPULSE Data Uploads/Discharge/FL_HAT_2019-09-01_XX.csv')

HATPressure = read_csv('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/Pressure/FL_HAT_2019-11-13_XX.csv')

HAT2 = HAT %>% 
  filter(DateTime_UTC >= as.Date('2019-02-22') & DateTime_UTC <=  as.Date('2019-08-01')) %>% 
  filter(is.na(flagtype)) %>% 
  select(DateTime_UTC, variable, value) %>% 
  spread(key = variable, value = value)

HAT2

HATPres = HATPressure %>% 
  filter(datetime >= as.Date('2019-02-22') & datetime <=  as.Date('2019-08-01'))

HATWaterTemp = HAT2 %>%
  filter(DateTime_UTC >= as.Date('2019-02-22') & DateTime_UTC <=  as.Date('2019-08-01'))


DO = bind_cols(HATPres, HATWaterTemp)


#because I've already converted to UTC, I can skip the conversion to local time and just use convert_UTC_to-solartime
#according to the package, light needs to be in PAR... I know we've gone over this multiple times, I just need to look back at notes to double check what the conclusion was. For now, light stays in lux
DO = DO %>%
  select(datetime, `Air Pressure (mb)`, AirTemp_C, Discharge_m3s, DO_mgL, Light_lux, WaterTemp_C) %>% 
  mutate(DO.sat = calc_DO_sat(temp.water = DO$WaterTemp_C, pressure.air = DO$`Air Pressure (mb)`, model = 'garcia-benson'), 
         depth = calc_depth(Q = DO$Discharge_m3s), 
         solar.time = convert_UTC_to_solartime(date.time = datetime, longitude = -82.22, time.type = 'mean' ))

DO$solar.time = as.POSIXct(x = DO$solar.time, tz = 'UTC')

HAT_metab_inputs = DO %>% 
  select(solar.time, DO_mgL, DO.sat, depth, WaterTemp_C, Light_lux, Discharge_m3s) %>% 
  rename(DO.obs = DO_mgL, temp.water = WaterTemp_C, light = Light_lux, discharge = Discharge_m3s)#rename columns 

HAT_metab_inputs$discharge = baytrends::fillMissing(HAT_metab_inputs$discharge)
HAT_metab_inputs$depth = baytrends::fillMissing(HAT_metab_inputs$depth)

HAT_metab_inputs %>% 
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
  select(solar.time, discharge, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO'), discharge) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')



HAT_metab_inputs = as.data.frame(HAT_metab_inputs)

#write_csv(HAT_metab_inputs,'C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/StreamPULSE Data Uploads/FL_HAT_2019-09-01_Metab_v2.csv' )

#dat = read_csv('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/StreamPULSE Data Uploads/FL_HAT_2019-09-01_Metab_v2.csv', col_names = TRUE)


#dat2 = as.data.frame(dat)
#colnames(dat2) = c( 'solar.time', 'DO.obs', 'DO.sat', 'depth', 'temp.water', 'light', 'discharge')

bayes_name <- mm_name(type='bayes', pool_K600='binned')# I specified 'binned' in poll_k600 after recieving the error that discharge data should only be included if & only if pool_k600_type indicates hierarchy...not sure what that means but setting the poll_k600 to normal did nothing so I tried binned and it ran
bayes_name

bayes_specs <- specs(bayes_name, day_start = 4, day_end = 28, burnin_steps = 500, saved_steps = 500)
#bayes_specs

mm <- metab(bayes_specs, data = HAT_metab_inputs)

mm

output<-as.data.frame(get_params(mm))
output#check output data

write.csv(output, 'HAT_Test_1_Output_3.csv') #whatever filename is

plot_DO_preds(mm)
plot_metab_preds(mm)

mcmc <- get_mcmc(mm)
rstan::traceplot(mcmc, pars='K600_daily', nrow=8)

#####POSSUM CREEK####

SiteData = read_csv('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/Metabolism/Metab Test 2/FL_POSNW16_sensorData.csv', col_types = cols(
  DateTime_UTC = col_datetime(format = "%y-%m-%d %H:%M:%S"), 
  region = col_character(),
  site = col_character(),
  variable = col_character(),
  value = col_double(),
  flagtype = col_character(),
  flagcomment = col_character()))

Raw_Discharge = read_csv('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/StreamPULSE Data Uploads/Discharge/FL_POSNW16_2019-08-14_XX.csv')

FAWN_Pressure = read_csv('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/Pressure/FL_HAT_2019-11-13_XX.csv')

SiteData2 = SiteData %>% 
  filter(DateTime_UTC >= as.Date('2019-02-22') & DateTime_UTC <=  as.Date('2019-08-01')) %>% 
  filter(is.na(flagtype)) %>% 
  select(DateTime_UTC, variable, value) %>% 
  spread(key = variable, value = value)

Pres = FAWN_Pressure %>% 
  filter(datetime >= as.Date('2019-02-22') & datetime <=  as.Date('2019-08-01'))

WaterTemp = SiteData2 %>%
  filter(DateTime_UTC >= as.Date('2019-02-22') & DateTime_UTC <=  as.Date('2019-08-01'))


DO = bind_cols(Pres, WaterTemp)


#because I've already converted to UTC, I can skip the conversion to local time and just use convert_UTC_to-solartime, just make sure to specify time.type to mean, otherwise it defaults to zenith peaks and your times start drifting and the bayes model won't run
#according to the package, light needs to be in PAR... I know we've gone over this multiple times, I just need to look back at notes to double check what the conclusion was. For now, light stays in lux
DO = DO %>%
  select(datetime, `Air Pressure (mb)`, AirTemp_C, Discharge_m3s, DO_mgL, Light_lux, WaterTemp_C) %>% 
  mutate(DO.sat = calc_DO_sat(temp.water = DO$WaterTemp_C, pressure.air = DO$`Air Pressure (mb)`, model = 'garcia-benson'), 
         depth = calc_depth(Q = DO$Discharge_m3s), 
         solar.time = convert_UTC_to_solartime(date.time = datetime, longitude = -82.36, time.type = 'mean' ))

DO$solar.time = as.POSIXct(x = DO$solar.time, tz = 'UTC')

Metab_inputs = DO %>% 
  select(solar.time, DO_mgL, DO.sat, depth, WaterTemp_C, Light_lux, Discharge_m3s) %>% 
  rename(DO.obs = DO_mgL, temp.water = WaterTemp_C, light = Light_lux, discharge = Discharge_m3s)#rename columns 

Metab_inputs$discharge = baytrends::fillMissing(Metab_inputs$discharge)
Metab_inputs$depth = baytrends::fillMissing(Metab_inputs$depth)

Metab_inputs %>% 
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
  select(solar.time, discharge, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO'), discharge) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')



Metab_inputs = as.data.frame(Metab_inputs)

#write_csv(HAT_metab_inputs,'C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/StreamPULSE Data Uploads/FL_HAT_2019-09-01_Metab.csv' )

#dat = read_csv('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/StreamPULSE Data Uploads/FL_HAT_2019-09-01_Metab.csv', col_names = TRUE)


#dat2 = as.data.frame(dat)
#colnames(dat2) = c( 'solar.time', 'DO.obs', 'DO.sat', 'depth', 'temp.water', 'light', 'discharge')

bayes_name <- mm_name(type='bayes', pool_K600='binned')
bayes_name

bayes_specs <- specs(bayes_name, day_start = 4, day_end = 28, burnin_steps = 500, saved_steps = 500)
#bayes_specs

mm <- metab(bayes_specs, data = Metab_inputs)


mm

output<-as.data.frame(get_params(mm))
output#check output data

write.csv(output, 'POS_Test_1_Output_v2.csv') #whatever filename is

plot_DO_preds(mm)
plot_metab_preds(mm)

mcmc <- get_mcmc(mm)
rstan::traceplot(mcmc, pars='K600_daily', nrow=8)


#####HOGDN######

SiteData = read_csv('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/Metabolism/Metab Test 2/FL_HOGDN_sensorData.csv', col_types = cols(
  DateTime_UTC = col_datetime(format = "%y-%m-%d %H:%M:%S"), 
  region = col_character(),
  site = col_character(),
  variable = col_character(),
  value = col_double(),
  flagtype = col_character(),
  flagcomment = col_character()))

Raw_Discharge = read_csv('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/StreamPULSE Data Uploads/Discharge/FL_HOGDN_2019-09-01_XX.csv')

FAWN_Pressure = read_csv('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/Pressure/FL_HAT_2019-11-13_XX.csv')

SiteData2 = SiteData %>% 
  filter(DateTime_UTC >= as.Date('2019-02-22') & DateTime_UTC <=  as.Date('2019-08-01')) %>% 
  filter(is.na(flagtype)) %>% 
  select(DateTime_UTC, variable, value) %>% 
  spread(key = variable, value = value)

Pres = FAWN_Pressure %>% 
  filter(datetime >= as.Date('2019-02-22') & datetime <=  as.Date('2019-08-01'))

WaterTemp = SiteData2 %>%
  filter(DateTime_UTC >= as.Date('2019-02-22') & DateTime_UTC <=  as.Date('2019-08-01'))

DO = bind_cols(Pres, WaterTemp)


#because I've already converted to UTC, I can skip the conversion to local time and just use convert_UTC_to-solartime, just make sure to specify time.type to mean, otherwise it defaults to zenith peaks and your times start drifting and the bayes model won't run
#according to the package, light needs to be in PAR... I know we've gone over this multiple times, I just need to look back at notes to double check what the conclusion was. For now, light stays in lux
DO = DO %>%
  select(datetime, `Air Pressure (mb)`, AirTemp_C, Discharge_m3s, DO_mgL, Light_lux, WaterTemp_C) %>% 
  mutate(DO.sat = calc_DO_sat(temp.water = DO$WaterTemp_C, pressure.air = DO$`Air Pressure (mb)`, model = 'garcia-benson'), 
         depth = calc_depth(Q = DO$Discharge_m3s), 
         solar.time = convert_UTC_to_solartime(date.time = datetime, longitude = -82.36, time.type = 'mean' ))

DO$solar.time = as.POSIXct(x = DO$solar.time, tz = 'UTC')

Metab_inputs = DO %>% 
  select(solar.time, DO_mgL, DO.sat, depth, WaterTemp_C, Light_lux, Discharge_m3s) %>% 
  rename(DO.obs = DO_mgL, temp.water = WaterTemp_C, light = Light_lux, discharge = Discharge_m3s)#rename columns 

Metab_inputs$discharge = baytrends::fillMissing(Metab_inputs$discharge)
Metab_inputs$depth = baytrends::fillMissing(Metab_inputs$depth)

Metab_inputs %>% 
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
  select(solar.time, discharge, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO'), discharge) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

Metab_inputs = as.data.frame(Metab_inputs)

#write_csv(HAT_metab_inputs,'C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/StreamPULSE Data Uploads/FL_HAT_2019-09-01_Metab.csv' )

#dat = read_csv('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/StreamPULSE Data Uploads/FL_HAT_2019-09-01_Metab.csv', col_names = TRUE)


#dat2 = as.data.frame(dat)
#colnames(dat2) = c( 'solar.time', 'DO.obs', 'DO.sat', 'depth', 'temp.water', 'light', 'discharge')

bayes_name <- mm_name(type='bayes', pool_K600='binned')
bayes_name

bayes_specs <- specs(bayes_name, day_start = 4, day_end = 28, burnin_steps = 500, saved_steps = 500)
#bayes_specs

mm <- metab(bayes_specs, data = Metab_inputs)

mm

output<-as.data.frame(get_params(mm))
output#check output data

write.csv(output, 'HOGDN_Test_1_Output.csv') #whatever filename is

plot_DO_preds(mm)
plot_metab_preds(mm)

mcmc <- get_mcmc(mm)
rstan::traceplot(mcmc, pars='K600_daily', nrow=8)


######HOGNW16th######

SiteData = read_csv('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/Metabolism/Metab Test 2/FL_HOGNW16_sensorData_202000206.csv', col_types = cols(
  DateTime_UTC = col_datetime(format = "%y-%m-%d %H:%M:%S"), 
  region = col_character(),
  site = col_character(),
  variable = col_character(),
  value = col_double(),
  flagtype = col_character(),
  flagcomment = col_character()))

Raw_Discharge = read_csv('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/StreamPULSE Data Uploads/Discharge/FL_HOGNW16_2019-08-14_XX.csv')

FAWN_Pressure = read_csv('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/Pressure/FL_HAT_2019-11-13_XX.csv')

SiteData2 = SiteData %>% 
  filter(DateTime_UTC >= as.Date('2019-04-12') & DateTime_UTC <=  as.Date('2019-08-01')) %>% 
  filter(is.na(flagtype)) %>% 
  select(DateTime_UTC, variable, value) %>% 
  spread(key = variable, value = value)

Pres = FAWN_Pressure %>% 
  filter(datetime >= as.Date('2019-04-12') & datetime <=  as.Date('2019-08-01'))

WaterTemp = SiteData2 %>%
  filter(DateTime_UTC >= as.Date('2019-04-12') & DateTime_UTC <=  as.Date('2019-08-01'))

DO = bind_cols(Pres, WaterTemp)


#because I've already converted to UTC, I can skip the conversion to local time and just use convert_UTC_to-solartime, just make sure to specify time.type to mean, otherwise it defaults to zenith peaks and your times start drifting and the bayes model won't run
#according to the package, light needs to be in PAR... I know we've gone over this multiple times, I just need to look back at notes to double check what the conclusion was. For now, light stays in lux
DO = DO %>%
  select(datetime, `Air Pressure (mb)`, AirTemp_C, Discharge_m3s, DO_mgL, Light_lux, WaterTemp_C) %>% 
  mutate(DO.sat = calc_DO_sat(temp.water = DO$WaterTemp_C, pressure.air = DO$`Air Pressure (mb)`, model = 'garcia-benson'), 
         depth = calc_depth(Q = DO$Discharge_m3s), 
         solar.time = convert_UTC_to_solartime(date.time = datetime, longitude = -82.36, time.type = 'mean' ))

DO$solar.time = as.POSIXct(x = DO$solar.time, tz = 'UTC')

Metab_inputs = DO %>% 
  select(solar.time, DO_mgL, DO.sat, depth, WaterTemp_C, Light_lux, Discharge_m3s) %>% 
  rename(DO.obs = DO_mgL, temp.water = WaterTemp_C, light = Light_lux, discharge = Discharge_m3s)#rename columns 

Metab_inputs$discharge = baytrends::fillMissing(Metab_inputs$discharge)
Metab_inputs$depth = baytrends::fillMissing(Metab_inputs$depth)

Metab_inputs %>% 
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
  select(solar.time, discharge, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO'), discharge) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

Metab_inputs = as.data.frame(Metab_inputs)

#write_csv(HAT_metab_inputs,'C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/StreamPULSE Data Uploads/FL_HAT_2019-09-01_Metab.csv' )

#dat = read_csv('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/StreamPULSE Data Uploads/FL_HAT_2019-09-01_Metab.csv', col_names = TRUE)


#dat2 = as.data.frame(dat)
#colnames(dat2) = c( 'solar.time', 'DO.obs', 'DO.sat', 'depth', 'temp.water', 'light', 'discharge')

bayes_name <- mm_name(type='bayes', pool_K600='binned')
bayes_name

bayes_specs <- specs(bayes_name, day_start = 4, day_end = 28, burnin_steps = 500, saved_steps = 500)
#bayes_specs

mm <- metab(bayes_specs, data = Metab_inputs)


mm

output<-as.data.frame(get_params(mm))
output#check output data

write.csv(output, 'HOGDN_Test_1_Output.csv') #whatever filename is

plot_DO_preds(mm)
plot_metab_preds(mm)

mcmc <- get_mcmc(mm)
rstan::traceplot(mcmc, pars='K600_daily', nrow=8)





######TUM441######

SiteData = read_csv('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/Metabolism/Metab Test 2/FL_TUM441_sensorData.csv', col_types = cols(
  DateTime_UTC = col_datetime(format = "%y-%m-%d %H:%M:%S"), 
  region = col_character(),
  site = col_character(),
  variable = col_character(),
  value = col_double(),
  flagtype = col_character(),
  flagcomment = col_character()))

Raw_Discharge = read_csv('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/StreamPULSE Data Uploads/Discharge/FL_TUM441_2019-08-14_XX.csv')

FAWN_Pressure = read_csv('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/Pressure/FL_HAT_2019-11-13_XX.csv')

SiteData2 = SiteData %>% 
  mutate(datetime = floor_date(DateTime_UTC, unit = '5 mins')) %>% 
  filter(DateTime_UTC >= as.Date('2019-02-23') & DateTime_UTC <=  as.Date('2019-06-18')) %>% 
  filter(is.na(flagtype)) %>% 
  select(datetime, variable, value) %>% 
  spread(key = variable, value = value) %>% 
  select(datetime, AirTemp_C, DO_mgL, Light_lux, WaterTemp_C)
  

#SiteData3 = SiteData %>% 
 # filter(DateTime_UTC >= as.Date('2019-02-23') & DateTime_UTC <=  as.Date('2019-06-18')) %>% 
  #filter(is.na(flagtype)) %>% 
  #select(DateTime_UTC, variable, value) %>% 
  #spread(key = variable, value = value) 

Dis = Raw_Discharge %>% 
  filter(datetime >= as.Date('2019-02-23') & datetime <=  as.Date('2019-06-18')) 



Pres = FAWN_Pressure %>% 
  filter(datetime >= as.Date('2019-02-23') & datetime <=  as.Date('2019-06-18')) 
  

WaterTemp = SiteData2 %>%
  filter(datetime >= as.Date('2019-02-23') & datetime <=  as.Date('2019-06-18'))

DO <- left_join(Pres, WaterTemp, by = NULL) %>% 
  left_join(Dis, DO, by = NULL)


#because I've already converted to UTC, I can skip the conversion to local time and just use convert_UTC_to-solartime, just make sure to specify time.type to mean, otherwise it defaults to zenith peaks and your times start drifting and the bayes model won't run
#according to the package, light needs to be in PAR... I know we've gone over this multiple times, I just need to look back at notes to double check what the conclusion was. For now, light stays in lux
DO = DO %>%
  select(datetime, `Air Pressure (mb)`, AirTemp_C, Discharge_m3s, DO_mgL, Light_lux, WaterTemp_C) %>% 
  mutate(DO.sat = calc_DO_sat(temp.water = DO$WaterTemp_C, pressure.air = DO$`Air Pressure (mb)`, model = 'garcia-benson'), 
         depth = calc_depth(Q = DO$Discharge_m3s), 
         solar.time = convert_UTC_to_solartime(date.time = datetime, longitude = -82.36, time.type = 'mean' ))

DO$solar.time = as.POSIXct(x = DO$solar.time, tz = 'UTC')

Metab_inputs = DO %>% 
  select(solar.time, DO_mgL, DO.sat, depth, WaterTemp_C, Light_lux, Discharge_m3s) %>% 
  rename(DO.obs = DO_mgL, temp.water = WaterTemp_C, light = Light_lux, discharge = Discharge_m3s)#rename columns 

Metab_inputs$discharge = baytrends::fillMissing(Metab_inputs$discharge)
Metab_inputs$depth = baytrends::fillMissing(Metab_inputs$depth)
Metab_inputs$DO.obs = baytrends::fillMissing(Metab_inputs$DO.obs)


Metab_inputs %>% 
  mutate(DO.pctsat = 100 * (DO.obs / DO.sat)) %>%
  select(solar.time, discharge, starts_with('DO')) %>%
  gather(type, DO.value, starts_with('DO'), discharge) %>%
  mutate(units=ifelse(type == 'DO.pctsat', 'DO\n(% sat)', 'DO\n(mg/L)')) %>%
  ggplot(aes(x=solar.time, y=DO.value, color=type)) + geom_line() + 
  facet_grid(units ~ ., scale='free_y') + theme_bw() +
  scale_color_discrete('variable')

Metab_inputs = as.data.frame(Metab_inputs)

#write_csv(HAT_metab_inputs,'C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/StreamPULSE Data Uploads/FL_HAT_2019-09-01_Metab.csv' )

#dat = read_csv('C:/Users/Emily/Dropbox (UFL)/AJR Lab/Students/Emily Taylor/GNV Streams/Data/StreamPULSE Data Uploads/FL_HAT_2019-09-01_Metab.csv', col_names = TRUE)


#dat2 = as.data.frame(dat)
#colnames(dat2) = c( 'solar.time', 'DO.obs', 'DO.sat', 'depth', 'temp.water', 'light', 'discharge')

bayes_name <- mm_name(type='bayes', pool_K600='binned')
bayes_name

bayes_specs <- specs(bayes_name, day_start = 4, day_end = 28, burnin_steps = 500, saved_steps = 500)
#bayes_specs

mm <- metab(bayes_specs, data = Metab_inputs)


mm


output<-as.data.frame(get_params(mm))
output#check output data

write.csv(output, 'TUM441_Test_1_Output_20200207_v2.csv') #whatever filename is

plot_DO_preds(mm)
plot_metab_preds(mm)

mcmc <- get_mcmc(mm)
rstan::traceplot(mcmc, pars='K600_daily', nrow=8)


