library(tidyverse) 
library(gridExtra) 
library(ggpubr)
library(cowplot)
library(viridis)
library(hrbrthemes)
library(esquisse)

esquisse::esquisser()

#####Only need to run this section to create the inital file which combines all the different metab info####

#files <- list.files("C:/Users/Emily/Documents/GitHub/GNV_Metabolism_19Jul/Metabolism Output_20Feb07", pattern = "*.csv", full.names = T)

#tbl <- sapply(files, read_csv, simplify=FALSE) %>% 
  #bind_rows(.id = "id")

#tbl

#metab_data <- tbl %>% 
  #select(id, date, GPP.daily, ER.daily, K600.daily) %>% 
  #filter(date >= as.Date('2019-02-23') & date <=  as.Date('2019-07-31') ) %>% 
  #na.omit(metab_data)

#metab_data

#(metab_data, 'C:/Users/Emily/Documents/GitHub/GNV_Metabolism_19Jul/Metabolism Output_20Feb07/GNV_Metab_all_20Feb07.csv')

#Couldn't figure out how to fix site ID so I had to do it manually#
#####Read in metab combo file####
data <- read_csv("C:/Users/Emily/Documents/GitHub/GNV_Metabolism_19Jul/Metabolism Output_20Feb07/GNV_Metab_all_20Feb07.csv", col_types = cols(
  Site = col_character(),
  date = col_date(format = '%m/%d/%Y'),
  GPP_daily = col_double(),
  ER_daily = col_double(),
  K600_daily = col_double()
), skip_empty_rows = T)


data2 <- as.data.frame(data) %>% 
  filter(Site == 'HAT' |Site == 'HOGNW16' | Site == 'HOGDN' | Site == 'POS' | Site == 'TUM441' )

data3 <- data %>% 
  gather(GPP_daily, ER_daily, key = 'daily', value = 'result') %>% 
  mutate(Site = factor(Site, levels =  c('HAT', 'HOGDN', 'HOGNW16', 'POS', 'TUM441')))

nut_ts <- data3 %>% 
  ggplot(aes(x = date, y = result, fill = Site)) + 
  geom_point(size = 4, aes(x = date, y = result, color = daily, shape = Site)) 

###GPP v ER####
nut_ts2 <- data2 %>% 
  ggplot(aes(x = GPP_daily, y = ER_daily, color = Site)) + 
  geom_point(size = 6, position = 'jitter', aes(x = GPP_daily, y = ER_daily, color = Site, shape = Site)) + 
  ylab(expression('ER (g O'[2] * ' m'^-2*'d'^-1*')')) + xlab(expression('GPP (g O'[2] * ' m'^-2*'d'^-1*')'))


nut_ts3 <- nut_ts2 + scale_color_manual(values = c('#56B4E9', '#0072B2', '#009E73', '#669900', '#D55E00'), 
                                        labels = c('Hatchet', 'S. Hogtown', 'N. Hogtown', 'Possum', 'Tumblin')) + 
  scale_shape_manual(labels = c('Hatchet', 'S. Hogtown', 'N. Hogtown', 'Possum', 'Tumblin'),
                       values = c(15, 16, 17, 8, 7)) +
  theme_ipsum_rc() 
  theme(axis.text = element_text(size = rel(2.5))) +
  theme(axis.title = element_text(size = rel(3))) +
  theme(legend.position = c(0.90, 0.15)) +
  theme(legend.title = element_text(size = rel(2), face = 'bold')) +
  theme(legend.background = element_blank()) +
  theme(legend.key = element_blank(), 
        legend.text = element_text(size = rel(2)))
  

nut_ts3

###GPP v ER - No Hatchet####
nut_ts2 <- data2 %>% 
  ggplot(aes(x = GPP_daily, y = ER_daily, color = Site)) + 
  geom_point(size = 6, position = 'jitter', aes(x = GPP_daily, y = ER_daily, color = Site, shape = Site)) + 
  ylab(expression('ER (g O'[2] * ' m'^-2*'d'^-1*')')) + xlab(expression('GPP (g O'[2] * ' m'^-2*'d'^-1*')'))


nut_ts3 <- nut_ts2 + scale_color_manual(values = c('#56B4E9', '#0072B2', '#009E73', '#669900', '#D55E00'), 
                                        labels = c('Hatchet', 'S. Hogtown', 'N. Hogtown', 'Possum', 'Tumblin')) + 
  scale_shape_manual(labels = c('Hatchet', 'S. Hogtown', 'N. Hogtown', 'Possum', 'Tumblin'),
                     values = c(15, 16, 17, 8, 7)) +
  theme_ipsum_rc(axis_title_size = 15) +
  theme(legend.position = 'bottom') +
  theme(legend.title = element_text(size = rel(1.5), face = 'bold')) +
  theme(legend.background = element_blank()) +
  theme(legend.key = element_blank(), 
        legend.text = element_text(size = rel(1.5))) +
  facet_wrap(. ~ Site, ncol = 5, nrow = 1) +
  geom_smooth(method = lm) + 
  labs(caption = 'Period of Record: February - July 2019') + 
  theme(strip.text.x = element_blank())


nut_ts3


ggsave("C:/Users/Emily/Documents/GitHub/GNV_EEMS/figures/GPPvER_lm_2021-05-05.png", plot = nut_ts3, width = 20, height = 12)


HAT <- data2 %>% 
  filter(Site == 'HAT') %>% 
  lm(formula = ER_daily ~ GPP_daily) %>% 
  summary()


HOGDN <- data2 %>% 
  filter(Site == 'HOGDN') %>% 
  lm(formula = ER_daily ~ GPP_daily) %>% 
  summary()

HOGNW16 <- data2 %>% 
  filter(Site == 'HOGNW16') %>% 
  lm(formula = ER_daily ~ GPP_daily) %>% 
  summary()

POS <- data2 %>% 
  filter(Site == 'POS') %>% 
  lm(formula = ER_daily ~ GPP_daily) %>% 
  summary()

TUM441 <- data2 %>% 
  filter(Site == 'TUM441') %>% 
  lm(formula = ER_daily ~ GPP_daily) %>% 
  summary()

HAT
HOGDN
HOGNW16
POS
TUM441

###GPP v ER - Density ####
nut_ts2 <- data2 %>% 
  ggplot(aes(x = GPP_daily, y = ER_daily, color = Site)) + 
  geom_density_2d(size = 1, position = 'jitter', aes(x = GPP_daily, y = ER_daily, color = Site)) + 
  ylab(expression('ER (g O'[2] * ' m'^-2*'d'^-1*')')) + xlab(expression('GPP (g O'[2] * ' m'^-2*'d'^-1*')')) +
  facet_wrap(. ~ Site, ncol = 4) + 
  theme_ipsum_rc(axis_title_size = 17) +
  theme(strip.text.x = element_blank()) +
  scale_color_manual(values = c('#56B4E9', '#0072B2', '#009E73', '#E69F00'), 
                                        labels = c('S. Hogtown', 'N. Hogtown', 'Possum', 'Tumblin')) 

nut_ts2

ggsave("C:/Users/Emily/OneDrive - University of Florida/Dissertation/Proposal/Presentation_Figures/GPPvER_free_20Dec04.png", plot = nut_ts2, width = 18, height = 12)


####GPP and ER Timeseries####
nut_ts3 <- data3 %>% 
  ggplot(aes(x = date, y = result)) + 
  geom_hline(aes(yintercept=0), colour="#000000", linetype="dashed") +
  geom_point(aes(x = date, y = result, color = Site, shape = daily)) +
  scale_color_manual(values = c('#56B4E9', '#0072B2', '#009E73', '#669900', '#D55E00'), 
                     labels = c('Hatchet', 'S. Hogtown', 'N. Hogtown', 'Possum', 'Tumblin')) +
  facet_wrap( . ~ Site, nrow = 5, scales = 'free_y', 
              labeller = as_labeller(c (HAT = 'Hatchet', 
                                        HOGDN = 'S. Hogtown',
                                        HOGNW16 = 'N. Hogtown', 
                                        POS = 'Possum', 
                                        TUM441 = 'Tumblin'))) +
  theme(axis.title.x = element_blank()) +
  theme(legend.position = 'none')#remove x-axis title of "date"...unecessary


nut_ts3




#####GPP v ER - Option 2#####
BARN = ggplot(data2, aes(x = date)) +
  geom_area(aes(y = GPP_daily, fill = "GPP_daily")) +
  geom_area(aes(y = ER_daily, fill = "ER_daily")) + 
  labs(y = expression(paste("O"[2]," Flux (g O"[2], " m"^"-2", " d"^"-1", ")")), 
       x = "Date") + 
  ylim(-15,15) +
  scale_fill_manual(values = wes_palette("Moonrise2", n = 2)) + 
  facet_wrap( . ~ Site, nrow = 2, scales = 'free_y', 
              labeller = as_labeller(c (HAT = 'Hatchet', 
                                        HOGDN = 'S. Hogtown',
                                        HOGNW16 = 'N. Hogtown', 
                                        POS = 'Possum', 
                                        TUM441 = 'Tumblin'))) +
  theme_ipsum_rc(base_size = 24, plot_title_size = 36,  axis_title_size = 26) + 
  theme(
    legend.position = "none"
  )

BARN

summary(data2)

#####Individual GPP and ER plots with related Q to be combined later####


#####Hatchet Creek####

HAT <- all_data3 %>% 
  filter(Site == 'HAT') %>% 
  filter(analyte == 'GPP_daily' | analyte == 'ER_daily')

HAT2 <- HAT %>% 
  filter(analyte == 'GPP_daily')

HAT3 <- HAT %>% 
  filter(analyte == 'ER_daily')

HAT_ts <- HAT %>% 
  ggplot(aes(x = datetime, y = result)) + 
  geom_hline(aes(yintercept=0), colour="#000000", linetype="dashed") +
  geom_point(size = 4, aes(x = datetime, y = result, color = Site, shape = analyte)) +
  labs(title = 'Hatchet') +
  scale_color_manual(values = c('#56B4E9', '#56B4E9'), 
                     labels = 'Hatchet') +
  theme(axis.title.x = element_blank()) +
  #theme(axis.text.x = element_blank()) +
  theme(legend.position = 'none') +
  theme(axis.text = element_text(size = 20)) +
  theme(axis.title = element_text(size = 20)) +
  ylab(expression('O'[2]* " Flux "* '(g O'[2] * ' m'^-2*'d'^-1*')')) +
  scale_y_continuous(breaks = c(0, -8))

HAT_ts

HAT_ts <- HAT_ts + geom_path(data = HAT2, aes(x = datetime, y = result, color = '#56B4E9')) +
  geom_path(data = HAT3, aes(x = datetime, y = result, color = '#56B4E9'))

HAT_Q_data <- all_data3 %>% 
  filter(Site == 'HAT') %>% 
  filter(analyte == 'Discharge_m3s')

HAT_Q <- HAT_Q_data %>% 
  ggplot(aes(x = datetime)) + 
  geom_area(aes(y = result), fill = '#56B4E9', color = '#56B4E9') +
  theme_ipsum_rc(base_size = 12, plot_title_size = 26, axis_title_size = 15) + 
  theme(axis.title.x = element_blank()) +
  theme(legend.position = 'none') +
  #theme(axis.text = element_text(size = 20)) +
  #theme(axis.title = element_text(size = 20)) +
  ylab(expression('Q (m' [3]* 's'^-1*')')) +
  scale_y_continuous(breaks = c(0, 3))
  

HAT_Q

ggpubr::ggarrange(HAT_ts, HAT_Q, ncol = 1)

  
#####S. Hogtown#####


HOGDN <- all_data3 %>% 
  filter(Site == 'HOGDN') %>% 
  filter(analyte == 'GPP_daily' | analyte == 'ER_daily')

HOGDN2 <- HOGDN %>% 
  filter(analyte == 'GPP_daily')

HOGDN3 <- HOGDN %>% 
  filter(analyte == 'ER_daily')

HOGDN <- data2 %>% 
  filter(Site == 'HOGDN')



HOGDN_ts <- HOGDN %>% 
  ggplot(aes(x = date)) +
  geom_area(aes(y = GPP_daily, fill = "GPP_daily")) +
  geom_area(aes(y = ER_daily, fill = "ER_daily")) + 
  labs(y = expression(paste("O"[2]," Flux (g O"[2], " m"^"-2", " d"^"-1", ")")), 
       x = "Date") + 
  ylim(-15, 15) +
  scale_fill_manual(values = wes_palette("Moonrise2", n = 2)) + 
    theme_ipsum_rc(base_size = 24, plot_title_size = 36,  axis_title_size = 26) + 
  theme(legend.position = "none")

  geom_hline(aes(yintercept=0), colour="#000000", linetype="dashed") +
  geom_point(size = 4, aes(x = datetime, y = result, color = Site, shape = analyte)) +
  labs(title = 'S. Hogtown') +
  scale_color_manual(values = c('#0072B2', '#0072B2'), 
                     labels = 'S. Hogtown') +
  theme_ipsum_rc(base_size = 12, plot_title_size = 26, axis_title_size = 15) + 
  theme(axis.title.x = element_blank()) +
  #theme(axis.text.x = element_blank()) +
  theme(legend.position = 'none') +
  theme(axis.text = element_text(size = 20)) +
  theme(axis.title = element_text(size = 20)) +
  ylab(expression('O'[2]* " Flux "* '(g O'[2] * ' m'^-2*'d'^-1*')'))+
  scale_y_continuous(breaks = c(0, -10))

HOGDN_ts <- HOGDN_ts + geom_path(data = HOGDN2, aes(x = datetime, y = result, color = '#0072B2')) +
  geom_path(data = HOGDN3, aes(x = datetime, y = result, color = '#0072B2'))

HOGDN_Q_data <- all_data3 %>% 
  filter(Site == 'HOGDN') %>% 
  filter(analyte == 'Discharge_m3s')

HOGDN_Q <- HOGDN_Q_data %>% 
  ggplot(aes(x = datetime)) + 
  geom_area(aes(y = result), fill = '#0072B2', color = '#0072B2') +
  theme_ipsum_rc(base_size = 12, plot_title_size = 18, axis_title_size = 15) + 
  labs(title = 'S. Hogtown') +
  theme(axis.title.x = element_blank()) +
  theme(legend.position = 'none') +
  #theme(axis.text = element_text(size = 20)) +
  #theme(axis.title = element_text(size = 20)) +
  ylab(expression('Q (m' [3]* 's'^-1*')')) +
  scale_y_continuous(breaks = c(0, 6))


HOGDN_Q


ggpubr::ggarrange(HOGDN_ts, HOGDN_Q, ncol = 1, align = 'h')

#####N. Hogtown#####


HOGUP <- all_data3 %>% 
  filter(Site == 'HOGNW16') %>% 
  filter(analyte == 'GPP_daily' | analyte == 'ER_daily')

HOGUP2 <- HOGUP %>% 
  filter(analyte == 'GPP_daily')

HOGUP3 <- HOGUP %>% 
  filter(analyte == 'ER_daily')

HOGUP_ts <- HOGUP %>% 
  ggplot(aes(x = datetime, y = result)) + 
  geom_hline(aes(yintercept=0), colour="#000000", linetype="dashed") +
  geom_point(size = 4, aes(x = datetime, y = result, color = Site, shape = analyte)) +
  labs(title = 'N. Hogtown') +
  scale_color_manual(values = c('#009E73', '#009E73'), 
                     labels = 'N. Hogtown') +
  theme_ipsum_rc(base_size = 12, plot_title_size = 26, axis_title_size = 15) + 
  theme(axis.title.x = element_blank()) +
  #theme(axis.text.x = element_blank()) +
  theme(legend.position = 'none') +
  #theme(axis.text = element_text(size = 20)) +
  #theme(axis.title = element_text(size = 20)) +
  ylab(expression('O'[2]* " Flux "* '(g O'[2] * ' m'^-2*'d'^-1*')')) +
  scale_y_continuous(breaks = c(0, -4))
  

HOGUP_ts <- HOGUP_ts + 
  geom_path(data = HOGUP2, aes(x = datetime, y = result, color = '#009E73')) +
  geom_path(data = HOGUP3, aes(x = datetime, y = result, color = '#009E73'))

HOGUP_ts

HOGUP_Q_data <- flow_data2 %>% 
  filter(Site == 'HOGNW16')

HOGUP_Q <- HOGUP_Q_data %>% 
  ggplot(aes(x = datetime)) + 
  geom_area(aes(y = Discharge_m3s), fill = '#009E73', color = '#009E73') +
  theme_ipsum_rc(base_size = 12, plot_title_size = 18, axis_title_size = 15) + 
  labs(title = 'N. Hogtown') +
  theme(axis.title.x = element_blank()) +
  theme(legend.position = 'none') +
  #theme(axis.text = element_text(size = 20)) +
  #theme(axis.title = element_text(size = 20)) +
  ylab(expression('Q (m' [3]* 's'^-1*')')) +
  scale_y_continuous(breaks = c(0, 8))


HOGUP_Q

ggpubr::ggarrange(HOGUP_ts, HOGUP_Q, ncol = 1)

#####Possum#####


POS <- all_data3 %>% 
  filter(Site == 'POS') %>% 
  filter(analyte == 'GPP_daily' | analyte == 'ER_daily')

POS2 <- POS %>% 
  filter(analyte == 'GPP_daily')

POS3 <- POS %>% 
  filter(analyte == 'ER_daily')

POS_ts <- POS %>% 
  ggplot(aes(x = datetime, y = result)) + 
  geom_hline(aes(yintercept=0), colour="#000000", linetype="dashed") +
  geom_point(size = 4, aes(x = datetime, y = result, color = Site, shape = analyte)) +
  labs(title = 'Possum') +
  scale_color_manual(values = c('#669900', '#669900'),
                     labels = 'Possum') +
  theme_ipsum_rc(base_size = 12, plot_title_size = 26, axis_title_size = 15) + 
  theme(axis.title.x = element_blank()) +
  #theme(axis.text.x = element_blank()) +
  theme(legend.position = 'none') +
  #theme(axis.text = element_text(size = 20)) +
  #theme(axis.title = element_text(size = 20)) +
  ylab(expression('O'[2]* " Flux "* '(g O'[2] * ' m'^-2*'d'^-1*')')) +
  scale_y_continuous(breaks = c(0, -8))

POS_ts <- POS_ts + geom_path(data = POS2, aes(x = datetime, y = result, color = '#669900')) +
  geom_path(data = POS3, aes(x = datetime, y = result, color = '#669900'))

POS_Q_data <- flow_data2 %>% 
  filter(Site == 'POS')


POS_Q <- POS_Q_data %>% 
  ggplot(aes(x = datetime)) + 
  geom_area(aes(y = Discharge_m3s), fill = '#669900', color = '#669900') +
  theme_ipsum_rc(base_size = 12, plot_title_size = 18, axis_title_size = 15) + 
  labs(title = 'Possum') +
  theme(axis.title.x = element_blank()) +
  theme(legend.position = 'none') +
  #theme(axis.text = element_text(size = 20)) +
  #theme(axis.title = element_text(size = 20)) +
  ylab(expression('Q (m' [3]* 's'^-1*')')) +
  scale_y_continuous(breaks = c(0, 4))


POS_Q

#ggpubr::ggarrange(POS_ts, POS_Q, ncol = 1)


#####Tumblin#####


TUM441 <- all_data3 %>% 
  filter(Site == 'TUM441') %>% 
  filter(analyte == 'GPP_daily' | analyte == 'ER_daily')

TUM4412 <- TUM441 %>% 
  filter(analyte == 'GPP_daily')

TUM4413 <- TUM441 %>% 
  filter(analyte == 'ER_daily')

TUM441_ts <- TUM441 %>% 
  ggplot(aes(x = datetime, y = result)) + 
  geom_hline(aes(yintercept=0), colour="#000000", linetype="dashed") +
  geom_point(size = 4, aes(x = datetime, y = result, color = Site, shape = analyte)) +
  labs(title = 'Tumblin') +
  scale_color_manual(values = c('#D55E00', '#D55E00'),
                     labels = 'Tumblin') +
  theme(axis.title.x = element_blank()) +
  #theme(axis.text.x = element_blank()) +
  theme(legend.position = 'none') +
  theme(axis.text = element_text(size = 20)) +
  theme(axis.title = element_text(size = 20)) +
  ylab(expression('O'[2]* " Flux "* '(g O'[2] * ' m'^-2*'d'^-1*')')) +
  scale_y_continuous(breaks = c(4, -6))

TUM441_ts <- TUM441_ts + geom_path(data = TUM4412, aes(x = datetime, y = result, color = '#D55E00')) +
  geom_path(data = TUM4413, aes(x = datetime, y = result, color = '#D55E00'))


TUM441_Q_data <- flow_data2 %>% 
  filter(Site == 'TUM')

summary(TUM441_Q_data)

TUM441_Q <- TUM441_Q_data %>% 
  ggplot(aes(x = datetime)) + 
  geom_area(aes(y = Discharge_m3s), fill = '#D55E00', color = '#D55E00') +
  theme_ipsum_rc(base_size = 12, plot_title_size = 18, axis_title_size = 15) +
  labs(title = 'Tumblin') +
  theme(axis.title.x = element_blank()) +
  theme(legend.position = 'none') +
  #theme(axis.text = element_text(size = 20)) +
  #theme(axis.title = element_text(size = 20)) +
  ylab(expression('Q (m' [3]* 's'^-1*')')) +
  scale_y_continuous(breaks = c(0, 2))


TUM441_Q

#ggpubr::ggarrange(TUM441_ts, TUM441_Q, ncol = 1)

#####Multiplot of Metab and Q#####

ggpubr::ggarrange(HAT_ts, HAT_Q, HOGDN_ts, HOGDN_Q, HOGUP_ts, HOGUP_Q, POS_ts, POS_Q, TUM441_ts, TUM441_Q, ncol = 1)

HOGDN_Q/HOGUP_Q/POS_Q/TUM441_Q


#First install the gridExtra package and then run the following code

library(gridExtra)


g1<-ggplotGrob(HAT_ts)
g2<-ggplotGrob(HAT_Q)
g3<-ggplotGrob(HOGDN_ts)
g4<-ggplotGrob(HOGDN_Q)
g5<-ggplotGrob(HOGUP_ts)
g6<-ggplotGrob(HOGUP_Q)
g7<-ggplotGrob(POS_ts)
g8<-ggplotGrob(POS_Q)
g9<-ggplotGrob(TUM441_ts)
g10<-ggplotGrob(TUM441_Q)
.
.
gn<-ggplotGrob(pn)

windows(height=Y, width=X)

grid::grid.newpage()
grid::grid.draw(rbind(g1,g2,g3, g4, g5, g6, g7, g8, g9, g10))
                


grid.export('myplot.svg')


####going to attempt to combine discharge and GPP and ER into a set of facets...maybe? ####
#####Only need to run this to create combo file, afterwards just read in combo file####

#read in and combine all flow data#
discharge_files <- list.files("C:/Users/Emily/Documents/GitHub/GNV_Metabolism_19Jul/Metabolism Output_20Feb07/Flow Data for Metab Figures ONLY", pattern = "*.csv", full.names = T)

discharge_tbl <- sapply(discharge_files, read_csv, simplify=FALSE) %>% 
  bind_rows(.id = "id")

discharge_tbl

discharge_data <- discharge_tbl %>% 
  select(id, datetime, Discharge_m3s) %>% 
  filter(datetime >= as_datetime('2019-02-23 00:00:00') & datetime <=  as_datetime('2019-07-31 00:00:00') ) %>% 
  na.omit(discharge_data)

write_csv(discharge_data, 'C:/Users/Emily/Documents/GitHub/GNV_Metabolism_19Jul/Metabolism Output_20Feb07/Flow Data for Metab Figures ONLY/GNV_Flow_all_20Feb07.csv')

#Couldn't figure out how to fix site ID so I had to do it manually#
#####Read in all discharge file####
flow_data <- read_csv('C:/Users/Emily/Documents/GitHub/GNV_Metabolism_19Jul/Metabolism Output_20Feb07/Flow Data for Metab Figures ONLY/GNV_Flow_all_20Feb07.csv', col_types = cols(
  Site = col_character(),
  datetime = col_datetime(format = ''),
  Discharge_m3s = col_double()), skip_empty_rows = T)


flow_data2 = as.data.frame(flow_data)

summary(flow_data)

#####Combine All the Data#####

all_files <- list.files("C:/Users/Emily/Documents/GitHub/GNV_Metabolism_19Jul/Metabolism Output_20Feb07/Combo File", pattern = "*.csv", full.names = T)


all_tbl <- sapply(all_files, read_csv, simplify=FALSE) %>% 
  bind_rows(.id = "id")

all_data2 <- all_tbl

#all_data2 = as.data.frame(all_tbl)


all_data3 <- all_data2 %>% 
  gather(GPP_daily, ER_daily, Discharge_m3s, K600_daily, key = 'analyte', value = 'result') %>% 
  mutate(Site = factor(Site, levels =  c('HAT', 'HOGDN', 'HOGNW16', 'POS', 'TUM441')))


flow_metab <- ggplot(flow_data, aes(x = datetime, y = Discharge_m3s))

fm <- flow_metab + geom_area(aes(x = datetime, y = Discharge_m3s), fill = '#99CCFF') +
  facet_wrap( . ~ Site, nrow = 5, scales = 'free_y',
              labeller = as_labeller(c (HAT = 'Hatchet',
                                        HOGDN = 'S. Hogtown', 
                                        HOGNW16 = 'N. Hogtown', 
                                        POS = 'Possum', 
                                        TUM = 'Tumblin'))) + 
  labs(y = 'Discharge (m3/s)')
  theme(strip.placement = 'outside', strip.text = element_text(size = rel(1.5))) +
  theme(axis.title.x = element_blank())

fm

fm2 <- fm + geom_point(aes(x = datetime, y = ER_daily)) +
  theme(legend.position = 'none') +
  theme(axis.text = element_text(size = rel(1.5))) 

fm2 + scale_y_continuous(sec.axis = sec_axis(~ . + 0, name = 'ER (g m^-2 d^-1)')) +
  theme(legend.position = 'none')
  

flow_metab


#####ER Graph#####
flow_ERmetab <- all_data2 %>%
  ggplot(aes(x = datetime, y = Discharge_m3s)) +
  geom_area(aes(x = datetime, y = Discharge_m3s), fill = '#99CCFF') +
  facet_wrap( . ~ Site, nrow = 5, scales = 'free_y', 
              labeller = as_labeller(c (HAT = 'Hatchet',
                                        HOGDN = 'S. Hogtown', 
                                        HOGNW16 = 'N. Hogtown', 
                                        POS = 'Possum', 
                                        TUM441 = 'Tumblin'))) + 
  theme(strip.placement = 'outside', strip.text = element_text(size = 11) +
          theme(axis.title.x = element_blank())
        


flow_ERmetab <- flow_ERmetab + geom_point(aes(x = datetime, y = ER_daily)) +
  theme(legend.position = 'none') +
  theme(axis.text = element_text(size = rel(1.5))) 

flow_ERmetab + scale_y_continuous(sec.axis = sec_axis(~ . + 0, name = 'ER (g m^-2 d^-1)')) +
  theme(legend.position = 'none')


flow_ERmetab