
##library
library(ggplot2)
library(ggsn)
library(rgdal)
library(tidyverse)
library(lubridate)
library(doBy)
library(here)
library(readr)
library(readxl)
library(scatterpie)
library(scales)
library(qwraps2)
library(png)
library(magick)
library(plotrix)
library(patchwork)
#library(scatterpie)
#library(ggspatial)


###########################################################################################################
##set default themes for all plots and maps
map_theme<-theme(axis.text=element_text(size=24, family='serif'), 
                 axis.title=element_text(size=24, family='serif'), 
                 plot.title=element_text(size=24, family='serif'),
                 plot.subtitle=element_text(size=20, family='serif'),
                 plot.caption=element_text(size=20, family='serif'), 
                 legend.position=c(0.08,0.7),
                 legend.text=element_text(size=24, family='serif'), 
                 legend.title=element_text(size=24, family='serif'),
                 strip.text=element_text(size=20, family='serif'))



plot_theme<-theme(axis.line=element_line(size=1, color='black'),
                  panel.background = element_rect(NA),
                  axis.text=element_text(size=24, family='serif'),
                  axis.title=element_text(size=24, family='serif'),
                  plot.margin = margin(.5,.5,.5,.5,"cm"),
                  legend.text=element_text(size=24, family='serif'),
                  axis.ticks=element_line(size=1, color='black'),
                  plot.title=element_text(size=24, family='serif'),
                  plot.subtitle=element_text(size=20, family='serif'),
                  plot.caption=element_text(size=20, family='serif'),
                  legend.background = element_blank(),
                  legend.key = element_blank(),
                  legend.title=element_text(size=24, family='serif'),
                  strip.text=element_text(size=20, family='serif'))

ann_data_access<-'Data: U.S. Geological Survey & The Nature Conservancy'

##In case we're making a map
ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)



###########################################################################################################
##Load data
ciscoes <-read_xlsx(here('Data','GICiscoes.xlsx'), sheet = 'Fish')

effort <-read_xlsx(here('Data','GICiscoes.xlsx'), sheet = 'Effort') %>%
mutate(BDepth_ft = case_when(
    GoodDepths == "yes" ~ StartDepth_ft, 
    GNetMesh_in == 1.5 & GoodDepths == "no" ~ StartDepth_ft, 
    GNetMesh_in == 2.0 & GoodDepths == "no" ~ StartDepth_ft + abs(StartDepth_ft - EndDepth_ft)/4,
    GNetMesh_in == 2.5 & GoodDepths == "no" ~ StartDepth_ft + (abs(StartDepth_ft - EndDepth_ft)/4)*2,
    GNetMesh_in == 3.0 & GoodDepths == "no" ~ StartDepth_ft + (abs(StartDepth_ft - EndDepth_ft)/4)*3)
    ) %>%
  mutate(EDepth_ft = case_when(
    GoodDepths == "yes" ~ EndDepth_ft,
    GNetMesh_in == 1.5 & GoodDepths == "no" ~ StartDepth_ft + abs(StartDepth_ft - EndDepth_ft)/4,
    GNetMesh_in == 2.0 & GoodDepths == "no" ~ StartDepth_ft + (abs(StartDepth_ft - EndDepth_ft)/4)*2,
    GNetMesh_in == 2.5 & GoodDepths == "no" ~ StartDepth_ft + (abs(StartDepth_ft - EndDepth_ft)/4)*3,
    GNetMesh_in == 3.0 & GoodDepths == "no" ~ EndDepth_ft)) %>%
  mutate(MidDepth_ft = ((BDepth_ft+EDepth_ft)/2), 
         MidDepth_m = MidDepth_ft*0.3048, 
         RangeDepth_m = abs(EDepth_ft-BDepth_ft) * 0.3048, 
         week = week(Date)) %>%
  mutate(depth.bin = case_when(
    MidDepth_m <= 50 ~ '<50 m',
    MidDepth_m > 50 & MidDepth_m <= 100 ~ '50-100 m',
    MidDepth_m > 100 & MidDepth_m <= 150 ~ '100-150 m',
    MidDepth_m > 150 & MidDepth_m <= 200 ~ '150-200 m',
    MidDepth_m > 200 & MidDepth_m <= 250 ~ '200-250 m',
    MidDepth_m > 250 ~ '>250 m')) %>%
  mutate(year = year(Date), month = month(Date)) %>%
  mutate(winter.year = case_when(
    month >10 ~ year + 1,
    month <=10 ~ year)) %>%
  mutate(winter.year2 = case_when(
    winter.year == 2017 ~ 'Winter 2017',
    winter.year == 2018 ~ 'Winter 2018',
    winter.year == 2019 ~ 'Winter 2019',
    winter.year == 2020 ~ 'Winter 2020',
    winter.year == 2021 ~ 'Winter 2021'))


##load the species names file for when needed
codes.to.names<-read_xlsx(here('Data','Species_Taxonomy.xlsx'))
sci.names<-select(codes.to.names, c(1:3))


##Merge with effort data
ciscoes.all <- ciscoes %>%
  left_join(effort) %>%
  left_join(sci.names) 

#  select(1:9, 13:20, 28:29, 33:37)

ciscoes.winter <- ciscoes.all %>%
  filter(Method == "Gillnet") 

##########################################################################################
##Bring data from Lake Michigan 1930-32
##########################################################################################
##Fulmar operations data
fulmar.op <-read_csv(here('Data','Fulmar_op.csv')) %>%
  mutate(GNetMesh_mm = case_when(
    MESH_SIZE == 23 ~ 60/25.4,
    MESH_SIZE == 24 ~ 64/25.4,
    MESH_SIZE == 25 ~ 67/25.4,
    MESH_SIZE == 26 ~ 70/25.4,
    MESH_SIZE == 30 ~ 76/25.4)) %>%
  mutate(GNetMesh_in = case_when(
    MESH_SIZE == 23 ~ '2.4',
    MESH_SIZE == 24 ~ '2.5',
    MESH_SIZE == 25 ~ '2.6',
    MESH_SIZE == 26 ~ '2.8',
    MESH_SIZE == 30 ~ '3')) %>%
  mutate(GNetMesh = case_when(
    MESH_SIZE == 23 ~ '2 3/8"',
    MESH_SIZE == 24 ~ '2 1/2"',
    MESH_SIZE == 25 ~ '2 5/8"',
    MESH_SIZE == 26 ~ '2 3/4"',
    MESH_SIZE == 30 ~ '3"')) %>%  mutate(Sample = OP_ID,
         Date = OP_DATE,
         StartDepth_m = BEG_DEPTH,
         EndDepth_m = END_DEPTH,
         Lat_DD = LAT,
         Long_DD = LON,
         MidDepth_m = ((StartDepth_m+EndDepth_m)/2),
         MidDepth_ft = MidDepth_m * 3.281,
         week = week(Date), 
         Month = month(Date), 
         GNetMeshLength_ft = 510) %>%
  mutate(depth.bin = case_when(
    MidDepth_m <= 50 ~ '<50 m',
    MidDepth_m > 50 & MidDepth_m <= 100 ~ '50-100 m',
    MidDepth_m > 100 & MidDepth_m <= 150 ~ '100-150 m',
    MidDepth_m > 150 & MidDepth_m <= 200 ~ '150-200 m',
    MidDepth_m > 200 & MidDepth_m <= 250 ~ '200-250 m',
    MidDepth_m > 250 ~ '>250 m')) 
  

##Fulmar fish data
fulmar.fish <-read_csv(here('Data','Fulmar_fishv2.csv')) 

##Join the two files and select relevant fields
fulmar.all <- fulmar.fish %>%
  left_join(fulmar.op) %>% 
  mutate(Length_mm = LENGTH) %>%
  mutate(Sex = case_when(
    SEX == 1 ~ 'male',
    SEX == 2 ~ 'female')) %>%
  mutate(Maturity = case_when(
           MATURITY == 1 ~ 'immature',
           MATURITY == 2 ~ 'developing', 
           MATURITY == 7 ~ 'abnormal', 
           MATURITY == 4 ~ 'ripe', 
           MATURITY == 6 ~ 'spent', 
           MATURITY == 3 ~ 'gravid')) %>%
  select(Sample, Date, Month, week, Lat_DD, Long_DD, StartDepth_m, EndDepth_m, MidDepth_m, MidDepth_ft, 
         depth.bin, GNetMeshLength_ft, GNetMesh_in, NIGHTS_OUT, SPECIES, Sex, Maturity, Length_mm) %>%
  left_join(sci.names) 

fulmar.maturity <- fulmar.all %>%
  subset(SPECIES == 206 & Sex == 'female') %>%
  drop_na() %>%
  group_by(week, Maturity) %>%
  summarise(Maturity.n = n()) 

ggplot(fulmar.maturity, aes(fill = Maturity, x=week, y=Maturity.n)) +
  geom_bar(position="fill", stat="identity") +
  #scale_x_date(breaks = "1 week") +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank()) +
  theme(legend.direction = "horizontal") +
  theme(legend.position = "bottom") +
  #  theme(legend.text=element_text(size=12, family='serif')) +
  labs( x='Week', y='Proportion of collection',
        title='Lake Michigan Kiyi Female Maturation',
        subtitle='Collections made 1930-1931 by the R/V Fulmar',
        caption='Data: USGS, https://www.sciencebase.gov/catalog/item/5e9f635482cefae35a128c38') 

ggsave(here('Plots and Tables/LakeMichigan.Kiyi.Female.MaturationClass.png'), dpi = 300, width = 35, height = 16, units = "cm")



################################################################
##Fulmar Kiyi catch rate
fulmar.catch <- fulmar.all %>%
  group_by(Sample, SPECIES, Sex) %>%
  summarize(fish=n()) %>%
  ungroup()

fulmar.catch.zeros <-complete(fulmar.catch, Sample, SPECIES, Sex, fill=list(fish=0)) 

fulmar.kiyi <- fulmar.catch.zeros %>%
  left_join(fulmar.op) %>%
  subset(SPECIES == 206 & Month >= 9) %>%
  mutate(catch.rate = (((fish/GNetMeshLength_ft)/0.3048)*100)/NIGHTS_OUT) # us this to standardize to one-night fished

fulmar.kiyi.sex <-fulmar.kiyi %>%
  subset(Sex == 'male' | Sex == 'female') %>%
  group_by(Sex) %>%
  summarize(sexfish=sum(fish)) %>%
  ungroup()

fulmar.kiyi.catch.sum1 <- fulmar.kiyi %>%
  subset(Sex == 'male' | Sex == 'female') %>%
  group_by(depth.bin, Sex) %>%
  summarize(mean_se(catch.rate)) %>%
  left_join(fulmar.kiyi.sex) %>%
  unite("legend", Sex, sexfish, sep=", ")


ggplot(fulmar.kiyi.catch.sum1,  aes(x=factor(depth.bin, level = c('<50 m', '50-100 m',
                                                                  '100-150 m', '150-200 m',
                                                                  '200-250 m', '>250 m')), 
                                    y=y, fill = legend))+
  geom_col()+
  scale_y_continuous(expand=c(0,0)) +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(),
        legend.position = c(.15, .85)) +
  labs(x=' ', y='Mean catch rate per 100 m per day',
       title='Lake Michigan Spawning Kiyi Collections',
       subtitle='Collections made September-November by the R/V Fulmar, 1930-31,  2 3/8 - 3" mesh',
       caption='Data: USGS, https://www.sciencebase.gov/catalog/item/5e9f635482cefae35a128c38') 

  ggsave(here('Plots and Tables/Fulmar.Kiyi.CatchRate.by.Depth.png'), dpi = 300, width = 35, height = 16, units = "cm")


##Catch rate by mesh
fulmar.kiyi.catch.sum2 <- fulmar.kiyi %>%
  group_by(depth.bin, GNetMesh_in) %>%
  summarize(mean_se(catch.rate)) 
  
fulmar.kiyi.catch.sum3 <- fulmar.kiyi.catch.sum2 
fulmar.kiyi.catch.sum3$GNetMesh_in <- factor(fulmar.kiyi.catch.sum3$GNetMesh_in,
                                      levels = c('2 3/8"', '2 1/2"', '2 5/8"', '2 3/4"', '3"'))
  



ggplot(fulmar.kiyi.catch.sum3,  aes(x=factor(depth.bin, level = c('<50 m', '50-100 m',
                                                                    '100-150 m', '150-200 m',
                                                                    '200-250 m', '>250 m')), 
                                      y=y))+
  geom_col(fill = 'cadetblue2')+
  geom_errorbar(aes(ymin=ymin, ymax=ymax), width=.3) +
  scale_y_continuous(expand=c(0,0)) +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(),
        legend.position = c(.1, .85), 
        axis.text.x=element_text(size=18, family='serif'),
        axis.text.y=element_text(size=20, family='serif')) +
  labs(x=' ', y='Mean catch rate per 100 m per day',
       title='Lake Michigan Spawning Kiyi Collections',
       subtitle='Collections made September-November by the R/V Fulmar, 1930-31, 2 3/8 - 3" mesh',
       caption='Data: USGS, https://www.sciencebase.gov/catalog/item/5e9f635482cefae35a128c38') +
  facet_wrap(vars(GNetMesh_in), nrow = 3)

ggsave(here('Plots and Tables/Fulmar.Kiyi.CatchRate.by.DepthSexMesh.png'), dpi = 300, width = 35, height = 16, units = "cm")




##########################################################################################
##Kiyi catch Rate - Grand Island
#####################################################################################################
##Sum catch by panel 
GI.catch <- ciscoes.winter %>%
  subset(SPECIES == 202 |
         SPECIES == 204 |
         SPECIES == 206 |
         SPECIES == 207 |
         SPECIES == 208 |
         SPECIES == 210) %>%
  group_by(Sample, SPECIES) %>%
  summarize(fish=n()) %>%
  ungroup()

GI.catch.zeros <-complete(GI.catch, Sample, SPECIES, fill=list(fish=0)) 

GI.catch <- GI.catch.zeros %>%
  left_join(effort) %>%
  mutate(catch.rate = (((fish/GNetMeshLength_ft)/0.3048)*100)/Soak_days)

GI.catchrate <- GI.catch %>%
  group_by(SPECIES, GNetMesh_in) %>%
  summarize(mean.catchrate=mean(catch.rate), min.catchrate=min(catch.rate), 
            max.catchrate=max(catch.rate), median.catchrate=median(catch.rate),
            sd.catchrate=sd(catch.rate), n=n(), se.catchrate= sd.catchrate/sqrt(n))  

  

##Catch rate by mesh
GI.catch.kiyi <- GI.catch %>%
  subset(SPECIES == 206) %>% 
  group_by(GNetMesh_in) %>%
  summarize(total=sum(fish))  

##Catch rate by depth bin
GI.catch.kiyi.depth.bin <- GI.catch %>%
  subset(SPECIES == 206 & GNetMesh_in == 1.5) %>% 
  select(SPECIES, Date, winter.year2, week, month, MidDepth_m, depth.bin, catch.rate) %>%
  mutate(month.name = case_when(
  month == 11 ~ "November",
  month == 12 ~ "December",
  month == 1 ~ "January")) 
kiyi.depthbin.anova <- aov(catch.rate ~ depth.bin, data = GI.catch.kiyi.depth.bin)
summary(kiyi.depthbin.anova)



##Catch rate by month
GI.catch.kiyi.month <- GI.catch.kiyi.depth.bin %>%
  group_by(month.name) %>%
  summarize(mean.catchrate=mean(catch.rate), min.catchrate=min(catch.rate), 
            max.catchrate=max(catch.rate), median.catchrate=median(catch.rate),
            sd.catchrate=sd(catch.rate), n=n(), se.catchrate= sd.catchrate/sqrt(n))  



ggplot(GI.catch.kiyi.depth.bin, aes(x=week, y=catch.rate))+
  geom_point() +
  scale_y_continuous(expand=c(0,0)) +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(),
        legend.position = c(.1, .85), 
        axis.text.x=element_text(size=18, family='serif'),
        axis.text.y=element_text(size=20, family='serif')) +
  labs(x='Bathymetric depth (m) ', y='Mean catch rate per 100 m per 24-h') 

ggsave(here('Plots and Tables/Kiyi.CatchRate.by.DepthScatter.png'), dpi = 300, width = 35, height = 16, units = "cm")



ggplot(GI.catch.kiyi.depth.bin, aes(x=factor(depth.bin, level = c('<50 m', '50-100 m',
                                                          '100-150 m', '150-200 m',
                                                          '200-250 m', '>250 m')), 
                            y=catch.rate))+
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(colour=factor(month.name, levels = c("November", "December", "January"))), size = 6) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=14, color="black", fill="grey", alpha=0.5) +
  scale_y_continuous(expand=c(0,1.5)) +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.position="bottom", 
        legend.direction = "horizontal",  
#      legend.background = element_blank(), 
#      legend.box.background = element_rect(colour = "black"), 
      axis.text.x=element_text(size=24, family='serif'),
      axis.text.y=element_text(size=24, family='serif')) +
  guides(colour = guide_legend(nrow = 1, title="Month:")) +
  labs(x='Bathymetric depth (m)', y='Mean catch rate per 100 m per 24-h')
 
ggsave(here('Plots and Tables/Kiyi.CatchRate.by.DepthBin.BoxPlot.png'), dpi = 300, width = 35, height = 16, units = "cm")

ggplot(GI.catch.kiyi.depth.bin, aes(x=factor(week), y=catch.rate))+
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(aes(colour=factor(week)), size = 6) +
  stat_summary(fun.y=mean, geom="point", shape=20, size=14, color="black", fill="grey", alpha=0.5) +
  scale_y_continuous(expand=c(0,1.5)) +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.position="bottom", 
        legend.direction = "horizontal",  
        #      legend.background = element_blank(), 
        #      legend.box.background = element_rect(colour = "black"), 
        axis.text.x=element_text(size=24, family='serif'),
        axis.text.y=element_text(size=24, family='serif')) +
  guides(colour = guide_legend(nrow = 1, title="Week:")) +
  labs(x='Bathymetric depth (m)', y='Mean catch rate per 100 m per 24-h')

ggsave(here('Plots and Tables/Kiyi.CatchRate.by.DepthBin.BoxPlot.png'), dpi = 300, width = 35, height = 16, units = "cm")

GI.catch.kiyi.depth.bin <- GI.catch %>%
  subset(SPECIES == 206 & GNetMesh_in == 1.5) %>% 
  group_by(depth.bin) %>%
  summarize(mean.catchrate=mean(catch.rate), min.catchrate=min(catch.rate), 
            max.catchrate=max(catch.rate), median.catchrate=median(catch.rate))  


GI.catch.kiyi2 <- GI.catch.kiyi  
GI.catch.kiyi2$GNetMesh_in <- factor(GI.catch.kiyi2$GNetMesh_in,
                                             levels = c('1.5"', '2"', '2.5"', '3"'))




ggplot(GI.catch.kiyi,  aes(x=factor(GI.catch.kiyi$GNetMesh_in,
                                            levels = c('1.5"', '2"', '2.5"', '3"')), 
                                    y=y))+
  geom_col(fill = 'cadetblue2')+
  geom_errorbar(aes(ymin=ymin, ymax=ymax), width=.3) +
  scale_y_continuous(expand=c(0,0)) +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(),
        legend.position = c(.1, .85), 
        axis.text.x=element_text(size=18, family='serif'),
        axis.text.y=element_text(size=20, family='serif')) +
  labs(x=' ', y='Mean catch rate per 100 m per day',
       title='Lake Michigan Spawning Kiyi Collections',
       subtitle='Collections made September-November by the R/V Fulmar, 1930-31, 2 3/8 - 3" mesh',
       caption='Data: USGS, https://www.sciencebase.gov/catalog/item/5e9f635482cefae35a128c38') +
  facet_wrap(vars(GNetMesh_in), nrow = 3)

ggsave(here('Plots and Tables/Fulmar.Kiyi.CatchRate.by.DepthSexMesh.png'), dpi = 300, width = 35, height = 16, units = "cm")



##subset out Kiyi and 1.5 inch mesh
kiyi.catch <- GI.catch %>%
  subset(SPECIES == 206 & GNetMesh_in == 1.5) 


kiyi.catch.sum1 <- kiyi.catch %>%
  group_by(depth.bin) %>%
  summarize(mean_se(catch.rate)) 


kiyi.catch.sum2 <- kiyi.catch %>%
  group_by(depth.bin, winter.year2) %>%
  summarize(mean_se(catch.rate)) 

ggplot(kiyi.catch.sum1, aes(x=factor(depth.bin, level = c('<50 m', '50-100 m',
                                                          '100-150 m', '150-200 m',
                                                          '200-250 m', '>250 m')), 
                                                          y=y))+
  geom_col(fill = 'cadetblue2')+
  geom_errorbar(aes(ymin=ymin, ymax=ymax), width=.3) +
  scale_y_continuous(expand=c(0,0)) +
  plot_theme+
  labs(x=' ', y='Mean catch rate per 100 m per day',
       title='Lake Superior Grand Island Winter Kiyi Collections',
       subtitle='Collections made November-January, 2017-2021, 1.5" mesh',
       caption=ann_data_access) 

ggsave(here('Plots and Tables/Kiyi.CatchRate.by.Depth.Sex.png'), dpi = 300, width = 35, height = 16, units = "cm")


ggplot(kiyi.catch.sum2, aes(x=factor(depth.bin, level = c('<50 m', '50-100 m',
                                                          '100-150 m', '150-200 m',
                                                          '200-250 m', '>250 m')), 
                            y=y))+
  geom_col(fill = 'cadetblue2')+
  geom_errorbar(aes(ymin=ymin, ymax=ymax), width=.3) +
  scale_y_continuous(expand=c(0,0)) +
  plot_theme+
  theme(axis.text.x=element_text(size=20, family='serif')) +
  labs(x=' ', y='Mean catch rate per 100 m per day',
       title='Lake Superior Grand Island Winter Kiyi Collections',
       subtitle='Collections made November-January, 2017-2021, 1.5" mesh',
       caption=ann_data_access) +

  facet_wrap(vars(winter.year2), nrow = 3)

ggsave(here('Plots and Tables/Kiyi.CatchRate.by.Depth.Year.png'), dpi = 300, width = 35, height = 16, units = "cm")

############################################################################################################
##Kiyi collection catch rate map

#Kiyi catch rate all years
ggplot(kiyi.catch, aes(x=Long_DD, y = Lat_DD)) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 1) +
  geom_point(data=kiyi.catch, 
             mapping=aes(Long_DD, Lat_DD, fill=catch.rate), shape=21, size=10) +
  scale_fill_gradient(low = "yellow", high = "blue", na.value = NA, breaks=pretty_breaks(4), 
                      name = "Kiyi per 100 m") +
  scale_y_continuous(name='Latitude', breaks=pretty_breaks(), limits=c(46.2,47)) +
  scale_x_continuous(name='Longitude', breaks=pretty_breaks(), limits=c(-87,-86.4)) +
  theme_bw() +
  map_theme +
  geom_label(label="Grand Island", 
             x=-86.67,
             y=46.53,
             label.size = NA,
             size=8, 
             color = "black") +
  geom_label(label="Munising, MI", 
             x=-86.64,
             y=46.39,
             label.size = 1,
             size=8, 
             color = "black") +
  theme(legend.position = c(.85, .8), 
        legend.title = element_text(size=20, family='serif'), 
        legend.text = element_text(size=20, family='serif')) +
  labs(title='Grand Island Winter Kiyi Collections',
       subtitle='Collections made November-January, 2017-2021, 1.5" mesh',
       caption=ann_data_access)  

ggsave(here('Plots and Tables/Kiyi.Catch.Rate.Map.png'), dpi = 300, width = 30, height = 40, units = "cm")


#Kiyi catch rate by years
ggplot(kiyi.catch, aes(x=Long_DD, y = Lat_DD)) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 1) +
  geom_point(data=kiyi.catch, 
             mapping=aes(Long_DD, Lat_DD, fill=catch.rate), shape=21, size=10) +
  scale_fill_gradient(low = "yellow", high = "blue", na.value = NA, breaks=pretty_breaks(4), 
                      name = "Kiyi per 100 m") +
  scale_y_continuous(name='Latitude', breaks=pretty_breaks(), limits=c(46.2,47)) +
  scale_x_continuous(name='Longitude', breaks=pretty_breaks(), limits=c(-86.8,-86.4)) +
  theme_bw() +
  map_theme +
  #  geom_label(label="Grand Island", 
  #             x=-86.67,
  #             y=46.53,
  #             label.size = NA,
  #             size=8, 
  #             color = "black") +
  geom_label(label="Munising, MI", 
             x=-86.64,
             y=46.39,
             label.size = 1,
             size=8, 
             color = "black") +
  theme(legend.position = c(.1, .9), 
        legend.title = element_text(size=20, family='serif'), 
        legend.text = element_text(size=20, family='serif'),
        axis.text=element_text(size=16, family='serif')) +
  labs(title='Grand Island Winter Kiyi Collections',
       subtitle='Collections made November-January, 2017-2021, 1.5" mesh',
       caption=ann_data_access)  +
  facet_wrap(vars(winter.year), nrow = 3)

ggsave(here('Plots and Tables/Kiyi.CatchRate.Map.Years.png'), dpi = 300, width = 30, height = 40, units = "cm")




##########################################################################################
##Annual summaries of effort and Species collected - winter gill netting
#####################################################################################################
###Species collected
Scatch <- ciscoes.winter %>%
  group_by(SPECIES) %>%
  summarize(fish=n())

year.sum.fish <-ciscoes.winter %>%
  select(4, 10, 24) %>%
  group_by(winter.year, SPECIES) %>%
  summarise(fishes = n()) %>%
  ungroup() %>%
  subset(SPECIES==206 | 
           SPECIES == 204 | 
           SPECIES == 202 |  
           SPECIES == 207 |
           SPECIES == 208 |
           SPECIES == 210) %>%
  left_join(sci.names) %>%
  left_join(Scatch) %>%
  unite("legend", COMMON_NAME,fish, sep=", ") %>%

    
ggplot(year.sum.fish, aes(x=winter.year, y=fishes)) +
  geom_point(aes(fill= legend), shape=21, alpha=0.8, size=12) +
  geom_segment(aes(x=winter.year, xend=winter.year, y=0, yend=fishes), size=1, color='black')+
  scale_x_continuous(breaks=pretty_breaks())+
  scale_y_continuous(breaks=pretty_breaks()) +
  annotate(geom="text", x=2018, y=100, label="Weather\nprevented\ncollections", size=8, family='serif') +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  theme(legend.position = c(.2, .7)) +
  guides(fill= guide_legend(size=5), 
         size = guide_legend(guide = 'none')) +
  labs( x='Winter', y='Count',
        title='Lake Superior Grand Island Winter Ciscoe Collections',
        subtitle='Collections made November-January, 2017-2021',
        caption=ann_data_access) 

ggsave(here('Plots and Tables/Catch.by.YearSpecies.png'), dpi = 300, width = 35, height = 16, units = "cm")

###########################################################################################################
###Effort number of sets by mesh
year.effort.sum <-ciscoes.winter %>%
#  select(9:10, 12:14, 21:22) %>%
  distinct(Date, GNetGang, GNetMesh_in) %>%
  mutate(year = year(Date), month = month(Date)) %>%
  mutate(winter.year = case_when(
    month >10 ~ year + 1,
    month <=10 ~ year)) %>%
  group_by(winter.year, GNetMesh_in) %>%
  summarise(sets = n()) %>%
  ungroup() 


ggplot(year.effort.sum, aes(x=winter.year, y=sets)) +
  geom_jitter(aes(fill= as_factor(GNetMesh_in)), shape=21, alpha=0.8, size=12) +
  geom_segment(x=2017.5, xend=2017.5, y=0, yend=4, linetype = "dashed", size=1, color='black') +
  geom_segment(x=2018.5, xend=2018.5, y=0, yend=7, linetype = "dashed", size=1, color='black') +
  geom_segment(x=2019.5, xend=2019.5, y=0, yend=16, linetype = "dashed", size=1, color='black') +
  geom_segment(x=2020.5, xend=2020.5, y=0, yend=16, linetype = "dashed", size=1, color='black') +
  scale_x_continuous(breaks=pretty_breaks())+
  scale_y_continuous(breaks=pretty_breaks()) +
  annotate(geom="text", x=2018, y=5, label="Weather\nprevented\ncollections", size=8, family='serif') +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.position = c(.25, .85)) +
  guides(fill= guide_legend(title = "Mesh in inches", direction="horizontal", title.position = "top")) + 
  labs(x='Winter', y='Number of gill net sets',
        title='Lake Superior Grand Island Winter Ciscoe Collections',
        subtitle='Collections made November-January, 2017-2021',
        caption=ann_data_access) 

ggsave(here('Plots and Tables/Mesh.Sets.ByYear.png'), dpi = 300, width = 35, height = 16, units = "cm")

       
###Depth of sets for across all dates
gnet.dates <-effort %>%
  subset(Method == 'Gillnet') %>%
  select(Sample, Date) %>%
  distinct(Date, .keep_all = TRUE) %>%
  arrange(Date) %>%
  mutate(Date2=lead(as.Date(Date),1)-as.Date(Date)) %>%
  mutate(Date3=as.Date(Date) +  Date2/2)

gnet.depths <-effort %>%
  select(Sample, Date, Location, Method, GNetGang, GNetMesh_in, MidDepth_m) %>%
#  distinct(Date, Location, .keep_all = TRUE) %>%
  mutate(year = year(Date), month = month(Date)) %>%
  mutate(winter.year = case_when(
    month >10 ~ year + 1,
    month <=10 ~ year)) %>%
  subset(Method == 'Gillnet') %>%
#  subset(Method == 'Gillnet' & GNetMesh_in == 1.5) %>%
  left_join(gnet.dates, by = "Date")

  
ggplot(gnet.depths, aes(x=as.factor(Date), ymin=0, ymax=MidDepth_m, group=factor(Location), fill=as.factor(GNetMesh_in))) +
  geom_jitter(aes(x=as.factor(Date), y=MidDepth_m, fill= as.factor(GNetMesh_in)), shape=21, alpha=0.8, size=10, position=position_dodge(width = 0.8)) +
  geom_linerange(position=position_dodge(width = 0.8), alpha=0.8, size=1) +
  scale_y_reverse(breaks=pretty_breaks(), lim=c(275,0)) +
  theme_bw() +
  plot_theme +
  scale_fill_brewer(palette="Accent") +

#  geom_vline(xintercept = as.numeric(as.factor(gnet.depths$Date3)), col = "red", size = 1) +  
#  geom_vline(xintercept = as.Date(gnet.depths$Date3), col = "red", size = 1) +  
  theme(legend.position = c(.5, .075)) +
  theme(axis.text.x=element_text(size=12, family='serif',angle = 60, vjust = 1, hjust=1), 
        legend.background = element_blank(), 
        legend.box.background = element_rect(colour = "black"), 
        legend.text=element_text(size=14, family='serif'), 
        legend.title=element_text(size=14, family='serif')) +
  guides(fill= guide_legend(title = "Mesh (inches)", direction="horizontal", title.position = "left")) + 
  labs(x='Date', y='Depth (m)',
       title='Lake Superior Grand Island Winter Ciscoe Collections',
       subtitle='Collections made November-January, 2017-2021',
       caption=ann_data_access) 

ggsave(here('Plots and Tables/SetsByDepth.png'), dpi = 300, width = 35, height = 16, units = "cm")

ggplot(subset(gnet.depths, GNetMesh_in == 1.5), aes(x=as.factor(Date), ymin=0, ymax=MidDepth_m, group=factor(Location), fill=as.factor(GNetMesh_in))) +
  geom_jitter(aes(x=as.factor(Date), y=MidDepth_m, fill= as.factor(GNetMesh_in)), shape=21, alpha=0.8, size=10, position=position_dodge(width = 0.8)) +
  geom_linerange(position=position_dodge(width = 0.8), alpha=0.8, size=1) +
  scale_y_reverse(breaks=pretty_breaks(), lim=c(275,0)) +
  theme_bw() +
  plot_theme +
  scale_fill_brewer(palette="Accent") +
  
  #  geom_vline(xintercept = as.numeric(as.factor(gnet.depths$Date3)), col = "red", size = 1) +  
  #  geom_vline(xintercept = as.Date(gnet.depths$Date3), col = "red", size = 1) +  
  theme(legend.position = c(.5, .075)) +
  theme(axis.text.x=element_text(size=12, family='serif',angle = 60, vjust = 1, hjust=1), 
        legend.background = element_blank(), 
        legend.box.background = element_rect(colour = "black"), 
        legend.text=element_text(size=14, family='serif'), 
        legend.title=element_text(size=14, family='serif')) +
  guides(fill= guide_legend(title = "Mesh (inches)", direction="horizontal", title.position = "left")) + 
  labs(x='Date', y='Depth (m)',
       title='Lake Superior Grand Island Winter Ciscoe Collections',
       subtitle='Collections made November-January, 2017-2021',
       caption=ann_data_access) 

ggsave(here('Plots and Tables/1.5SetsByDepth.png'), dpi = 300, width = 35, height = 16, units = "cm")

##########################################################################################
##Sample site map
#####################################################################################################

ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

ciscoes.map <- ciscoes.all %>%
  select(Sample, Date, Method, Lat_DD, Long_DD, MidDepth_m) %>%
  mutate(Season = case_when(
    month(Date) >10 ~ 'winter',
    month(Date) <=10 & month(Date) >=9 ~ 'fall', 
    month(Date) <9 & month(Date) >=6 ~ 'summer', 
    month(Date) <6 ~ 'spring')) %>%
  mutate(year = year(Date), month = month(Date)) %>%
  mutate(winter.year = case_when(
    month >10 ~ year + 1,
    month <=10 ~ year)) %>%
  mutate(winter.year2 = case_when(
    winter.year == 2017 ~ 'Winter 2017',
    winter.year == 2018 ~ 'Winter 2018',
    winter.year == 2019 ~ 'Winter 2019',
    winter.year == 2020 ~ 'Winter 2020',
    winter.year == 2021 ~ 'Winter 2021')) 

img <- image_read(here('Data/NorthArrow2.png'))
raster <- as.raster(img)

ggplot(subset(ciscoes.map, Method == "Gillnet"), aes(x=Long_DD, y = Lat_DD)) +
  geom_point(shape=21, fill = 'deepskyblue', size=7) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 1) +
#  scale_y_continuous(name='Latitude', breaks=pretty_breaks(), limits=c(46.3,46.85)) +
#  scale_x_continuous(name='Longitude', breaks=pretty_breaks(), limits=c(-86.75,-86.5)) +
  scale_y_continuous(name='Latitude', breaks=pretty_breaks(), limits=c(46.2,47)) +
  scale_x_continuous(name='Longitude', breaks=pretty_breaks(), limits=c(-87,-86.4)) +
  theme_bw() +
  map_theme +
  geom_label(label="Grand Island", 
             x=-86.67,
             y=46.53,
             label.size = NA,
             size=8, 
             color = "black") +
  geom_label(label="Munising, MI", 
             x=-86.64,
             y=46.39,
             label.size = 1,
             size=8, 
             color = "black") +
  theme(legend.position = c(.85, .8), 
        legend.title = element_text(size=20, family='serif'), 
        legend.text = element_text(size=20, family='serif')) +
  labs(title='Grand Island Winter Ciscoe Collection Sites',
       subtitle='Collections made November-January, 2017-2021',
       caption=ann_data_access) +
  scalebar(x.min = -86.4, x.max = -87, 
           y.min = 46.2, y.max = 47, 
           dist_unit = "km", 
           dist = 5, 
           transform = TRUE, 
           model = "WGS84", 
           height = 0.05, 
           st.dist = 0.03, 
           st.bottom = FALSE,
           st.size = 5, 
           location = "bottomright") +
  annotation_raster(raster,-87, -86.95, 46.9, 46.95)

#  north(x.min = -86.9, x.max = -86.5, 
#        y.min = 46.3, y.max = 46.9, 
#        location = "topleft", symbol = 1) 
  
ggsave(here('Plots and Tables/GrandIsleGillNetLocations.png'), dpi = 300, width = 30, height = 40, units = "cm")


ggplot(subset(ciscoes.map, Method == "Gillnet"), aes(x=Long_DD, y = Lat_DD)) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 1) +
  geom_point(data=subset(ciscoes.map, Method == "Gillnet"), 
             mapping=aes(Long_DD, Lat_DD, fill=MidDepth_m), shape=21, size=10) +
  scale_fill_gradient(low = "yellow", high = "blue", na.value = NA, breaks=pretty_breaks(4), 
                      name = "Depth (m)") +
  scale_y_continuous(name='Latitude', breaks=pretty_breaks(), limits=c(46.2,47)) +
  scale_x_continuous(name='Longitude', breaks=pretty_breaks(), limits=c(-86.8,-86.4)) +
  theme_bw() +
  map_theme +
  geom_label(label="Grand Island", 
             x=-86.67,
             y=46.53,
             label.size = NA,
             size=8, 
             color = "black") +
  geom_label(label="Munising, MI", 
             x=-86.64,
             y=46.39,
             label.size = 1,
             size=8, 
             color = "black") +
  theme(legend.position = c(.85, .8), 
        legend.title = element_text(size=20, family='serif'), 
        legend.text = element_text(size=20, family='serif')) +
  labs(title='Grand Island Winter Ciscoe Collection Sites',
       subtitle='Collections made November-January, 2017-2021',
       caption=ann_data_access) 

ggsave(here('Plots and Tables/GrandIsleGillNetLocationsDepths.png'), dpi = 300, width = 30, height = 40, units = "cm")


##Depth map by Year
ggplot(subset(ciscoes.map, Method == "Gillnet"), aes(x=Long_DD, y = Lat_DD)) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 1) +
  geom_point(data=subset(ciscoes.map, Method == "Gillnet"), 
             mapping=aes(Long_DD, Lat_DD, fill=MidDepth_m), shape=21, size=10) +
  scale_fill_gradient(low = "yellow", high = "blue", na.value = NA, breaks=pretty_breaks(4), 
                      name = "Depth (m)") +
  scale_y_continuous(name='Latitude', breaks=pretty_breaks(), limits=c(46.2,47)) +
  scale_x_continuous(name='Longitude', breaks=pretty_breaks(), limits=c(-86.8,-86.4)) +
  theme_bw() +
  map_theme +
#  geom_label(label="Grand Island", 
#             x=-86.67,
#             y=46.53,
#             label.size = NA,
#             size=8, 
#             color = "black") +
  geom_label(label="Munising, MI", 
             x=-86.64,
             y=46.39,
             label.size = 1,
             size=8, 
             color = "black") +
  theme(legend.position = c(.1, .9), 
        legend.title = element_text(size=20, family='serif'), 
        legend.text = element_text(size=20, family='serif'),
        axis.text=element_text(size=16, family='serif')) +
  labs(title='Grand Island Winter Ciscoe Collection Sites',
       subtitle='Collections made November-January, 2017-2021',
       caption=ann_data_access) +
  facet_wrap(vars(winter.year2), nrow = 3)

ggsave(here('Plots and Tables/GrandIsleGillNetLocationsDepthsYear.png'), dpi = 300, width = 30, height = 40, units = "cm")


ggplot(ciscoes.map, aes(x=Long_DD, y = Lat_DD)) +
  geom_jitter(aes(fill = Method), shape=21, size=7) + ## , alpha=0.4) +
  scale_fill_brewer(palette="Accent") +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 1)+
  scale_y_continuous(name='Latitude', breaks=pretty_breaks()) +
  scale_x_continuous(name='Longitude', breaks=pretty_breaks()) +
  theme_bw() +
  map_theme +
  theme(legend.title = element_blank(), 
        legend.position = c(.15, .85), 
        legend.text = element_text(size=20, family='serif')) +
  labs(title='Kiyi Gonad Maturation Collection Locations',
       subtitle='Collections made November-January, 2017-2021',
       caption=ann_data_access) 

ggsave(here('Plots and Tables/Kiyi_GSI_Sites.png'), dpi = 300, width = 40, height = 20, units = "cm")


ggplot(ciscoes.map, aes(x=Long_DD, y = Lat_DD)) +
  geom_jitter(aes(fill = Season), shape=21, size=7) + ## , alpha=0.4) +
  scale_fill_brewer(palette="Accent") +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 1)+
  scale_y_continuous(name='Latitude', breaks=pretty_breaks()) +
  scale_x_continuous(name='Longitude', breaks=pretty_breaks()) +
  theme_bw() +
  map_theme +
  theme(legend.title = element_blank(), 
        legend.position = c(.15, .85), 
        legend.text = element_text(size=20, family='serif')) +
  labs(title='Kiyi Gonad Maturation Collection Locations',
       subtitle='Collections made November-January, 2017-2021',
       caption=ann_data_access) 

ggsave(here('Plots and Tables/Kiyi_GSI_Seasons.png'), dpi = 300, width = 40, height = 20, units = "cm")



##Pie maps showing collections of cisoes by species
#########################################################################################################
ciscoe.pies <- ciscoes.all %>%
  select(Sample, Location, Date, Method, Lat_DD, Long_DD, StartDepth_m, MidDepth_m, COMMON_NAME) %>%
  mutate(Season = case_when(
    month(Date) >10 ~ 'winter',
    month(Date) <=10 & month(Date) >=9 ~ 'fall', 
    month(Date) <9 & month(Date) >=6 ~ 'summer', 
    month(Date) <6 ~ 'spring')) %>%
  group_by(Location, COMMON_NAME) %>%
  summarize(fish.n=n()) %>%
  left_join(ciscoes.all) %>%
  select(Location, Date, Method, Lat_DD, Long_DD, StartDepth_m, MidDepth_m, COMMON_NAME, fish.n) %>%
  distinct(Location, COMMON_NAME, .keep_all = TRUE) %>%
  subset(Method == "Gillnet") %>%
  subset(COMMON_NAME == 'Bloater' |
           COMMON_NAME == 'Cisco' |
           COMMON_NAME == 'Kiyi' |
           COMMON_NAME == 'Shortnose Cisco' |
           COMMON_NAME == 'Shortjaw Cisco' |
           COMMON_NAME == 'Blackfin Cisco') %>% 
  pivot_wider(names_from = COMMON_NAME, values_from = fish.n, values_fill = 0) 

ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)


##Relative occurrence pie map for bloater, cisco, kiyi 
ggplot(data=ciscoe.pies, aes(x=Long_DD, y=Lat_DD)) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5)+ 
  geom_scatterpie(aes(x=Long_DD, y=Lat_DD), 
                  data=ciscoe.pies,
##                  cols= c("Bloater", "Cisco", "Kiyi", "Blackfin Cisco", "Shortjaw Cisco", "Shortnose Cisco"),
                  cols= c("Bloater", "Cisco", "Kiyi"),
                  pie_scale = 0.75) +
  scale_fill_brewer(palette="Accent") +
  scale_y_continuous(name='Latitude', breaks=pretty_breaks(), limits=c(46.35,46.85)) +
  scale_x_continuous(name='Longitude', breaks=pretty_breaks(), limits=c(-86.75,-86.5)) +
  theme_bw() +
  map_theme +
  geom_label(label="Grand Island", 
             x=-86.67,
             y=46.53,
             label.size = NA,
             size=8, 
             color = "black") +
  geom_label(label="Munising, MI", 
             x=-86.64,
             y=46.39,
             label.size = 1,
             size=8, 
             color = "black") +
  theme(legend.title = element_blank(),
        legend.position = c(.7, .85),
        legend.text=element_text(size=16, family='serif')) +
  labs(title='Grand Island Winter Ciscoe Collections',
            subtitle='Collections made November-January, 2017-2021',
            caption=ann_data_access) 

ggsave(here('Plots and Tables/Ciscoe.Pies.png'), dpi = 300, width = 40, height = 20, units = "cm")




##########################################################################################
##Ciscoe species collection by mesh
#####################################################################################################
##summarize catch by mesh and depth - bubble plot
Scatch <- ciscoes.winter %>%
  group_by(SPECIES) %>%
  summarize(fish=n())

catch.mesh <- ciscoes.winter %>%
  group_by(SPECIES, GNetMesh_in, MidDepth_m) %>%
  summarize(catch=n()) %>%
  mutate(freq = catch / sum(catch)) %>%
  ungroup() %>%
  subset(SPECIES==206 | 
           SPECIES == 204 | 
           SPECIES == 202 |  
           SPECIES == 207 |
           SPECIES == 208 |
           SPECIES == 210) %>%
  left_join(sci.names) %>%
  left_join(Scatch) %>%
  unite("legend", COMMON_NAME, fish, sep=", ")

## Catch by mesh and depth, sized by catch
ggplot(catch.mesh, aes(x=GNetMesh_in, y=MidDepth_m)) +
  geom_point(data=catch.mesh, aes(x=GNetMesh_in, y=MidDepth_m, size=freq, fill=legend), 
             shape=21, alpha=0.8) +
  scale_x_continuous(breaks=pretty_breaks())+
  scale_y_reverse(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme +
  scale_fill_brewer(palette="Accent") +
#  theme(legend.title = element_blank(), 
#        legend.position = "none") +
#  guides(fill= guide_legend(size=5), 
#         size = guide_legend(guide = 'none')) +
  labs( x='Mesh (inches)', y='Depth (m)',
        title='Lake Superior Grand Island Winter Ciscoe Collections',
        subtitle='Collections made November-January, 2017-2021',
        caption=ann_data_access) 

#########################################################################################
##All ciscoes
##Catch by mesh
Scatch <- ciscoes.winter %>%
  group_by(SPECIES) %>%
  summarize(fish=n())

# sample size by mesh
Mcatch <- ciscoes.winter %>%
  group_by(GNetMesh_in) %>% 
  summarize(mesh=n())


catch.mesh <- ciscoes.winter %>%
  group_by(SPECIES, GNetMesh_in) %>%
  summarize(catch=n()) %>%
  mutate(freq = catch / sum(catch)) %>%
  ungroup() %>%
  subset(SPECIES==206 | 
           SPECIES == 204 | 
           SPECIES == 202 |  
           SPECIES == 207 |
           SPECIES == 208 |
           SPECIES == 210) %>%
  left_join(sci.names) %>%
  left_join(Mcatch) %>%
  mutate(naxis = paste0(GNetMesh_in, "\n", "n=", mesh)) %>% 
  mutate(fish = "fish") %>%
  unite("xaxis", naxis, fish, sep=" ") %>%
  left_join(Scatch) %>%
  unite("legend", COMMON_NAME, fish, sep=", ") 

ggplot(catch.mesh, aes(x=xaxis, y=freq)) +
  geom_point(aes(fill= legend), shape=21, alpha=0.8, size=12) +
  geom_segment(aes(x=xaxis, xend=xaxis, y=0, yend=freq), size=1, color='black')+
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(), 
        legend.position = c(.85, .7)) + 
#        axis.text.x=element_text(size=20, family='serif')) +
  guides(fill= guide_legend(size=5), 
         size = guide_legend(guide = 'none')) +
  labs( x='Gill net mesh (inches)', y='Proporton of species` total catch by mesh',
        title='Lake Superior Grand Island Winter Ciscoe Collections',
        subtitle='Collections made November-January, 2017-2021',
        caption=ann_data_access) 

ggsave(here('Plots and Tables/Catch.by.MeshSpecies.png'), dpi = 300, width = 35, height = 20, units = "cm")


#########################################################################################
##All ciscoes - by depth in 1.5 inch mesh
Scatch <- ciscoes.winter %>%
  filter(GNetMesh_in == 1.5) %>%
  group_by(SPECIES) %>%
  summarize(fish=n())

catch.depth <- ciscoes.winter %>%
  filter(GNetMesh_in == 1.5) %>%
  group_by(MidDepth_m, SPECIES) %>%
  summarize(catch=n()) %>%
  mutate(freq = catch / sum(catch)) %>%
  ungroup() %>%
  subset(SPECIES==206 | 
           SPECIES == 204 | 
           SPECIES == 202 |  
           SPECIES == 207 |
           SPECIES == 208 |
           SPECIES == 210) %>%
  left_join(sci.names) %>%
  left_join(Scatch) %>%
  left_join(ciscoes.winter) %>%
  unite("legend", COMMON_NAME,fish, sep=", ")


ggplot(catch.depth, aes(x=MidDepth_m, y=freq)) +
  geom_point(aes(fill= legend), shape=21, alpha=0.8, size=12) +
  geom_segment(aes(x=MidDepth_m, xend=MidDepth_m, y=0, yend=freq), size=1, color='black')+
#  geom_segment(aes(x=min(MidDepth_ft), xend=max(MidDepth_ft), y=0.05, yend=0.05), size=3, color='black') +
  scale_x_continuous(breaks=pretty_breaks()) +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(), 
        legend.position = c(.12, .73), 
        legend.text = element_text(size=20, family='serif')) + 
  guides(fill= guide_legend(size=5), 
         size = guide_legend(guide = 'none')) +
  labs( x='Depth (m)', y='Proporton of catch by species',
        title='Lake Superior Grand Island Winter Ciscoe Collections in 1.5 inch Mesh',
        subtitle='Collections made November-January, 2017-2021',
        caption=ann_data_access) 
#+ 
#  facet_wrap(vars(winter.year2), nrow = 3)

ggsave(here('Plots and Tables/Catch.by.Depth_1.5Inch.png'), dpi = 300, width = 35, height = 16, units = "cm")


#########################################################################################
##All ciscoes - by depth all meshes
Scatch <- ciscoes.winter %>%
  group_by(SPECIES) %>%
  summarize(fish=n())

catch.depth <- ciscoes.winter %>%
  group_by(MidDepth_m, SPECIES) %>%
  summarize(catch=n()) %>%
  mutate(freq = catch / sum(catch)) %>%
  ungroup() %>%
  subset(SPECIES==206 | 
           SPECIES == 204 | 
           SPECIES == 202 |  
           SPECIES == 207 |
           SPECIES == 208 |
           SPECIES == 210) %>%
  left_join(sci.names) %>%
  left_join(Scatch) %>%
  left_join(ciscoes.winter) %>%
  unite("legend", COMMON_NAME,fish, sep=", ")


ggplot(catch.depth, aes(x=MidDepth_m, y=freq)) +
  geom_point(aes(fill= legend), shape=21, alpha=0.8, size=12) +
  geom_segment(aes(x=MidDepth_m, xend=MidDepth_m, y=0, yend=freq), size=1, color='black')+
  #  geom_segment(aes(x=min(MidDepth_ft), xend=max(MidDepth_ft), y=0.05, yend=0.05), size=3, color='black') +
  scale_x_continuous(breaks=pretty_breaks()) +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  theme(legend.position = c(.15, .72)) +
  guides(fill= guide_legend(size=5), 
         size = guide_legend(guide = 'none')) +
  labs( x='Depth (m)', y='Proporton of catch by species',
        title='Lake Superior Grand Island Winter Ciscoe Collections',
        subtitle='Collections made November-January, 2017-2021',
        caption=ann_data_access) 

#+ facet_wrap(~COMMON_NAME, scales="free_y")

ggsave(here('Plots and Tables/Catch.by.DepthSpecies.png'), dpi = 300, width = 35, height = 16, units = "cm")


##############################################################################################
##Depth Density all meshes

ggplot(catch.depth, aes(x=MidDepth_m, y = ..scaled.., weight=catch, group=legend, fill=legend)) + 
  geom_density(alpha=0.4) +
  plot_theme + 
  theme(legend.position=c(0.15,0.85), 
        legend.title = element_blank()) +
  scale_x_continuous(breaks=pretty_breaks()) +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_brewer(palette="Accent") +
  labs( x='Depth (m)', y='Relative density',
        title='Lake Superior Grand Island Winter Ciscoe Collections',
        subtitle='Collections made November-January, 2017-2021',
        caption=ann_data_access) 

ggsave(here('Plots and Tables/Ciscoes.DepthDensity.png'), dpi = 300, width = 35, height = 16, units = "cm")


#########################################################################################
##Depth density for bloater, cisco, and kiyi

##All ciscoes - by depth all meshes
Scatch <- ciscoes.winter %>%
  group_by(SPECIES) %>%
  summarize(fish=n())

catch.depth <- ciscoes.winter %>%
  group_by(SPECIES, MidDepth_m) %>%
  summarize(catch=n()) %>%
  mutate(freq = catch / sum(catch)) %>%
  subset(SPECIES==206 | 
           SPECIES == 204 | 
           SPECIES == 202) %>%
  left_join(sci.names) %>%
  left_join(Scatch) %>%
  unite("legend", COMMON_NAME,fish, sep=", ")

ggplot(catch.depth, aes(x=MidDepth_m, y = ..scaled.., weight=catch, group=legend, fill=legend)) + 
  geom_density(alpha=0.4) +
  plot_theme + 
  theme(legend.position=c(0.15,0.85), 
        legend.title = element_blank()) +
  scale_x_continuous(breaks=pretty_breaks()) +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_brewer(palette="Accent") +
  labs( x='Depth (m)', y='Relative density',
        title='Lake Superior Grand Island Winter Ciscoe Collections',
        subtitle='Collections made November-January, 2017-2021',
        caption=ann_data_access) 

ggsave(here('Plots and Tables/Cisco.Bloater.Kiyi.DepthDensity.png'), dpi = 300, width = 35, height = 16, units = "cm")

#########################################################################################
##Kiyi- by depth - density plot for 1.5 inch mesh

catch.depth <- ciscoes.winter %>%
  filter(GNetMesh_in == 1.5) %>%
  group_by(SPECIES, MidDepth_m) %>%
  summarize(catch=n()) %>%
  mutate(freq = catch / sum(catch)) %>%
  ungroup() %>%
  subset(SPECIES==206) %>%
  left_join(sci.names)

Scatch <- ciscoes.winter %>%
  filter(GNetMesh_in == 1.5) %>%
  group_by(SPECIES) %>%
  summarize(fish=n())

catch.depth <- catch.depth %>%
  left_join(Scatch) %>%
  unite("legend", COMMON_NAME,fish, sep=", ")


ggplot(catch.depth, aes(x=MidDepth_m, y = ..scaled.., weight=catch, group=legend, fill=legend)) + 
  geom_density(alpha=0.4) +
  plot_theme + 
  theme(legend.position=c(0.15,0.85), 
        legend.title = element_blank()) +
  scale_x_continuous(breaks=pretty_breaks()) +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_brewer(palette="Accent") +
labs( x='Depth (m)', y='Relative density',
      title='Lake Superior Grand Island Winter Kiyi Collections in 1.5 inch Mesh',
      subtitle='Collections made November-January, 2017-2021',
      caption=ann_data_access) 

ggsave(here('Plots and Tables/Kiyi.DepthDensity_1.5inch.png'), dpi = 300, width = 35, height = 16, units = "cm")


#########################################################################################
##All ciscoes - by mesh - density plot
Scatch <- ciscoes.winter %>%
  group_by(SPECIES) %>%
  summarize(fish=n())

catch.mesh <- ciscoes.winter %>%
  group_by(SPECIES, GNetMesh_in) %>%
  summarize(catch=n()) %>%
  mutate(freq = catch / sum(catch)) %>%
  ungroup() %>%
  subset(SPECIES==206 | 
           SPECIES == 204 | 
           SPECIES == 202) %>%
  left_join(sci.names) %>%
  left_join(Scatch) %>%
  unite("legend", COMMON_NAME,fish, sep=", ")

ggplot(catch.mesh, aes(x=GNetMesh_in, y = ..scaled.., weight=catch, group=legend, fill=legend)) + 
  geom_density(alpha=0.4) +
  plot_theme + 
  theme(legend.position=c(0.12,0.9)) +
  theme(legend.title = element_blank()) +
  scale_x_continuous(breaks=pretty_breaks()) +
  scale_y_continuous() +
  scale_fill_brewer(palette="Accent") +
  labs( x='Gill net mesh (inches)', y='Relative density',
        title='Lake Superior Grand Island Winter Ciscoe Collections',
        subtitle='Collections made November-January, 2017-2021',
        caption=ann_data_access) 

ggsave(here('Plots and Tables/Catch.by.MeshDensity.png'), dpi = 300, width = 35, height = 16, units = "cm")


#########################################################################################
#Catch by date
##All ciscoes - all meshes all depths
Scatch <- ciscoes.winter %>%
 # filter(GNetMesh_in == 1.5) %>%
  group_by(SPECIES) %>%
  summarize(fish=n())

catch.date <- ciscoes.winter %>%
 # filter(GNetMesh_in == 1.5) %>%
  group_by(Date, SPECIES) %>%
  summarize(catch=n()) %>%
  mutate(freq = catch / sum(catch)) %>%
  ungroup() %>%
  subset(SPECIES==206 | 
           SPECIES == 204 | 
           SPECIES == 202 |  
           SPECIES == 207 |
           SPECIES == 208 |
           SPECIES == 210) %>%
  left_join(sci.names) %>%
  left_join(Scatch) %>%
  unite("legend", COMMON_NAME,fish, sep=", ")


ggplot(catch.date, aes(x=as.factor(Date), y=freq)) +
  geom_point(aes(fill= legend), shape=21, alpha=0.8, size=10) +
  geom_segment(aes(x=as.factor(Date), xend=as.factor(Date), y=0, yend=freq), size=1, color='black')+
#  scale_x_discrete(breaks=pretty_breaks()) +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  theme(legend.position = c(.5, .75)) +
  theme(axis.text.x=element_text(size=12, family='serif')) +
  theme(legend.text=element_text(size=12, family='serif')) +
  guides(fill= guide_legend(size=5), 
         size = guide_legend(guide = 'none')) +
  labs( x='Date', y='Proportion of catch by species',
        title='Lake Superior Grand Island Winter Ciscoe Collections',
        subtitle='Collections made November-January, 2017-2021',
        caption=ann_data_access) 

#+ facet_wrap(~COMMON_NAME, scales="free_y")

ggsave(here('Plots and Tables/Catch.by.DateSpecies.png'), dpi = 300, width = 35, height = 16, units = "cm")

#########################################################################################
#Catch by date - 1.5 inch mesh
##All ciscoes - all meshes all depths
Tcatch <- ciscoes.winter %>%
  filter(GNetMesh_in == 1.5) %>%
  group_by(SPECIES) %>%
  summarize(fish=n())

catch.date <- ciscoes.winter %>%
  filter(GNetMesh_in == 1.5) %>%
  group_by(Date, SPECIES) %>%
  summarize(catch=n()) %>%
  mutate(freq = catch / sum(catch)) %>%
  ungroup() %>%
  subset(SPECIES==206 | 
           SPECIES == 204 | 
           SPECIES == 202 |  
           SPECIES == 207 |
           SPECIES == 208 |
           SPECIES == 210) %>%
  left_join(sci.names) %>%
  left_join(Tcatch) %>%
  unite("legend", COMMON_NAME,fish, sep=", ")


ggplot(catch.date, aes(x=as.factor(Date), y=freq)) +
  geom_point(aes(fill= legend), shape=21, alpha=0.8, size=10) +
  geom_segment(aes(x=as.factor(Date), xend=as.factor(Date), y=0, yend=freq), size=1, color='black')+
  #  scale_x_discrete(breaks=pretty_breaks()) +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  theme(legend.position = c(.5, .75)) +
  theme(axis.text.x=element_text(size=12, family='serif')) +
  theme(legend.text=element_text(size=12, family='serif')) +
  guides(fill= guide_legend(size=5), 
         size = guide_legend(guide = 'none')) +
  labs( x='Date', y='Proportion of catch by species',
        title='Lake Superior Grand Island Winter Ciscoe Collections in 1.5 inch Mesh',
        subtitle='Collections made November-January, 2017-2021',
        caption=ann_data_access) 

#+ facet_wrap(~COMMON_NAME, scales="free_y")

ggsave(here('Plots and Tables/Catch.by.DateSpecies.1.5inch.png'), dpi = 300, width = 35, height = 16, units = "cm")



#############################################################################################
## GSI for Kiyi
#############################################################################################
###Females
kiyi.gsi <- ciscoes.all %>%
  select(Date, SPECIES, Length_mm, Weight_g, GonadWeight_g, Sex, Maturity) %>%
  subset(SPECIES == 206 & Sex == 'female') %>%
  subset(Sex != 'unknown') %>%
  subset(Maturity != 'spent') %>%
  mutate(GSI = (GonadWeight_g / Weight_g) * 100, jday = yday(Date), week = week(Date)) %>%
  drop_na() %>%
  group_by(week) %>%
  summarise(GSI.n = n(), GSIweek.median = median(GSI), GSIweek.mean = mean(GSI), GSIweek.se = std.error(GSI)) %>%
  mutate(Dorder = case_when(
    week < 15 ~ week+53,
    week > 15 ~ week)) 

ggplot(kiyi.gsi, aes(x=Dorder, y=GSIweek.mean)) +
  geom_point(shape=21, fill = 'deepskyblue', size=10) +
  geom_errorbar(aes(x = Dorder, ymin=GSIweek.mean-GSIweek.se, ymax=GSIweek.mean+GSIweek.se), width=.5) + 
  geom_text(data = kiyi.gsi, aes(x=Dorder, y=GSIweek.mean, label = GSI.n, vjust = -1.4, hjust = 0.5), size=6, family='serif') +
#  scale_x_date(breaks = "1 week") +
  scale_y_continuous(breaks=pretty_breaks()) +
  ylim(0,20) +
  theme_bw() +
  plot_theme+
  labs( x='Week', y='Mean weekly gonadosomatic index',
        title='Lake Superior Kiyi Female Maturation',
        subtitle='Collections made 2017-2021',
        caption=ann_data_access) 

ggsave(here('Plots and Tables/Kiyi.Female.Maturation.png'), dpi = 300, width = 35, height = 16, units = "cm")



##GSI by week and year
kiyi.gsi <- ciscoes.all %>%
  select(Date, SPECIES, Length_mm, Weight_g, GonadWeight_g, Sex, Maturity) %>%
  subset(SPECIES == 206 & Sex == 'female') %>%
  subset(Sex != 'unknown') %>%
  mutate(GSI = (GonadWeight_g / Weight_g) * 100, year = year(Date), jday = yday(Date), month = month(Date), week = week(Date)) %>%
  mutate(winter.year = case_when(
    month >10 ~ year + 1,
    month <=10 ~ year)) %>%
  drop_na() %>%
  group_by(winter.year, week) %>%
  summarise(GSI.n = n(), GSIweek.median = median(GSI), GSIweek.mean = mean(GSI), GSIweek.se = std.error(GSI)) %>%
  mutate(Dorder = case_when(
    week < 15 ~ week+53,
    week > 15 ~ week)) 

ggplot(kiyi.gsi, aes(x=Dorder, y=GSIweek.mean, fill=as.factor(winter.year))) +
  geom_jitter(shape=21, size=10) +
  geom_line() + 
#  geom_errorbar(aes(x = Dorder, ymin=GSIweek.mean-GSIweek.se, ymax=GSIweek.mean+GSIweek.se), width=.5) + 
#  geom_text(data = kiyi.gsi, aes(x=Dorder, y=GSIweek.mean, label = GSI.n, vjust = -1.4, hjust = 0.5), size=6, family='serif') +
  #scale_x_date(breaks = "1 week") +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank()) +
  theme(legend.position = c(.5, .75)) +
  theme(axis.text.x=element_text(size=12, family='serif')) +
  theme(legend.text=element_text(size=12, family='serif')) +
  guides(fill= guide_legend(size=5), 
         size = guide_legend(guide = 'none')) +
  labs( x='Week', y='Mean weekly gonadosomatic index',
        title='Lake Superior Kiyi Female Maturation',
        subtitle='Collections made 2017-2021',
        caption=ann_data_access) 

ggsave(here('Plots and Tables/Kiyi.Female.Maturation.Week.Year.png'), dpi = 300, width = 35, height = 16, units = "cm")

ggplot(kiyi.gsi, aes(x=Dorder, y=GSIweek.mean, fill=as.factor(winter.year))) +
  geom_jitter(shape=21, size=10) +
  #  geom_errorbar(aes(x = Dorder, ymin=GSIweek.mean-GSIweek.se, ymax=GSIweek.mean+GSIweek.se), width=.5) + 
  #  geom_text(data = kiyi.gsi, aes(x=Dorder, y=GSIweek.mean, label = GSI.n, vjust = -1.4, hjust = 0.5), size=6, family='serif') +
  #scale_x_date(breaks = "1 week") +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank()) +
  theme(legend.position = c(.5, .75)) +
  theme(axis.text.x=element_text(size=12, family='serif')) +
  theme(legend.text=element_text(size=12, family='serif')) +
  guides(fill= guide_legend(size=5), 
         size = guide_legend(guide = 'none')) +
  labs( x='Week', y='Mean weekly gonadosomatic index',
        title='Lake Superior Kiyi Female Maturation',
        subtitle='Collections made 2017-2021',
        caption=ann_data_access) 

ggsave(here('Plots and Tables/Kiyi.Female.Maturation.Year.png'), dpi = 300, width = 35, height = 16, units = "cm")


###Males & Females
kiyi.gsi <- ciscoes.all %>%
  select(Date, SPECIES, Length_mm, Weight_g, GonadWeight_g, Sex, Maturity) %>%
  subset(SPECIES == 206) %>%
  subset(Sex != 'unknown') %>%
  mutate(GSI = (GonadWeight_g / Weight_g) * 100, jday = yday(Date), week = week(Date)) %>%
  drop_na() %>%
  group_by(week, Sex) %>%
  summarise(GSI.n = n(), GSIweek.mean = mean(GSI), GSIweek.se = std.error(GSI)) %>%
  mutate(Dorder = case_when(
    week < 15 ~ week+53,
    week > 15 ~ week)) 


ggplot(kiyi.gsi, aes(x=Dorder, y=GSIweek.mean)) +
  geom_point(aes(fill = Sex), shape=21, size=10, alpha=0.4) +
  geom_errorbar(aes(x = Dorder, ymin=GSIweek.mean-GSIweek.se, ymax=GSIweek.mean+GSIweek.se), width=.5) + 
  geom_text(data = kiyi.gsi, aes(x=Dorder, y=GSIweek.mean, label = GSI.n, vjust = -1.4, hjust = 0.5), size=6, family='serif') +
  #scale_x_date(breaks = "1 week") +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank()) +
#  theme(legend.position = "none") +
  theme(legend.position = c(.15, .9)) +
  #  theme(legend.text=element_text(size=12, family='serif')) +
  guides(fill= guide_legend(size=5), 
         size = guide_legend(guide = 'none')) +
  labs( x='Week', y='Mean weekly gonadosomatic index',
        title='Lake Superior Kiyi Maturation',
        subtitle='Collections made 2017-2021',
        caption=ann_data_access) 


ggsave(here('Plots and Tables/Kiyi.Maturation.png'), dpi = 300, width = 35, height = 16, units = "cm")


###Stacked bar maturity class for kiyi
kiyi.maturity <- ciscoes.all %>%
  select(Date, SPECIES, Sex, Maturity) %>%
  subset(SPECIES == 206 & Sex == 'female') %>%
  drop_na() %>%
  mutate(Mature.class = case_when(
    Maturity == 'immature' ~ 'immature',
    Maturity == 'mature' ~ 'developing',
    Maturity == 'developing' ~ 'developing',
    Maturity == 'ripe' ~ 'ripe',
    Maturity == 'running' ~ 'running',
    Maturity == 'spent' ~ 'spent')) %>%
  filter(Mature.class != 'immature') %>%
  group_by(Date, Mature.class) %>%
  summarise(Maturity.n = n()) %>%
  mutate(day = day(Date), month = month(Date), week = week(Date)) %>%
  mutate(month = lubridate::month(Date, label = TRUE, abbr = TRUE)) %>%
  unite("Date1", month, day, sep=" ") %>%
  mutate(Dorder = case_when(
    week < 15 ~ week+53,
    week > 15 ~ week)) %>%
  mutate(winter.year = case_when(
    month >10 ~ year + 1,
    month <=10 ~ year)) %>%
  mutate(winter.year2 = case_when(
    winter.year == 2017 ~ 'Winter 2017',
    winter.year == 2018 ~ 'Winter 2018',
    winter.year == 2019 ~ 'Winter 2019',
    winter.year == 2020 ~ 'Winter 2020',
    winter.year == 2021 ~ 'Winter 2021')) 
  subset(Dorder >=40) %>%
  arrange(Date) 

kiyi.maturityb <- kiyi.maturity %>%
  distinct(Date, .keep_all = TRUE) %>%
  select(Date)

kiyi.maturityb$ID <- 1:nrow(kiyi.maturityb)

kiyi.maturityc <- kiyi.maturity %>%
  left_join(kiyi.maturityb) %>%
  subset(winter.year2 == "Winter 2020" | winter.year2 == "Winter 2021") %>%
  distinct(Date, Mature.class, .keep_all = TRUE)


##Kiyi maturation plot without titles for paper
ggplot(kiyi.maturityc, aes(fill = Mature.class, x=ID, y=Maturity.n)) +
  geom_bar(position="fill", stat="identity") +
  scale_y_continuous(breaks=pretty_breaks()) +
  scale_x_continuous(breaks=kiyi.maturityc$ID, labels=kiyi.maturityc$Date1) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(), 
        axis.text.x=element_text(size=16, family='serif'),
        legend.position = "bottom") +
  labs( x='Date', y='Proportion of collection') +
  facet_wrap(~ winter.year2, scales='free_x')

ggsave(here('Plots and Tables/Kiyi.Female.MaturationClass.png'), dpi = 300, width = 35, height = 16, units = "cm")

##Kiyi maturation plot with titles for talk
ggplot(kiyi.maturityc, aes(fill = Mature.class, x=ID, y=Maturity.n)) +
  geom_bar(position="fill", stat="identity") +
  scale_y_continuous(breaks=pretty_breaks()) +
  scale_x_continuous(breaks=kiyi.maturityc$ID, labels=kiyi.maturityc$Date1) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(), 
        axis.text.x=element_text(size=16, family='serif'),
        legend.position = "bottom") +
  labs( x='Date', y='Proportion of collection',
        title='Lake Superior Kiyi Female Maturation',
        subtitle='Collections made 2017-2021',
        caption=ann_data_access) +
  facet_wrap(~ winter.year2, scales='free_x')

ggsave(here('Plots and Tables/Kiyi.Female.MaturationClass.png'), dpi = 300, width = 35, height = 16, units = "cm")


###Bloater
###Stacked bar maturity class for kiyi
bloater.gsi <- ciscoes.all %>%
  select(Date, SPECIES, Sex, Maturity) %>%
  subset(SPECIES == 204 & Sex == 'female') %>%
  mutate(jday = yday(Date), week = week(Date)) %>%
  drop_na() %>%
  group_by(week, Maturity) %>%
  summarise(Maturity.n = n()) %>%
  mutate(Dorder = case_when(
    week < 15 ~ week+53,
    week > 15 ~ week)) %>%
  mutate(Mature.class = case_when(
    Maturity == 'immature' ~ 'immature',
    Maturity == 'mature' ~ 'developing',
    Maturity == 'developing' ~ 'developing',
    Maturity == 'ripe' ~ 'ripe',
    Maturity == 'running' ~ 'running',
    Maturity == 'spent' ~ 'spent')) %>%
  filter(Mature.class != 'immature') %>%
  mutate(weekdate = case_when(
    week == '48' ~ 'Nov 22',
    week == '49' ~ 'Dec 1' ,
    week == '50' ~ 'Dec 8' ,
    week == '51' ~ 'Dec 15' ,
    week == '52' ~ 'Dec 22' ,
    week == '53' ~ 'Dec 29' ,
    week == '54' ~ 'Jan 3' ,
    week == '55' ~ 'Jan 10' ,
    week == '56' ~ 'Jan 17' ,
    week == '57' ~ 'Jan 24')) %>%
  left_join(ciscoes.winter)

ggplot(bloater.gsi, aes(fill = Mature.class, x=Dorder, y=Maturity.n)) +
  geom_bar(position="fill", stat="identity") +
  #scale_x_date(breaks = "1 week") +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  theme(legend.position = c(.6, .8)) +
  #  theme(legend.text=element_text(size=12, family='serif')) +
  labs( x='Week', y='Proportion of collection',
        title='Lake Superior Kiyi Female Maturation',
        subtitle='Collections made 2017-2021',
        caption=ann_data_access) 


ggsave(here('Plots and Tables/Bloater.Female.MaturationClass.png'), dpi = 300, width = 35, height = 16, units = "cm")

bloater.gsi.2 <-bloater.gsi %>%
  subset(winter.year == 2020 | winter.year == 2021) 


ggplot(bloater.gsi.2, aes(fill = Mature.class, x=Dorder, y=Maturity.n)) +
  geom_bar(position="fill", stat="identity") +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank()) +
  theme(legend.position = "none") +
  theme(legend.position = "bottom") +
  #  theme(legend.text=element_text(size=12, family='serif')) +
  labs( x='Week', y='Proportion of collection',
        title='Lake Superior Bloater Female Maturation',
        #     subtitle='Collections made 2017-2021',
        caption=ann_data_access) +
  facet_wrap(~winter.year2)


ggsave(here('Plots and Tables/Bloater.Female.MaturationClassYears.png'), dpi = 300, width = 35, height = 16, units = "cm")



###Cisco
cisco.gsi <- ciscoes.all %>%
  select(Date, SPECIES, Length_mm, Weight_g, GonadWeight_g, Sex, Maturity) %>%
  subset(SPECIES == 202) %>%
  mutate(GSI = (GonadWeight_g / Weight_g) * 100, jday = yday(Date), week = week(Date)) %>%
  drop_na() %>%
  group_by(week, Sex) %>%
  summarise(GSI.n = n(), GSIweek.mean = mean(GSI), GSIweek.se = std.error(GSI)) %>%
  mutate(Dorder = case_when(
    week < 15 ~ week+53,
    week > 15 ~ week)) 


ggplot(cisco.gsi, aes(x=Dorder, y=GSIweek.mean)) +
  geom_point(aes(fill = Sex), shape=21, size=10, alpha=0.4) +
  geom_errorbar(aes(x = Dorder, ymin=GSIweek.mean-GSIweek.se, ymax=GSIweek.mean+GSIweek.se), width=.5) + 
  geom_text(data = cisco.gsi, aes(x=Dorder, y=GSIweek.mean, label = GSI.n, vjust = -1.4, hjust = 0.5), size=6, family='serif') +
  #scale_x_date(breaks = "1 week") +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  labs( x='Week', y='Mean weekly gonadosomatic index',
        title='Lake Superior Cisco Maturation',
        subtitle='Collections made November-January, 2017-2021',
        caption=ann_data_access) 


ggsave(here('Plots and Tables/Cisco.Maturation.png'), dpi = 300, width = 35, height = 16, units = "cm")



####################################################################################################
##Combine Lake Michigan and Grand Island Fish
####################################################################################################

allfish <- ciscoes.all %>%
  select(Sample, Date, week, Lat_DD, Long_DD, MidDepth_m, 
         GNetMesh_in, SPECIES, Sex, Maturity, Length_mm, COMMON_NAME) %>%
  bind_rows(fulmar.all) %>%
  mutate(Period = case_when(
    year(Date) < 2000 ~ 'Lake Michigan',
    year(Date) > 2000 ~ 'Lake Superior')) 

###Stacked bar maturity class
maturity <- allfish %>%
  select(Date, week, Period, SPECIES, Sex, Maturity) %>%
  subset(SPECIES == 206 & Sex == 'female') %>%
  drop_na() %>%
  mutate(Mature.class = case_when(
    Maturity == 'immature' ~ 'immature',
    Maturity == 'mature' ~ 'developing',
    Maturity == 'developing' ~ 'developing',
    Maturity == 'ripe' ~ 'ripe',
    Maturity == 'spent' ~ 'spent',
    Maturity == 'gravid' ~ 'gravid')) %>%
  filter(Mature.class != 'immature') %>%
  group_by(Period, week, Mature.class) %>%
  summarise(Maturity.n = n()) %>%
  mutate(Dorder = case_when(
    week < 15 ~ week+53,
    week > 15 ~ week)) 
  
  
ggplot(maturity, aes(fill = Mature.class, x=Dorder, y=Maturity.n)) +
  geom_bar(position="fill", stat="identity") +
  #scale_x_date(breaks = "1 week") +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(), 
        legend.position = "bottom") +
  #  theme(legend.text=element_text(size=12, family='serif')) +
  labs( x='Week', y='Proportion of collection',
        title='Lakes Michigan and Superior Female Kiyi Maturation',
        subtitle='Collections made in Lake Michigan 1930-32 and in Lake Superior 2017-2021',
        caption=ann_data_access) +
  facet_wrap(~Period)
  
ggsave(here('Plots and Tables/KiyiMaturation.ThenandNow.png'), dpi = 300, width = 35, height = 16, units = "cm")


##Sex ratios - Kiyi
sexratio <- allfish %>%
  group_by(Period, week, SPECIES, Sex) %>%
  summarise(fish = n()) %>%
  subset(SPECIES == 206) %>%
  filter(Sex != 'unknown') 
  
ggplot(sexratio, aes(fill = Sex, x=week, y=fish)) +
  geom_bar(position="fill", stat="identity") +
  #scale_x_date(breaks = "1 week") +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank()) +
  #theme(legend.position = "bottom") +
  theme(legend.position = c(.07, .8)) +
  #  theme(legend.text=element_text(size=12, family='serif')) +
  labs( x='Week', y='Proportion of collection',
        title='Lakes Michigan and Superior Kiyi Sex Ratio',
        subtitle='Collections made in Lake Michigan 1930-32 and in Lake Superior 2017-2021',
        caption=ann_data_access) +
  facet_wrap(~Period)

ggsave(here('Plots and Tables/Kiyi.SexRatios.ThenandNow.png'), dpi = 300, width = 35, height = 16, units = "cm")


##Sex ratios for Lake SUperior only - Kiyi
##sex ratio by week
sexratio2 <- ciscoes.all %>%
  group_by(week, SPECIES, Sex) %>%
  summarise(fish = n()) %>%
  subset(SPECIES == 206) %>%
  filter(Sex != 'unknown') %>%
  mutate(Dorder = case_when(
    week < 15 ~ week+53,
    week > 15 ~ week)) 

##sex ratio by sample
sexratio3 <- ciscoes.all %>%
  subset(SPECIES == 206) %>%
  group_by(Sample, Sex) %>%
  summarise(fishes = n()) %>%
  pivot_wider(names_from = Sex, values_from = fishes) %>%
  mutate(kiyi = female+male, propf = female/(female+male), propm = male/(female+male))  %>%
  left_join(effort) %>%
  select(Sample, month, week, Date, GNetMesh_in, kiyi, female, male, propf, propm) %>%
  mutate(Dorder = case_when(
    week < 15 ~ week+53,
    week > 15 ~ week)) %>%
  subset(week >40) %>%
  drop_na() %>%
  ungroup()

sexratio4 <- sexratio3 %>%
  summarize(mean.female=mean(propf), min.female=min(propf), 
            max.female=max(propf), median.female=median(propf),
            sd.female=sd(propf), n=n(), se.female= sd.female/sqrt(n))  

##sex ratio by sampling date
sexratio3 <- ciscoes.all %>%
  subset(SPECIES == 206) %>%
  group_by(Sample, Sex) %>%
  summarise(fishes = n()) %>%
  pivot_wider(names_from = Sex, values_from = fishes) %>%
  mutate(propf = female/(female+male), propm = male/(female+male))  %>%
  left_join(effort) %>%
  mutate(winter.year = case_when(
    month >10 ~ year + 1,
    month <=10 ~ year)) %>%
  mutate(winter.year2 = case_when(
    winter.year == 2017 ~ 'Winter 2017',
    winter.year == 2018 ~ 'Winter 2018',
    winter.year == 2019 ~ 'Winter 2019',
    winter.year == 2020 ~ 'Winter 2020',
    winter.year == 2021 ~ 'Winter 2021')) %>%
  mutate(Dorder = case_when(
    week < 15 ~ week+53,
    week > 15 ~ week)) %>%
  select(Sample, week, Dorder, winter.year2, Date, propf, propm) %>%
  drop_na() %>%
  subset(Dorder >=40) %>%
  mutate(day = day(Date), month = month(Date)) %>%
  mutate(month = lubridate::month(Date, label = TRUE, abbr = TRUE)) %>%
  unite("Date1", month, day, sep=" ") %>%
  arrange(Date) 


sexratio3b <- sexratio3 %>%
  group_by(Date) %>%
  summarise(y.mean = mean(propf), y.min = min(propf), y.max=max(propf))
  
sexratio3b$ID <- 1:nrow(sexratio3b)

sexratio3 <- sexratio3 %>%
  left_join(sexratio3b) 


ggplot(sexratio3, aes(x=ID, y = propf)) +
  geom_point(size=4) +
  geom_segment(aes(x=ID, xend=ID, y=y.min, yend=y.max), size=1, color='black')+
  geom_point(aes(x=ID, y=y.mean), size = 4, pch=2) +
  geom_line(aes(x=ID, y=y.mean), size = 1, linetype=2) +
  scale_y_continuous(breaks=pretty_breaks()) +
  scale_x_continuous(breaks=sexratio3$ID, labels=sexratio3$Date1) +
  geom_hline(yintercept=0.5) +
  theme_bw() +
  #scale_x_date(date_labels = "%b %d") +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(),
        axis.text.x=element_text(size=16, family='serif'),
        panel.spacing.x = unit(2.0, "lines")) +
  labs( x='Date', y='Female proportion of collection',
        title='Lake Superior Kiyi Sex Ratio',
        caption=ann_data_access) +
  facet_wrap(~ winter.year2, scales='free_x')

ggsave(here('Plots and Tables/Kiyi.SexRatios.BestYeta.png'), dpi = 300, width = 35, height = 16, units = "cm")

ggplot(sexratio3, aes(x=ID, y = propf)) +
  geom_point(size=4) +
  geom_segment(aes(x=ID, xend=ID, y=y.min, yend=y.max), size=1, color='black')+
  geom_point(aes(x=ID, y=y.mean), size = 4, pch=2) +
  geom_line(aes(x=ID, y=y.mean), size = 1, linetype=2) +
  scale_y_continuous(breaks=pretty_breaks()) +
  scale_x_continuous(breaks=sexratio3$ID, labels=sexratio3$Date1) +
  geom_hline(yintercept=0.5) +
  theme_bw() +
  #scale_x_date(date_labels = "%b %d") +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(),
        axis.text.x=element_text(size=18, family='serif'),
        panel.spacing.x = unit(2.0, "lines")) +
  labs( x='Date', y='Female proportion of collection') +
  facet_wrap(~ winter.year2, scales='free_x')

ggsave(here('Plots and Tables/Kiyi.SexRatios.BestYetb.png'), dpi = 300, width = 35, height = 16, units = "cm")


sexratio4 <- sexratio3 %>%
  drop_na() %>%
  subset(Dorder >=40) %>%
  group_by(winter.year2) %>%
  summarize(mean.female=mean(propf), min.female=min(propf), 
            max.female=max(propf), median.female=median(propf),
            sd.female=sd(propf), n=n(), se.female= sd.female/sqrt(n))  %>%
  ungroup() %>%
  
  

sexratio5 <- ciscoes.all %>%
  subset(SPECIES == 206) %>%
  group_by(Sample, Sex) %>%
  summarise(fish = n()) %>%
  left_join(effort)   %>%
  mutate(Dorder = case_when(
    week < 15 ~ week+53,
    week > 15 ~ week)) 



ggplot(sexratio5, aes(fill = Sex, x=Dorder, y=fish)) +
  geom_bar(position="fill", stat="identity") +
  #  scale_x_date() +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank()) +
  #theme(legend.position = "bottom") +
  theme(legend.position = c(.6, .9)) +
  #  theme(legend.text=element_text(size=12, family='serif')) +
  labs( x='Date', y='Proportion of collection',
        title='Lake Superior Kiyi Sex Ratio',
        subtitle='Collections made 2017-2021',
        caption=ann_data_access) 

ggsave(here('Plots and Tables/Kiyi.SexRatios.Date.Superior.png'), dpi = 300, width = 35, height = 16, units = "cm")


ggplot(sexratio2, aes(fill = Sex, x=Date, y=fish)) +
  geom_bar(position="fill", stat="identity") +
  #scale_x_date(breaks = "1 week") +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank()) +
  #theme(legend.position = "bottom") +
  theme(legend.position = c(.6, .9)) +
  #  theme(legend.text=element_text(size=12, family='serif')) +
  labs( x='Week', y='Proportion of collection',
        title='Lake Superior Kiyi Sex Ratio',
        subtitle='Collections made 2017-2021',
        caption=ann_data_access) 

ggsave(here('Plots and Tables/Kiyi.SexRatios.Superior.png'), dpi = 300, width = 35, height = 16, units = "cm")


##Sex ratios for Lake SUperior only - Kiyi in 2 " mesh
kiyi.2inch.sexratio2 <- ciscoes.all %>%
  subset(SPECIES == 206 & GNetMesh_in == 2) %>%
  group_by(week, SPECIES, Sex) %>%
  summarise(fish = n()) %>%
  filter(Sex != 'unknown') %>%
  mutate(Dorder = case_when(
    week < 15 ~ week+53,
    week > 15 ~ week)) 

ggplot(kiyi.2inch.sexratio2, aes(fill = Sex, x=Dorder, y=fish)) +
  geom_bar(position="fill", stat="identity") +
  #scale_x_date(breaks = "1 week") +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank()) +
  #theme(legend.position = "bottom") +
  theme(legend.position = c(.85, .9)) +
  #  theme(legend.text=element_text(size=12, family='serif')) +
  labs( x='Week', y='Proportion of collection',
        title='Lake Superior Kiyi Sex Ratio in 2" Mesh',
        subtitle='Collections made 2017-2021',
        caption=ann_data_access) 

ggsave(here('Plots and Tables/Kiyi.2inch.SexRatios.Superior.png'), dpi = 300, width = 35, height = 16, units = "cm")



#########################################################################################
##All ciscoes - by depth all meshes
allfish <- ciscoes.winter %>%
  select(Sample, Date, week, Lat_DD, Long_DD, MidDepth_m, 
         GNetMesh_in, SPECIES, Sex, Maturity, Length_mm, COMMON_NAME, SCIENTIFIC_NAME) %>%
  bind_rows(fulmar.all) %>%
  mutate(Period = case_when(
    year(Date) < 2000 ~ 'Lake Michigan',
    year(Date) > 2000 ~ 'Lake Superior')) 

Tcatch <- allfish %>%
  group_by(Period, SPECIES) %>%
  summarize(fish=n())

catch.depth <- allfish %>%
  group_by(Period, MidDepth_m, SPECIES) %>%
  summarize(catch=n()) %>%
  mutate(freq = catch / sum(catch)) %>%
  ungroup() %>%
  subset(SPECIES==206) %>%
  #subset(SPECIES==206 | 
  #         SPECIES == 204 | 
  #         SPECIES == 202 |  
  #         SPECIES == 207 |
  #         SPECIES == 208 |
  #         SPECIES == 210) %>%
  left_join(sci.names) %>%
  left_join(Tcatch) %>%
  unite("legend", COMMON_NAME,fish, sep=", ")

depth.sum <- allfish %>%
  group_by(Period, SPECIES) %>%
  summarise(depth.mean=mean(MidDepth_m)) %>%
  subset(SPECIES==206) %>%
  ungroup()

ggplot(catch.depth, aes(x=MidDepth_m, y = ..scaled.., weight=catch, group=Period, fill=Period)) + 
  geom_density(alpha=0.4) +
#  geom_vline(data=depth.sum, aes(xintercept=depth.mean, color= Period), size=1, show.legend = FALSE) +
  geom_vline(data=depth.sum, aes(xintercept=depth.mean), color = 'black', size=1, show.legend = FALSE) +  
  geom_segment(aes(x=min(fulmar.all$MidDepth_m), xend=max(fulmar.all$MidDepth_m), y=0.03, yend=0.03), size=1, color='black') +
  geom_segment(aes(x=min(ciscoes.winter$StartDepth_m), xend=max(ciscoes.winter$EndDepth_m), y=0.1, yend=0.1), size=1, color='black') +
  annotate(geom="text", x=70, y=0.07, label="Lake Michigan sampled depths", size=8, family='serif') +
  annotate(geom="text", x=170, y=0.13, label="Lake Superior sampled depths", size=8, family='serif') +
  plot_theme + 
  theme(legend.position=c(0.15,0.9)) +
  theme(legend.title = element_blank()) +
  scale_x_continuous(breaks=pretty_breaks()) +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_brewer(palette="Accent") +
  labs( x='Depth (m)', y='Relative density',
        title='Lakes Michigan and Superior Kiyi Depth Distribution',
        subtitle='Collections made in Lake Michigan 1930-32 and in Lake Superior 2017-2021',
        caption=ann_data_access) 


ggsave(here('Plots and Tables/KiyiDepths.ThenandNow.png'), dpi = 300, width = 35, height = 16, units = "cm")


#### All ciscoes

catch.depth <- allfish %>%
  group_by(Period, MidDepth_m, SPECIES) %>%
  summarize(catch=n()) %>%
  mutate(freq = catch / sum(catch)) %>%
  ungroup() %>%
  subset(SPECIES==206 | 
           SPECIES == 204 | 
           SPECIES == 202 |  
           SPECIES == 207 |
           SPECIES == 208 |
           SPECIES == 210) %>%
  left_join(sci.names) %>%
  left_join(Tcatch) 

## unite("legend", COMMON_NAME,fish, sep=", ")

depth.sum <- allfish %>%
  group_by(Period, SPECIES) %>%
  summarise(depth.mean=mean(MidDepth_m)) %>%
  subset(SPECIES==206 | 
           SPECIES == 204 | 
           SPECIES == 202 |  
           SPECIES == 207 |
           SPECIES == 208 |
           SPECIES == 210) %>%
    ungroup()  %>%
  left_join(sci.names) 
  

ggplot(catch.depth, aes(x=MidDepth_m, y = ..scaled.., weight=catch, group=Period, fill=Period)) + 
  geom_density(alpha=0.4) +
  geom_vline(data=depth.sum, aes(xintercept=depth.mean), color = 'black', size=1, show.legend = FALSE) +  
#  geom_segment(aes(x=min(fulmar.all$MidDepth_m), xend=max(fulmar.all$MidDepth_m), y=0.03, yend=0.03), size=1, color='black') +
#  geom_segment(aes(x=min(ciscoes.winter$StartDepth_m), xend=max(ciscoes.winter$EndDepth_m), y=0.1, yend=0.1), size=1, color='black') +
#  annotate(geom="text", x=70, y=0.07, label="Lake Michigan sampled depths", size=8, family='serif') +
#  annotate(geom="text", x=170, y=0.13, label="Lake Superior sampled depths", size=8, family='serif') +
  plot_theme + 
  theme(legend.position=c(0.2,-0.16), legend.direction = "horizontal") +
  
  theme(legend.title = element_blank()) +
  scale_x_continuous(breaks=pretty_breaks()) +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_brewer(palette="Accent") +
  labs( x='Depth (m)', y='Relative density',
        title='Lakes Michigan and Superior Ciscoe Depth Distributions',
        subtitle='Collections made in Lake Michigan 1930-32 and in Lake Superior 2017-2021',
        caption=ann_data_access) +
  facet_wrap(~COMMON_NAME)

ggsave(here('Plots and Tables/SpeciesDepths.ThenandNow.png'), dpi = 300, width = 35, height = 16, units = "cm")



####################################################################################################
##Combine Lake Michigan and Grand Island Fish for months October - January
####################################################################################################

##Sex ratios
allfish <- ciscoes.all %>%
  select(Sample, Date, week, Lat_DD, Long_DD, MidDepth_m, 
         GNetMesh_in, SPECIES, Sex, Maturity, Length_mm, COMMON_NAME) %>%
  bind_rows(fulmar.all) %>%
  subset(month(Date) >= 10) %>%
  mutate(Period = case_when(
    year(Date) < 2000 ~ 'Lake Michigan',
    year(Date) > 2000 ~ 'Lake Superior')) 

sexratio <- allfish %>%
  group_by(Period, week, SPECIES, Sex) %>%
  summarise(fish = n()) %>%
  subset(SPECIES == 206) %>%
  filter(Sex != 'unknown') 


ggplot(sexratio, aes(fill = Sex, x=week, y=fish)) +
  geom_bar(position="fill", stat="identity") +
  #scale_x_date(breaks = "1 week") +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank()) +
  #theme(legend.position = "bottom") +
  theme(legend.position = c(.4, .8)) +
  #  theme(legend.text=element_text(size=12, family='serif')) +
  labs( x='Week', y='Proportion of collection',
        title='Lakes Michigan and Superior Kiyi Sex Ratio',
        subtitle='Collections made in Lake Michigan Oct-Dec 1930-32 and in Lake Superior Nov-Jan 2017-2021',
        caption=ann_data_access) +
  facet_wrap(~Period)

ggsave(here('Plots and Tables/Kiyi.SexRatios.ThenandNowWinter.png'), dpi = 300, width = 35, height = 16, units = "cm")


#########################################################################################
##All ciscoes - by depth all meshes
allfish <- ciscoes.winter %>%
  select(Sample, Date, week, Lat_DD, Long_DD, MidDepth_m,
         GNetMesh_in, SPECIES, Sex, Maturity, Length_mm, COMMON_NAME) %>%
  bind_rows(fulmar.all) %>%
  subset(month(Date) >= 10) %>%
  mutate(Period = case_when(
    year(Date) < 2000 ~ 'Lake Michigan',
    year(Date) > 2000 ~ 'Lake Superior')) 

Tcatch <- allfish %>%
  group_by(Period, SPECIES) %>%
  summarize(fish=n())

catch.depth <- allfish %>%
  group_by(Period, MidDepth_m, SPECIES) %>%
  summarize(catch=n()) %>%
  mutate(freq = catch / sum(catch)) %>%
  ungroup() %>%
  subset(SPECIES==206) %>%
  #subset(SPECIES==206 | 
  #         SPECIES == 204 | 
  #         SPECIES == 202 |  
  #         SPECIES == 207 |
  #         SPECIES == 208 |
  #         SPECIES == 210) %>%
  left_join(sci.names) %>%
  left_join(Tcatch) %>%
  unite("legend", COMMON_NAME,fish, sep=", ")

depth.sum <- allfish %>%
  group_by(Period, SPECIES) %>%
  summarise(depth.mean=mean(MidDepth_m)) %>%
  subset(SPECIES==206) %>%
  ungroup()

ggplot(catch.depth, aes(x=MidDepth_m, y = ..scaled.., weight=catch, group=Period, fill=Period)) + 
  geom_density(alpha=0.4) +
  #  geom_vline(data=depth.sum, aes(xintercept=depth.mean, color= Period), size=1, show.legend = FALSE) +
  geom_vline(data=depth.sum, aes(xintercept=depth.mean), color = 'black', size=1, show.legend = FALSE) +  
  geom_segment(aes(x=min(fulmar.all$MidDepth_m), xend=max(fulmar.all$MidDepth_m), y=0.03, yend=0.03), size=1, color='black') +
  geom_segment(aes(x=min(ciscoes.winter$StartDepth_m), xend=max(ciscoes.winter$EndDepth_m), y=0.1, yend=0.1), size=1, color='black') +
  annotate(geom="text", x=70, y=0.07, label="Lake Michigan sampled depths", size=8, family='serif') +
  annotate(geom="text", x=170, y=0.13, label="Lake Superior sampled depths", size=8, family='serif') +
  plot_theme + 
  theme(legend.position=c(0.15,0.9)) +
  theme(legend.title = element_blank()) +
  scale_x_continuous(breaks=pretty_breaks()) +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_brewer(palette="Accent") +
  labs( x='Depth (m)', y='Relative density',
        title='Lakes Michigan and Superior Kiyi Depth Distribution',
        subtitle='Collections made in Lake Michigan Oct-Dec 1930-32 and in Lake Superior Nov-Jan 2017-2021',
        caption=ann_data_access) 


ggsave(here('Plots and Tables/KiyiDepths.ThenandNowWinter.png'), dpi = 300, width = 35, height = 16, units = "cm")


#### All ciscoes

catch.depth <- allfish %>%
  group_by(Period, MidDepth_m, SPECIES) %>%
  summarize(catch=n()) %>%
  mutate(freq = catch / sum(catch)) %>%
  ungroup() %>%
  subset(SPECIES==206 | 
           SPECIES == 204 | 
           SPECIES == 202 |  
           SPECIES == 207 |
           SPECIES == 208 |
           SPECIES == 210) %>%
  left_join(sci.names) %>%
  left_join(Tcatch) 

## unite("legend", COMMON_NAME,fish, sep=", ")

depth.sum <- allfish %>%
  group_by(Period, SPECIES) %>%
  summarise(depth.mean=mean(MidDepth_m)) %>%
  subset(SPECIES==206 | 
           SPECIES == 204 | 
           SPECIES == 202 |  
           SPECIES == 207 |
           SPECIES == 208 |
           SPECIES == 210) %>%
  ungroup()  %>%
  left_join(sci.names) 


ggplot(catch.depth, aes(x=MidDepth_m, y = ..scaled.., weight=catch, group=Period, fill=Period)) + 
  geom_density(alpha=0.4) +
  geom_vline(data=depth.sum, aes(xintercept=depth.mean), color = 'black', size=1, show.legend = FALSE) +  
  #  geom_segment(aes(x=min(fulmar.all$MidDepth_m), xend=max(fulmar.all$MidDepth_m), y=0.03, yend=0.03), size=1, color='black') +
  #  geom_segment(aes(x=min(ciscoes.winter$StartDepth_m), xend=max(ciscoes.winter$EndDepth_m), y=0.1, yend=0.1), size=1, color='black') +
  #  annotate(geom="text", x=70, y=0.07, label="Lake Michigan sampled depths", size=8, family='serif') +
  #  annotate(geom="text", x=170, y=0.13, label="Lake Superior sampled depths", size=8, family='serif') +
  plot_theme + 
  theme(legend.position=c(0.2,-0.16), legend.direction = "horizontal") +
  
  theme(legend.title = element_blank()) +
  scale_x_continuous(breaks=pretty_breaks()) +
  scale_y_continuous(expand=c(0,0)) +
  scale_fill_brewer(palette="Accent") +
  labs( x='Depth (m)', y='Relative density',
        title='Lakes Michigan and Superior Ciscoe Depth Distributions',
        subtitle='Collections made in Lake Michigan Oct-Dec 1930-32 and in Lake Superior Nov-Jan 2017-2021',
        caption=ann_data_access) +
  facet_wrap(~COMMON_NAME)

ggsave(here('Plots and Tables/SpeciesDepths.ThenandNowWinter.png'), dpi = 300, width = 35, height = 16, units = "cm")



##########################################################################################################
#Kiyi fecundity 
##########################################################################################################
library(ggpmisc)

f1 <-read_xlsx(here('Data','GICiscoes.xlsx'), sheet = 'KiyiEggCount')

f2 <-read_xlsx(here('Data','GICiscoes.xlsx'), sheet = 'Fish')

f3 <-read_xlsx(here('Data','GICiscoes.xlsx'), sheet = 'Effort') %>%
  mutate(year = year(Date), 
         jday = yday(Date), 
         week = week(Date))

fecundity <-read_xlsx(here('Data','GICiscoes.xlsx'), sheet = 'KiyiEggCount') %>%
  pivot_longer(7:11, names_to = "Replicate", values_to = "Eggs_g", values_drop_na = TRUE) %>%
  group_by(Fish_Number) %>%
  summarise(EggWt.mean = mean(Eggs_g), na.rm=TRUE)

KiyiEggs <- fecundity %>%
  left_join(f1) %>%
#  subset(OvFrozenWt_g >= 1) %>%
  mutate(Fresh = OvFreshWt_g/EggWt.mean * 100, na.rm = TRUE) %>%
  mutate(Frozen = OvFrozenWt_g/EggWt.mean * 100, na.rm = TRUE) %>%
  select(Fish_Number, Length_mm, Weight_g, Fresh, Frozen) %>%
  left_join(f2) %>%
  left_join(f3) %>% 
#  subset(SPECIES == 206  & Maturity == 'ripe') 
  
  subset(SPECIES == 206  & Maturity != 'developing' & Maturity != 'mature') 


fecundity = lm(Fresh ~ Length_mm, data=KiyiEggs)
summary(fecundity)

Kiyi.EggSum <- KiyiEggs %>%
  summarise(fish = n(), mean.eggs = mean(Fresh), median.eggs = median(Fresh), 
            min.eggs = min(Fresh), max.eggs = max(Fresh),
            min.length = min(Length_mm), max.length = max(Length_mm))

meaneggs <- mean(KiyiEggs$Fresh)
mean(KiyiEggs$Fresh)

my.formula <- y ~ x

ggplot(KiyiEggs, aes(x=Length_mm, y = Fresh)) + 
  stat_poly_eq(formula = my.formula, 
               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
               parse = TRUE) +         
  geom_jitter(fill = 'skyblue',  shape=21, size=8) +
##  geom_jitter(data = KiyiEggs, aes(x=Length_mm, y = Fresh, fill = as.factor(Maturity)),  shape=21, size=6) +
  geom_smooth(method=lm, size=2) +
  plot_theme + 
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank()) +
  theme(legend.position = c(.1, .85)) +
  theme(legend.text=element_text(size=24, family='serif')) +
  guides(fill= guide_legend(size=5), 
         size = guide_legend(guide = 'none')) +
    scale_x_continuous(breaks=pretty_breaks()) +
  scale_y_continuous(breaks=pretty_breaks()) +
  labs( x='Total length (mm)', y='Egg count',
        title='Lake Superior Kiyi Fecundity',
        subtitle='Fish collected December-January in 2019-2021 near Grand Island, Michigan',
        caption=ann_data_access) 

ggsave(here('Plots and Tables/KiyiFecundity.length.ripeA.png'), dpi = 300, width = 35, height = 16, units = "cm")

ggplot(KiyiEggs, aes(x=Length_mm, y = Fresh)) + 
#  stat_poly_eq(formula = my.formula, 
#               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
#               parse = TRUE) +         
  geom_jitter(fill = 'black',  shape=21, size=8) +
  ##  geom_jitter(data = KiyiEggs, aes(x=Length_mm, y = Fresh, fill = as.factor(Maturity)),  shape=21, size=6) +
  geom_smooth(method=lm, size=2, color = 'black') +
  plot_theme + 
  theme(legend.title = element_blank()) +
  theme(legend.position = c(.1, .85)) +
  theme(legend.text=element_text(size=24, family='serif')) +
  guides(fill= guide_legend(size=5), 
         size = guide_legend(guide = 'none')) +
  scale_x_continuous(breaks=pretty_breaks()) +
  scale_y_continuous(breaks=pretty_breaks()) +
  labs( x='Total length (mm)', y='Fecundity') 

ggsave(here('Plots and Tables/KiyiFecundity.length.ripeB.png'), dpi = 300, width = 35, height = 16, units = "cm")


##Plots by maturity class
KiyiEggs <- fecundity %>%
  left_join(f1) %>%
  mutate(Fresh = OvFreshWt_g/EggWt.mean * 100, na.rm = TRUE) %>%
  mutate(Frozen = OvFrozenWt_g/EggWt.mean * 100, na.rm = TRUE) %>%
  select(Fish_Number, Length_mm, Weight_g, Fresh, Frozen) %>%
  left_join(f2) %>%
  left_join(f3) %>% 
  subset(SPECIES == 206  & Maturity != 'developing' & Maturity != 'mature') 

ggplot(KiyiEggs, aes(x=Length_mm, y = Fresh)) + 
  #  geom_jitter(fill = 'skyblue',  shape=21, size=8) +
  geom_jitter(data = KiyiEggs, aes(x=Length_mm, y = Fresh, fill = as.factor(Maturity)),  shape=21, size=6) +
  geom_smooth(method=lm, size=2) +
  plot_theme + 
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank()) +
  theme(legend.position = c(.1, .85)) +
  theme(legend.text=element_text(size=24, family='serif')) +
  guides(fill= guide_legend(size=5), 
         size = guide_legend(guide = 'none')) +
  scale_x_continuous(breaks=pretty_breaks()) +
  scale_y_continuous(breaks=pretty_breaks()) +
  labs( x='Total length (mm)', y='Egg count',
        title='Lake Superior Kiyi Fecundity',
        subtitle='Fish collected December-January in 2019-2021 near Grand Island, Michigan',
        caption=ann_data_access) 

ggsave(here('Plots and Tables/KiyiFecundity.length.maturity.png'), dpi = 300, width = 35, height = 16, units = "cm")


ggplot(KiyiEggs, aes(x=Weight_g, y = Fresh)) + 
  #  geom_jitter(fill = 'skyblue',  shape=21, size=8) +
  geom_jitter(data = KiyiEggs, aes(x=Weight_g, y = Fresh, fill = as.factor(Maturity)),  shape=21, size=6) +
  geom_smooth(method=lm, size=2) +
  plot_theme + 
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank()) +
  theme(legend.position = c(.1, .85)) +
  theme(legend.text=element_text(size=24, family='serif')) +
  guides(fill= guide_legend(size=5), 
         size = guide_legend(guide = 'none')) +
  scale_x_continuous(breaks=pretty_breaks()) +
  scale_y_continuous(breaks=pretty_breaks()) +
  labs( x='Total weight (g)', y='Egg count',
        title='Lake Superior Kiyi Fecundity',
        subtitle='Fish collected December-January in 2019-2021 near Grand Island, Michigan',
        caption=ann_data_access) 

ggsave(here('Plots and Tables/KiyiFecundity.weight.maturity.png'), dpi = 300, width = 35, height = 16, units = "cm")

ggplot(KiyiEggs, aes(x=Weight_g, y = Fresh)) + 
  #  geom_jitter(fill = 'skyblue',  shape=21, size=8) +
  geom_jitter(data = KiyiEggs, aes(x=Weight_g, y = Fresh, fill = as.factor(Maturity)),  shape=21, size=6) +
  geom_smooth(method=lm, size=2) +
  plot_theme + 
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank()) +
  theme(legend.position = c(.1, .85)) +
  theme(legend.text=element_text(size=24, family='serif')) +
  guides(fill= guide_legend(size=5), 
         size = guide_legend(guide = 'none')) +
  scale_x_continuous(breaks=pretty_breaks()) +
  scale_y_continuous(breaks=pretty_breaks()) +
  labs( x='Total weight (g)', y='Egg count',
        title='Lake Superior Kiyi Fecundity',
        subtitle='Fish collected December-January in 2019-2021 near Grand Island, Michigan',
        caption=ann_data_access) 

ggsave(here('Plots and Tables/KiyiFecundity.weight.maturity.png'), dpi = 300, width = 35, height = 16, units = "cm")

KiyiEggs.2 <- fecundity %>%
  left_join(f1) %>%
  #  subset(OvFrozenWt_g >= 1) %>%
  mutate(Fresh = OvFreshWt_g/EggWt.mean * 100, na.rm = TRUE) %>%
  mutate(Frozen = OvFrozenWt_g/EggWt.mean * 100, na.rm = TRUE) %>%
  select(Fish_Number, Length_mm, Weight_g, Fresh, Frozen) %>%
  left_join(f2) %>%
  left_join(f3) %>% 
  subset(SPECIES == 206) 

ggplot(KiyiEggs, aes(x=Length_mm, y = Fresh)) + 
  #  geom_jitter(fill = 'skyblue',  shape=21, size=8) +
  geom_jitter(data = KiyiEggs, aes(x=Length_mm, y = Fresh, fill = as.factor(week)),  shape=21, size=6) +
  geom_smooth(method=lm, size=2) +
  plot_theme + 
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank()) +
  theme(legend.position = c(.1, .65)) +
  theme(legend.text=element_text(size=24, family='serif')) +
  guides(fill= guide_legend(size=5), 
         size = guide_legend(guide = 'none')) +
  scale_x_continuous(breaks=pretty_breaks()) +
  scale_y_continuous(breaks=pretty_breaks()) +
  labs( x='Total length (mm)', y='Egg count',
        title='Lake Superior Kiyi Fecundity',
        subtitle='Fish collected December-January in 2019-2021 near Grand Island, Michigan',
        caption=ann_data_access) 

ggsave(here('Plots and Tables/KiyiFecundity.week.png'), dpi = 300, width = 35, height = 16, units = "cm")


##########################################################################################################
#Kiyi egg diameter
##########################################################################################################
egg.fish<-read_xlsx(here('Data','GICiscoes.xlsx'), sheet = 'Fish') %>%
  select(Fish_Number, Specimen_Number, SPECIES, Length_mm, Weight_g, Sex, Maturity )

egg.data<-read_xlsx(here('Data','GICiscoes.xlsx'), sheet = 'Egg_Diameters') %>%
  subset(select=-c(Comments)) %>% 
  pivot_longer(cols = starts_with("D"), 
               names_to = "egg.rep",
               names_prefix = "D",
               values_to = "scale", 
               values_drop_na = TRUE
  ) %>%
  mutate(egg.diam = scale/Zoom)

egg.table <- egg.data %>%
  group_by(Specimen_Number) %>% 
  summarize(mean.egg.diam = mean(egg.diam), min.egg.diam = min(egg.diam), 
            max.egg.diam=max(egg.diam), median.egg.diam=median(egg.diam),
            sd.egg.diam=sd(egg.diam), n=n(), se.egg.diam= sd.egg.diam/sqrt(n)) %>%
  left_join(egg.fish)  %>%
  subset(n == 25)
  

egg.fish.sum <- egg.table %>%
#  group_by(Specimen_Number) %>% 
  summarise(min.fish = min(Length_mm), max.fish = max(Length_mm),
            mean.fish = mean(Length_mm), median.fish = median(Length_mm), 
            sd.fish=sd(Length_mm), n=n(), se.fish= sd.fish/sqrt(n))


egg.sumtable <- egg.data %>%
  summarize(mean.egg.diam = mean(egg.diam), min.egg.diam = min(egg.diam), 
            max.egg.diam=max(egg.diam), median.egg.diam=median(egg.diam),
            sd.egg.diam=sd(egg.diam), n=n(), se.egg.diam= sd.egg.diam/sqrt(n))


##Egg Diameter histogram - counts plot
ggplot(egg.data, aes(x=egg.diam)) +
  geom_histogram(closed="right", fill="gray70",
                 color="black", binwidth = 0.1, boundary=0) +
  geom_vline(aes(xintercept = mean(egg.diam)), col = 'black', size = 2, linetype = 1) +
  geom_vline(aes(xintercept = median(egg.diam)), col = 'black', size = 2, linetype = 2) +
  scale_y_continuous(name="Count")+ 
  scale_x_continuous(name="Egg diameter (mm)", seq(0,2.2,0.5)) +
  plot_theme + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(NA)) 
# labs(x='Mean egg diameter (mm)', y='Frequency',
#        title='Lake Superior Kiyi Egg Diamter',
#        subtitle='Fish collected December-January in 2019-2021 near Grand Island, Michigan',
#        caption=ann_data_access)
  
ggsave(here('Plots and Tables/egg.size.counts.png'), dpi = 300, width = 35, height = 16, units = "cm")


ggplot(egg.data, aes(x=egg.diam)) +
  geom_histogram(aes(y=..density..), closed="right", fill="skyblue",
                 color="black", binwidth = 0.1, boundary=0) +
  scale_y_continuous(name="Frequency")+ 
  scale_x_continuous(name="Egg diameter (mm)", seq(0,2.2,0.5)) +
  plot_theme + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(NA))+
  labs( x='Mean egg diameter (mm)', y='Frequency',
        title='Lake Superior Kiyi Egg Diamter',
        subtitle='Fish collected December-January in 2019-2021 near Grand Island, Michigan',
        caption=ann_data_access)

ggsave(here('Plots and Tables/egg.size.frequency.png'), dpi = 300, width = 35, height = 16, units = "cm")


ggplot(egg.table, aes(x=Length_mm, y=mean.egg.diam)) +
  geom_jitter(aes(colour=Maturity), size = 4) +
    geom_errorbar(aes(ymin=min.egg.diam, ymax=max.egg.diam), width=2)+
  xlab("Fish length (mm)")+
  ylab("Mean egg diameter (mm)")+
  scale_fill_brewer(palette="Accent") +
  plot_theme + 
  theme(panel.background = element_blank(), axis.line=element_line(color = "black"),legend.position="bottom")+
  theme(legend.text=element_text(size=20, family='serif')) +
  guides(fill= guide_legend(size=5), 
         size = guide_legend(guide = 'none')) +
  labs( x='Total length (mm)', y='Egg diameter (mm)',
        title='Lake Superior Kiyi Egg Diameter',
        subtitle='Fish collected December-January in 2019-2021 near Grand Island, Michigan',
        caption=ann_data_access) 

ggsave(here('Plots and Tables/egg.size.fishlength.png'), dpi = 300, width = 35, height = 16, units = "cm")



######################################################################
##New more data sex ratios
#################################################################################################################
##Sex ratios - Kiyi
##Sex ratios for Lake SUperior only - Kiyi
##sex ratio by week

newsex1 <- kiyi3



newsex1 <- ciscoes.all %>%
  group_by(week, SPECIES, Sex) %>%
  summarise(fish = n()) %>%
  subset(SPECIES == 206) %>%
  filter(Sex != 'unknown') %>%
  mutate(Dorder = case_when(
    week < 15 ~ week+53,
    week > 15 ~ week)) 

##Sex ratio summary by week
sexratio.week <- ciscoes.all %>%
  subset(SPECIES == 206 & Sex != 'unknown') %>%
  group_by(Sample, Sex) %>%
  summarise(fishes = n()) %>%
  pivot_wider(names_from = Sex, values_from = fishes) %>%
  replace(is.na(.), 0) %>%
  mutate(fishes = female+male, propf = female/(female+male), propm = male/(female+male))  %>%
  left_join(effort) %>%
  select(Sample, week, fishes, propf, propm) %>%
  drop_na() %>%
  group_by(week) %>%
  summarise(mean.female=mean(propf), min.female=min(propf), 
            max.female=max(propf), median.female=median(propf), 
            sd.female=sd(propf), n=n(), se.female= sd.female/sqrt(n))

##sex ratio by sample
sexratio3 <- ciscoes.all %>%
  subset(SPECIES == 206 & Sex != 'unknown') %>%
  group_by(Sample, Sex) %>%
  summarise(fishes = n()) %>%
  pivot_wider(names_from = Sex, values_from = fishes) %>%
  replace(is.na(.), 0) %>%
  mutate(fishes = female+male, propf = female/(female+male), propm = male/(female+male))  %>%
  left_join(effort) %>%
  select(Sample, month, week, Date, GNetMesh_in, fishes, female, male, propf, propm) %>%
  mutate(Dorder = case_when(
    week < 15 ~ week+53,
    week > 15 ~ week)) %>%
  subset(week >40) %>%
  #  drop_na() %>%
  ungroup()


sexratio4 <- sexratio3 %>%
  summarize(mean.female=mean(propf), min.female=min(propf), 
            max.female=max(propf), median.female=median(propf),
            sd.female=sd(propf), n=n(), se.female= sd.female/sqrt(n))  

##sex ratio by sampling date
sexratio3 <- ciscoes.all %>%
  subset(SPECIES == 206 & Sex != 'unknown') %>%
  group_by(Sample, Sex) %>%
  summarise(fishes = n()) %>%
  pivot_wider(names_from = Sex, values_from = fishes) %>%
  replace(is.na(.), 0) %>%
  mutate(fishes = female+male, propf = female/(female+male), propm = male/(female+male))  %>%
  left_join(effort) %>%
  mutate(winter.year = case_when(
    month >10 ~ year + 1,
    month <=10 ~ year)) %>%
  mutate(winter.year2 = case_when(
    winter.year == 2017 ~ 'Winter 2017',
    winter.year == 2018 ~ 'Winter 2018',
    winter.year == 2019 ~ 'Winter 2019',
    winter.year == 2020 ~ 'Winter 2020',
    winter.year == 2021 ~ 'Winter 2021')) %>%
  subset(winter.year2 != 'Winter 2017') %>%
  mutate(Dorder = case_when(
    week < 15 ~ week+53,
    week > 15 ~ week)) %>%
  select(Sample, week, Dorder, winter.year2, Date, fishes, propf, propm) %>%
  drop_na() %>%
  subset(Dorder >=40) %>%
  mutate(day = day(Date), month = month(Date)) %>%
  mutate(month = lubridate::month(Date, label = TRUE, abbr = TRUE)) %>%
  unite("Date1", month, day, sep=" ") %>%
  arrange(Date) 


sexratio3b <- sexratio3 %>%
  group_by(Date) %>%
  summarise(y.mean = mean(propf), y.min = min(propf), y.max=max(propf), Tfish = sum(fishes))

sexratio3b$ID <- 1:nrow(sexratio3b)

sexratio3 <- sexratio3 %>%
  left_join(sexratio3b) 


ggplot(sexratio3, aes(x=ID, y = propf)) +
  geom_point(size=4, color = "grey50") +
  geom_segment(aes(x=ID, xend=ID, y=y.min, yend=y.max), size=1, color='black')+
  geom_point(aes(x=ID, y=y.mean), size = 4, pch=2) +
  geom_line(aes(x=ID, y=y.mean), size = 1, linetype=2) +
  geom_text(data = sexratio3, aes(x=ID, y=y.mean, label = Tfish), nudge_y = 0.1, nudge_x = -0.15, size=6, family='serif') +  
  scale_y_continuous(breaks=pretty_breaks()) +
  scale_x_continuous(breaks=sexratio3$ID, labels=sexratio3$Date1) +
  geom_hline(yintercept=0.5) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(),
        axis.text.x=element_text(size=18, family='serif'),
        panel.spacing.x = unit(2.0, "lines")) +
  labs( x='Date', y='Female proportion of collection') +
  facet_wrap(~ winter.year2, scales='free_x')

ggsave(here('Plots and Tables/KiyiPaper/Kiyi.SexRatios.png'), dpi = 300, width = 35, height = 16, units = "cm")


sexratio4 <- sexratio3 %>%
  drop_na() %>%
  subset(Dorder >=40) %>%
  group_by(winter.year2) %>%
  summarize(mean.female=mean(propf), min.female=min(propf), 
            max.female=max(propf), median.female=median(propf),
            sd.female=sd(propf), n=n(), se.female= sd.female/sqrt(n))  %>%
  ungroup()



#Sex ratio for all samples  
kiyi.sex.events <- ciscoes.all %>%
  select(Date, SPECIES, Sex) %>%
  subset(SPECIES == 206 & Sex != 'unknown') %>%
  mutate(Date1 = 'year<-'(Date, 2019), week = week(Date1)) %>%
  distinct(Date1, .keep_all = TRUE ) %>%
  group_by(week) %>%
  summarise(lifts = n())



kiyi.sex.dates <- ciscoes.all %>%
  subset(SPECIES == 206 & Sex != 'unknown') %>%
  group_by(Date, Sex) %>%
  summarise(fishes = n()) %>%
  pivot_wider(names_from = Sex, values_from = fishes) %>%
  replace(is.na(.), 0) %>%
  mutate(propf = female/(female+male), propm = male/(female+male), fishes = female+male)  %>%
  mutate(Date1 = 'year<-'(Date, 2019), week = week(Date1)) %>%
  group_by(week) %>%
  summarise(Date = Date1, day = day(Date1), sexf.mean = mean(propf), sexf.max = max(propf), sexf.min = min(propf), sexf.n = sum(fishes), sexf.se = std.error(propf)) %>%
  mutate(month = lubridate::month(Date, label = TRUE, abbr = TRUE)) %>%
  unite("Date1", month, day, sep=" ") %>%
  distinct(week, .keep_all = TRUE) %>%
  mutate(Dorder = case_when(
    week < 15 ~ week+53,
    week > 15 ~ week)) %>%
  left_join(kiyi.sex.events) %>%
  ungroup()


sex1 <- ggplot(kiyi.sex.dates, aes(x = Dorder, y=sexf.mean)) +
  #  geom_pointrange(mapping = aes(ymin = sexf.min, ymax = sexf.max)) +
  geom_point(shape=21, fill = 'grey70', size=6) +
  geom_smooth(se=FALSE, colour = 'black') +
  geom_errorbar(aes(x = Dorder, ymin=sexf.mean-sexf.se, ymax=sexf.mean+sexf.se), width=.5) + 
  geom_text(data = kiyi.sex.dates, aes(x=Dorder, y=sexf.mean, label = sexf.n), nudge_y = -0.1, nudge_x = -0.2, size=6, family='serif') +  
  geom_text(data = kiyi.sex.dates, aes(x=Dorder, y=-0.1, label = lifts), size=6, family='serif') + 
  scale_x_continuous(breaks=kiyi.sex.dates$Dorder, labels=kiyi.sex.dates$Date1, guide = guide_axis(n.dodge = 3)) +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(), 
        axis.text.x=element_text(size=16, family='serif'),
        legend.position = "none") +
  labs( x='', y='Female proportion of collection', title = "a)") 

ggsave(here('Plots and Tables/KiyiPaper/Kiyi.Sex.AllDates.png'), dpi = 300, width = 35, height = 16, units = "cm")


sex2 <-  ggplot(sexratio3, aes(x=ID, y = propf)) +
  geom_point(size=4, color = "grey50") +
  geom_segment(aes(x=ID, xend=ID, y=y.min, yend=y.max), size=1, color='black')+
  geom_point(aes(x=ID, y=y.mean), size = 4, pch=2) +
  geom_line(aes(x=ID, y=y.mean), size = 1, linetype=2) +
  geom_text(data = sexratio3, aes(x=ID, y=y.mean, label = Tfish), nudge_y = -0.2, nudge_x = 0.2, size=6, family='serif') +  
  scale_y_continuous(breaks=pretty_breaks()) +
  scale_x_continuous(breaks=sexratio3$ID, labels=sexratio3$Date1) +
  geom_hline(yintercept=0.5) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(),
        axis.text.x=element_text(size=18, family='serif'),
        panel.spacing.x = unit(2.0, "lines")) +
  labs( x='Date', y='Female proportion of collection', title = "b)") +
  facet_wrap(~ winter.year2, scales='free_x')

##Dual plot for paper
sex1 / sex2
ggsave(here('Plots and Tables/KiyiPaper/Kiyi.SexRatio.Paper.png'), dpi = 300, width = 35, height = 35, units = "cm")



