
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

GI.catch.years <- ciscoes.winter %>%
  subset(SPECIES == 202 |
           SPECIES == 204 |
           SPECIES == 206 |
           SPECIES == 207 |
           SPECIES == 208 |
           SPECIES == 210) %>%
  mutate(winter.year = case_when(
    month >10 ~ year + 1,
    month <=10 ~ year)) %>%
  mutate(winter.year2 = case_when(
    winter.year == 2017 ~ 'Winter 2017',
    winter.year == 2018 ~ 'Winter 2018',
    winter.year == 2019 ~ 'Winter 2019',
    winter.year == 2020 ~ 'Winter 2020',
    winter.year == 2021 ~ 'Winter 2021')) %>%
  group_by(SPECIES, winter.year2) %>%
  summarize(fish=n())

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
 
ggsave(here('Plots and Tables/KiyiPaper/Kiyi.CatchRate.by.DepthBin.BoxPlot.png'), dpi = 300, width = 35, height = 16, units = "cm")





##########################################################################################
##Annual summaries of effort 
#####################################################################################################
###########################################################################################################
###Effort number of sets by mesh
###Depth of sets for across all dates
gnet.sets <- effort %>%
  select(Sample, Date, Location, Method, GNetGang, GNetMesh_in, MidDepth_m) %>%
  mutate(day = day(Date), month = month(Date), week = week(Date), year = year(Date)) %>%
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
    winter.year == 2021 ~ 'Winter 2021')) %>%
  mutate(month = lubridate::month(Date, label = TRUE, abbr = TRUE)) %>%
  unite("Date1", month, day, sep=" ") %>%
  subset(Dorder >=40) %>%
  arrange(Date) 

gnet.setsb <- gnet.sets %>%
  distinct(Date, .keep_all = TRUE) %>%
  select(Date)

gnet.setsb$ID <- 1:nrow(gnet.setsb)

gnet.setsc <- gnet.sets %>%
  left_join(gnet.setsb) %>%
 # subset(winter.year2 == "Winter 2020" | winter.year2 == "Winter 2021") %>%
  distinct(Date, Location, GNetMesh_in, .keep_all = TRUE) %>%
  mutate(ID2 = case_when(
    ID == 1 ~ 1.5,
  #  ID == 2 ~ 0,
    ID == 3 ~ 3.5,
    ID == 4 ~ 4.5,
   # ID == 5 ~ 0,
    ID == 6 ~ 6.5,
    ID == 7 ~ 7.5,
    ID == 8 ~ 8.5,
    ID == 9 ~ 9.5,
  #  ID == 10 ~ 0,
    ID == 11 ~ 11.5,
    ID == 12 ~ 12.5,
    ID == 13 ~ 13.5,
    ID == 14 ~ 14.5))


ggplot(gnet.setsc, aes(x=ID, ymin=0, ymax=MidDepth_m, group=factor(Location), fill=as.factor(GNetMesh_in))) +
  geom_jitter(aes(x=ID, y=MidDepth_m, fill= as.factor(GNetMesh_in)), shape=21, alpha=0.8, size=6, position=position_dodge(width = 0.8)) +
  geom_linerange(position=position_dodge(width = 0.8), alpha=0.8, size=1) +
  scale_y_reverse(breaks=pretty_breaks(), lim=c(250,0)) +
  scale_x_continuous(breaks=gnet.setsc$ID+0.35, labels=gnet.setsc$Date1) +
  geom_vline(aes(xintercept = ID2), linetype = 1, color = "red", size = 1) + 
  theme_bw() +
  plot_theme +
  scale_fill_brewer(palette="Accent") +
  theme(axis.text.x=element_text(size=14, family='serif',angle = 0, vjust = 1, hjust=1), 
        legend.background = element_blank(), 
        axis.ticks.x=element_blank(),
      #  legend.box.background = element_rect(colour = "black"), 
        legend.text=element_text(size=20, family='serif'), 
        legend.title=element_text(size=14, family='serif'),
        panel.spacing = unit(1, "lines"),
        legend.position = "bottom") +
  guides(fill= guide_legend(title = "Mesh (inches)", direction="horizontal", title.position = "left")) + 
  labs(x='Date', y='Depth (m)') +
  facet_wrap(~ winter.year2, scales='free_x', nrow = 1)

ggsave(here('Plots and Tables/KiyiPaper/SetsByDepth.png'), dpi = 300, width = 35, height = 16, units = "cm")


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



mapa <- ggplot(subset(ciscoes.map, Method == "Gillnet"), aes(x=Long_DD, y = Lat_DD)) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5) +
  geom_point(data=subset(ciscoes.map, Method == "Gillnet"), 
             mapping=aes(Long_DD, Lat_DD, fill=MidDepth_m), shape=21, size=3) +
  scale_fill_gradient(low = "yellow", high = "blue", na.value = NA, breaks=pretty_breaks(4), 
                      name = "Depth (m)") +
  scale_y_continuous(name='Latitude', breaks=pretty_breaks(), limits=c(46.35,47)) +
  scale_x_continuous(name='Longitude', breaks=pretty_breaks(), limits=c(-86.8,-86.4)) +
  theme_bw() +
  map_theme +
  geom_label(label="Grand Island", 
             x=-86.62,
             y=46.38,
             label.size = NA,
             size=5, 
             family='serif', 
             color = "black") +
  geom_label(label="Depth (m)", 
             x=-86.65,
             y=47,
             label.size = NA,
             size=5, 
             family='serif', 
             color = "black") +
  theme(axis.text=element_text(size=10, family='serif'),
        axis.title=element_blank(), 
        #legend.title = element_text(size=8, family='serif'), 
        legend.title = element_blank(), 
        legend.text = element_text(size=12, family='serif'),
        legend.direction = "horizontal", 
        legend.position = c(.5, .82))  +
  guides(colour = guide_legend(title.position = "bottom"))

mapb <- ggplot(ciscoes.map, aes(x=Long_DD, y = Lat_DD)) +
  geom_jitter(aes(fill = Season), shape=21, size=5, alpha=0.8) +
  scale_fill_brewer(palette="Accent") +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.7)+
  scale_y_continuous(name='Latitude', breaks=pretty_breaks()) +
  scale_x_continuous(name='Longitude', breaks=pretty_breaks()) +
  theme_bw() +
  map_theme + 
  geom_rect(data=ls_poly.fort, 
            aes(xmin=-86.85, ymin=46.3, xmax=-86.38, ymax=46.85),
                size=0.5, color="black", fill = NA, inherit.aes=FALSE) +
  theme(axis.text=element_text(size=20, family='serif'), 
        axis.title=element_text(size=20, family='serif'), 
        legend.title = element_text(size=20, family='serif'), 
        legend.text = element_text(size=20, family='serif'),
        legend.position = c(.85, .8)) +
  geom_label(label="Grand Island", 
             x=-86.65,
             y=46.36,
             label.size = NA,
             size=5, 
             family='serif', 
             color = "black") 
  
papermap <- mapb + inset_element(mapa, left = 0.01, bottom = 0.48, right = 0.21, top = 0.99)

ggsave(here('Plots and Tables/KiyiPaper/Kiyi_PaperMap.png'))


#############################################################################################
## GSI for Kiyi
#############################################################################################
###Females and males - all sizes
##Kiyi gsi plot without titles for paper
##mid-week dates by week - need for plotting a pretty plot 
##changed all years to 2019 to faciliate calculating weekly averages

kiyi.gsi.events <- ciscoes.all %>%
  mutate(GSI = (GonadWeight_g / Weight_g) * 100) %>%
  drop_na(GSI) %>%
  select(Date, SPECIES, Sex, GSI) %>%
  subset(SPECIES == 206 & Sex != 'unknown') %>%
  mutate(Date1 = 'year<-'(Date, 2019), week = week(Date1)) %>%
  distinct(Date1, .keep_all = TRUE ) %>%
  group_by(week) %>%
  summarise(lifts = n())

kiyi.gsi.dates <- ciscoes.all %>%
  mutate(GSI = (GonadWeight_g / Weight_g) * 100) %>%
  drop_na(GSI) %>%
  select(Date, SPECIES, Sex, GSI) %>%
  subset(SPECIES == 206 & Sex != 'unknown') %>%
  mutate(Date1 = 'year<-'(Date, 2019), week = week(Date1)) %>%
  group_by(week, Sex) %>%
  summarise(Date = Date1, day = day(Date1), GSI.mean = mean(GSI),GSI.max = max(GSI), GSI.min = min(GSI), GSI.n = n(), GSI.se = std.error(GSI)) %>%
  mutate(month = lubridate::month(Date, label = TRUE, abbr = TRUE)) %>%
  unite("Date1", month, day, sep=" ") %>%
  distinct(week, .keep_all = TRUE) %>%
  mutate(Dorder = case_when(
    week < 15 ~ week+53,
    week > 15 ~ week)) %>%
  left_join(kiyi.gsi.events) %>%
  mutate(Date2 = case_when(
    Sex == 'female' ~ Date1,
    Sex == 'male'  ~ ' ')) %>% 
  ungroup()


p1 <- ggplot(kiyi.gsi.dates, aes(x = Dorder, y=GSI.mean, fill = Sex)) +
#  geom_pointrange(mapping = aes(ymin = GSI.min, ymax = GSI.max)) +
  geom_point(shape=21, fill = 'grey70', size=6) +
  geom_smooth(se=FALSE, colour = 'black') +
  geom_errorbar(aes(x = Dorder, ymin=GSI.mean-GSI.se, ymax=GSI.mean+GSI.se), width=.5) + 
  geom_text(data = kiyi.gsi.dates, aes(x=Dorder, y=GSI.mean, label = GSI.n), nudge_y = 0.6, nudge_x = -0.5, size=6, family='serif') +  
  geom_text(data = kiyi.gsi.dates, aes(x=Dorder, y=-0.1, label = lifts), size=6, family='serif') + 
  annotate("text", x = 40, y = 6.1, label = "female", size=6, family='serif') +
  annotate("text", x = 40, y = 1.1, label = "male", size=6, family='serif') +
  scale_x_continuous(breaks=kiyi.gsi.dates$Dorder, labels=kiyi.gsi.dates$Date2, guide = guide_axis(n.dodge = 3)) +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(), 
        axis.text.x=element_text(size=16, family='serif'),
        legend.position = "none") +
  labs( x='', y='Gonadosomatic index', title = "a)") 
ggsave(here('Plots and Tables/KiyiPaper/Kiyi.GSI.Female.Male.All.png'), dpi = 300, width = 35, height = 16, units = "cm")



###Females - all sizes
##Kiyi gsi plot without titles for paper
###Females - all Dates, all years by week
##mid-week dates by week - need for plotting a pretty plot 
##changed all years to 2019 to faciliate calculating weekly averages

kiyi.gsi.events <- ciscoes.all %>%
  mutate(GSI = (GonadWeight_g / Weight_g) * 100) %>%
  drop_na(GSI) %>%
  select(Sample, Date, SPECIES, Sex, GSI) %>%
  subset(SPECIES == 206 & Sex == 'female') %>%
  mutate(Date1 = 'year<-'(Date, 2019), week = week(Date1)) %>%
  distinct(Sample, .keep_all = TRUE ) %>%
  group_by(week) %>%
  summarise(lifts = n())

kiyi.gsi.dates <- ciscoes.all %>%
  mutate(GSI = (GonadWeight_g / Weight_g) * 100) %>%
  drop_na(GSI) %>%
  select(Date, SPECIES, Sex, GSI) %>%
  subset(SPECIES == 206 & Sex == 'female') %>%
  mutate(Date1 = 'year<-'(Date, 2019), week = week(Date1)) %>%
  group_by(week) %>%
  summarise(Date = Date1, day = day(Date1), GSI.mean = mean(GSI),GSI.max = max(GSI), GSI.min = min(GSI), GSI.n = n(), GSI.se = std.error(GSI)) %>%
  mutate(month = lubridate::month(Date, label = TRUE, abbr = TRUE)) %>%
  unite("Date1", month, day, sep=" ") %>%
  distinct(week, .keep_all = TRUE) %>%
  mutate(Dorder = case_when(
    week < 15 ~ week+53,
    week > 15 ~ week)) %>%
  left_join(kiyi.gsi.events) %>%
  ungroup()


p3 <- ggplot(kiyi.gsi.dates, aes(x = Dorder, y=GSI.mean)) +
  geom_point(shape=21, fill = 'grey70', size=6) +
  geom_smooth(se=FALSE, colour = 'black') +
  geom_errorbar(aes(x = Dorder, ymin=GSI.mean-GSI.se, ymax=GSI.mean+GSI.se), width=.5) + 
  geom_text(data = kiyi.gsi.dates, aes(x=Dorder, y=GSI.mean, label = GSI.n), nudge_y = 0.6, nudge_x = -0.5, size=6, family='serif') +  
  geom_text(data = kiyi.gsi.dates, aes(x=Dorder, y=0.5, label = lifts), size=6, family='serif') + 
  scale_x_continuous(breaks=kiyi.gsi.dates$Dorder, labels=kiyi.gsi.dates$Date1, guide = guide_axis(n.dodge = 3)) +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(), 
        axis.text.x=element_text(size=16, family='serif'),
        legend.position = "bottom") +
  labs( x='', y='Gonadosomatic index') 
ggsave(here('Plots and Tables/KiyiPaper/Kiyi.Female.GSI.All.png'), dpi = 300, width = 35, height = 16, units = "cm")



##Kiyi female GSI by year
kiyi.gsi <- ciscoes.all %>%
  select(Date, SPECIES, Sex, Maturity, Length_mm, Weight_g, GonadWeight_g) %>%
  subset(SPECIES == 206 & Sex == 'female') %>%
  mutate(GSI = (GonadWeight_g / Weight_g) * 100) %>%
  drop_na(GSI) %>%
  group_by(Date) %>%
  summarise(GSI.n = n(), GSI.median = median(GSI), GSI.mean = mean(GSI), GSI.se = std.error(GSI), GSI.min = min(GSI), GSI.max = max(GSI)) %>%
  mutate(day = day(Date), month = month(Date), year = year(Date), week = week(Date)) %>%
  mutate(winter.year = case_when(
    month >10 ~ year + 1,
    month <=10 ~ year)) %>%
  mutate(winter.year2 = case_when(
    winter.year == 2017 ~ 'Winter 2017',
    winter.year == 2018 ~ 'Winter 2018',
    winter.year == 2019 ~ 'Winter 2019',
    winter.year == 2020 ~ 'Winter 2020',
    winter.year == 2021 ~ 'Winter 2021')) %>% 
  mutate(month = lubridate::month(Date, label = TRUE, abbr = TRUE)) %>%
  unite("Date1", month, day, sep=" ") %>%
  mutate(Dorder = case_when(
    week < 15 ~ week+53,
    week > 15 ~ week)) %>%
subset(Dorder >=40) %>%
  arrange(Date) 

kiyi.gsib <- kiyi.gsi %>%
  distinct(Date, .keep_all = TRUE) %>%
  select(Date)

kiyi.gsib$ID <- 1:nrow(kiyi.gsib)

kiyi.gsic <- kiyi.gsi %>%
  left_join(kiyi.gsib) %>%
#  subset(winter.year2 == "Winter 2020" | winter.year2 == "Winter 2021") %>%
  distinct(Date, .keep_all = TRUE)

##Kiyi gsi plot without titles for paper
p2 <- ggplot(kiyi.gsic, aes(x = ID, y=GSI.mean)) +
  #geom_pointrange(mapping = aes(ymin = GSI.min, ymax = GSI.max), size = 1) +
  geom_point(shape=21, fill = 'grey70', size=6) +
  geom_errorbar(aes(x = ID, ymin=GSI.mean-GSI.se, ymax=GSI.mean+GSI.se), width=.5, size = 1) + 
  geom_text(data = kiyi.gsic, aes(x=ID, y=GSI.mean, label = GSI.n), nudge_y = 2.2, nudge_x = -0.2, size=6, family='serif') +  
  scale_y_continuous(breaks=pretty_breaks()) +
  scale_x_continuous(breaks=kiyi.gsic$ID, labels=kiyi.gsic$Date1) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(), 
        axis.text.x=element_text(size=16, family='serif'),
        legend.position = "bottom") +
  labs( x='Date', y='Female gonadosomatic index',title = "b)") +
  facet_wrap(~ winter.year2, scales='free_x')

ggsave(here('Plots and Tables/KiyiPaper/Kiyi.Female.GSI.ByYear.png'), dpi = 300, width = 35, height = 16, units = "cm")


##Dual plot for paper
p1 / p2
ggsave(here('Plots and Tables/KiyiPaper/Kiyi.MaleFemale.GSI.png'), dpi = 300, width = 35, height = 35, units = "cm")



###Male - all sizes
##Kiyi gsi plot without titles for paper
###Females - all Dates, all years by week
##mid-week dates by week - need for plotting a pretty plot 
##changed all years to 2019 to faciliate calculating weekly averages

kiyi.gsi.events <- ciscoes.all %>%
  mutate(GSI = (GonadWeight_g / Weight_g) * 100) %>%
  drop_na(GSI) %>%
  select(Date, SPECIES, Sex, GSI) %>%
  subset(SPECIES == 206 & Sex == 'male') %>%
  mutate(Date1 = 'year<-'(Date, 2019), week = week(Date1)) %>%
  distinct(Date1, .keep_all = TRUE ) %>%
  group_by(week) %>%
  summarise(lifts = n())

kiyi.gsi.dates <- ciscoes.all %>%
  mutate(GSI = (GonadWeight_g / Weight_g) * 100) %>%
  drop_na(GSI) %>%
  select(Date, SPECIES, Sex, GSI) %>%
  subset(SPECIES == 206 & Sex == 'male') %>%
  mutate(Date1 = 'year<-'(Date, 2019), week = week(Date1)) %>%
  group_by(week) %>%
  summarise(Date = Date1, day = day(Date1), GSI.mean = mean(GSI),GSI.max = max(GSI), GSI.min = min(GSI), GSI.n = n(), GSI.se = std.error(GSI)) %>%
  mutate(month = lubridate::month(Date, label = TRUE, abbr = TRUE)) %>%
  unite("Date1", month, day, sep=" ") %>%
  distinct(week, .keep_all = TRUE) %>%
  mutate(Dorder = case_when(
    week < 15 ~ week+53,
    week > 15 ~ week)) %>%
  left_join(kiyi.gsi.events) %>%
  ungroup()


p4 <- ggplot(kiyi.gsi.dates, aes(x = Dorder, y=GSI.mean)) +
  geom_point(shape=21, fill = 'grey70', size=6) +
  geom_smooth(se=FALSE, colour = 'black') +
  geom_errorbar(aes(x = Dorder, ymin=GSI.mean-GSI.se, ymax=GSI.mean+GSI.se), width=.5) + 
  geom_text(data = kiyi.gsi.dates, aes(x=Dorder, y=GSI.mean, label = GSI.n), nudge_y = 0.6, nudge_x = -0.5, size=6, family='serif') +  
  geom_text(data = kiyi.gsi.dates, aes(x=Dorder, y=0.5, label = lifts), size=6, family='serif') + 
  scale_x_continuous(breaks=kiyi.gsi.dates$Dorder, labels=kiyi.gsi.dates$Date1, guide = guide_axis(n.dodge = 3)) +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(), 
        axis.text.x=element_text(size=16, family='serif'),
        legend.position = "bottom") +
  labs( x='', y='Gonadosomatic index') 
ggsave(here('Plots and Tables/KiyiPaper/Kiyi.Male.GSI.All.png'), dpi = 300, width = 35, height = 16, units = "cm")


##Kiyi GSI by year
kiyi.gsi <- ciscoes.all %>%
  select(Date, SPECIES, Sex, Maturity, Length_mm, Weight_g, GonadWeight_g) %>%
  subset(SPECIES == 206 & Sex == 'male') %>%
  mutate(GSI = (GonadWeight_g / Weight_g) * 100) %>%
  drop_na(GSI) %>%
  group_by(Date) %>%
  summarise(GSI.n = n(), GSI.median = median(GSI), GSI.mean = mean(GSI), GSI.se = std.error(GSI)) %>%
  mutate(day = day(Date), month = month(Date), year = year(Date), week = week(Date)) %>%
  mutate(winter.year = case_when(
    month >10 ~ year + 1,
    month <=10 ~ year)) %>%
  mutate(winter.year2 = case_when(
    winter.year == 2017 ~ 'Winter 2017',
    winter.year == 2018 ~ 'Winter 2018',
    winter.year == 2019 ~ 'Winter 2019',
    winter.year == 2020 ~ 'Winter 2020',
    winter.year == 2021 ~ 'Winter 2021')) %>% 
  mutate(month = lubridate::month(Date, label = TRUE, abbr = TRUE)) %>%
  unite("Date1", month, day, sep=" ") %>%
  mutate(Dorder = case_when(
    week < 15 ~ week+53,
    week > 15 ~ week)) %>%
  subset(Dorder >=40) %>%
  arrange(Date) 

kiyi.gsib <- kiyi.gsi %>%
  distinct(Date, .keep_all = TRUE) %>%
  select(Date)

kiyi.gsib$ID <- 1:nrow(kiyi.gsib)

kiyi.gsic <- kiyi.gsi %>%
  left_join(kiyi.gsib) %>%
  #  subset(winter.year2 == "Winter 2020" | winter.year2 == "Winter 2021") %>%
  distinct(Date, .keep_all = TRUE)

##Kiyi gsi plot without titles for paper
p5 <- ggplot(kiyi.gsic, aes(x = ID, y=GSI.mean)) +
  geom_point(shape=21, fill = 'grey70', size=6) +
  geom_errorbar(aes(x = ID, ymin=GSI.mean-GSI.se, ymax=GSI.mean+GSI.se), width=.5) + 
  geom_text(data = kiyi.gsic, aes(x=ID, y=GSI.mean, label = GSI.n), nudge_y = -1.5, nudge_x = -0.2, size=6, family='serif') +  
  scale_y_continuous(breaks=pretty_breaks()) +
  scale_x_continuous(breaks=kiyi.gsic$ID, labels=kiyi.gsic$Date1) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(), 
        axis.text.x=element_text(size=16, family='serif'),
        legend.position = "bottom") +
  labs( x='Date', y='Gonadosomatic index') +
  facet_wrap(~ winter.year2, scales='free_x')

ggsave(here('Plots and Tables/KiyiPaper/Kiyi.Male.GSI.ByYear.png'), dpi = 300, width = 35, height = 16, units = "cm")


##Dual plot for paper
p4 / p5
ggsave(here('Plots and Tables/KiyiPaper/Kiyi.Male.GSI.png'), dpi = 300, width = 35, height = 35, units = "cm")


#gsi.nice <- p3 + inset_element(p4, left = 0.01, bottom = 0.5, right = 0.80, top = 0.99) / p2

#ggsave(here('Plots and Tables/KiyiPaper/Kiyi.GSI.Male.Female.byYear.png'), dpi = 300, width = 35, height = 35, units = "cm")




################################################################################################
################################################################################################
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
  summarise(Maturity.n = n(), .groups = 'drop') %>%
  mutate(day = day(Date), month = month(Date), week = week(Date), year = year(Date)) %>%
  mutate(winter.year = case_when(
    month >10 ~ year + 1,
    month <=10 ~ year)) %>%
  mutate(winter.year2 = case_when(
    winter.year == 2017 ~ 'Winter 2017',
    winter.year == 2018 ~ 'Winter 2018',
    winter.year == 2019 ~ 'Winter 2019',
    winter.year == 2020 ~ 'Winter 2020',
    winter.year == 2021 ~ 'Winter 2021')) %>% 
  mutate(month = lubridate::month(Date, label = TRUE, abbr = TRUE)) %>%
  unite("Date1", month, day, sep=" ") %>%
  mutate(Dorder = case_when(
    week < 15 ~ week+53,
    week > 15 ~ week)) %>%
  subset(Dorder >=40) %>%
  arrange(Date) 

kiyi.maturityb <- kiyi.maturity %>%
  distinct(Date, .keep_all = TRUE) %>%
  select(Date) 

kiyi.maturityb$ID <- 1:nrow(kiyi.maturityb)

kiyi.maturityb <- kiyi.maturityb %>%
  mutate(wtemp = case_when(
    ID == 1 ~ 4.0,
    ID == 2 ~ 4.4,
    ID == 3 ~ 4.4,
    ID == 4 ~ 4.4,
    ID == 5 ~ 4.0,
    ID == 6 ~ 3.6,
    ID == 7 ~ 2.2,
    ID == 8 ~ 1.0,
    ID == 9 ~ 5.1,
    ID == 10 ~ 4.3,
    ID == 11 ~ 3.7,
    ID == 12 ~ 3.6,
    ID == 13 ~ 3.0))
  

kiyi.maturityc <- kiyi.maturity %>%
  left_join(kiyi.maturityb) %>%
#  unite(Date2, Date1, wtemp, sep=" ") %>%
  subset(winter.year2 == "Winter 2020" | winter.year2 == "Winter 2021") %>%
  distinct(Date, Mature.class, .keep_all = TRUE)

##Kiyi maturation plot with titles for talk
ggplot(kiyi.maturityc, aes(fill = Mature.class, x=ID, y=Maturity.n)) +
  geom_bar(position="fill", stat="identity") +
  scale_y_continuous(breaks=pretty_breaks()) +
  scale_x_continuous(breaks=kiyi.maturityc$ID, labels=kiyi.maturityc$Date1) +
  geom_text(aes(x = ID, label = wtemp, y = 0.1), size=12, family='serif') +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(), 
        axis.text.x=element_text(size=20, family='serif'),
        legend.position = "bottom") +
  labs( x='Date', y='Proportion of collection',
        title='Lake Superior Kiyi Female Maturation',
        subtitle='Collections made 2017-2021',
        caption=ann_data_access) +
  facet_wrap(~ winter.year2, scales='free_x')

ggsave(here('Plots and Tables/Kiyi.Female.MaturationClass.png'), dpi = 300, width = 35, height = 16, units = "cm")


##Kiyi maturation plot without titles for paper
ggplot(kiyi.maturityc, aes(fill = Mature.class, x=ID, y=Maturity.n)) +
  geom_bar(position="fill", stat="identity") +
  #scale_x_date(breaks = "1 week") +
  scale_y_continuous(breaks=pretty_breaks()) +
  scale_x_continuous(breaks=kiyi.maturityc$ID, labels=kiyi.maturityc$Date1) +
  geom_text(aes(x = ID, label = wtemp, y = 0.1), size=8, family='serif') +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(), 
        axis.text.x=element_text(size=18, family='serif'),
        legend.position = "bottom") +
  labs( x='Date', y='Proportion of collection') +
  facet_wrap(~ winter.year2, scales='free_x')

ggsave(here('Plots and Tables/KiyiPaper/Kiyi.Female.MaturationClass.png'), dpi = 300, width = 35, height = 16, units = "cm")


#################################################################################################################
#################################################################################################################
##Sex ratios - Kiyi
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
  subset(SPECIES == 206 & Sex != 'unknown') %>%
  select(Sample, Date, SPECIES, Sex) %>%
  mutate(Date1 = 'year<-'(Date, 2019), week = week(Date1)) %>%
  distinct(Sample, .keep_all = TRUE ) %>%
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
  subset(SPECIES == 206  & Maturity == 'ripe') 
  
#  subset(SPECIES == 206  & Maturity != 'developing' & Maturity != 'mature') 


fecundity.length = lm(Fresh ~ Length_mm, data=KiyiEggs)
summary(fecundity.length)
coef(fecundity.length)

fecundity.weight = lm(Fresh ~ Weight_g, data=KiyiEggs)
summary(fecundity.weight)
coef(fecundity.weight)

log.fecundity = lm(log(Fresh) ~ log(Length_mm), data=KiyiEggs)
summary(log.fecundity)
coef(log.fecundity)


Kiyi.EggSum <- KiyiEggs %>%
  summarise(fish = n(), mean.eggs = mean(Fresh), median.eggs = median(Fresh), 
            min.eggs = min(Fresh), max.eggs = max(Fresh),
            min.length = min(Length_mm), max.length = max(Length_mm))

meaneggs <- mean(KiyiEggs$Fresh)
mean(KiyiEggs$Fresh)

my.formula <- y ~ x

ggplot(KiyiEggs, aes(x=Length_mm, y = Fresh)) + 
#  stat_poly_eq(formula = my.formula, 
#               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
#               parse = TRUE) +         
  geom_jitter(fill = 'black',  shape=21, size=6) +
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

ggsave(here('Plots and Tables/KiyiPaper/KiyiFecundity.length.ripeB.png'), dpi = 300, width = 35, height = 16, units = "cm")

ggplot(KiyiEggs, aes(x=Weight_g, y = Fresh)) + 
  #  stat_poly_eq(formula = my.formula, 
  #               aes(label = paste(..eq.label.., ..rr.label.., sep = "~~~")), 
  #               parse = TRUE) +         
  geom_jitter(fill = 'black',  shape=21, size=6) +
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
  labs( x='Total weight (g)', y='Fecundity') 

ggsave(here('Plots and Tables/KiyiPaper/KiyiFecundity.weight.ripe.png'), dpi = 300, width = 35, height = 16, units = "cm")


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
  mutate(egg.diam = scale/Zoom) %>%
  left_join(egg.fish) %>%
  subset(SPECIES == 206 & Maturity == 'ripe') 


egg.table <- egg.data %>%
  group_by(Specimen_Number) %>% 
  summarize(mean.egg.diam = mean(egg.diam), min.egg.diam = min(egg.diam), 
            max.egg.diam=max(egg.diam), median.egg.diam=median(egg.diam),
            sd.egg.diam=sd(egg.diam), n=n(), se.egg.diam= sd.egg.diam/sqrt(n)) %>%
  left_join(egg.fish)  %>%
  subset(n == 25)


egg.sumtable <- egg.data %>%
  summarize(mean.egg.diam = mean(egg.diam), min.egg.diam = min(egg.diam), 
            max.egg.diam=max(egg.diam), median.egg.diam=median(egg.diam),
            sd.egg.diam=sd(egg.diam), n=n(), se.egg.diam= sd.egg.diam/sqrt(n))


egg.sumtable.individ <- egg.table %>%
  summarize(mean = mean(mean.egg.diam), min = min(mean.egg.diam), 
            max=max(mean.egg.diam), median.egg.diam=median(mean.egg.diam),
            sd.egg.diam=sd(mean.egg.diam), n=n(), se.egg.diam= sd.egg.diam/sqrt(n)) 


egg.fish.sum <- egg.table %>%
  summarise(min.fish = min(Length_mm), max.fish = max(Length_mm),
            mean.fish = mean(Length_mm), median.fish = median(Length_mm), 
            sd.fish=sd(Length_mm), n=n(), se.fish= sd.fish/sqrt(n))



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
 
ggsave(here('Plots and Tables/KiyiPaper/egg.size.counts.png'), dpi = 300, width = 35, height = 16, units = "cm")



##Egg Diameter histogram - counts plot
ggplot(egg.table, aes(x=mean.egg.diam)) +
  geom_histogram(closed="right", fill="gray70",
                 color="black", binwidth = 0.1, boundary=0) +
  geom_vline(aes(xintercept = mean(mean.egg.diam)), col = 'black', size = 2, linetype = 1) +
  geom_vline(aes(xintercept = median(mean.egg.diam)), col = 'black', size = 2, linetype = 2) +
  scale_y_continuous(name="Count")+ 
  scale_x_continuous(name="Egg diameter (mm)", seq(0,2.2,0.5)) +
  plot_theme + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(NA)) 

ggsave(here('Plots and Tables/KiyiPaper/egg.size.counts.byIndividual.png'), dpi = 300, width = 35, height = 16, units = "cm")


##Egg Diameter histogram - counts plot by maturity class

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
  mutate(egg.diam = scale/Zoom) %>%
  left_join(egg.fish) %>%
  subset(SPECIES == 206) 

egg.table <- egg.data %>%
  group_by(Specimen_Number) %>% 
  summarize(mean.egg.diam = mean(egg.diam), min.egg.diam = min(egg.diam), 
            max.egg.diam=max(egg.diam), median.egg.diam=median(egg.diam),
            sd.egg.diam=sd(egg.diam), n=n(), se.egg.diam= sd.egg.diam/sqrt(n)) %>%
  left_join(egg.fish)  %>%
  subset(n == 25)



ggplot(egg.data, aes(x=egg.diam, fill=Maturity)) +
  geom_histogram(closed="right", 
                 color="black", binwidth = 0.1, boundary=0) +
#  geom_vline(aes(xintercept = mean(egg.diam)), col = 'black', size = 2, linetype = 1) +
#  geom_vline(aes(xintercept = median(egg.diam)), col = 'black', size = 2, linetype = 2) +
  scale_y_continuous(name="Count")+ 
  scale_x_continuous(name="Egg diameter (mm)", seq(0,2.2,0.5)) +
  plot_theme + 
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        legend.position = c(.2, .7), 
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(NA)) 

ggsave(here('Plots and Tables/KiyiPaper/egg.size.byMaturity.png'), dpi = 300, width = 35, height = 16, units = "cm")


##########################################################################################################
##Kiyi length frequency
##########################################################################################################
kiyi.length <- ciscoes.winter %>%
  select(Date, GNetMesh_in, SPECIES, Sex, Maturity, Length_mm, Weight_g) %>%
  mutate(Mature.class = case_when(
    Maturity == 'immature' ~ 'immature',
    Maturity == 'mature' ~ 'developing',
    Maturity == 'developing' ~ 'developing',
    Maturity == 'ripe' ~ 'ripe',
    Maturity == 'running' ~ 'running',
    Maturity == 'spent' ~ 'spent')) %>%
  #filter(Mature.class != 'immature') %>%
  mutate(day = day(Date), month = month(Date), week = week(Date)) %>%
  mutate(Dorder = case_when(
    week < 15 ~ week+53,
    week > 15 ~ week)) %>%
  mutate(GNetMeshLabel1 =  case_when(
    GNetMesh_in == '1.5' ~ '1.5 inch',
    GNetMesh_in == '2'   ~ '2 inch',
    GNetMesh_in == '2.5' ~ '2.5 inch',
    GNetMesh_in == '3'  ~ '3 inch'
    )) %>%
  mutate(GNetMeshLabel2 =  case_when(
    GNetMesh_in == '1.5' ~ '1.5-inch, 38-mm ',
    GNetMesh_in == '2'   ~ '2-inch, 51-mm',
    GNetMesh_in == '2.5' ~ '2.5-inch, 64-mm',
    GNetMesh_in == '3'  ~ '3-inch, 76-mm'
  )) %>%
  subset(Dorder >=40 & SPECIES == 206)  
#  subset(SPECIES == 206 & Sex != 'unknown') %>%
#  subset(SPECIES == 206 & Maturity == 'ripe') 

length.sum <- kiyi.length %>%
  group_by(GNetMesh_in) %>%
  summarise(min.fish = min(Length_mm), max.fish = max(Length_mm),
            mean.fish = mean(Length_mm), median.fish = median(Length_mm), 
            sd.fish=sd(Length_mm), n=n(), se.fish= sd.fish/sqrt(n))

###Length frequency by Mesh

ggplot(kiyi.length, aes(x=Length_mm, fill = GNetMeshLabel2)) +
  geom_histogram(closed="right", color="black", 
                 binwidth = 2, boundary=0) +
  scale_y_continuous(name="Count")+ 
  scale_x_continuous(name="Total length (mm)", limits = c(150, 275)) +
  plot_theme + 
  scale_fill_brewer(palette="Accent") +  
 # scale_fill_brewer(palette="Greys") +  
  guides(fill=guide_legend(title="Gill net mesh size")) +
  theme(legend.position = c(.8, .85)) +
  theme(legend.text=element_text(size=24, family='serif')) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(NA)) 

ggsave(here('Plots and Tables/KiyiPaper/length.frequency.byMesh.png'), dpi = 300, width = 35, height = 16, units = "cm")


###Length frequency by Sex

ggplot(kiyi.length, aes(x=Length_mm, fill = Sex)) +
  geom_histogram(closed="right", color="black", 
                 binwidth = 2, boundary=0) +
  # geom_vline(aes(xintercept = mean(Length_mm)), col = 'black', size = 2, linetype = 1) +
  #  geom_vline(aes(xintercept = median(Length_mm)), col = 'black', size = 2, linetype = 2) +
  scale_y_continuous(name="Count")+ 
  scale_x_continuous(name="Total length (mm)", limits = c(150, 275)) +
  plot_theme + 
  scale_fill_brewer(palette="Greys") +  
  theme(legend.title = element_blank()) +
  theme(legend.position = c(.8, .85)) +
  theme(legend.text=element_text(size=24, family='serif')) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(NA)) 

ggsave(here('Plots and Tables/KiyiPaper/length.frequency.bySex.png'), dpi = 300, width = 35, height = 16, units = "cm")


##Add LSBS historical kiyi length data
kiyi.length.lsbs <-read_xlsx(here('Data','kiyi.lengths.xlsx'), sheet = 'kiyi.lengths') #%>%
  #subset(YEAR>=2014)

ggplot() +
  geom_histogram(data=kiyi.length.lsbs, aes(x=LENGTH), binwidth=2) +
  geom_vline(data=kiyi.length.lsbs, aes(xintercept = mean(LENGTH)), col = 'black', size = 2, linetype = 1) 
  
  
  
ggplot() +
  geom_histogram(data=kiyi.length, aes(x=Length_mm, fill = GNetMeshLabel2),
                 closed="right", color="black", 
                 binwidth = 2, boundary=0)  +
  geom_freqpoly(data=kiyi.length.lsbs, aes(x=LENGTH), binwidth=10) +
#  geom_vline(data=kiyi.length.lsbs, aes(xintercept = mean(LENGTH)), col = 'black', size = 2, linetype = 1) +
  scale_y_continuous(name="Count")+ 
  scale_x_continuous(name="Total length (mm)", limits = c(150, 275)) +
  plot_theme + 
  scale_fill_brewer(palette="Accent") +  
  # scale_fill_brewer(palette="Greys") +  
  guides(fill=guide_legend(title="Gill net mesh size")) +
  theme(legend.position = c(.8, .85)) +
  theme(legend.text=element_text(size=24, family='serif')) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(NA)) 

ggsave(here('Plots and Tables/KiyiPaper/length.frequency.withhistoric.png'), dpi = 300, width = 35, height = 16, units = "cm")




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
    MESH_SIZE == 30 ~ '3"')) %>%  
  mutate(Sample = OP_ID,
         Date = OP_DATE,
         StartDepth_m = BEG_DEPTH,
         EndDepth_m = END_DEPTH,
         Lat_DD = LAT,
         Long_DD = LON,
         MidDepth_m = ((StartDepth_m+EndDepth_m)/2),
         MidDepth_ft = MidDepth_m * 3.281,
         day = day(Date), 
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
  select(Sample, Date, Month, week, day, Lat_DD, Long_DD, StartDepth_m, EndDepth_m, MidDepth_m, MidDepth_ft, 
         depth.bin, GNetMeshLength_ft, GNetMesh_in, NIGHTS_OUT, SPECIES, Sex, Maturity, Length_mm) %>%
  mutate(month = lubridate::month(Date, label = TRUE, abbr = TRUE)) %>%
  unite("Date1", month, day, sep=" ") %>%
  arrange(Date) %>%
  left_join(sci.names) 

##Maturity plot data table
fulmar.maturity <- fulmar.all %>%
  select(Date, SPECIES, Sex, Maturity) %>%
  subset(SPECIES == 206 & Sex == 'female') %>%
  group_by(Date, Maturity) %>%
  summarise(Maturity.n = n(), .groups = 'drop') %>%
  mutate(day = day(Date), month = month(Date), week = week(Date), year = year(Date)) %>%
  mutate(day.month = lubridate::month(Date, label = TRUE, abbr = TRUE)) %>%
  unite("Date1", day.month, day, sep=" ") %>%
  arrange(Date) 


fulmar.dates <- fulmar.maturity %>%
  distinct(Date, .keep_all = TRUE) %>%
  select(Date)

fulmar.dates$ID <- 1:nrow(fulmar.dates)

fulmar.maturity.dates <- fulmar.maturity %>%
  left_join(fulmar.dates) %>%
  distinct(Date, Maturity, .keep_all = TRUE)

fulmar.maturity.sum <-fulmar.maturity %>% 
  group_by(month, Maturity) %>%
  summarise(fish = sum(Maturity.n)) %>%
  mutate(tfish = sum(fish), propfish = (fish/tfish)*100) 

fulmar.catch.sum <- fulmar.maturity


ggplot(fulmar.maturity.dates, aes(fill = Maturity, x=ID, y=Maturity.n)) +
  geom_bar(position="fill", stat="identity") +
  #scale_x_date(breaks = "1 week") +
  scale_y_continuous(breaks=pretty_breaks()) +
  scale_x_continuous(breaks=fulmar.maturity.dates$ID, labels=fulmar.maturity.dates$Date1) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(), 
        axis.text.x=element_text(size=16, family='serif', angle = 90, vjust = 0.5, hjust=1),
        legend.position = "bottom") +
  labs( x='Date', y='Proportion of collection') 
#+
#  facet_wrap(~ year, scales='free_x')



#  labs( x='Week', y='Proportion of collection',
##        title='Lake Michigan Kiyi Female Maturation',
#        subtitle='Collections made 1930-1931 by the R/V Fulmar',
#        caption='Data: USGS, https://www.sciencebase.gov/catalog/item/5e9f635482cefae35a128c38') 

ggsave(here('Plots and Tables/KiyiPaper/LMKiyi.Female.Maturation.png'), dpi = 300, width = 35, height = 16, units = "cm")

################################################################
##Fulmar Kiyi catch rate
fulmar.catch <- fulmar.all %>%
  subset(Month >=10) %>%
  group_by(Sample, SPECIES) %>%
  summarize(fish=n()) %>%
  ungroup()

fulmar.catch.zeros <-complete(fulmar.catch, Sample, SPECIES, fill=list(fish=0)) 

fulmar.kiyi <- fulmar.catch.zeros %>%
  left_join(fulmar.op) %>%
  subset(SPECIES == 206) %>%
  mutate(catch.rate = (((fish/GNetMeshLength_ft)/0.3048)*100)/NIGHTS_OUT)   #  to standardize to one-night fished 


fulmar.kiyi.catch.sum1 <- fulmar.kiyi %>%
  group_by(depth.bin) %>%
  summarize(catch=mean(catch.rate)) 
  

ggplot(fulmar.kiyi.catch.sum1,  aes(x=factor(depth.bin, level = c('<50 m', '50-100 m',
                                                                  '100-150 m', '150-200 m',
                                                                  '200-250 m', '>250 m')), 
                                    y=catch))+
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




ggplot(fulmar.kiyi.catch.sum3,  aes(x=factor(depth.bin, level = c('2 3/8"', '2 1/2"', '2 5/8"', '2 3/4"', '3"')), 
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

ggsave(here('Plots and Tables/Fulmar.Kiyi.CatchRate.by.Mesh.png'), dpi = 300, width = 35, height = 16, units = "cm")


pig <- fulmar.op %>%
  distinct(OP_DATE)


###################################################################################
###################################################################################
###################################################################################
##All LSBS Kiyi records for length distribution and lsength at maturity
##Load data

kiyi1 <-read.csv(here('Data','Kiyi.LABSHEET.csv')) 
kiyi1$OP_DATE<-as.Date(with(kiyi1,paste(DAY,MONTH,YEAR,sep="-")),"%d-%m-%Y")
#kiyi1$OP_DATE<-as.character(kiyi1$OP_DATE)

kiyi2 <-read.csv(here('Data','Kiyi.TRFISH.csv')) %>%
  select(OP_ID, LENGTH, WEIGHT, SEX, MATURITY)

raw.data <- read.csv(here('Data','RVCAT.csv')) %>%
  distinct(OP_ID, .keep_all = TRUE)


##change date format into usable form
#raw.data$OP_DATE<-as.character(raw.data$OP_DATE)
raw.data$OP_DATE<-parse_date(raw.data$OP_DATE, format='%d-%b-%y')

##Replace zeros which are known to be wrong to na for all variables other than SPECIES, NUM, WEIGHT
raw.data$TIME <- replace(raw.data$TIME, raw.data$TIME == 0, NA)             
raw.data$TOW_TIME <- replace(raw.data$TOW_TIME, raw.data$TOW_TIME == 0, NA)             
raw.data$FISHING_DEPTH <- replace(raw.data$FISHING_DEPTH, raw.data$FISHING_DEPTH == 0, NA)
raw.data$SURF_TEMP <- replace(raw.data$SURF_TEMP, raw.data$SURF_TEMP == 0, NA)
raw.data$BEG_SURF <- replace(raw.data$BEG_SURF, raw.data$BEG_SURF == 0, NA)
raw.data$END_SURF <- replace(raw.data$END_SURF, raw.data$END_SURF == 0, NA)
raw.data$BEG_BOTTOM <- replace(raw.data$BEG_BOTTOM, raw.data$BEG_BOTTOM == 0, NA)
raw.data$END_BOTTOM <- replace(raw.data$END_BOTTOM, raw.data$END_BOTTOM == 0, NA)

raw.data$STATE <- replace(raw.data$STATE, raw.data$STATE == 'E', 'ONT')
raw.data$STATE <- replace(raw.data$STATE, raw.data$STATE == 'W', 'ONT')
raw.data$STATE <- str_trim(raw.data$STATE)
raw.data$M_UNIT <- str_trim(raw.data$M_UNIT)

raw.data <- raw.data %>%
  group_by(LOCATION) %>%
  mutate(BEG_LATITUDE_DD = case_when(is.na(BEG_LATITUDE_DD) ~ mean(BEG_LATITUDE_DD, na.rm=TRUE),
                                     TRUE ~ as.numeric(BEG_LATITUDE_DD))) %>%
  mutate(BEG_LONGITUDE_DD = case_when(is.na(BEG_LONGITUDE_DD) ~ mean(BEG_LONGITUDE_DD, na.rm=TRUE),
                                      TRUE ~ as.numeric(BEG_LONGITUDE_DD))) %>%
  mutate(END_LATITUDE_DD = case_when(is.na(END_LATITUDE_DD) ~ mean(END_LATITUDE_DD, na.rm=TRUE),
                                     TRUE ~ as.numeric(END_LATITUDE_DD))) %>%
  mutate(END_LONGITUDE_DD = case_when(is.na(END_LONGITUDE_DD) ~ mean(END_LONGITUDE_DD, na.rm=TRUE),
                                      TRUE ~ as.numeric(END_LONGITUDE_DD))) %>%
  ungroup()

raw.site.lat.long <- raw.data %>%
  select(LOCATION, BEG_LATITUDE_DD, END_LATITUDE_DD, BEG_LONGITUDE_DD, 
         END_LONGITUDE_DD) %>%
  group_by(LOCATION) %>%
  summarise(BEG_LATITUDE_DD = mean(BEG_LATITUDE_DD,na.rm=TRUE),
            END_LATITUDE_DD = mean(END_LATITUDE_DD,na.rm=TRUE),
            BEG_LONGITUDE_DD = mean(BEG_LONGITUDE_DD,na.rm=TRUE),
            END_LONGITUDE_DD = mean(END_LONGITUDE_DD,na.rm=TRUE)) %>%
  ungroup()

##calculate the mid-point (average) latitude and longitude for each trawl where beg and end lat/longs are known
raw.data$Mid.Lat.DD <- rowMeans(raw.data[,c("BEG_LATITUDE_DD","END_LATITUDE_DD")], na.rm=TRUE)
raw.data$Mid.Long.DD <- rowMeans(raw.data[,c("BEG_LONGITUDE_DD","END_LONGITUDE_DD")], na.rm=TRUE)

##Calculate an average Surface and bottom temp
raw.data$Surface.Temp <- rowMeans(raw.data[,c("SURF_TEMP", "BEG_SURF", "END_SURF")], na.rm = TRUE)
raw.data$Bottom.Temp <- rowMeans(raw.data[,c("BEG_BOTTOM", "END_BOTTOM", "TEMP_BEG_BOTTOM", "TEMP_END_BOTTOM")], na.rm = TRUE)

raw.data <- raw.data %>%
  mutate(Mid.Lat.DD = ifelse(is.nan(Mid.Lat.DD), NA, Mid.Lat.DD),
         Mid.Long.DD = ifelse(is.nan(Mid.Long.DD), NA, Mid.Long.DD),
         Surface.Temp = ifelse(is.nan(Surface.Temp), NA, Surface.Temp),
         Bottom.Temp = ifelse(is.nan(Bottom.Temp), NA, Bottom.Temp))

##Replace FISHING_DEPTH with END_DEPTH for bottom trawls and keep as is for mid-water trawls
raw.data <- raw.data %>%
  mutate(FISHING_DEPTH = case_when(
    TR_DESIGN == 4 | TR_DESIGN == 5 | TR_DESIGN == 25 | TR_DESIGN == 26 |
      TR_DESIGN == 27 | TR_DESIGN == 44 ~ END_DEPTH, 
    TR_DESIGN == 21 | TR_DESIGN == 22 | TR_DESIGN == 28 | TR_DESIGN == 41 |
      TR_DESIGN == 45  ~ FISHING_DEPTH)) 

##calculate mid trawl depth from beg and end depths
raw.data <- raw.data %>%
  mutate(MidDepth_m = (BEG_DEPTH + END_DEPTH) /2) %>%
  mutate(Method = case_when(
    TR_DESIGN == 4 | TR_DESIGN == 5 | TR_DESIGN == 25 | TR_DESIGN == 26 |
      TR_DESIGN == 27 | TR_DESIGN == 44 ~ "Bottom trawl", 
    TR_DESIGN == 21 | TR_DESIGN == 22 | TR_DESIGN == 28 | TR_DESIGN == 41 |
      TR_DESIGN == 45  ~ "Mid-water trawl")) 
  

    
##add country based on states
raw.data <- raw.data %>%
  mutate(Country = case_when(
    STATE == 'WI'  ~ "USA", 
    STATE == 'MN'  ~ "USA", 
    STATE == 'MI'  ~ "USA", 
    STATE == 'ONT'  ~ "Canada"))

##Reduce fields to those needed
op.trawls <- raw.data %>%
  select(OP_ID, OP_DATE, LOCATION, Mid.Lat.DD, Mid.Long.DD, MidDepth_m, Method) %>%
  distinct(OP_ID, .keep_all = TRUE)



##Join OP table to Kiyi length, sex maturity tables
kiyi3 <- kiyi1 %>%
  select(OP_DATE, LOCATION, LENGTH, WEIGHT, SEX, MATURITY) %>% 
  left_join(op.trawls) %>%
  select(OP_ID, LENGTH, WEIGHT, SEX, MATURITY) %>%
  drop_na(OP_ID) %>%
  bind_rows(kiyi2) %>%
  distinct(OP_ID,LENGTH,SEX,MATURITY, .keep_all = TRUE) %>%  
  left_join(op.trawls) %>%
  select(OP_ID, OP_DATE, LOCATION, Mid.Lat.DD, Mid.Long.DD, MidDepth_m, LENGTH, WEIGHT, SEX, MATURITY, Method) %>%
  mutate(Sex = case_when(
    SEX == 0 ~ 'unknown',
    SEX == 1 ~ 'male', 
    SEX == 2 ~ 'female')) %>%
  mutate(Maturity = case_when(
    MATURITY == 1 ~ 'immature',
    MATURITY >= 2 ~ 'mature')) %>%
  select(OP_ID, OP_DATE, LOCATION, Mid.Lat.DD, Mid.Long.DD, MidDepth_m, LENGTH, WEIGHT, Sex, Maturity, Method) %>%
  drop_na(Sex, Maturity, LENGTH) %>%
  subset(Sex != "unknown") %>%
  subset(LENGTH <330) %>%
  subset(LENGTH >50) %>%
  drop_na(OP_DATE)
 
##Maturity at length
kiyi4 <- kiyi3 %>%
  group_by(Sex, LENGTH, Maturity) %>%
  summarize(n=n()) %>% 
  ungroup()

##total Number of dates sampled for sex and maturity data
kiyi5 <- kiyi3 %>%
  distinct(OP_DATE) %>%
  mutate(month = month(OP_DATE)) %>% 
  ungroup()

##total Number of locations wth sex and maturity data
kiyi6 <- kiyi3 %>%
  distinct(LOCATION)

##Maturity by sex and lengh
kiyi7 <- kiyi3 %>%
  group_by(Sex, LENGTH, Maturity) %>%
  summarise(fishes = n()) %>%
  pivot_wider(names_from = Maturity, values_from = fishes) %>%
  replace(is.na(.), 0) %>%
  mutate(propMat = mature/(mature+immature)) %>% 
  ungroup()

##Number of immature and mature by sex
kiyi8 <- kiyi4 %>%
  group_by(Sex, Maturity) %>%
  summarise(fishes = sum(n)) %>% 
  ungroup()


##total number by sex
kiyi9 <- kiyi4 %>%
  group_by(Sex) %>%
  summarise(fishes = sum(n)) %>% 
  ungroup()

##total number by maturityx
kiyi10 <- kiyi4 %>%
  group_by(Maturity) %>%
  summarise(fishes = sum(n)) %>% 
  ungroup()


###Length summary for RVCAT data
kiyi11 <- kiyi3 %>%
  summarize(mean.length = mean(LENGTH), min.length = min(LENGTH), 
            max.length=max(LENGTH), median.length=median(LENGTH),
            sd.length=sd(LENGTH), n=n(), se.length= sd.length/sqrt(n))
 


###Length frequency distribution for RVCAT data
ggplot(kiyi3, aes(x=LENGTH)) +
#  geom_histogram(data=kiyi3, aes(x=LENGTH,),
#                 closed="right", color="black", 
#                 binwidth = 2, boundary=0)  +
  geom_freqpoly(binwidth=10) +
  geom_vline(aes(xintercept = mean(LENGTH)), col = 'black', size = 2, linetype = 1) +
  geom_vline(aes(xintercept = median(LENGTH)), col = 'black', size = 2, linetype = 2) +
  scale_y_continuous(name="Count")+ 
  scale_x_continuous(name="Total length (mm)", limits = c(80, 300)) +
  plot_theme + 
  scale_fill_brewer(palette="Accent") +  
  # scale_fill_brewer(palette="Greys") +  
  guides(fill=guide_legend(title="Gill net mesh size")) +
  theme(legend.position = c(.8, .85)) +
  theme(legend.text=element_text(size=24, family='serif')) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(NA)) 

ggsave(here('Plots and Tables/KiyiPaper/RVCAT.Kiyi.lengthfrequency.png'), dpi = 300, width = 35, height = 16, units = "cm")



###Maturity at Length by Sex 

ggplot(kiyi4, aes(fill=Maturity, x=LENGTH, y = n)) + 
  geom_tile(position="fill", stat="identity") +
  scale_y_continuous(name="Relative proportion")+ 
  scale_x_continuous(name="Total length (mm)", limits = c(80, 300)) +
  plot_theme + 
  scale_fill_brewer(palette="Accent") +  
  geom_text(label = "immature",
    x = 140, y = .8, size = 8, family='serif') + 
  geom_text(label = "mature",
            x = 200, y = .2, size = 8, family='serif') + 
  geom_hline(yintercept=0.5) +
  theme(legend.position = "none") +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(NA)) +
  facet_wrap(~ Sex)

ggsave(here('Plots and Tables/KiyiPaper/LSBS.length.frequency.byMaturity.png'), dpi = 300, width = 35, height = 16, units = "cm")


##Length frequency of Grand Island fish with historic
##########################################################################################################
GIkiyi.length <- ciscoes.winter %>%
  select(Sample, Date, GNetMesh_in, SPECIES, Sex, Maturity, Length_mm, Weight_g) %>%
  mutate(Mature.class = case_when(
    Maturity == 'immature' ~ 'immature',
    Maturity == 'mature' ~ 'developing',
    Maturity == 'developing' ~ 'developing',
    Maturity == 'ripe' ~ 'ripe',
    Maturity == 'running' ~ 'running',
    Maturity == 'spent' ~ 'spent')) %>%
  #filter(Mature.class != 'immature') %>%
  mutate(day = day(Date), month = month(Date), week = week(Date)) %>%
  mutate(Dorder = case_when(
    week < 15 ~ week+53,
    week > 15 ~ week)) %>%
  mutate(GNetMeshLabel1 =  case_when(
    GNetMesh_in == '1.5' ~ '1.5 inch',
    GNetMesh_in == '2'   ~ '2 inch',
    GNetMesh_in == '2.5' ~ '2.5 inch',
    GNetMesh_in == '3'  ~ '3 inch'
  )) %>%
  mutate(GNetMeshLabel2 =  case_when(
    GNetMesh_in == '1.5' ~ '1.5-inch, 38-mm ',
    GNetMesh_in == '2'   ~ '2-inch, 51-mm',
    GNetMesh_in == '2.5' ~ '2.5-inch, 64-mm',
    GNetMesh_in == '3'  ~ '3-inch, 76-mm'
  )) %>%
  subset(Dorder >=40 & SPECIES == 206)  


ggplot() +
  geom_histogram(data=GIkiyi.length, aes(x=Length_mm, fill = GNetMeshLabel2),
                 closed="right", color="black", 
                 binwidth = 2, boundary=0)  +
  geom_freqpoly(data=kiyi3, aes(x=LENGTH), binwidth=10) +
  scale_y_continuous(name="Count")+ 
  scale_x_continuous(name="Total length (mm)", limits = c(80, 300)) +
  plot_theme + 
  scale_fill_brewer(palette="Accent") +  
  # scale_fill_brewer(palette="Greys") +  
  guides(fill=guide_legend(title="Gill net mesh size")) +
  theme(legend.position = c(.8, .85)) +
  theme(legend.text=element_text(size=24, family='serif')) +
  theme(panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(NA)) 

ggsave(here('Plots and Tables/KiyiPaper/length.frequency.withhistoric.png'), dpi = 300, width = 35, height = 16, units = "cm")

############################################################################################
############################################################################################
##New different length distribution
kiyi.GILengths <- GIkiyi.length %>%
  select(Sample, Sex, Length_mm, GNetMeshLabel2) %>%
    mutate(Data = 'Grand Island gill nets')
  
  
kiyi.all.lengths <- kiyi3 %>%
  select(OP_ID, Sex, LENGTH) %>%
  renameCol('OP_ID', 'Sample') %>%
  renameCol('LENGTH', 'Length_mm') %>%
  mutate(GNetMeshLabel2 = ' ') %>%
  mutate(Data = 'Lakewide trawls') %>%
  bind_rows(kiyi.GILengths) 


 
all.L.freq <- ggplot(kiyi.all.lengths, aes(x=Length_mm, y = ..scaled.., group = Data, fill = Data)) +
  geom_density(alpha = 0.6) +
  scale_y_continuous(name="Proportional density")+ 
  scale_x_continuous(name=" ", limits = c(60, 330)) +
  plot_theme + 
  scale_fill_brewer(palette="Accent") +  
  labs(title = 'a)') +
  theme(legend.position = c(.8, .85),
        legend.text=element_text(size=24, family='serif'), 
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(NA)) 

ggsave(here('Plots and Tables/KiyiPaper/all.length.frequency.png'), dpi = 300, width = 35, height = 16, units = "cm")


###Length frequency by Mesh

GI.L.freq <- ggplot(kiyi.length, aes(x=Length_mm, fill = GNetMeshLabel2)) +
  geom_histogram(closed="right", color="black", 
                 binwidth = 2, boundary=0) +
  scale_y_continuous(name="Count")+ 
  scale_x_continuous(name="Total length (mm)", limits = c(150, 275)) +
  plot_theme + 
  scale_fill_brewer(palette="Accent") +  
  # scale_fill_brewer(palette="Greys") +  
  guides(fill=guide_legend(title="Gill net mesh size")) +
  labs(title = 'b)') + 
  theme(legend.position = c(.8, .85),
        legend.text=element_text(size=24, family='serif'),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(NA)) 

ggsave(here('Plots and Tables/KiyiPaper/length.frequency.byMesh.png'), dpi = 300, width = 35, height = 16, units = "cm")


##Dual plot for paper

all.L.freq / GI.L.freq

ggsave(here('Plots and Tables/KiyiPaper/All.Kiyi.LengthFreq.withMesh.Paper.png'), dpi = 300, width = 35, height = 35, units = "cm")



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

fulmar.length <- fulmar.all %>%
  subset(SPECIES == 206) %>%
  mutate(GNetMeshLabel2 = GNetMesh_in, Data = 'Lake Michigan lakewide gill nets, 1930-32') %>%
  select(Sample, Sex, Length_mm, GNetMeshLabel2, Data)  %>%
  drop_na() 

kiyi.GILengths <- GIkiyi.length %>%
  select(Sample, Sex, Length_mm, GNetMeshLabel2) %>%
  mutate(Data = 'Lake Superior Grand Island gill nets, 2017-21')


kiyi.all.lengths <- kiyi3 %>%
  select(OP_ID, Sex, LENGTH) %>%
  renameCol('OP_ID', 'Sample') %>%
  renameCol('LENGTH', 'Length_mm') %>%
  mutate(GNetMeshLabel2 = ' ') %>%
  mutate(Data = 'Lake Superior lakewide trawls, 1996-2021') %>%
  bind_rows(kiyi.GILengths) 

kiyi.all.lengths <- kiyi.all.lengths %>%
  bind_rows(fulmar.length) 

ggplot(kiyi.all.lengths, aes(x=Length_mm, y = ..scaled.., group = Data, fill = Data)) +
  geom_density(alpha = 0.6) +
  scale_y_continuous(name="Proportional density")+ 
  scale_x_continuous(name="Total length (mm)", limits = c(60, 330)) +
  plot_theme + 
  scale_fill_brewer(palette="Accent") +  
  theme(legend.position = c(.25, .95),
        legend.text=element_text(size=20, family='serif'), 
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(NA)) 

ggsave(here('Plots and Tables/KiyiPaperFulmar/all.all.length.frequency.png'), dpi = 300, width = 35, height = 16, units = "cm")


#######################################################################################
##Bring in LS Length data
##compare fulmar data to LS data for a few ciscoes

##load the raw RVCAT data file
raw.data<-read.csv(here('Data','RVCAT.csv'))

##change date into usable form
raw.data$date<-dmy(raw.data$OP_DATE)

###Calculate mid lat and long for each trawl

raw.data[is.na(raw.data[,"END_LATITUDE_DD"]), "END_LATITUDE_DD"] <- raw.data[is.na(raw.data[, "END_LATITUDE_DD"]),"BEG_LATITUDE_DD"]
raw.data[is.na(raw.data[,"BEG_LATITUDE_DD"]), "BEG_LATITUDE_DD"] <- raw.data[is.na(raw.data[, "BEG_LATITUDE_DD"]),"END_LATITUDE_DD"]

raw.data[is.na(raw.data[,"END_LONGITUDE_DD"]), "END_LONGITUDE_DD"] <- raw.data[is.na(raw.data[, "END_LONGITUDE_DD"]),"BEG_LONGITUDE_DD"]
raw.data[is.na(raw.data[,"BEG_LONGITUDE_DD"]), "BEG_LONGITUDE_DD"] <- raw.data[is.na(raw.data[, "BEG_LONGITUDE_DD"]),"END_LONGITUDE_DD"]

raw.data$Mid.Lat.DD<-(raw.data$BEG_LATITUDE_DD+raw.data$END_LATITUDE_DD)/2
raw.data$Mid.Long.DD<-(raw.data$BEG_LONGITUDE_DD+raw.data$END_LONGITUDE_DD)/2

raw.data$YearClass<-raw.data$YEAR-1

##add country based on states
raw.data <- raw.data %>%
  mutate(Country = case_when(
    STATE == 'WI'  ~ "USA", 
    STATE == 'MN'  ~ "USA", 
    STATE == 'MI'  ~ "USA", 
    STATE == 'E'  ~ "Canada",
    STATE == 'W'  ~ "Canada"))


##Select minimum number of fields of interest
data1<-select(raw.data,1,32,4,5,8, 9, 33,34,35, 36)

##data1<-subset(data1, TARGET==2 & YEAR >1973 &  M_UNIT == "WI2")

data1<-subset(data1, TARGET==2 & YEAR >1977 &  Country == "USA")


###########################
##load Fish Lengths file into R
raw.data<-read.csv(here('Data','LENGTHS_RVCAT.csv'))
raw.data<-subset(raw.data, EXP_N>0)
reprows<-rep(1:nrow(raw.data), raw.data$EXP_N)
data2 <- raw.data[reprows,] %>%
  as.data.frame()
data2 <-select(data2, 1,4:6)


###JOIN TRAWL EFFORT TO LENGTH DATA
fun.lengths.trawl <- inner_join(data2,unique(data1)) %>%
  select(OP_ID, SPECIES, LENGTH) %>%
  left_join(sci.names) %>%
  select(OP_ID, SPECIES, LENGTH, COMMON_NAME) %>%
  mutate(Data = "LS lakewide trawls, 1978-2021") %>%
  renameCol('OP_ID', 'Sample') %>%
  renameCol('LENGTH', 'Length_mm') 

fun.lengths.GI <- ciscoes.winter %>%
  select(Sample, SPECIES, Length_mm, COMMON_NAME) %>%
  mutate(Data = 'LS Grand Island gill nets, 2017-2021')

fun.lengths.fulmar <- fulmar.all %>%
  select(Sample, SPECIES, Length_mm, COMMON_NAME) %>%
  mutate(Data = 'Lake Michigan gill nets, 1930-32')


fun.lengths <- fun.lengths.trawl %>%
  bind_rows(fun.lengths.GI) %>%
  bind_rows(fun.lengths.fulmar) %>%
  subset(SPECIES   == '202' |
           SPECIES == '204' |
           SPECIES == '206' |
           SPECIES == '207' |
           SPECIES == '208' |
           SPECIES == '210') 
#%>%
#  subset(Length_mm >150)


ggplot(fun.lengths, aes(x=Length_mm, y = ..scaled.., group = Data, fill = Data)) +
  geom_density(alpha = 0.6) +
  scale_y_continuous(name="Proportional density")+ 
  scale_x_continuous(name="Total length (mm)") +
  plot_theme + 
  scale_fill_brewer(palette="Accent") +  
  theme(legend.position = "bottom",
        legend.direction = "horizontal", 
        legend.text=element_text(size=20, family='serif'), 
        legend.title = element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(NA)) +
  facet_wrap(~ COMMON_NAME)

#legend.position = c(.25, .75),
ggsave(here('Plots and Tables/KiyiPaperFulmar/fun.length.frequency.png'), dpi = 300, width = 40, height = 20, units = "cm")




############################################################################################
############################################################################################
##new sex ratio with more historical data

newsex2 <- kiyi3 %>%
  mutate(month=month(OP_DATE), week = week(OP_DATE)) %>%
  select(OP_ID, OP_DATE, Sex, Method)

newsex1 <-ciscoes.all %>%
  subset(SPECIES == 206) %>%
  select(Sample, Date, Sex, Method) %>%
  renameCol('Sample', 'OP_ID') %>%
  renameCol('Date', 'OP_DATE') %>%
  bind_rows(newsex2) %>% 
  subset(Sex != 'unknown') %>%
  drop_na() %>%
  mutate(Habitat = case_when(
    Method == 'Mid-water trawl' ~ 'pelagic',
    Method == 'Bottom trawl' ~ 'demersal', 
    Method == 'Gillnet' ~ 'demersal'))
  

##Number of gill nets sets per week
newsex.events <- newsex1 %>%
  select(OP_ID, OP_DATE) %>% 
  distinct(OP_DATE, .keep_all = TRUE) %>%
  mutate(week = week(OP_DATE), day = day(OP_DATE)) %>%
  mutate(month = lubridate::month(OP_DATE, label = TRUE, abbr = TRUE)) %>%
  unite("Date1", month, day, sep=" ") %>%
  arrange(OP_DATE) %>%
  group_by(week) %>%
  summarise(lifts = n()) %>%
  ungroup() 


newsex.ratios <- newsex1 %>%
  group_by(OP_DATE, Sex) %>%
  summarise(fishes = n()) %>%
  pivot_wider(names_from = Sex, values_from = fishes) %>%
  replace(is.na(.), 0) %>%
  mutate(propf = female/(female+male), propm = male/(female+male), fishes = female+male)  %>%
  mutate(Date1 = 'year<-'(OP_DATE, 2019), week = week(Date1)) %>%
  group_by(week) %>%
  summarise(Date = Date1, day = day(Date1), 
            sexf.mean = mean(propf), 
            sexf.max = max(propf), 
            sexf.min = min(propf), 
            sexf.n = sum(fishes), 
            sexf.se = std.error(propf)) %>%
  mutate(Type = 'Sex ratio') %>%
  mutate(month = lubridate::month(Date, label = TRUE, abbr = TRUE)) %>%
  unite("Date1", month, day, sep=" ") %>%
  distinct(week, .keep_all = TRUE) %>%
  mutate(Dorder = case_when(
    week < 15 ~ week+53,
    week > 15 ~ week)) %>%
  left_join(newsex.events) %>%
  ungroup() %>%
  subset(sexf.n >50)


##sex ratio by sample


##sex ratio by sampling week
newsexp1 <- ggplot(newsex.ratios, aes(x = Dorder, y=sexf.mean)) +
  geom_point(shape=21, fill = 'grey70', size=6) +
  geom_smooth(se=FALSE, colour = 'black') +
  geom_errorbar(aes(x = Dorder, ymin=sexf.mean-sexf.se, ymax=sexf.mean+sexf.se), width=.5) + 
#  geom_text(data = newsex.ratios, aes(x=Dorder, y=sexf.mean, label = sexf.n), nudge_y = 0.1, nudge_x = -0.2, size=6, family='serif') +  
  geom_text(data = newsex.ratios, aes(x=Dorder, y=sexf.mean, label = lifts), nudge_y = 0.1, nudge_x = -0.2, size=5, family='serif') +  
#  geom_text(data = newsex.ratios, aes(x=Dorder, y=-0.1, label = lifts), size=6, family='serif') + 
  scale_x_continuous(breaks=newsex.ratios$Dorder, labels=newsex.ratios$Date1, guide = guide_axis(n.dodge = 4)) +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(), 
        axis.text.x=element_text(size=16, family='serif'),
        legend.position = "none") +
  labs( x='', y='Female proportion of collection', title = "a)") 

ggsave(here('Plots and Tables/KiyiPaper/Kiyi.NewSexRatios.png'), dpi = 300, width = 35, height = 16, units = "cm")


newsexp2 <-  ggplot(sexratio3, aes(x=ID, y = propf)) +
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
newsexp1 / newsexp2
ggsave(here('Plots and Tables/KiyiPaper/Kiyi.NewSexRatio.Paper.png'), dpi = 300, width = 35, height = 35, units = "cm")



################################
##sex ratios by habitat sampled
newsex.ratios.habitat <- newsex1 %>%
  group_by(OP_ID, Sex) %>%
  summarise(fishes = n()) %>%
  pivot_wider(names_from = Sex, values_from = fishes) %>%
  replace(is.na(.), 0) %>%
  mutate(propf = female/(female+male), propm = male/(female+male), fishes = female+male)  %>%
  left_join(newsex1) %>%
  mutate(Date1 = 'year<-'(OP_DATE, 2019), week = week(Date1)) %>%
  group_by(week, Habitat) %>%
  summarise(Date = Date1, day = day(Date1), 
            sexf.mean = mean(propf), 
            sexf.max = max(propf), 
            sexf.min = min(propf), 
            sexf.n = sum(fishes), 
            sexf.se = std.error(propf)) %>%
  mutate(Type = 'Sex ratio') %>%
  mutate(month = lubridate::month(Date, label = TRUE, abbr = TRUE)) %>%
  unite("Date1", month, day, sep=" ") %>%
  distinct(week, .keep_all = TRUE) %>%
  mutate(Dorder = case_when(
    week < 15 ~ week+53,
    week > 15 ~ week)) %>%
  left_join(newsex.events) %>%
  ungroup() %>%
  subset(sexf.n >50)


ggplot(newsex.ratios.habitat, aes(x = Dorder, y=sexf.mean)) +
  geom_point(shape=21, size=6) +
  geom_smooth(se=FALSE, colour = 'black') +
  geom_errorbar(aes(x = Dorder, ymin=sexf.mean-sexf.se, ymax=sexf.mean+sexf.se), width=.5) + 
  #  geom_text(data = newsex.ratios, aes(x=Dorder, y=sexf.mean, label = sexf.n), nudge_y = 0.1, nudge_x = -0.2, size=6, family='serif') +  
  #geom_text(data = newsex.ratios, aes(x=Dorder, y=sexf.mean, label = lifts), nudge_y = 0.1, nudge_x = -0.2, size=5, family='serif') +  
  #  geom_text(data = newsex.ratios, aes(x=Dorder, y=-0.1, label = lifts), size=6, family='serif') + 
  scale_x_continuous(breaks=newsex.ratios$Dorder, labels=newsex.ratios$Date1, guide = guide_axis(n.dodge = 4)) +
  scale_y_continuous(breaks=pretty_breaks()) +
  theme_bw() +
  plot_theme+
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(), 
        axis.text.x=element_text(size=16, family='serif'),
        legend.position = "none") +
  labs( x='', y='Female proportion of collection') +
  facet_wrap(~ Habitat)
  
ggsave(here('Plots and Tables/KiyiPaper/Kiyi.NewSexRatios.Habitat.png'), dpi = 300, width = 35, height = 16, units = "cm")




##############################################################################################
##############################################################################################
##New cisco site map with survey collection sites 1970s-2021


ls_poly <- readOGR(dsn = here('Data',"shapefiles/LakeSuperior"), layer = "lake_superior")
ls_poly <- spTransform(ls_poly, CRS("+proj=longlat"))
ls_poly.fort <- fortify(ls_poly)

op.trawls$LOCATION = as.character(op.trawls$LOCATION)
kiyi3$LOCATION = as.character(kiyi3$LOCATION)

kiyi.trawls <- kiyi3 %>% 
  distinct(LOCATION, .keep_all = TRUE) %>%
  mutate(Type = 'Sex ratio') %>%
  select(OP_ID, OP_DATE, LOCATION, Mid.Lat.DD, Mid.Long.DD, MidDepth_m, Method, Type) %>%
  drop_na()


kiyi.map <- ciscoes.all %>%
  select(Sample, Date, Location, Lat_DD, Long_DD, MidDepth_m, Method) %>%
  renameCol('Sample', 'OP_ID') %>%
  renameCol('Date', 'OP_DATE') %>%
  renameCol('Location', 'LOCATION') %>%
  renameCol('Lat_DD', 'Mid.Lat.DD') %>%
  renameCol('Long_DD', 'Mid.Long.DD') %>%
  mutate(Type = 'GSI & sex ratio') %>%
  bind_rows(kiyi.trawls) %>%
  distinct(LOCATION, .keep_all = TRUE) %>%
  mutate(month=month(OP_DATE)) %>%
  mutate(day = day(OP_DATE), jday = yday(OP_DATE)) %>%
  mutate(Season = case_when(
    jday <= 80 ~ 'winter',
    jday >  80  & jday < 173 ~ 'spring',
    jday >= 173 & jday < 265 ~ 'summer',
    jday >= 265 & jday < 355 ~ 'fall',
    jday >= 355 ~ 'winter')) %>% 
  mutate(year = year(OP_DATE), month = month(OP_DATE)) %>%
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



mapa <- ggplot(subset(kiyi.map, Method == "Gillnet"), aes(x=Long_DD, y = Lat_DD)) +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.5) +
  geom_point(data=subset(kiyi.map, Method == "Gillnet"), 
             mapping=aes(Mid.Long.DD, Mid.Lat.DD, fill=MidDepth_m), shape=21, size=3) +
  scale_fill_gradient(low = "yellow", high = "blue", na.value = NA, breaks=pretty_breaks(4), 
                      name = "Depth (m)") +
  scale_y_continuous(name='Latitude', breaks=pretty_breaks(), limits=c(46.35,47.1)) +
  scale_x_continuous(name='Longitude', breaks=pretty_breaks(), limits=c(-86.8,-86.4)) +
  theme_bw() +
  map_theme +
  geom_label(label="Grand Island", 
             x=-86.62,
             y=46.38,
             label.size = NA,
             size=4.5, 
             family='serif', 
             color = "black") +
  geom_label(label="Depth (m)", 
             x=-86.65,
             y=47.1,
             label.size = NA,
             size=4, 
             family='serif', 
             color = "black") +
  theme(axis.text=element_text(size=10, family='serif'),
        axis.title=element_blank(), 
        #legend.title = element_text(size=8, family='serif'), 
        legend.title = element_blank(), 
        legend.text = element_text(size=10, family='serif'),
        legend.direction = "horizontal", 
        legend.position = c(.5, .80))  +
  guides(colour = guide_legend(title.position = "bottom"))



mapb <- ggplot(kiyi.map, aes(x=Mid.Long.DD, y=Mid.Lat.DD)) +
  geom_jitter(aes(fill = Type), shape=21, size=4, alpha=0.8) +
  scale_fill_brewer(palette="Accent") +
  geom_path(data=ls_poly.fort, aes(long, lat, group = group), size = 0.7)+
  scale_y_continuous(name='Latitude', breaks=pretty_breaks()) +
  scale_x_continuous(name='Longitude', breaks=pretty_breaks()) +
  theme_bw() +
  map_theme + 
  geom_rect(data=ls_poly.fort, 
            aes(xmin=-86.85, ymin=46.3, xmax=-86.38, ymax=46.85),
            size=0.5, color="black", fill = NA, inherit.aes=FALSE) +
  theme(axis.text=element_text(size=20, family='serif'), 
        axis.title=element_text(size=20, family='serif'), 
        legend.title = element_blank(),  
        legend.text = element_text(size=20, family='serif'),
        legend.position = c(.87, .87)) +
  geom_label(label="Grand Island", 
             x=-86.65,
             y=46.36,
             label.size = NA,
             size=5, 
             family='serif', 
             color = "black") 

papermap <- mapb + inset_element(mapa, left = 0.01, bottom = 0.48, right = 0.21, top = 0.99)

ggsave(here('Plots and Tables/KiyiPaper/Kiyi_NewPaperMap.png'))


#############################################################################################
##Water temperatures at Grand Island

wtemps18 <-read_xlsx(here('Data','GI.wtemps.2018_2021.xlsx'), sheet = 'GI.wtemps.2018')
wtemps19 <-read_xlsx(here('Data','GI.wtemps.2018_2021.xlsx'), sheet = 'GI.wtemps.2019')
wtemps20 <-read_xlsx(here('Data','GI.wtemps.2018_2021.xlsx'), sheet = 'GI.wtemps.2020')
wtemps21 <-read_xlsx(here('Data','GI.wtemps.2018_2021.xlsx'), sheet = 'GI.wtemps.2021')

wtemps <- wtemps18 %>%
  bind_rows(wtemps19) %>%
  bind_rows(wtemps20) %>%
  bind_rows(wtemps21) %>%
  na.omit() %>%
  mutate(Date = as.POSIXct(UTC)) %>%
  mutate(year = year(Date), month = month(Date), jday = yday(Date)) 


wtemps.daily <- wtemps %>%
  group_by(Date) %>%
  summarise(wtemp.c = mean(wtemp.c)) %>%
  mutate(year = year(Date), month = month(Date), day=day(Date), jday = yday(Date)) %>%
  mutate(jday2 = case_when(
    jday <= 31  ~ jday+365,
    jday >  31  ~ jday)) %>%
  mutate(winter.year = case_when(
    month >8 ~ year + 1,
    month <=8 ~ year)) %>%
  mutate(winter.year2 = case_when(
    winter.year == 2017 ~ 'Winter 2017',
    winter.year == 2018 ~ 'Winter 2018',
    winter.year == 2019 ~ 'Winter 2019',
    winter.year == 2020 ~ 'Winter 2020',
    winter.year == 2021 ~ 'Winter 2021')) %>%
  subset(month >8 | month == 1) %>%
  subset(winter.year2 != 'Winter 2017') %>%
  subset(winter.year2 != 'Winter 2018') %>%
  subset(winter.year2 != 'Winter 2022') %>%
  mutate(month = lubridate::month(Date, label = TRUE, abbr = TRUE)) %>%
  unite("Date1", month, day, sep=" ") %>%
  mutate(month = month(Date))

  

jdaylabels <-  c("Sep", "Oct", "Nov", "Dec", "Jan","Feb")

ggplot(wtemps.daily, aes(x=jday2, y=wtemp.c, colour = winter.year2, group=winter.year2)) +
  geom_line(size=1.5) +
  scale_y_continuous(name="Surface Water temperature (C)", limits = c(0, 20))+ 
  scale_x_continuous(breaks=c(244,274,305,335,366, 397), labels = jdaylabels) +
  labs(x="Date") + 
  plot_theme + 
  scale_fill_brewer(palette="Accent") +  
  theme(legend.position = c(.8, .85),
        legend.text=element_text(size=24, family='serif'),
        legend.title=element_blank(),
        panel.grid.major = element_blank(), 
        panel.grid.minor = element_blank(), 
        axis.line = element_line(colour = "black"),
        panel.background = element_rect(NA)) 

ggsave(here('Plots and Tables/KiyiPaper/wtemps.series.png'), dpi = 300, width = 35, height = 16, units = "cm")



wtemps.monthly <- wtemps.daily %>%
  group_by(month, year) %>%
  summarise(mean.temp=mean(wtemp.c), min.temp=min(wtemp.c), 
            max.temp=max(wtemp.c), median.temp=median(wtemp.c), 
            sd.temp=sd(wtemp.c), n=n(), se.temp= sd.temp/sqrt(n)) %>%
  mutate(winter.year = case_when(
    month >7 ~ year + 1,
    month <=7 ~ year)) %>%
  mutate(winter.year2 = case_when(
    winter.year == 2017 ~ 'Winter 2017',
    winter.year == 2018 ~ 'Winter 2018',
    winter.year == 2019 ~ 'Winter 2019',
    winter.year == 2020 ~ 'Winter 2020',
    winter.year == 2021 ~ 'Winter 2021')) %>%
  mutate(month.name = case_when(
    month == 9 ~ 'Sep',
    month == 10 ~ 'Oct',
    month == 11 ~ 'Nov',
    month == 12 ~ 'Dec',
    month == 1 ~ 'Jan')) %>%
   mutate(month.order = case_when(
    month == 1 ~ 5,
    month == 9 ~ 1,
    month == 10 ~ 2,
    month == 11 ~ 3,
    month == 12 ~ 4)) %>%
  subset(month.name !='Sep')
  

ggplot(wtemps.monthly, aes(x=month.order, y = mean.temp)) +
  geom_point(size=4, color = "black") +
  geom_segment(aes(x=month.order, xend=month.order, y=min.temp, yend=max.temp), size=1, color='black')+
  geom_hline(aes(x=month.order, yintercept=4), size = 1) +
  geom_line(aes(x=month.order, y=mean.temp), size = 1, linetype=2) +
  scale_y_continuous(breaks=pretty_breaks(), limits=c(0,15)) +
  scale_x_continuous(breaks=wtemps.monthly$month.order, labels=wtemps.monthly$month.name) +
  theme_bw() +
  plot_theme +
  scale_fill_brewer(palette="Accent") +
  theme(legend.title = element_blank(),
        axis.text.x=element_text(size=18, family='serif'),
        panel.spacing.x = unit(2.0, "lines")) +
  labs( x='Month', y='Surface water temperature (C)') +
  facet_wrap(~ winter.year2)

ggsave(here('Plots and Tables/KiyiPaper/wtemps.monthly.png'), dpi = 300, width = 35, height = 16, units = "cm")



###Temperatures <4

NASAexport <- wtemps %>%
  group_by(Date) %>%
  summarise(wtemp.c = mean(wtemp.c)) %>%
  mutate(year = year(Date), jday = yday(Date)) %>%
  select(year, jday, wtemp.c)

openxlsx::write.xlsx(NASAexport, here('Plots and Tables/','NASA.2019_2021.xlsx'))

isothermal <- read_xlsx(here('Data','LS_GLERL_WTemp.xlsx'), sheet = 'NASA') %>%
  subset(mean.sst <= 3.98) %>%
  subset(jday > 305) %>%
  group_by(year) %>%
  summarize(iso.fall = min(jday))

ggplot(isothermal, aes(x=year, y=as.Date(iso.fall, origin = as.Date("2018-01-01")))) +
  geom_point(size=2) +
  geom_line() +
  geom_smooth(method=lm, se = FALSE, colour='red') +
  scale_y_date(date_labels = "%b %e") +
  plot_theme +
  labs(x='Year', y='Date surface water temperature <4 C') +
  geom_segment(aes(x=min(year-1), xend=max(year+1), 
                   y=mean(as.Date(iso.fall, origin = as.Date("2018-01-01"))), 
                   yend=mean(as.Date(iso.fall, origin = as.Date("2018-01-01")))),
               size=1, color='black') + 
  scale_x_continuous(expand=c(0,0), breaks = scales::pretty_breaks(5), 
                     limits=c(min(year-1),max(year+5))) +

 
ggsave(here('Plots and Tables/KiyiPaper/isothermal.trend.png'), dpi = 300, width = 35, height = 16, units = "cm")

