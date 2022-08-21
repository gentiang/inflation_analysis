## Setting working directory and loading necessary libraries

setwd("C:/Users/gashi/Desktop/Inflation Analysis")
library(tidyverse) #manipulating data
library(ggplot2)   #creating graphs
library(gganimate) #animations
library(scales)    #scaling for axes in ggplot
library(bbplot)    #BBC graph theme
library(lubridate) #working with dates
library(readxl)    #reading Excel files
library(tidylog)   #logging data transformations
library(RColorBrewer) #colors for ggplots
library(sf)        #for shapefiles
library(shadowtext) #geom_shadowtext - shadow behind geom_text
################################################################################
## Importing Data

xk <- read_csv('Raw Data/cpi04_20220815-170508.csv', 
               skip = 3, #skips the first 3 rows; header included so that i can set my own column names
               col_names = c("date", "hicp"),  #setting custom column names
               na=":") %>% #missing values are denoted like that
    mutate(hicp=hicp/100)
xk

al <- read_csv("Raw Data/Instat3_20220815-150458.csv",
               skip = 3,
               col_names = c("date", "hicp"),
               na="..") %>% 
    mutate(hicp=hicp/100)
al

mkd_srb <- read_csv("Raw Data/prc_hicp_manr_1_Data.csv",
                    skip=1,
                    na=":",
                    col_names = c("date", "country", "unit", "coicop", "hicp", "fn")) %>% 
    select(country, date, hicp) %>% 
    mutate(hicp=hicp/100)
mkd_srb

mne <- read_xls("Raw Data/Podaci_Data base (HICP).xls",
         sheet = "Data base_HICP_ENG", 
         range = "B9:E158",
         skip = 3,
         col_names =c("date", "index", "monthlyhicp", "hicp")) %>% 
    select(date, hicp)
mne
################################################################################
## Creating country ID, converting the date variables from text to date, appending the data

xk <- xk %>% 
    mutate(country="Kosovo",
           date=ym(date)) %>% 
    relocate(country)

al <- al %>% 
    mutate(country="Albania",
           date=ym(date)) %>% 
    relocate(country)

# The date of Montenegro is slightly different, and we can fix it
# Lubridate does actually manage to recognize it properly
mne <- mne %>% 
    mutate(country="Montenegro",
           date=my(date)) %>% 
    relocate(country)


mkd_srb <- mkd_srb %>% 
    mutate(date=ym(date)) %>% 
    relocate(country)

df <- rbind(al,mne,mkd_srb,xk)
################################################################################
## Cleaning the data

df2 <- na.omit(df) #remove rows with NA values
################################################################################
## Visualisations

#Scatter + Line (Kosovo) Graph
df2 %>%
    ggplot(aes(date,hicp)) + 
    geom_point(aes(col=country), alpha=0.7, size=7) +
    geom_line(data = df2[df2$country == "Kosovo",], color="#eb3434", size=1.5, alpha=0.7) + #adding a line for Kosovo only
    scale_x_date(date_labels = "%Y", date_breaks = "2 years") + #changing the date scale to only every two years
    geom_hline(yintercept = 0, size = 1, colour="#333333") + #custom horizontal line
    scale_y_continuous(labels = label_percent()) + #changing y-axis labels to percent
    scale_color_manual(values = c("#eb3434", "#58c9b4", "#ccd67c", "#275ea9", "#eb9834"),
                       breaks = c("Kosovo", "Albania", "Montenegro", "North Macedonia", "Serbia")) + #manual colors and order in legend
    geom_vline(xintercept = as.Date("2022-02-01"), linetype="dashed", size=1.5, colour="#333333") + #vertical line
    geom_vline(xintercept = as.Date("2007-12-01"), linetype="dashed", size=1.5, colour="#333333") + #vertical line #2
    bbc_style() + #BBC theme (fix axis labels in theme)
    labs(title="Paying more",
         subtitle = "Inflation in the Western Balkans, 2003-2022",
         y = "Harmonized index of consumer prices, annual change",
         caption = "Data for Bosnia and Herzegovina is missing.\nData source: Statistical offices of Kosovo, Albania, and Montenegro; Eurostat (Serbia and North Macedonia)") +
    geom_label(aes(x = as.Date("2017-01-01"), y = 0.1, label = "Russian Invasion\nof Ukraine"), #annotation
               hjust = 0, 
               vjust = 0.5, 
               colour = "#555555",
               label.size = NA, #border
               fill = "white", #inside border
               size = 7) +
    geom_curve(aes(x = as.Date("2019-01-01"), y = 0.11, xend = as.Date("2021-12-01"), yend = 0.14), #custom line connecting annotation with vline
               colour = "#555555", 
               curvature = -0.2,
               size=1,
               arrow = arrow(length = unit(0.03, "npc"))) + #adds arrowhead
    geom_label(aes(x = as.Date("2003-06-01"), y = 0.1, label = "Global Financial\nCrisis"), 
               hjust = 0, 
               vjust = 0.5, 
               colour = "#555555",
               label.size = NA,
               fill = "white",
               size = 7) +
    geom_curve(aes(x = as.Date("2005-06-01"), y = 0.11, xend = as.Date("2007-09-01"), yend = 0.14), 
               colour = "#555555", 
               curvature = -0.2,
               size=1,
               arrow = arrow(length = unit(0.03, "npc"))) +
    theme(plot.margin = margin(1,1,1.5,1.2, "cm"),
          legend.position='top', 
          legend.justification="left",
          legend.direction='horizontal',
          axis.title.y = element_text(size=20),
          plot.caption = element_text(hjust = 0, size=14)) #changes axis label from blank (defualt BBC theme) to preferred size

ggsave("hicp_year.png", width = 13, height = 10, dpi=300) 

################################################################################
#Shapefile Balkan Maps
#Generating an additional observation
bih <- data.frame(country="Bosnia and Herzegovina", #creating a row with Bosnia (as part of the WB6) but with missing data
                  date="2022-01-01",
                  hicp="")

wb6 <- rbind(df2,bih)

#Reducing wb6 to just a single value per country (year==2022, month latest)
wb6_s <- wb6 %>% 
    mutate(year=year(date),
           month=month(date)) %>% 
    filter(year==2022) %>% #filtering by year first
    group_by(country) %>% #grouping by country
    filter(month==6) %>% #filtering by the max month for each country
    select(country,hicp)


#Loading Shapefiles
ks0 <- read_sf("Raw Data/KO-_adm/KO__adm0.shp")
srb0 <- read_sf("Raw Data/SRB_adm/SRB_adm0.shp")
al0 <- read_sf("Raw Data/ALB_adm/ALB_adm0.shp")
mne0 <- read_sf("Raw Data/MNE_adm/MNE_adm0.shp")
mkd0 <- read_sf("Raw Data/MKD_adm/MKD_adm0.shp")
bih0 <- read_sf("Raw Data/BIH_adm/BIH_adm0.shp")


#Pre-processing shapefiles to match with each other
al01 <- al0 %>% 
    select(NAME_0)

ks01 <- ks0 %>% 
    select(NAME_ENGLI) %>% 
    rename(NAME_0=NAME_ENGLI)

srb01 <- srb0 %>% 
    select(NAME_0)

mne01 <- mne0 %>% 
    select(NAME_0)

mkd01 <- mkd0 %>% 
    select(NAME_0)

bih01 <- bih0 %>% 
    select(NAME_0)

wb6m <- rbind(ks01,srb01,al01,mne01,mkd01,bih01) %>%
    rename(country=NAME_0) %>% 
    mutate(country=replace(country, country=="Macedonia", "North Macedonia"))
plot(wb6m)

#Joining shapefiles with inflation variable hicp
wb6_map2 <- left_join(wb6m,wb6_s,by="country")

#Generating centroid and coordinates for each shapefile (country) - for some reason i have to run them separately
wb6_map2 <- wb6_map2 %>% 
    mutate(centroids = st_centroid(st_geometry(.)))

wb6_map2 <- wb6_map2 %>%
    mutate(lat = st_coordinates(wb6_map2$centroids)[,1], #latitude extraction from centroid 
           long = st_coordinates(wb6_map2$centroids)[,2]) #longitude extraction from centroid

#Map
wb6_map2 %>%
    ggplot() +
    geom_sf(color="black", aes(fill=as.numeric(hicp)), size=1) +
    scale_fill_viridis_c(label=percent, direction = -1, guide = guide_legend(title.position = "left",
                                                                             label.position = "bottom",
                                                                             keywidth = 3,
                                                                             keyheight = 0.5)) +
    geom_shadowtext(aes(label=percent(as.numeric(hicp), accuracy=0.1), x=lat, y=long, fontface="bold"), size=5) +
    geom_shadowtext(aes(label=country, x=lat, y=long, vjust=-1, fontface="bold")) +
    labs(fill="HICP",
         title="Inflation in the Western Balkans, 2022",
         subtitle="Harmonized index of consumer prices, \nannual change",
         caption="Note: Data for latest commonly available month for each country \n(June, 2022). Data for Bosnia and Herzegovina is missing.\n \nData source: Statistical offices of Kosovo, Albania, and Montenegro; \nEurostat (Serbia and North Macedonia)") +
    theme_minimal()+
    theme(plot.margin = margin(1,0,1.5,0, "cm"),
          plot.background = element_rect(color="white", fill="white"),
          plot.title = element_text(size=20, face = "bold", color = "#222222"),
          plot.subtitle = element_text(size=18),
          plot.caption = element_text(hjust = 0, size=12),
          legend.position='bottom', 
          legend.justification="right",
          legend.direction='horizontal',
          legend.title = element_text(face="bold", vjust=1),
          axis.title = element_blank(),
          axis.text = element_text(face = "bold", size = 18, color = "#222222"))

ggsave("hicp_map.png", width = 7, height = 9, dpi=300) 

################################################################################
## Kosovo Inflation Spiral

df3 <- df2 %>% 
    filter(country=="Kosovo") %>% 
    mutate(year=year(date),
           month=month(date, label=T, abbr = T))

df3 %>% 
    ggplot(aes(x=month, y=hicp, color=year, group=year)) + #we group by year so that we can have separate lines
    geom_line()

#To extend the lines before Jan and after Dec, we create data frames specifically for those times
last_dec <- df3 %>% 
    filter(month=="Dec") %>% 
    mutate(year=year+1,       #getting the December of the previous year
           month="last_Dec")

next_jan <- df3 %>% 
    filter(month=="Jan") %>% 
    mutate(year=year-1,
           month="next_Jan")

h_data <- rbind(last_dec, df3, next_jan) %>% 
    mutate(month=factor(month, levels=c("last_Dec", month.abb, "next_Jan")),
           month_number = as.numeric(month)-1,
           this_year = year==2022)

annotation <- h_data %>% 
    slice_max(year) %>%  #gets latest data (max)
    slice_max(month_number) #gets latest data (max)

#Cartesian Coordinate Line Visual
h_data %>% 
    ggplot(aes(x=month_number, y=hicp, color=year, group=year, size=this_year)) +
    geom_line() +
    geom_text(data=annotation, aes(x=month_number, y=hicp, label=year, color=year), inherit.aes=F, hjust=0, size=5, nudge_x = 0.15, fontface="bold") +
    geom_hline(yintercept=0, color="white") +
    scale_x_continuous(breaks=1:12,
                       labels=month.abb,
                       sec.axis = dup_axis(name=NULL, labels=NULL)) +
    scale_y_continuous(labels = percent,
                       breaks = c(seq(-0.05,0.15, 0.02)),
                       sec.axis = dup_axis(name=NULL, labels=NULL)) +
    scale_size_manual(breaks = c(FALSE, TRUE),
                      values = c(0.25,1),
                      guide="none") +
    coord_cartesian(xlim=c(1,12)) +
    labs(x=NULL,
         y="Harmonized index of consumer prices, annual change",
         title = "Change in the index of consumer prices of Kosovo by month, 2003-2022") +
    scale_color_viridis_c(breaks=seq(2003,2022,2),
                          guide=guide_colorbar(frame.colour = "white",
                                               frame.linewidth = 1)) +
    theme(panel.background = element_rect(fill="black", color="white", size=1),
          plot.background = element_rect(fill="#444444"),
          panel.grid = element_blank(),
          axis.text = element_text(color="white"),
          axis.ticks = element_line(color="white"),
          axis.ticks.length = unit(-5, "pt"),
          axis.title = element_text(color="white", size=13),
          plot.title = element_text(color="white", size=15),
          legend.title = element_blank(),
          legend.background = element_blank(),
          legend.text = element_text(color="white"),
          legend.key.height = unit(55, "pt"))

ggsave("hicp_lines.png", width = 8, height = 4.5)


#Polar Coordinate Inflation Spiral - Static
h_data2 <- df3 %>% rbind(df3, next_jan) %>% 
    mutate(month=factor(month, levels=c(month.abb, "next_Jan")),
           month_number = as.numeric(month)) %>% 
    arrange(year, month) %>% 
    mutate(step_number = 1:nrow(.))

inf_line <- tibble(
    x=12,
    y=c(-0.02, 0.05, 0.15),
    labels=c("-2%", "5%", "15%"))

month_labels <- tibble(
    x=1:12,
    labels = month.abb,
    y=0.22)

h_data2 %>% 
    ggplot(aes(x=month_number, y=hicp, color=year, group=year)) +
    geom_rect(aes(xmin=1, xmax=13, ymin=-0.08, ymax=0.18), color="black", fill="black", inherit.aes = F) +
    geom_hline(yintercept=c(-0.02, 0.05, 0.15), color="#cd0033", size=1.5) +
    geom_label(data=inf_line, aes(x=x, y=y, label=labels), inherit.aes = F, color="red", fill="black", label.size = 0) +
    geom_text(data=month_labels, aes(x=x, y=y, label=toupper(labels)), inherit.aes = F, color="white", size = 4) +
    geom_line(size=1) +
    geom_label(aes(x=1, y=-0.08, label=year), size=6, color="white", fill="black", label.padding = unit(1, "pt"), label.size = 0) +
    scale_x_continuous(breaks=1:12,
                       labels=month.abb, expand = c(0,0),
                       sec.axis = dup_axis(name=NULL, labels=NULL)) +
    scale_y_continuous(labels = percent,
                       breaks = c(seq(-0.05,0.15, 0.02)),
                       sec.axis = dup_axis(name=NULL, labels=NULL),
                       limits = c(-0.12,0.22), expand = c(0,-0.04)) +
    coord_polar(start=2*pi/12) +
    labs(x=NULL,
         y=NULL,
         title = "Monthly HICP in Kosovo, 2003-2022") +
    scale_color_viridis_c(breaks=seq(2003,2022,2),
                          guide="none") +
    theme(panel.background = element_rect(fill="#444444", size=10),
          plot.background = element_rect(fill="#444444", color="#444444"),
          panel.grid = element_blank(),
          axis.text = element_text(color="white"),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(color="white", size=15, hjust = 0.5),
          axis.text.y = element_blank(),
          axis.text.x = element_blank())

ggsave("hicp_spiral.png", width = 6, height = 4.5) 


##Animation
h_data2 <- df3 %>% rbind(df3, next_jan) %>% 
    mutate(month=factor(month, levels=c(month.abb, "next_Jan")),
           month_number = as.numeric(month)) %>% 
    arrange(year, month) %>% 
    mutate(step_number = 1:nrow(.))

inf_line <- tibble(
    x=12,
    y=c(-0.02, 0.05, 0.15),
    labels=c("-2%", "5%", "15%"))

month_labels <- tibble(
    x=1:12,
    labels = month.abb,
    y=0.22)

a <- h_data2 %>% 
    ggplot(aes(x=month_number, y=hicp, color=year, group=year)) +
    geom_rect(aes(xmin=1, xmax=13, ymin=-0.08, ymax=0.18), color="black", fill="black", inherit.aes = F) +
    geom_hline(yintercept=c(-0.02, 0.05, 0.15), color="#cd0033", size=1.5) +
    geom_label(data=inf_line, aes(x=x, y=y, label=labels), inherit.aes = F, color="red", fill="black", label.size = 0) +
    geom_text(data=month_labels, aes(x=x, y=y, label=toupper(labels)), inherit.aes = F, color="white", size = 4) +
    geom_line(size=1) +
    geom_label(aes(x=1, y=-0.08, label=year), size=6, color="white", fill="black", label.padding = unit(1, "pt"), label.size = 0) +
    scale_x_continuous(breaks=1:12,
                       labels=month.abb, expand = c(0,0),
                       sec.axis = dup_axis(name=NULL, labels=NULL)) +
    scale_y_continuous(labels = percent,
                       breaks = c(seq(-0.05,0.15, 0.02)),
                       sec.axis = dup_axis(name=NULL, labels=NULL),
                       limits = c(-0.12,0.22), expand = c(0,-0.04)) +
    coord_polar(start=2*pi/12) +
    labs(x=NULL,
         y=NULL,
         title = "Monthly HICP in Kosovo, 2003-2022") +
    scale_color_viridis_c(breaks=seq(2003,2022,2),
                          guide="none") +
    theme(panel.background = element_rect(fill="#444444", size=10),
          plot.background = element_rect(fill="#444444", color="#444444"),
          panel.grid = element_blank(),
          axis.text = element_text(color="white"),
          axis.ticks = element_blank(),
          axis.title = element_blank(),
          plot.title = element_text(color="white", size=15, hjust = 0.5),
          axis.text.y = element_blank(),
          axis.text.x = element_blank())+
    transition_manual(step_number, cumulative = T)

animate(a, end_pause=15, width=5, height=4.5, unit="in", res=300)
anim_save("climate_spiral4.gif")

