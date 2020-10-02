
library("RSocrata")
library(lubridate)
library(tidyverse)
library(magrittr)
library(dplyr)
library(ggplot2)
library(CausalImpact)
library(GGally)
library(skimr)
library(hrbrthemes)
library(fpp)
library(lattice)
library(padr)
library(Hmisc)

''' This script will first show how to download the data from the API, and then 
will illustrate all the steps to required to pre-process the data in order 
to use them for the SBTS models. It is worth noting that, by downloading the
data from the LA Open Data API, one needs to filter the dates presented in the
paper if aiming to replicate that analyses. Furthermore, given that the LAPD 
often inserts new crime reports with a significant delay, statistical outcomes may 
differ from the ones presented in the paper '''

# Read-in 2010-2019 dataset from LA Open Data API

df1719 <- read.socrata(
  "https://data.lacity.org/resource/63jg-8b9z.json?$where=date_rptd > '2017-01-01T00:00:00.000'",
  app_token = "...", #add your app token
  email     = "...", #add your email
  password  = "..." #add your password
)



#Read in 2020-present dataset from LA Open Data API

df20 = read.socrata(
  "https://data.lacity.org/resource/2nrs-mtv8.json",
  app_token = "...", #add your app token
  email     = "...", #add your email
  password  = "..." #add your password
)

#read in holidays and weather dataset

getwd()
setwd("C:/")
hol <- read.csv("C:/covariates/climate_and_holidays17_0804csv.csv", header=TRUE)


# merge both datasets 

df_la <- rbind(df1719, df20)



##### Subsetting dataset based on crime category

#battery
battery <- df_la[df_la$crm_cd_desc==
                   "BATTERY - SIMPLE ASSAULT",]

#burglary
burglary <- df_la[df_la$crm_cd_desc==
                    "BURGLARY",]

# theft (petty+grand) 
theft <- df_la[(df_la$crm_cd_desc==
                  "THEFT PLAIN - PETTY ($950 & UNDER)") | 
                 (df_la$crm_cd_desc==
                    "THEFT-GRAND ($950.01 & OVER)EXCPT")  |
                 (df_la$crm_cd_desc==
                    "THEFT-GRAND ($950.01 & OVER)EXCPT,GUNS,FOWL,LIVESTK,PROD"),]

# assault with deadly weapon 
assault_dw <- df_la[(df_la$crm_cd_desc==
                       "ASSAULT WITH DEADLY WEAPON")| 
                      (df_la$crm_cd_desc=="ASSAULT WITH DEADLY WEAPON, AGGRAVATED ASSAULT"),]

# robbery
robbery <-  df_la[df_la$crm_cd_desc==
                    "ROBBERY",]


# shoplifting (petty+grand)
shoplifting <- df_la[(df_la$crm_cd_desc==
                        "SHOPLIFTING - PETTY THEFT ($950 & UNDER)") |
                       (df_la$crm_cd_desc==
                          "SHOPLIFTING-GRAND THEFT ($950.01 & OVER)"),]


# stolen vehicle
vehicle <- df_la[df_la$crm_cd_desc==
                   "VEHICLE - STOLEN",]

#homicide
homicide <- df_la[df_la$crm_cd_desc==
                    "CRIMINAL HOMICIDE",]

# intimate partner assault (simple + aggravated)
intimate <- df_la[(df_la$crm_cd_desc==
                     "INTIMATE PARTNER - AGGRAVATED ASSAULT") |
                    (df_la$crm_cd_desc==
                       "INTIMATE PARTNER - SIMPLE ASSAULT"),]


#count - general aggregation by date (day)

la_agg<-aggregate(df_la, by=list(df_la$date_rptd), length)

p <- ggplot(la_agg, aes(x=Group.1, y=dr_no)) +
  geom_line() + 
  xlab("")



################### aggregation by day
data_list <-list(assault_dw, battery, burglary, 
                 intimate, robbery, shoplifting, 
                 theft, vehicle, df_la)


# battery
battery_ag <-aggregate(battery, by=list(battery$date_rptd), length)

# burglary
burglary_ag <- aggregate(burglary, by=list(burglary$date_rptd), length)

#  theft
theft_ag <- aggregate(theft, by=list(theft$date_rptd), length)

# assault with d.w.
assault_ag <- aggregate(assault_dw, by=list(assault_dw$date_rptd), length)

# robbery
robbery_ag <- aggregate(robbery, by=list(robbery$date_rptd), length)

# shoplifting
shoplifting_ag <- aggregate(shoplifting, by=list(shoplifting$date_rptd), length)

# stolen vehicle
vehicle_ag <- aggregate(vehicle, by=list(vehicle$date_rptd), length)

# intimate assault
intimate_ag <- aggregate(intimate, by=list(intimate$date_rptd), length)

# criminal homicide
homicide_ag <- aggregate(homicide, by=list(homicide$date_rptd), length)
length_hom <- length(homicide_ag$Group.1)
date.min <- homicide_ag$Group.1[1]
date.max <- homicide_ag$Group.1[590]
all.dates <-seq(date.min, date.max, by="day")
all.dates.frame <- data.frame(list(time=all.dates))
merged.data <- merge(all.dates.frame, homicide_ag, all=F)



###----------------------------- Further processing :  keep only those columns we need for the anaysis calling the following object

tokeep <- c("Group.1", "dr_no")

# create reduced datasets and transform into ts object for the analysis
la_agg2 <-la_agg[tokeep]

# battery time series
battery_ag <-battery_ag[tokeep]
battery_ts <- read.zoo(battery_ag, index.column = 1, sep = ",", format = "%Y-%m-%d")

# burglary time series
burglary_ag <- burglary_ag[tokeep]
burglary_ts <- read.zoo(burglary_ag, index.column = 1, sep = ",", format = "%Y-%m-%d")

#petty theft time series
theft_ag <- theft_ag[tokeep]
theft_ts <- read.zoo(theft_ag, index.column = 1, sep = ",", format = "%Y-%m-%d")

# assault time series
assault_ag <- assault_ag[tokeep]
assault_ts <- read.zoo(assault_ag, index.column = 1, sep = ",", format = "%Y-%m-%d")


#robbery time series
robbery_ag <- robbery_ag[tokeep]
robbery_ts <- read.zoo(robbery_ag, index.column = 1, sep = ",", format = "%Y-%m-%d")


# shoplifting time series
shoplifting_ag <- shoplifting_ag[tokeep]
shoplifting_ts <- read.zoo(shoplifting_ag, index.column = 1, sep = ",", format = "%Y-%m-%d")

# stolen vehicle time series
vehicle_ag <- vehicle_ag[tokeep]
vehicle_ts <- read.zoo(vehicle_ag, index.column = 1, sep = ",", format = "%Y-%m-%d")

# intimate time series
intimate_ag <- intimate_ag[tokeep]
intimate_ts <- read.zoo(intimate_ag, index.column = 1, sep = ",", format = "%Y-%m-%d")

# homicide time series
homicide_ag <- homicide_ag[tokeep]
homicide_ts <- read.zoo(homicide_ag, index.column = 1, sep = ",", format = "%Y-%m-%d")
dt <- seq(start(homicide_ts), end(homicide_ts)+3, by = "day")
homicide_ts <-merge(homicide_ts, zoo(,dt))
homicide_ts<-na.fill(homicide_ts, 0)

# homicide
homicide_first <- window(homicide_ts, 
                         start=as.POSIXct("2017-01-02"), end=as.POSIXct("2020-03-28"))



homicide_first_merged <-window(homicide_merged, 
                               start=as.POSIXct("2017-01-02"), end=as.POSIXct("2020-03-28"))
# all los angeles data time series
la_ts <- read.zoo(la_agg2, index.column = 1, sep = ",", format = "%Y-%m-%d")


############# merging test to include control series
hol2 <- dplyr::select(hol, DATE, HOLIDAY, TMAX)
hol_ts <-read.zoo(hol2, index_column=1, sep=",", format="%Y-%m-%d")

merged<-merge(la_ts, hol_ts, all=FALSE)


####### final preparation of datasets for causal impact (univariate vs multivariate)


assault_merged<-merge(assault_ts, hol_ts, all=FALSE)
battery_merged<-merge(battery_ts, hol_ts, all=FALSE)
burglary_merged<-merge(burglary_ts, hol_ts, all=FALSE)
intimate_merged<-merge(intimate_ts, hol_ts, all=FALSE)
robbery_merged<-merge(robbery_ts, hol_ts, all=FALSE)
shoplifting_merged<-merge(shoplifting_ts, hol_ts, all=FALSE)
theft_merged<-merge(theft_ts, hol_ts, all=FALSE)
vehicle_merged<-merge(vehicle_ts, hol_ts, all=FALSE)
homicide_merged<-merge(homicide_ts, hol_ts, all=FALSE)


