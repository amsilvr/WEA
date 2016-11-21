#setup
require(tidyverse)
require(lubridate)
require(stringr)

#create download and save-file URLs
dldir <- "https://www1.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/"
d <- c(1990:2016)

mod_date1 <- "20160223" # As of 11/21/16, most files through 2013 have mod date of 2016-02-23
mod_date2 <- "20161118" # As of 11/21/16, files through 2014 have mod date of 2016-11-18

dld_base <- "StormEvents_details-ftp_v1.0_d"
dlf_base <- "StormEvents_fatalities-ftp_v1.0_d"
dll_base <- "StormEvents_locations-ftp_v1.0_d"
        
if (!file.exists("data")) {
        dir.create("data")
}

# get 2006 file w/ later mod date

df_name <- paste0(dld_base,2006,"_c",mod_date2,".csv.gz")
temp <- paste0("data/",df_name)
fileURL <- paste0(dldir, df_name)
if (!file.exists(temp)) {
        download.file(url = fileURL, destfile = temp)
}
# get files from 1995 - 2013, if they don't already exist
for(i in 1995:2013) {
        df_name <- paste0(dld_base,i,"_c",mod_date1,".csv.gz")
        temp <- paste0("data/",df_name)
        fileURL <- paste0(dldir, df_name)
        if (!file.exists(temp)) {
                download.file(url = fileURL, destfile = temp)
        }
}
# get files with the later download date
# 
for(i in 2014:2016) {
        df_name <- paste0(dld_base,i,"_c",mod_date2,".csv.gz")
        temp <- paste0("data/",df_name)
        fileURL <- paste0(dldir, df_name)
        if (!file.exists(temp)) {
                download.file(url = fileURL, destfile = temp)
        }
}
files <- dir("data/")

SED <- read_csv(file = (paste0("data/", files[1]))
                 , col_names = TRUE
                 , col_types = c(
                         "i" #BEGIN_YEARMONTH
                         ,"i" #BEGIN_DAY
                         ,"t" #BEGIN_TIME
                         ,"i" #END_YEARMONTH
                         ,"i" #END_DAY
                         ,"t" #END_TIME
                         ,"i" #EPISODE_ID
                         ,"i" #EVENT_ID
                         ,"c" #STATE
                         ,"i" #STATE_FIPS
                         ,"i" #YEAR
                         ,"-" #MONTH_NAME
                         ,"c" #EVENT_TYPE
                         ,"c" #CZ_TYPE
                         ,"i" #CZ_FIPS
                         ,"c" #CZ_NAME
                         ,"c" #WFO
                         ,"c" #BEGIN_DATE_TIME
                         ,"c" #CZ_TIMEZONE
                         ,"c" #END_DATE_TIME
                         ,"i" #INJURIES_DIRECT
                         ,"i" #INJURIES_INDIRECT
                         ,"i" #DEATHS_DIRECT
                         ,"i" #DEATHS_INDIRECT
                         ,"d" #MAGNITUDE 
                         ,"d" #TOR_LENGTH
                         ,"i" #TOR_WIDTH
                         ,"i" #BEGIN_RANGE
                         ,"i" #END_RANGE = col_integer(),
                         ,"d" #BEGIN_LAT = col_double()
                         ,"d" #END_AZIMUTH
                         ,"d" #END_LOCATION
                         ,"d" #BEGIN_LAT         
                         ,"d" #BEGIN_LON
                         ,"d" #END_LAT
                         ,"d" #END_LON
                         ,"c" #EPISODE_NARRATIVE"  
                         ,"c" #"EVENT_NARRATIVE"
                         ,"c" #"DATA_SOURCE" 
                 )
)

for (j in 2:length(files)){
        message(c("reading ",files[j]))
        temp <- read_csv(paste0("data/",files[j]))
        SED <- rbind(SED, temp)
        j = j+1
}

# Select relevant columns

w_events <- select(SED
        , id = EVENT_ID
        , st = STATE_FIPS
        , cz = CZ_FIPS
        , type = EVENT_TYPE
        , begin = BEGIN_DATE_TIME
        , tz = CZ_TIMEZONE
        , INJURIES_DIRECT:DAMAGE_CROPS
        ) %>%
        mutate(type = tolower(type)) %>%
        mutate(type = gsub("heavy wind", "high wind", type)) %>%
        mutate(type = gsub("high snow", "heavy snow", type)) %>%
        mutate(type = gsub("^hurricane$", "hurricane (typhoon)", type)) %>%
        mutate(type = gsub("landslide", "avalanche", type)) %>%
        mutate(type = gsub("thunderstorm winds?.*", "thunderstorm wind", type))%>%
        mutate(type = gsub("volcanic ashfall", "volcanic ash", type)) %>%
        mutate(type = gsub("tornado/waterspout", "waterspout", type)) %>%
        dplyr::filter(type != "northern lights") %>%
        dplyr::filter(type != "other")
        
names(w_events) <- tolower(names(w_events))

# r pattern_matching
# 

all_events <- as.data.frame(read.csv("data/event_types.csv", header = FALSE, strip.white = TRUE))
names(all_events) <- "type"



