#setup
require(tidyverse)
require(lubridate)
require(stringr)

#create download and save-file URLs
url <- "ftp://ftp.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/"

# mod_date1 <- "20160223" # As of 11/21/16, most files through 2013 have mod date of 2016-02-23
# mod_date2 <- "20161118" # As of 11/21/16, files through 2014 have mod date of 2016-11-18

dld_base <- "StormEvents_details-ftp_v1.0_d"
# dlf_base <- "StormEvents_fatalities-ftp_v1.0_d"
# dll_base <- "StormEvents_locations-ftp_v1.0_d"
        
if (!file.exists("data")) {
        dir.create("data")
}

filenames <- getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
filenames <- strsplit(filenames, "\r\n")
filenames = unlist(filenames)

detail.files <- filenames[grep(dld_base, filenames)]
if (!exists("filenames")){
        filenames = ""
        for(i in 1995:2016) {
                filenames[i-1994] <- detail.files[grep(paste0(".*_d",i,"_.*"),detail.files)]
        }
}

if (!exists("wedf")){
        wedf <- data.frame()
        # read files into Weather Event Data Frame (wedf)
        for(filename in filenames) {
         message("downloading and parsing ", filename)        
         wedf <- bind_rows(wedf,
                read_csv(file = (paste0(url, filename))
                          , col_types = 
                                cols(BEGIN_YEARMONTH = "i",
                                 BEGIN_DAY = "i",
                                 BEGIN_TIME = "c",
                                 END_YEARMONTH = "i",
                                 END_DAY = "i",
                                 END_TIME = "c",
                                 EPISODE_ID = "i",
                                 EVENT_ID = "i",
                                 STATE = "c",
                                 STATE_FIPS = "i",
                                 YEAR = "i",
                                 MONTH_NAME = "c",
                                 EVENT_TYPE = "c",
                                 CZ_TYPE = "c",
                                 CZ_FIPS = "i",
                                 CZ_NAME = "c",
                                 WFO = "c",
                                 BEGIN_DATE_TIME = "c",
                                 CZ_TIMEZONE = "c",
                                 END_DATE_TIME = "c",
                                 INJURIES_DIRECT = "i",
                                 INJURIES_INDIRECT = "i",
                                 DEATHS_DIRECT = "i",
                                 DEATHS_INDIRECT = "i",
                                 DAMAGE_PROPERTY = "c",
                                 DAMAGE_CROPS = "c",
                                 SOURCE = "c",
                                 MAGNITUDE = "d",
                                 MAGNITUDE_TYPE = "c",
                                 FLOOD_CAUSE = "c",
                                 CATEGORY = "d",
                                 TOR_F_SCALE = "c",
                                 TOR_LENGTH = "d",
                                 TOR_WIDTH = "d",
                                 TOR_OTHER_WFO = "c",
                                 TOR_OTHER_CZ_STATE = "c",
                                 TOR_OTHER_CZ_FIPS = "i",
                                 TOR_OTHER_CZ_NAME = "c",
                                 BEGIN_RANGE = "i",
                                 BEGIN_AZIMUTH = "c",
                                 BEGIN_LOCATION = "c",
                                 END_RANGE = "i",
                                 END_AZIMUTH = "c",
                                 END_LOCATION = "c",
                                 BEGIN_LAT = "d",
                                 BEGIN_LON = "d",
                                 END_LAT = "d",
                                 END_LON = "d",
                                 EPISODE_NARRATIVE = "c",
                                 EVENT_NARRATIVE = "c",
                                 DATA_SOURCE = "c"
                          )              
                )
         )
        }

}
# Select relevant columns into the Weather Event Tbl (wetbl)
wetbl <-as.tbl( 
        select(wedf
        , id = EVENT_ID
        , st = STATE_FIPS
        , cz = CZ_FIPS
        , type = EVENT_TYPE
        , begin = BEGIN_DATE_TIME
        , tz = CZ_TIMEZONE
        , INJURIES_DIRECT:DAMAGE_CROPS
        ) )%>%
        mutate(type = tolower(type)) %>%
        mutate(type = gsub("heavy wind", "high wind", type)) %>%
        mutate(type = gsub("high snow", "heavy snow", type)) %>%
        mutate(type = gsub("^hurricane$", "hurricane (typhoon)", type)) %>%
        mutate(type = gsub("landslide", "avalanche", type)) %>%
        mutate(type = gsub("thunderstorm winds?.*", "thunderstorm wind", type))%>%
        mutate(type = gsub("volcanic ashfall", "volcanic ash", type)) %>%
        mutate(type = gsub("tornado/waterspout", "waterspout", type)) %>%
        dplyr::filter(type != "northern lights") %>%
        dplyr::filter(type != "other") %>%
        mutate(type = as.factor(type))
names(wetbl) <- tolower(names(wetbl))

# Read Dates
# Plot fatalities & injuries for tornadoes
# 

