#setup
require(tidyverse)
require(lubridate)
require(stringr)
require(RCurl)
require(reshape2)

#create download and save-file URLs
url <- "ftp://ftp.ncdc.noaa.gov/pub/data/swdi/stormevents/csvfiles/"

# mod_date1 <- "20160223" # As of 11/21/16, most files through 2013 have mod date of 2016-02-23
# mod_date2 <- "20161118" # As of 11/21/16, files through 2014 have mod date of 2016-11-18

dld_base <- "StormEvents_details-ftp_v1.0_d"

if (!file.exists("data")) {
        dir.create("data")
}

# Have we created the dataframe and downloaded data into it?

if (!exists("wedf")){ # If not, get to work

# All files in remote directory are loaded into the character vector
# "filenames"
 
filenames <- getURL(url, ftp.use.epsv = FALSE, dirlistonly = TRUE)
filenames <- strsplit(filenames, "\r\n")
filenames = unlist(filenames)

# detail.files contains the list of the files that we actually 
# want to download 

detail.files <- filenames[grep(dld_base, filenames)]


        filenames = ""
        for(i in 1996:2016) {
                filenames[i-1995] <- detail.files[grep(paste0(".*_d",i,"_.*"),detail.files)]
        }

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
 
wetbl <- as.tbl(
        select(wedf
        , id = EVENT_ID
        , st = STATE_FIPS
        , cz = CZ_FIPS
        , type = EVENT_TYPE
        , begin = BEGIN_DATE_TIME
        , tz = CZ_TIMEZONE
        , INJURIES_DIRECT:DAMAGE_CROPS
        , fscale = TOR_F_SCALE
        , begin_lat = BEGIN_LAT
        , begin_lon = BEGIN_LON 
        , end_lat = END_LAT
        , end_lon = END_LON
        )) %>%
        mutate(type = tolower(type)) %>%
        mutate(type = gsub("heavy wind", "high wind", type)) %>%
        mutate(type = gsub("high snow", "heavy snow", type)) %>%
        mutate(type = gsub("^hurricane$", "hurricane (typhoon)", type)) %>%
        mutate(type = gsub("landslide", "avalanche", type)) %>%
        mutate(type = gsub("thunderstorm winds?.*", "thunderstorm wind", type)) %>%
        mutate(type = gsub("volcanic ashfall", "volcanic ash", type)) %>%
        mutate(type = gsub("tornado/waterspout", "waterspout", type)) %>%
        mutate(type = str_to_title(type)) %>%
        mutate(fscale = as.factor(gsub("^E?F","F",fscale))) %>%
        dplyr::filter(type != "Northern Lights") %>%
        dplyr::filter(type != "Other") %>%
        mutate( type = as.factor(type)
                ,st = as.factor(sprintf("%02d",st))
                ,cz = as.factor(sprintf("%05d",cz))
                ,begin = dmy_hms(begin)
                ,tz = toupper(str_trunc(tz, 3, side = "r", ellipsis = ""))
                ) %>%
        arrange(begin) 

        
names(wetbl) <- tolower(names(wetbl))

# Multiply out propety and crop damage variables to get full amounts
tmp <- as.numeric(str_extract(wetbl$damage_property,"^[^HKMB]*"))
mlt <- str_extract(wetbl$damage_property,"[HKMB]$")      
tmp2 <- if_else( mlt == "H",  10^2
         , if_else(mlt == "K",  10^3
                , if_else(mlt == "M", 10^6
                        , if_else(mlt == "B", 10^9
                                    , 10^0)
                  )
        )
)

wetbl$damage_property <- tmp * tmp2

tmp <- as.numeric(str_extract(wetbl$damage_crops,"^[^HKMB]*"))
mlt <- str_extract(wetbl$damage_crops,"[HKMB]$")      
tmp2 <- if_else( mlt == "H",  10^2
                 , if_else(mlt == "K",  10^3
                           , if_else(mlt == "M", 10^6
                                     , if_else(mlt == "B", 10^9
                                               , 10^0)
                           )
                 )
)
wetbl$damage_crops <- tmp * tmp2
rm(list = c("mlt", "tmp", "tmp2"))
# Plot fatalities & injuries for tornadoes
event.damage <- transmute(wetbl
                        , id = id
                        , type
                        , begin
                        , year = as.factor(year(as.character.Date(begin)))
                        , injuries_direct 
                        , injuries_indirect 
                        , deaths_direct 
                        , deaths_indirect
                        , damage_crops
                        , damage_property
                        , fips = paste0(st,cz)
                        , fscale
                       ) %>%
                arrange(year, type)

top.t <- select(event.damage
                         , type
                         , year
                         , ends_with("direct")) %>%
        group_by(type) %>%
        summarize_at(c(3:6), sum) %>%
        top_n(5, wt = deaths_direct + injuries_direct) %>%
        mutate(type = factor(type, ordered = TRUE))

health.5 <- filter(event.damage, type %in% top.t$type) %>%
             mutate(type = factor(type, ordered = TRUE))

health.mean.5 <- select(event.damage,1:8) %>%
        filter(type %in% top.t$type) %>%
        mutate(type = factor(type)) %>%
        group_by(year, type) %>%
        summarize_at(c(5:8), mean)

health.median.5 <- select(event.damage,1:8) %>%
        filter(type %in% top.t$type) %>%
        mutate(type = factor(type)) %>%
        group_by(year, type) %>%
        summarize_at(c(5:8), median)

health.sum.5 <- select(event.damage,1:8) %>%
        filter(type %in% top.t$type) %>%
        mutate(type = factor(type)) %>%
        group_by(year, type) %>%
        summarize_at(c(5:8), sum)


hs.long <- melt(health.mean.5, id.var = c("year","type")
                ,variable.name = "casualties")
# Injuries as a result of Weather Events#
 g <- ggplot(data = filter(hs.long, casualties == "injuries_direct"), 
            mapping = aes(year, value
                          , fill = type
                          , facets = casualties
                          , ylim(0,2))
            )

# 
g + geom_bar(stat = "identity", position = "stack") + facet_grid(~casualties) + theme(axis.text.x = element_text(angle = 90), complete = FALSE)

# Total Deaths as a Direct consequence of Weather Events

hsum.long <- melt(health.sum.5, id.var = c("year","type")
     ,variable.name = "casualties")

g <- ggplot(data = filter(hsum.long, casualties == "deaths_direct"), 
            mapping = aes(year, value
                          , fill = type
                          , facets = casualties
                          , ylim(0,2))
)

# 
g + geom_bar(stat = "identity", position = "stack") + facet_grid(~casualties) + theme(axis.text.x = element_text(angle = 90), complete = FALSE)

# Plot property damage against deaths and injuries

event.damage <- mutate(event.damage, 
       wea.possible = as.logical(
               if_else(
                       as.numeric(
                           as.character(year)) >= 2012
                       , true = "TRUE"
                       , false = "FALSE")
               )
)
       
m <- ggplot(data = filter(event.damage
                  , type %in% c("Flash Flood"
                                , "Thunderstorm Wind"
                                , "Tornado")
                  , damage_property > 0
                  ) %>% group_by(wea.possible)
            , mapping = aes(deaths_direct + injuries_direct
                            , damage_property / 1000000
                            , color = type
                            , facets = wea.possible))
m + geom_point(na.rm = TRUE) + facet_grid(~wea.possible) + xlim(0,50) + ylim(0,300) + geom_smooth(method = "lm", na.rm = TRUE, fullrange = TRUE)

# Strength of Tornado (Fscale), vs deaths and injuries
# Looking at the mean direct deaths and injuries pre and post 
# the implementation of WEA. 
# 
# There has only been one F5 tornado since 2012, and  are more rare and so much more deadly that 
# make the decreases in lower strength tornadoes hard to see
# They are ommitted from this graph.

tornado <- filter(event.damage, type == "Tornado") %>%
#        select(begin, year:wea.possible) %>%
        mutate(fscale = as.factor(
                str_extract(fscale, "[0-9]"))) %>%
        filter(!is.na(fscale), as.numeric(fscale) <= 5) %>%
        group_by(wea.possible)

big_tornado <- filter(tornado, as.numeric(fscale) > 4)

tornado_summary <- summarise_at(group_by(tornado
                                         , wea.possible
                                         , fscale)
                                , c("deaths_direct"
                                    , "injuries_direct"
                                    # , "deaths_indirect"
                                    # , "injuries_indirect"
                                    )
                                , mean
                                ) %>%        
        filter(as.numeric(fscale) < 6)


tornado_summary <- melt(tornado_summary
     , id.vars = c("wea.possible","fscale")
     , variable.name = "type"
     , value.name = "casualty")

n <- ggplot(data = tornado_summary
             , mapping = aes(fscale
                             , casualty
                             , facets = type
                             , fill = wea.possible
                             ))
n + geom_bar(stat = "identity", position = "dodge") + 
        facet_wrap(~type,nrow = 2 ,labeller = label_both, scales = "free") +
        labs(title = "Decreased Tornado Casualties Post-WEA"
             , x = "Tornado Strength (F Scale)"
             , y = "Mean Casualties") +
        scale_fill_brewer(palette = "Set3") + 
        theme_minimal()
        
