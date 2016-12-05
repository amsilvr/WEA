library(googlesheets)
require(tidyverse)
require(lubridate)

 
  ss <- gs_title("CMAS_Alerts_Processed")
  
  msg <- gs_read(ss = ss
                 , col_names = c("rec_time", "cmac", "full_text")
                 , coltypes = "Tcc", skip = 1, trim_ws = TRUE) %>%
         mutate(rec_time = mdy_hms(gsub(" at ", " ", rec_time)
                                      , tz = "America/New_York"
                                      , truncated = 3) 
                ) %>%
         separate(full_text,
                c("blank", "gateway_id" ,"id"
                   ,"special_handling", "message_type" 
                   , "category", "response_type", "severity" 
                   , "urgency", "certainty", "expire_time"
                   , "text_language", "alert_message","dummy")
                , sep = "CMAC_[:word:]*: "
                , fill = "right" ## drops the warning for rows with too many records
                , remove = TRUE
                ) 

                
## creates a table for fields with "update" records
                
updates <- filter(msg, nchar(special_handling) < 10) %>%
                select(rec_time, cmac, gateway_id, id
                       , ref_id = special_handling
                       , special_handling = message_type
                       , message_type = category
                       , category = response_type
                       , response_type = severity
                       , severity = urgency 
                       , urgency = certainty 
                       , certainty = expire_time 
                       , text_language = alert_message 
                       , alert_message = dummy
                )

msg <- filter(msg, nchar(special_handling) >= 10) %>%
        select(-blank, -dummy)
                
## puts all the records back into a single table and 
## uses two different separators to split out the alert 
## text from the plain English "area" field
## and finally removes the tcs boilerplate
                
 msg <- bind_rows(msg, updates) %>%
        mutate(expire_time = ymd_hms(expire_time)) %>%
        separate(alert_message, c("message_text","t2")
                        , sep = "Targeted Areas: "
                        , remove = TRUE) %>%
        separate(t2, c("areas"), sep = "[:punct:]{4}"
                 , extra = "drop", remove = TRUE) %>%
        mutate(threat_type = gsub("\\. .*","", cmac)) %>%
        dplyr::filter(!(gateway_id == "http://tcs.tsis.com\n") ) 

 msg <- msg[-grep(" test", msg$threat_type),]
 
 msg_grp <- group_by(msg, year = as.factor(year(rec_time))
                     , month = as.factor(month(rec_time)))

        
 write_messages <- function(text){
         for (i in 2014:2016) {
                for (j in 1:12){
                        tt <- filter(msg_grp, year == i, month == j) %>%
                                select(year, month, threat_type)
                           if (nrow(tt) > 0) {       
                                fn <- paste("cmac", i, j, sep = "_")
                                path = paste0("data/texts/", fn, ".txt")
                                # print(fn)
                                write_lines(tt$threat_type, path = path)
                           }
                }        
         }
 }  
 
write_messages(msg_grp)
  