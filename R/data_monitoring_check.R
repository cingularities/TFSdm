#' This function filters down data monitoring/inverter down and merges with reference files.
#' @param se SolarEdge Data.
#' @param sma SunnyPortal Data
#' @param sp SunPower Data.
#' @param lg Life's Good Data.
#' @param status Reference list on site status.
#' @param clients Reference for customer job information.
#' @return Updated recent down data monitoring data.frame as an object, and also written to recent_down_dm.csv.
#' @export
#' @examples
#' data_monitoring_check<- function(se, sma, sp,lg, status, clients)

##FUNCTION
data_monitoring_check<- function(se_raw, sma_raw, sp_raw, lg_raw, status, clients) {
      se <- se_raw %>% select(-c(1,3)) %>% #removes unwanted columns
        slice(-c(1:8)) %>% #removes unwanted rows
        rename(PORTAL.NAME = 1, Severity = 2, kWh =3) %>% #renames columns
        mutate(PORTAL.SERVER = "SE")  %>% #adds PORTAL.SERVER column
        filter(Severity > "0" | kWh == "0") %>% #filters sites that have high and low severity, no data and 0 production
        select(PORTAL.NAME, PORTAL.SERVER) %>% #selects columns wanted
        left_join(status %>% select(PORTAL.NAME, STATUS, CATEGORY, DATE, YEAR, NOTES), by = "PORTAL.NAME")%>% #joins recent down dm list with current portal status
        left_join(clients, by = "PORTAL.NAME") #joins filtered & joined recent down dm list with the portal key to gather FACILITY.NAME and JOB.NAME
      
      sma <- sma_raw %>% select(-c(2,3,5,6,7,8,9)) %>% #removes unwanted columns
        rename(PORTAL.NAME = 1, YESTERDAY = 2) %>% #renames column
        mutate(PORTAL.SERVER = "SMA")%>% #adds PORTAL.SERVER column
        filter(YESTERDAY == "No data" | YESTERDAY == "0") %>% #filters sites that have no data and 0 production
        select(PORTAL.NAME, PORTAL.SERVER)%>% #selects columns wanted
        left_join(status %>% select(PORTAL.NAME, STATUS, CATEGORY, DATE, YEAR, NOTES), by = "PORTAL.NAME")%>% #joins recent down dm list with current portal status
        left_join(clients, by = "PORTAL.NAME")#joins filtered & joined recent down dm list with the portal key to gather FACILITY.NAME and JOB.NAME
      
      sp <- sp_raw %>% select(-c(2,4,8,9,10)) %>% #removes unwanted columns 
        rename(PORTAL.NAME = 1) %>% #renames column
        mutate(PORTAL.SERVER = "SP") %>% #adds PORTAL.SERVER column
        mutate(PORTAL.NAME = str_squish(PORTAL.NAME))%>% #removes double spaces
        filter(Status == "Open") %>% #filters sites that have open alerts
        select(PORTAL.NAME, PORTAL.SERVER) %>% #selects the columns we want
        left_join(status %>% select(PORTAL.NAME, STATUS, CATEGORY, DATE, YEAR, NOTES), by = "PORTAL.NAME")%>% #joins recent down dm list with current portal status
        left_join(clients, by = "PORTAL.NAME")%>% #joins filtered & joined recent down dm list with the portal key to gather FACILITY.NAME and JOB.NAME
        distinct(PORTAL.NAME, .keep_all= TRUE) #removes duplicates
    
      lg <- lg_raw %>% select(-c(1,5,6,7,8,9,10,11)) %>% #removes unwanted columns
        rename(PORTAL.NAME = 1) %>% #renames column
        mutate(PORTAL.SERVER = "LG") %>% #adds PORTAL.SERVER column
        filter(Status != "Normal") %>% #filters sites are not in a normal state
        select(PORTAL.NAME, PORTAL.SERVER) %>% #selects the columns we want
        left_join(status %>% select(PORTAL.NAME, STATUS, CATEGORY, DATE, YEAR, NOTES), by = "PORTAL.NAME")%>% #joins recent down dm list with current portal status
        left_join(clients, by = "PORTAL.NAME")#joins filtered & joined recent down dm list with the portal key to gather FACILITY.NAME and JOB.NAME

      se_sma_sp_lg <- rbind(se, sma, sp, lg)  #binds all sites to one list
      se_sma_sp_lg <- distinct(se_sma_sp_lg, PORTAL.NAME, .keep_all= TRUE) #removes duplicates, mostly SP multiple alert system
      return(se_sma_sp_lg) #returns the final file
    }
