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
      se <- se_raw %>% select(-c(1,3)) %>%
        slice(-c(1:8)) %>%
        rename_(PORTAL.NAME = 1, Severity = 2, kWh =3) %>%
        mutate(PORTAL.SERVER = "SE")
      se_filter <- se %>%
        filter(Severity > "1" | kWh == "0") %>% #filters sites that have high and low severity, no data and 0 production
        left_join(status %>% select(PORTAL.NAME, STATUS, CATEGORY, DATE, YEAR, NOTES), by = "PORTAL.NAME") #joins recent down dm list with current portal status
      sma <- sma_raw %>% select(-c(2,3,5,6,7,8,9)) %>%
        rename_(PORTAL.NAME = 1, YESTERDAY = 2) %>%
        mutate(PORTAL.SERVER = "SMA")
      sma_filter <- sma %>%
        filter(YESTERDAY == "No data" | YESTERDAY == "0") %>% #filters sites that have no data and 0 production
        left_join(status %>% select(PORTAL.NAME, STATUS, CATEGORY, DATE, YEAR, NOTES), by = "PORTAL.NAME") #joins recent down dm list with current portal status
      sp <- sp_raw %>% select(-c(2,4,8,9,10)) %>%
        rename_(PORTAL.NAME = 1) %>%
        mutate(PORTAL.SERVER = "SP") %>%
        mutate(PORTAL.NAME = str_squish(PORTAL.NAME))
      sp_filter <- sp %>% 
        filter(Status == "Open") %>% #filters sites that have open alerts
        left_join(status %>% select(PORTAL.NAME, STATUS, CATEGORY, DATE, YEAR, NOTES), by = "PORTAL.NAME") #joins recent down dm list with current portal status
      sp_filter <- distinct(sp_filter, PORTAL.NAME, .keep_all= TRUE) #removes duplicates
      lg <- lg_raw %>% select(-c(1,5,6,7,8,9,10,11)) %>%
        rename_(PORTAL.NAME = 1) %>%
        mutate(PORTAL.SERVER = "LG")
      lg_filter <- lg %>%
        filter(Status != "Normal") %>% #filters sites are not in a normal state
        left_join(status %>% select(PORTAL.NAME, STATUS, CATEGORY, DATE, YEAR, NOTES), by = "PORTAL.NAME") #joins recent down dm list with current portal status
      se_final <- left_join(se_filter %>% select(PORTAL.NAME, STATUS, CATEGORY, DATE, YEAR, NOTES, PORTAL.SERVER), ##joins filtered & joined recent down dm list with the portal key to gather FACILITY.NAME and JOB.NAME
                            clients, by = "PORTAL.NAME")
      sma_final <- left_join(sma_filter %>% select(PORTAL.NAME, STATUS, CATEGORY, DATE, YEAR, NOTES, PORTAL.SERVER), ##joins filtered & joined recent down dm list with the portal key to gather FACILITY.NAME and JOB.NAME
                             clients, by = "PORTAL.NAME")
      sp_final <- left_join(sp_filter %>% select(PORTAL.NAME, STATUS, CATEGORY, DATE, YEAR, NOTES, PORTAL.SERVER), ##joins filtered & joined recent down dm list with the portal key to gather FACILITY.NAME and JOB.NAME
                            clients, by = "PORTAL.NAME")
      lg_final <- left_join(lg_filter %>% select(PORTAL.NAME, STATUS, CATEGORY, DATE, YEAR, NOTES, PORTAL.SERVER), ##joins filtered & joined recent down dm list with the portal key to gather FACILITY.NAME and JOB.NAME
                            clients, by = "PORTAL.NAME")
      se_sma_sp_lg <- rbind(se_final, sma_final, sp_final, lg_final)  #binds all sites to one list
      se_sma_sp_lg <- distinct(se_sma_sp_lg, PORTAL.NAME, .keep_all= TRUE) #removes duplicates, mostly SP multiple alert system
      return(se_sma_sp_lg) #returns the final file
    }
