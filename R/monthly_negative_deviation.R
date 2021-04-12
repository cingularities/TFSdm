#' This function compares estimated monthly yield to actual yield for portal SE and SMA.
#' @param se_performance SolarEdge Data.
#' @param sma_performance SunnyPortal Data
#' @param estimates Reference for .
#' @param status Reference list on site status.
#' @return Monthly production below estimates data.frame as an object, and also written to monthly_negative_deviation.csv.
#' @export
#' @examples
#' monthly_negative_deviation(sma_performance,se_performance,estimates,status)


monthly_negative_deviation <- function(sma_performance_raw,se_performance_raw,estimates,status) {
  se_performance <- se_performance_raw %>% select(-c(1,3)) %>%
    slice(-c(1:8)) %>%
    rename_(PORTAL.NAME = 1, MONTH = 2) %>%
    mutate(PORTAL.SERVER = "SE")  %>%
    select(PORTAL.NAME, MONTH, PORTAL.SERVER) %>%
    left_join(estimates %>% select(PORTAL.NAME,Email, 
                                Installation.Date,JOB.NAME,MonitorCode,MAR,PvEstMonthlyProd), by = "PORTAL.NAME") %>%
    left_join(status %>% select(PORTAL.NAME, STATUS, CATEGORY, DATE, YEAR, NOTES), by = "PORTAL.NAME")%>% #joins recent down dm list with current portal status
    distinct(PORTAL.NAME, .keep_all= TRUE)
  
  sma_performance <- sma_performance_raw %>% select(-c(2,3,4,6,7,8)) %>%
    rename_(PORTAL.NAME = 1, MONTH = 2, YEAR = 3) %>%
    mutate(PORTAL.SERVER = "SMA") %>%
    select(PORTAL.NAME, MONTH, PORTAL.SERVER) %>%
    left_join(estimates %>% select(PORTAL.NAME,Email, Installation.Date,JOB.NAME,MonitorCode,MAR,PvEstMonthlyProd), by = "PORTAL.NAME") %>%
    left_join(status %>% select(PORTAL.NAME, STATUS, CATEGORY, DATE, YEAR, NOTES), by = "PORTAL.NAME")%>% #joins recent down dm list with current portal status
    distinct(PORTAL.NAME, .keep_all= TRUE)
  
  se_sma_bind <- rbind(se_performance, sma_performance)
  
  se_sma_deviation <- se_sma_bind %>%
    transform(PERFORMANCE = as.numeric(MONTH)/as.numeric(MAR))%>%
    left_join(status %>% select(PORTAL.NAME,STATUS, CATEGORY), by = "PORTAL.NAME")
  
  negative <- filter(se_sma_deviation, PERFORMANCE < 0.85)
  
  return(negative)
}
