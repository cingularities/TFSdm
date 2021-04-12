#' This function compares estimated yearly yield to actual yield for portals SE and SMA.
#' @param se_performance SolarEdge Data.
#' @param sma_performance SunnyPortal Data
#' @param estimates Reference for .
#' @param status Reference list on site status.
#' @return Yearly production above estimates data.frame as an object, and also written to yearly_positive_deviation.csv.
#' @export
#' @examples
#' yearly_positive_deviation(sma_performance,se_performance,estimates,status)



yearly_positive_deviation <- function(sma_performance,se_performance,estimates,status) {
  se_performance <- se_performance_raw %>% select(-c(1,3)) %>%
    slice(-c(1:8)) %>%
    rename_(PORTAL.NAME = 1, YEAR = 2) %>%
    mutate(PORTAL.SERVER = "SE")  %>%
    select(PORTAL.NAME, YEAR, PORTAL.SERVER) %>%
    left_join(estimates %>% select(PORTAL.NAME,Address,City,State,Postal,Email,Installation.Date,JOB.NAME,MonitorCode,PvEstYearlyProd), by = "PORTAL.NAME")
  
  sma_performance <- sma_performance_raw %>% select(-c(2,3,4,6,7,8)) %>%
    rename_(PORTAL.NAME = 1, MONTH = 2, YEAR = 3) %>%
    mutate(PORTAL.SERVER = "SMA") %>%
    select(PORTAL.NAME, YEAR, PORTAL.SERVER) %>%
    left_join(estimates %>% select(PORTAL.NAME,Address,City,State,Postal,Email,Installation.Date,JOB.NAME,MonitorCode,PvEstYearlyProd), by = "PORTAL.NAME")
  
  se_sma_bind <- rbind(se_performance, sma_performance)
  
  se_sma_deviation <- se_sma_bind %>%
    transform(PERFORMANCE = (as.numeric(YEAR)/PvEstYearlyProd)*100)%>%
    left_join(status %>% select(PORTAL.NAME,STATUS, CATEGORY), by = "PORTAL.NAME")
  
  positive <- filter(se_sma_deviation, PERFORMANCE > 95.0)
  
  distinct(positive, PORTAL.NAME, .keep_all= TRUE)
  
  return(positive)
}
