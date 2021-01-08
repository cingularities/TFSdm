#' This function compares estimated monthly yield to actual yield for portal SE and SMA.
#' @param se_performance SolarEdge Data.
#' @param sma_performance SunnyPortal Data
#' @param estimates Reference for .
#' @param status Reference list on site status.
#' @return Monthly production below estimates data.frame as an object, and also written to monthly_negative_deviation.csv.
#' @export
#' @examples
#' monthly_negative_deviation(sma_performance,se_performance,estimates,status)


monthly_negative_deviation <- function(sma_performance,se_performance,estimates,status) {
  se_filter <- left_join(se_performance %>% select(PORTAL.NAME,MONTH),
          estimates %>% select(PORTAL.NAME,Email,
          Installation.Date,JOB.NAME,MonitorCode,AUG,PvEstMonthlyProd), by = "PORTAL.NAME")
  sma_filter <- left_join(sma_performanc %>% select(PORTAL.NAME,MONTH),
          estimates %>% select(PORTAL.NAME,Email,
          Installation.Date,JOB.NAME,MonitorCode,AUG,PvEstMonthlyProd), by = "PORTAL.NAME")
  se_sma_bind <- rbind(se_filter, sma_filter)
  se_sma_deviation <- se_sma_bind %>%
    transform(PERFORMANCE = as.numeric(MONTH)/as.numeric(AUG))%>%
    left_join(status %>% select(PORTAL.NAME,STATUS, CATEGORY), by = "PORTAL.NAME")
  negative <- filter(se_sma_deviation, PERFORMANCE < 0.85)
  distinct(negative, PORTAL.NAME, .keep_all= TRUE)
  return(negative)
}
