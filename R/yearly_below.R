#' This function compares estimated yearly yield to actual yield for portals SE and SMA.
#' @param se_performance SolarEdge Data.
#' @param sma_performance SunnyPortal Data
#' @param estimates Reference for .
#' @param status Reference list on site status.
#' @return Yearly production below estimates data.frame as an object, and also written to yearly_negative_deviation.csv.
#' @export
#' @examples
#' yearly_negative_deviation(sma_performance,se_performance,estimates,status)


yearly_negative_deviation <- function(sma_performance,se_performance,estimates,status) {
  se_filter <- left_join(se_performance %>% select(PORTAL.NAME,YEAR),
            estimates %>% select(PORTAL.NAME,Address,City,State,Postal,Email,
            Installation.Date,JOB.NAME,MonitorCode,PvEstYearlyProd), by = "PORTAL.NAME")
  sma_filter <- left_join(sma_performance %>% select(PORTAL.NAME,YEAR),
            estimates %>% select(PORTAL.NAME,Address,City,State,Postal,Email,
            Installation.Date,JOB.NAME,MonitorCode,PvEstYearlyProd), by = "PORTAL.NAME")
  se_sma_bind <- rbind(se_filter, sma_filter)
  se_sma_deviation <- se_sma_bind %>%
    transform(PERFORMANCE = (as.numeric(YEAR)/PvEstYearlyProd)*100)%>%
    left_join(status %>% select(PORTAL.NAME,STATUS, CATEGORY), by = "PORTAL.NAME")
  negative <- filter(se_sma_deviation, PERFORMANCE < 95.0)
  distinct(negative, PORTAL.NAME, .keep_all= TRUE)
  return(negative)
}
