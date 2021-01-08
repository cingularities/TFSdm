#' This function filters down data monitoring/inverter down and merges with reference files.
#' @param se SolarEdge Data.
#' @param sma SunnyPortal Data
#' @param sp SunPower Data.
#' @param staus Reference list on site status.
#' @param clients Reference for customer job information.
#' @return Updated recent down data monitoring data.frame as an object, and also written to recent_down_dm.csv.
#' @export
#' @examples
#' data_monitoring_check<- function(se, sma, sp, status, clients)

##FUNCTION
data_monitoring_check<- function(se, sma, sp, status, clients) {
  se_filter <- se %>%
    filter(Severity == "High" | Severity == "Low" | Severity == "No Data" | kWh == "0") %>%
    left_join(status, by = "PORTAL.NAME")
  sma_filter <- sma %>%
    filter(YESTERDAY == "No data" | YESTERDAY == "0") %>%
    left_join(status, by = "PORTAL.NAME")
  sp_filter <- sp %>%
    filter(Status == "Open") %>%
    left_join(status, by = "PORTAL.NAME")
  se_final <- left_join(se_filter %>% select(PORTAL.NAME, STATUS, CATEGORY),
                        clients, by = "PORTAL.NAME")
  sma_final <- left_join(sma_filter %>% select(PORTAL.NAME, STATUS, CATEGORY),
                         clients, by = "PORTAL.NAME")
  sp_final <- left_join(sp_filter %>% select(PORTAL.NAME, STATUS, CATEGORY),
                        clients, by = "PORTAL.NAME")
  se_sma_sp <- rbind(se_final, sma_final, sp_final)
  se_sma_sp <- distinct(se_sma_sp, PORTAL.NAME, .keep_all= TRUE)
  return(se_sma_sp)
}
