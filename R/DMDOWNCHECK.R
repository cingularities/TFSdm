#' This function filters down data monitoring/inverter down and merges with reference files.
#' @param SE SolarEdge Data.
#' @param SMA SunnyPortal Data
#' @param SP SunPower Data.
#' @param RECENTDDMLIST Reference list on site status.
#' @param FINALPORTALKEY Reference for customer job information.
#' @return Updated RECENTDDMALIST.
#' @export
#' @examples
#' DMDOWNCHECK(SE, SMA, SP, RECENTDDMLIST, FINALPORTALKEY)


##FUNCTION
DMDOWNCHECK <- function(SE, SMA, SP, RECENTDDMLIST, FINALPORTALKEY) {
  SEDMCHECKFILTER <- filter(SE, Severity == "High" | Severity == "Low" | Severity == "No Data" | kWh == "0")
  SEDMCHECKLEFTJOIN <- left_join(SEDMCHECKFILTER, RECENTDDMLIST, by = "PORTAL.NAME")
  SEDMCHECKFINALJOIN <- left_join(SEDMCHECKLEFTJOIN[c("PORTAL.NAME","STATUS","CATEGORY")], FINALPORTALKEY,    by = "PORTAL.NAME")
  SMADMCHECKFILTER <- filter(SMA, YESTERDAY == "No data" | YESTERDAY == "0")
  SMADMCHECKLEFTJOIN <- left_join(SMADMCHECKFILTER, RECENTDDMLIST, by = "PORTAL.NAME")
  SMADMCHECKFINALJOIN <- left_join(SMADMCHECKLEFTJOIN[c("PORTAL.NAME","STATUS","CATEGORY")], FINALPORTALKEY,   by = "PORTAL.NAME")
  SPDMCHECKFILTER <- filter(SP, Status == "Open")
  SPDMCHECKFILTER <- distinct(SPDMCHECKFILTER, PORTAL.NAME, .keep_all= TRUE)
  SPDMCHECKLEFTJOIN <- left_join(SPDMCHECKFILTER, RECENTDDMLIST, by = "PORTAL.NAME")
  SPDMCHECKFINALJOIN <- left_join(SPDMCHECKLEFTJOIN[c("PORTAL.NAME","STATUS","CATEGORY")], FINALPORTALKEY,   by = "PORTAL.NAME")
  SESMASPmerge <- rbind(SEDMCHECKFINALJOIN, SMADMCHECKFINALJOIN, SPDMCHECKFINALJOIN)
  write.csv(SESMASPmerge, file = "RECENTDDMLIST.csv")
  return(SESMASPmerge)
}

