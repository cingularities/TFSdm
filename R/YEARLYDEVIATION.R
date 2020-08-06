#' This function compares estimated yearly yield to actual yield for portals SE and SMA.
#' @param SE SolarEdge Data.
#' @param SMA SunnyPortal Data
#' @param PERFORMANCEKEY Reference for .
#' @param RECENTDDMLIST Reference list on site status.
#' @return Yearly yield deviations.
#' @export
#' @examples
#' YEARLYDEVIATION(SE, SMA, PERFORMANCEKEY, RECENTDDMLIST)


YEARLYDEVIATION <- function(SMA,SE,PERFORMANCEKEY,RECENTDDMLIS) {
  SEPGCHECKJOINFACILITY <- left_join(SEPerformance[c("PORTAL.NAME","YEAR")], PERFORMANCEKEY[c("PORTAL.NAME","Address","City","State","Postal","Email","Installation.Date","JOB.NAME","MonitorCode","PvEstYearlyProd")], by = "PORTAL.NAME")
  SMAPGCHECKJOINFACILITY <- left_join(SMAPerformance[c("PORTAL.NAME","YEAR")], PERFORMANCEKEY[c("PORTAL.NAME","Address","City","State","Postal","Email", "Installation.Date","JOB.NAME","MonitorCode","PvEstYearlyProd")], by = "PORTAL.NAME")
  SESMAPGCbind <- rbind(SMAPGCHECKJOINFACILITY, SEPGCHECKJOINFACILITY)
  SESMAPGCtransform <- transform(SEPGCHECKJOINFACILITY, PERFORMANCE = (YEAR2019/PvEstYearlyProd)*100)
  SESMAPGCBELOWFILTER <- filter(SESMAPGCtransform, PERFORMANCE < 95.0)
  SESMAPGCBELOWFILTER <- distinct(SESMAPGCBELOWFILTER, PORTAL.NAME, .keep_all= TRUE) #Remove doubles. This section need some cleaning. Making sure the right jobs are being selected
  write.csv(SESMAPGCBELOWFILTER, file = "SESMAPGCBELOWFILTERYEARLY.csv")
  SESMAPGCABOVEFILTER <- filter(SESMAPGCtransform, PERFORMANCE > 95.0)
  write.csv(SESMAPGCABOVEFILTER, file = "SESMAPGCABOVEFILTERYEARLY.csv")
}
