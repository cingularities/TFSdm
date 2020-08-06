#' This function compares estimated monthly yield to actual yield for portal SE and SMA.
#' @param SE SolarEdge Data.
#' @param SMA SunnyPortal Data
#' @param PERFORMANCEKEY Reference for .
#' @param RECENTDDMLIST Reference list on site status.
#' @return Monthly yield deviations.
#' @export
#' @examples
#' MONTHLYDEVIATION(SE, SMA, PERFORMANCEKEY,RECENTDDMLIST)


MONTHYLDEVIATION <- function(SMA,SE,PERFORMANCEKEY,RECENTDDMLIST) {
  SEPGCHECKJOINFACILITY <- left_join(SEPerformance[c("PORTAL.NAME","MONTH")], PERFORMANCEKEY[c("PORTAL.NAME","Email", "Installation.Date","JOB.NAME","MonitorCode","JUN","PvEstMonthlyProd")], by = "PORTAL.NAME")
  SMAPGCHECKJOINFACILITY <- left_join(SMAPerformance, PERFORMANCEKEY[c("PORTAL.NAME","Email", "Installation.Date","JOB.NAME","MonitorCode","JUN","PvEstMonthlyProd")], by = "PORTAL.NAME")
  SESMAPGCbind <- rbind(SMAPGCHECKJOINFACILITY, SEPGCHECKJOINFACILITY)
  SESMAPGCtransform <- transform(SESMAPGCbind, PERFORMANCE = as.numeric(June)/as.numeric(JUN))
  SESMAPGCleft_join <- left_join(SESMAPGCtransform, RECENTDDMLIST[c("PORTAL.NAME","STATUS", "CATEGORY")], by = "PORTAL.NAME")
  SESMAPGCBELOWFILTER <- filter(SESMAPGCleft_join, PERFORMANCE < 0.85)
  SESMAPGCBELOWFILTER <- distinct(SESMAPGCBELOWFILTER, PORTAL.NAME, .keep_all= TRUE) #Remove doubles. This section need some cleaning. Making sure the right jobs are being selected
  write.csv(SESMAPGCBELOWFILTER, file = "SESMAPGCBELOWFILTER.csv")
  SESMAPGCABOVEFILTER <- filter(SESMAPGCleft_join, PERFORMANCE > 0.85)
  write.csv(SESMAPGCABOVEFILTER, file = "SESMAPGCABOVEFILTER.csv")

}
