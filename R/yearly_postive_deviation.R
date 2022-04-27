#' This function compares estimated yearly yield to actual yield for portals SE and SMA.
#' @param se_performance SolarEdge Data.
#' @param sma_performance SunnyPortal Data
#' @param estimates Reference for .
#' @param status Reference list on site status.
#' @return Yearly production above estimates data.frame as an object, and also written to yearly_positive_deviation.csv.
#' @export
#' @examples
#' yearly_positive_deviation(sma_performance,se_performance,estimates,status)

yearly_positive_deviation <- function(sma_performance_yearly,se_performance_yearly,estimates_raw,status_raw) {
  
  estimates <- estimates_raw %>% group_by(JobName) %>% slice(which.max(YearlyEstimate)) %>% select(c(PORTAL.NAME,PORTAL.SERVER,JobAddress, City, State, Postal, Email, InstallationDate, JobName, MonitorCode, YearlyEstimate))
  
  se_performance <- se_performance_yearly %>% select(-c(1,3)) %>% #Selects column
    slice(-c(1:8)) %>%#selects row
    rename_(PORTAL.NAME = 1, YEAR = 2) %>% #renames column
    select(PORTAL.NAME, YEAR) %>%  #selects columns wanted
    mutate(PORTAL.SERVER = "SE")%>%  #adds PORTAL.SERVER column
    left_join(estimates %>% select(PORTAL.NAME,PORTAL.SERVER,JobAddress, City, State, Postal, Email, InstallationDate, JobName, MonitorCode, YearlyEstimate), by = "PORTAL.NAME") #joins with performance residence
  
  sma_performance <- sma_performance_yearly %>% select(-c(3,4,6,7,8)) %>% #Selects column
    rename_(PORTAL.NAME = 1, SYSTEM.SIZE = 2, MONTH = 3, RAW = 4) %>% #renames column
    transform(YEAR = (as.numeric(RAW)*as.numeric(SYSTEM.SIZE)))%>% #Sunnyportal exports yearly production kWh/kWp, reference file is in kWh. This transforms YEAR data to kWh to match reference. 
    select(PORTAL.NAME, YEAR) %>% #selects columns wanted
    mutate(PORTAL.SERVER = "SMA")%>%  #adds PORTAL.SERVER column
    left_join(estimates %>% select(PORTAL.NAME,PORTAL.SERVER,JobAddress, City, State, Postal, Email, InstallationDate, JobName, MonitorCode, YearlyEstimate), by = "PORTAL.NAME") #joins with performance residence
  
  se_sma_bind <- rbind(se_performance, sma_performance) #binds columns two portals
  
  se_sma_deviation <- se_sma_bind %>%
    transform(PERFORMANCE = (as.numeric(YEAR)/YearlyEstimate)*100)%>% #calculating performance based on TFS reference numbers. actual production/estimated production
    left_join(status_raw %>% select(PORTAL.NAME,STATUS, CATEGORY), by = "PORTAL.NAME")%>% #joins recent down dm list with current portal status
    filter(PERFORMANCE > 99)%>% #filters sites that are performing above 95%
    distinct(PORTAL.NAME, .keep_all= TRUE)
  
  return(se_sma_deviation) #final file to be returned when function is run
}
