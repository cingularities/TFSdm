#' This function creates a client reference data table to use for yield deviations
#' @param se SolarEdge Data.
#' @param sma SunnyPortal Data
#' @param sp SunPower Data.
#' @param lg Life's Good Data.
#' @param jobs Reference for customer job information, downloaded from admin
#' @return A client reference file with known portal names and added TFS production estimates
#' @export
#' @examples reference_update(se,sma,sp,lg,jobs)


reference_update <- function(se, sma, sp, lg, jobs) {
  
  jobs$JobAddress <- gsub("[.]","",as.character(jobs$JobAddress))
  
  se <- se %>% select(2)%>% #Selects column
    slice(-c(1:8)) %>% #selects row
    rename_(PORTAL.NAME = 1) %>%
    mutate(PORTAL.SERVER = "SE") %>% #adds MonitorCode column
    mutate(PORTAL.KEY = PORTAL.NAME)
  
  sma <- sma %>% 
    rename_(PORTAL.NAME = 1) %>% 
    select(c(PORTAL.NAME)) %>%
    mutate(PORTAL.SERVER = "SMA") %>%   #adds MonitorCode column
    mutate(PORTAL.KEY = PORTAL.NAME)
  
  sp <- sp %>% select(1) %>% 
    rename_(PORTAL.NAME = 1) %>%
    mutate(PORTAL.SERVER = "SP") %>% #adds MonitorCode column
    mutate(PORTAL.KEY = PORTAL.NAME)
  
  
  lg <- lg %>% select(2) %>%
    rename_(PORTAL.NAME = 1) %>%
    mutate(PORTAL.SERVER = "LG") %>% #adds MonitorCode column
    mutate(PORTAL.KEY = PORTAL.NAME)
  
  portal_bind <- rbind(se, sma) 
  
  
  jobs_estimate <- jobs %>%
    filter(PvSystemSizeKw > 0) %>% 
    group_by(Address) %>%
    #add line to remove periods on job address 030722
    dplyr::summarise(TotalPvSystemSizeKw = sum(PvSystemSizeKw))%>%
    transform(JAN = (((as.numeric(TotalPvSystemSizeKw)*1850)*0.07)*0.85)) %>% #calculating performance based on TFS reference numbers. actual production/estimated production
    transform(FEB = (((as.numeric(TotalPvSystemSizeKw)*1850)*0.068)*0.85)) %>% #calculating performance based on TFS reference numbers. actual production/estimated production
    transform(MAR = (((as.numeric(TotalPvSystemSizeKw)*1850)*0.092)*0.85)) %>% #calculating performance based on TFS reference numbers. actual production/estimated production
    transform(APR = (((as.numeric(TotalPvSystemSizeKw)*1850)*0.097)*0.85)) %>% #calculating performance based on TFS reference numbers. actual production/estimated production
    transform(MAY = (((as.numeric(TotalPvSystemSizeKw)*1850)*0.098)*0.85)) %>% #calculating performance based on TFS reference numbers. actual production/estimated production
    transform(JUN = (((as.numeric(TotalPvSystemSizeKw)*1850)*0.094)*0.85)) %>% #calculating performance based on TFS reference numbers. actual production/estimated production
    transform(JUL = (((as.numeric(TotalPvSystemSizeKw)*1850)*0.086)*0.85)) %>% #calculating performance based on TFS reference numbers. actual production/estimated production
    transform(AUG = (((as.numeric(TotalPvSystemSizeKw)*1850)*0.089)*0.85)) %>% #calculating performance based on TFS reference numbers. actual production/estimated production
    transform(SEP = (((as.numeric(TotalPvSystemSizeKw)*1850)*0.087)*0.85)) %>% #calculating performance based on TFS reference numbers. actual production/estimated production
    transform(OCT = (((as.numeric(TotalPvSystemSizeKw)*1850)*0.083)*0.85)) %>% #calculating performance based on TFS reference numbers. actual production/estimated production
    transform(NOV = (((as.numeric(TotalPvSystemSizeKw)*1850)*0.071)*0.85)) %>% #calculating performance based on TFS reference numbers. actual production/estimated production
    transform(DEC = (((as.numeric(TotalPvSystemSizeKw)*1850)*0.065)*0.85)) %>% #calculating performance based on TFS reference numbers. actual production/estimated production
    transform(YearlyEstimate = (((as.numeric(TotalPvSystemSizeKw)*1850)*0.85)))%>% #calculating performance based on TFS reference numbers. actual production/estimated production
    left_join(jobs, by = "Address")  %>%
    filter(PvSystemSizeKw > 0) %>% 
    mutate(PORTAL.NAME = Clientname) %>%
    distinct(YearlyEstimate, Clientname, .keep_all = TRUE) 
  
  portal_bind$PORTAL.NAME <-gsub(",","",as.character(portal_bind$PORTAL.NAME))
  portal_bind$PORTAL.NAME <-gsub("(DNM)","",as.character(portal_bind$PORTAL.NAME))
  portal_bind$PORTAL.NAME <-gsub("~","",as.character(portal_bind$PORTAL.NAME))
  portal_bind$PORTAL.NAME <-gsub("*","",as.character(portal_bind$PORTAL.NAME))
  portal_bind$PORTAL.NAME <-gsub("  "," ",as.character(portal_bind$PORTAL.NAME))
  jobs_estimate$PORTAL.NAME <- gsub("  "," ",as.character(jobs_estimate$PORTAL.NAME))
  jobs_estimate$PORTAL.NAME <- gsub("[.]","",as.character(jobs_estimate$PORTAL.NAME))
  
  
  jobs_separate <- jobs_estimate %>% separate(PORTAL.NAME, c("lastname", "firstname"))
  portal_bind_separate <- portal_bind %>% separate(PORTAL.NAME, c("lastname", "firstname")) 
  portal_bind_join <- left_join(portal_bind_separate, jobs_separate, by = c("lastname", "firstname"))
  portal_bind_naomit <- portal_bind_join %>% drop_na(PvSystemSizeKw)  %>%
    mutate(PORTAL.NAME = PORTAL.KEY) 
  portal_bind_naomit$MonitorCode <-gsub("SolarEdge","SE",as.character(portal_bind_naomit$MonitorCode))
  
  
  #portal_bind_specialjoin <- portal_bind_naonly %>%
  #  left_join(jobs_separate, by = c("lastname"="firstname", "firstname"="lastname"))
  #portal_bind_specialjoin_naomit <- portal_bind_specialjoin %>% drop_na(PvSystemSizeKw) 
  #portal_bind_specialjoin_naonly <- subset(portal_bind_specialjoin, is.na(portal_bind_specialjoin$PvSystemSizeKw)) %>%
  #  select(c(lastname, firstname, PORTAL.KEY))
  
  #portal_final <- rbind(portal_bind_naomit,portal_bind_specialjoin_naomit)%>%
  #  mutate(PORTAL.NAME = PORTAL.KEY)
  
  return(portal_bind_naomit)
}