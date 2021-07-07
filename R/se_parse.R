#' Written by Cynthia L. Norton, TFS Data Monitoring
#' This function parses through raw SolarEdge (SE) API data
#' @param cont_bind input binded API content
#' @return se_parsed returned parsed SE API data with PORTAL.NAME, DATE, Severity and siteID
#' @export
#' @examples
#' se_parse<- function(cont_bind)

se_parse <- function(cont_bind) {
  cont_parse <- cont_bind %>%
    map_df(as_tibble)%>%
    cSplit("site", "(")%>%
    cSplit(c(1:8), "=")%>%
    cSplit(29, '"')%>%
    select(c(28,29,48,49,50,62,63,105))%>%
    cSplit(c(1,2,3,4,5,6,7), ",")%>%
    select(c(1,2,7,8,9,10,13,14))
  
  
  one <- cont_parse %>% 
    filter(str_detect(site_3_10_2, "highestImpact"))%>%
    select(c(1,2,4))%>%
    rename(PORTAL.NAME = 1, siteID = 2, Severity = 3)
  
  
  two <- cont_parse %>% 
    filter(str_detect(site_3_11_2, "highestImpact"))%>%
    select(c(1,2,6))%>%
    rename(PORTAL.NAME = 1, siteID = 2, Severity = 3)
  
  three <- cont_parse %>% 
    filter(str_detect(site_4_11_2, "highestImpact"))%>%
    select(c(1,2,8))%>%
    rename(PORTAL.NAME = 1, siteID = 2, Severity = 3)
  
  
  parse_bind <- rbind(one,two,three)
  
  get_list <- list() #creates empty list for kWh 
  
  #create a loop to GET all energy information from each site and stack int one file
  for (i in 1:nrow(parse_bind)) {
    
    siteid <- parse_bind$siteID[i]
    date <- "2021-07-07"
    #load energy
    se = GET(paste("https://monitoringapi.solaredge.com/site/",siteid,"/energy?timeUnit=DAY&endDate=",date,"&startDate=",date,"&api_key=84TDQMCBAGW1SY202Z8UESGDZ3PHPMP9", sep =''))
    cont_se_raw <- httr::content(se)
    
    cont_se_parse <- cont_se_raw %>%
      map_df(as_tibble)%>%
      add_column(siteID = as.numeric(siteid))
    
    get_list[[i]] <- cont_se_parse
  }
  
  get_tibble <- get_list %>% map_df(as_tibble)%>%
    cSplit("values", "=")%>%
    select(c(4,6,7))%>%
    cSplit(2, ",")%>%
    cSplit(3, " ")%>%
    select(c(1,2,4))%>%
    as.data.frame()%>%
    rename(Wh = 2, DATE = 3)%>%
    mutate(DATE = str_remove_all(DATE, '"'))%>%
    mutate(Wh = str_remove_all(Wh, '[)]'))%>%
    transform(kWh = as.numeric(Wh)*0.001) #calculating kWh
  
  SE <- parse_bind %>% left_join(get_tibble, by = "siteID")
  
  return(SE)
}
