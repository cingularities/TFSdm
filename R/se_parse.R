#' Written by Cynthia L. Norton, TFS Data Monitoring
#' This function parses through raw SolarEdge (SE) API data
#' @param cont_bind input binded API content
#' @return se_parsed returned parsed SE API data with PORTAL.NAME, DATE, Severity and siteID
#' @export
#' @examples
#' se_parse<- function(cont_bind)

se_parse <- function(cont_bind) {
  cont_parse <- cont_bind %>%
    map_df(as_tibble)%>% #adding raw data to a table
    cSplit("site", "(")%>% #splitting columns based on parenthesis
    cSplit(c(1:8), "=")%>% #splitting columns based on equal sign
    cSplit(29, '"')%>% #remove quotations
    select(c(28,29,48,49,50,62,63,105))%>% #selecting wanted columns
    cSplit(c(1,2,3,4,5,6,7), ",")%>% #splitting columns based on comma
    select(c(1,2,7,8,9,10,13,14)) #selecting watned columns
  
#parsing through SE raw data was a bit due to having two unique column number which cause delimited data to have mismatched columns. I had to find the columns that were mismatched and bind them some how
  
  one <- cont_parse %>% 
    filter(str_detect(site_3_10_2, "highestImpact"))%>% #filtering columns with highest impact
    select(c(1,2,4))%>% #selecting wanted columns
    rename(PORTAL.NAME = 1, siteID = 2, Severity = 3) #renaming column
  
  
  two <- cont_parse %>% 
    filter(str_detect(site_3_11_2, "highestImpact"))%>% #filtering columns with highest impact
    select(c(1,2,6))%>%# selecting wanted columns
    rename(PORTAL.NAME = 1, siteID = 2, Severity = 3)#renaming column
  
  three <- cont_parse %>% 
    filter(str_detect(site_4_11_2, "highestImpact"))%>%#filtering columns with highest impact
    select(c(1,2,8))%>% #selecting wanted columns
    rename(PORTAL.NAME = 1, siteID = 2, Severity = 3)#renaming column
  
  
  parse_bind <- rbind(one,two,three) #row binding columns by stacking rows on top of each other
  
  get_list <- list() #creates empty list for kWh 
  
  #create a loop to GET all energy information from each site and stack int one file. SolarEdge API limitation in gathering one site energy at a time. See SolarEdge API Monitoring Handbook
  for (i in 1:nrow(parse_bind)) {
    
    siteid <- parse_bind$siteID[i] #assigning the site ID to be gathered 
    date <- "2021-07-12" #set date you want energy information from
    
    #load energy
    se = GET(paste("https://monitoringapi.solaredge.com/site/",siteid,"/energy?timeUnit=DAY&endDate=",date,"&startDate=",date,"&api_key=84TDQMCBAGW1SY202Z8UESGDZ3PHPMP9", sep ='')) #call loop x variable in list of siteID for API data
    cont_se_raw <- httr::content(se) #content of GET
    
    cont_se_parse <- cont_se_raw %>%
      map_df(as_tibble)%>% #raw data in table
      add_column(siteID = as.numeric(siteid)) #add siteID as numeric
    
    get_list[[i]] <- cont_se_parse #put result of loop in the list created above one siteID at a time
  }
  
  #parsing through the raw production data loop
  production <- get_list %>% map_df(as_tibble)%>%
    cSplit("values", "=")%>% #seperate by equal sign
    select(c(4,6,7))%>% #select wanted columns
    cSplit(2, ",")%>% #split column based on comma
    cSplit(3, " ")%>% #split column based space
    select(c(1,2,4))%>% #select wanted columns
    as.data.frame()%>% #put in a data.frame
    rename(Wh = 2, DATE = 3)%>% #rename columns
    mutate(DATE = str_remove_all(DATE, '"'))%>% #removes quotes
    mutate(Wh = str_remove_all(Wh, '[)]'))%>% #removes parenthesis
    transform(kWh = as.numeric(Wh)*0.001) #calculating kWh #transforms Wh to kWh
  
  SE <- parse_bind %>% left_join(production, by = "siteID")  #joining se siteID, severity and portal name with production data
  
  return(SE)
}
