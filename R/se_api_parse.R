#' This function is for parsing through Solar Edge API data
#' @param se_pluck SolarEdge API data selist1 = jsonlite::fromJSON("https://monitoringapi.solaredge.com/sites/list?&api_key=84TDQMCBAGW1SY202Z8UESGDZ3PHPMP9") cont_raw_pluck1 <- pluck(selist1, "sites", "site") %>% as_tibble()%>% select(c("id","name","lastUpdateTime","highestImpact"))
#' @return a plucked solaredge API data
#' @export
#' @examples se_parse(se_all_pluck)


se_api_parse <- function(se_all_pluck) {

  se_filter <- se_all_pluck %>% filter(!is.na(lastUpdateTime))
  se_na <- se_all_pluck %>% filter(is.na(lastUpdateTime))

  get_list <- list() #creates empty list for kWh

  #create a loop to GET all energy information from each site and stack int one file. SolarEdge API limitation in gathering one site energy at a time. See SolarEdge API Monitoring Handbook
  for (i in 1:nrow(se_filter)) {

    id <- se_filter$id[i] #assigning the site ID to be gathered
    date <- "2021-08-30" #set date you want energy information from

    #load energy

    se_url = paste("https://monitoringapi.solaredge.com/site/",id,"/energy?timeUnit=DAY&endDate=",date,"&startDate=",date,"&api_key=84TDQMCBAGW1SY202Z8UESGDZ3PHPMP9", sep ='') #call loop x variable in list of siteID for API data
    se = GET(se_url)
    cat("About to request full sample:", se_url, "\n")

    print(se$status_code)
    cont_se_raw <- httr::content(se) #content of GET


    cont_se_pluck <- cont_se_raw %>%
      pluck("energy", "values")%>%
      as_tibble(.name_repair = 'minimal')


    cont_se_parse <- as.data.frame(do.call(cbind, cont_se_pluck), stringsAsFactors = FALSE, col.names = c('time', 'Wh')) %>%
      rownames_to_column()%>%
      gather(date, value, -rowname)%>%
      spread(rowname, value)%>%
      rename(Wh = 2, last.contact = 1)%>%
      select(2)%>% #rename columns
      add_column(id = as.numeric(se_filter$id[i]))

    #cont_se_parse %>% mutate(Wh = as.integer(Wh))

    get_list[[i]] <- cont_se_parse #put result of loop in the list created above one siteID at a time

    gc()
  }

  get_df<- get_list %>% bind_rows()
  get_df[get_df == "NULL"] <- NA
  get_df$Wh <- as.numeric(get_df$Wh)

  production <- get_df %>% transform(kWh = as.numeric(Wh)*0.001)
  SE <- se_all_pluck %>% left_join(production, by = "id")  #joining se siteID, severity and portal name with production data


  return(SE)
}
