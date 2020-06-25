### GET DATA ###
library(magrittr)
library(polite)
library(rvest)
library(readxl)
library(purrr)

# Get Excel file links from the A&E statistics page
data_scrape <- polite::scrape(polite::bow('https://www.england.nhs.uk/statistics/statistical-work-areas/ae-waiting-times-and-activity/ae-attendances-and-emergency-admissions-2019-20/')) %>%
  rvest::html_nodes('article') %>%
  rvest::html_nodes('p') %>%
  rvest::html_nodes('a') 

data_titles <- data_scrape %>%
  rvest::html_text()
data_links <- data_scrape %>%
  rvest::html_attr('href')
data_names <- paste0(rstudioapi::getActiveProject(), '/data', 
                     stringr::str_extract(data_links, '/[a-zA-Z0-9-]*\\.xls'))

months <- c('January', 'February', 'March', 'April', 'May',
            'June', 'July', 'August', 'September', 'October', 
            'November', 'December')
month_reg <- paste0('((', paste0(months, collapse = ')|('), ')) [0-9]{1,5}')
data_df <- data.frame(titles = data_titles,
                      links = data_links,
                      names = data_names,
                      stringsAsFactors = FALSE) %>%
  dplyr::filter(grepl('Monthly', titles) & grepl('xls', names)) %>%
  dplyr::mutate(month = stringr::str_extract(titles, month_reg)) %>%
  dplyr::mutate(month = paste0('1 ', month)) %>%
  dplyr::mutate(month = as.Date(month, format = "%d %B %Y"))



ae <- data.frame(stringsAsFactors = FALSE)
for(i in 1:nrow(data_df)){
  # Download excel files to folder
  downloader::download(data_df$links[i], 
                       destfile = data_df$names[i], 
                       mode = 'wb')
  tryCatch({
    # Read excel file to dataframe
    t <- readxl::read_excel(data_df$names[i], 
                            sheet = 'Provider Level Data',
                            skip = 15,
                            na = '-') %>%
      janitor::clean_names() %>%
      dplyr::filter(!is.na(code) & code != '-') %>%
      # Add month for this data
      dplyr::mutate(month = data_df$month[i])
    ae <- rbind(ae, t)
  }, error=function(e){})
}

write.csv(ae, 'combined_ae_data.csv', row.names = FALSE)
