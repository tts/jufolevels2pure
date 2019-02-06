library(RCurl)
library(tidyverse)
library(httr)
library(jsonlite)

url <- "https://jufo-rest.csc.fi/v1.0/massa.json.zip"

temp <- tempfile()

download.file(url, temp)
data <- unzip(temp)

jufo <- fromJSON(data, flatten=TRUE)

unlink(temp)

jufo$Type <- gsub("Lehti/Sarja", "Lehti/sarja", jufo$Type)

# https://stackoverflow.com/a/28575551
jufo_history <- jufo %>% 
  tbl_df() %>%
  mutate(jufos = strsplit(Jufo_history, ";")) %>% # split each year:level into a vector
  unnest(jufos) %>%  # unnest the vectors into multiple rows
  separate(jufos, c("Year", "JuFo_level"), ":") # separate to two new columns

#-------------------
#  Journals to CSV
#-------------------

do_j <- function(sn, year) {
  
  # https://stackoverflow.com/a/48579816
  # You have to tell the function that the parameter is given as a bare variable name
  # For this first use enquo to evaluate what is given in the argument and store it as a quosure
  sn <- enquo(sn)
  year <- enquo(year)
  
  ret <- jufo_history %>% 
    filter(Type == 'Lehti/sarja',
           Year == !!year) %>% 
    # Then with !! in front of the parameter the mutate function knows that 
    # it doesn't have to quote this argument, as it is already a quosure. 
    mutate(ISSN = !!sn,
           TITLE = Name,
           UUID = '',
           RATING = JuFo_level) %>% 
    select(ISSN, TITLE, UUID, RATING) 
  
  return(ret)
  
}

journals_ISSNL_2017 <- do_j(ISSNL, 2017)
journals_ISSNL_2018 <- do_j(ISSNL, 2018)
journals_ISSNL_2019 <- do_j(ISSNL, 2019)

journals_ISSN1_2017 <- do_j(ISSN1, 2017)
journals_ISSN1_2018 <- do_j(ISSN1, 2018)
journals_ISSN1_2019 <- do_j(ISSN1, 2019)

journals_ISSN2_2017 <- do_j(ISSN2, 2017)
journals_ISSN2_2018 <- do_j(ISSN2, 2018)
journals_ISSN2_2019 <- do_j(ISSN2, 2019)

# Bind all rows together
journals_2017 <- rbind(journals_ISSNL_2017, journals_ISSN1_2017, journals_ISSN2_2017)
journals_2018 <- rbind(journals_ISSNL_2018, journals_ISSN1_2018, journals_ISSN2_2018)
journals_2019 <- rbind(journals_ISSNL_2019, journals_ISSN1_2019, journals_ISSN2_2019)

journals_2017_sorted <- journals_2017 %>% 
  arrange(ISSN)

journals_2018_sorted <- journals_2018 %>% 
  arrange(ISSN)

journals_2019_sorted <- journals_2019 %>% 
  arrange(ISSN)

write.csv(journals_2017_sorted, "j_2017_topure.csv", row.names = FALSE, fileEncoding = "ISO-8859-1")
write.csv(journals_2018_sorted, "j_2018_topure.csv", row.names = FALSE, fileEncoding = "ISO-8859-1")
write.csv(journals_2019_sorted, "j_2019_topure.csv", row.names = FALSE, fileEncoding = "ISO-8859-1")

#-------------------
#  Publishers to CSV
#-------------------

do_p <- function(year) {
  
  year <- enquo(year)
  
  ret <- jufo_history %>% 
    filter(Type == 'Kirjakustantaja',
           Year == !!year) %>% 
    mutate(NAME = Name,
           UUID = '',
           RATING = JuFo_level) %>% 
    select(NAME, UUID, RATING) 
  
  return(ret)
  
}

publishers_2016 <- do_p(2016)
publishers_2017 <- do_p(2017)
publishers_2018 <- do_p(2018)
publishers_2019 <- do_p(2019)

write.csv(publishers_2016, "publ_2016_topure.csv", row.names = FALSE, fileEncoding = "ISO-8859-1")
write.csv(publishers_2017, "publ_2017_topure.csv", row.names = FALSE, fileEncoding = "ISO-8859-1")
write.csv(publishers_2018, "publ_2018_topure.csv", row.names = FALSE, fileEncoding = "ISO-8859-1")
write.csv(publishers_2019, "publ_2019_topure.csv", row.names = FALSE, fileEncoding = "ISO-8859-1")
