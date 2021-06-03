# data scraping script -- this is run to extract all polling data from Wikipedia
# up to (and including) the 2019 General Election. It'll then be written to a
# CSV file so as to not require as much web scraping.

#------------------------------------------------------------------------------

# LOAD LIBRARIES
pacman::p_load(tidyverse, rvest, polite, lubridate)

# establish session
session <- bow("https://en.wikipedia.org") # acceptable path

path05 <- "wiki/Opinion_polling_for_the_2005_United_Kingdom_general_election"

# make cleaning function
scrape_clean <- function(path, xpath, year = NA) {
  df <- nod(session, path) %>% scrape %>%
    html_node(xpath = xpath) %>% html_table(fill = T) %>%
    tibble %>% 
    mutate(year = year)
}

# scrape data

# 2001-2005
r05 <- list(scrape_clean(path05,
                         '//*[@id="mw-content-text"]/div[1]/table[1]', 2005),
            scrape_clean(path05,
                         '//*[@id="mw-content-text"]/div[1]/table[2]', 2004),
            scrape_clean(path05,
                         '//*[@id="mw-content-text"]/div[1]/table[3]', 2003),
            scrape_clean(path05,
                         '//*[@id="mw-content-text"]/div[1]/table[4]', 2002),
            scrape_clean(path05,
                         '//*[@id="mw-content-text"]/div[1]/table[5]', 2001)) %>%
  bind_rows %>% 
filter(str_detect(`Pollster`,
                  "Pollster|Results|election|Howard|Iraq|Iain") == F) %>%
  mutate(Pollster = str_remove(`Pollster`,
                               "/.*"),
         nsize = as.numeric(str_remove_all(`Samplesize`,
                                           ",|\\[.*")),
         date = dmy(paste(str_remove(Datesconducted, ".*[:punct:]"),
                          as.character(year))),
         na = is.na(nsize),
         nsize = if_else(na == T, 0, nsize),
         Lab = as.numeric(str_remove(Lab, "%")),
         Con = as.numeric(str_remove(Con, "%")),
         LD = as.numeric(str_remove(`Lib Dem`, "%")),
         Other = as.numeric(str_remove(Others, "%")),
         rcon = 33.2,
         rlab = 36.2, rld = 22.7, roth = 7.9,
         time = as_date(2005-05-05) - date) %>% 
  select(c(Pollster, Lab, Con, LD, Other, nsize, date, na,
           rcon, rlab, rld, roth, time))

# 2005-2010

path10 <- "wiki/Opinion_polling_for_the_2010_United_Kingdom_general_election"

r10 <- list(scrape_clean(path10,
                         '//*[@id="mw-content-text"]/div[1]/table[2]',
                         2010),
            scrape_clean(path10,
                         '//*[@id="mw-content-text"]/div[1]/table[3]',
                         2009),
            scrape_clean(path10,
                         '//*[@id="mw-content-text"]/div[1]/table[4]',
                         2008),
            scrape_clean(path10,
                         '//*[@id="mw-content-text"]/div[1]/table[5]',
                         2007),
            scrape_clean(path10,
                         '//*[@id="mw-content-text"]/div[1]/table[6]',
                         2006),
            scrape_clean(path10,
                         '//*[@id="mw-content-text"]/div[1]/table[7]',
                         2005)) %>% bind_rows %>% 
  filter(str_detect(`Pollster`,
                    "Pollster|Results|Prime|election|leader") == F) %>%
  mutate(Pollster = str_remove(`Pollster`,
                               "/.*|\\[.*"),
         nsize = as.numeric(str_remove_all(`Samplesize`,
                                           ",|\\[.*")),
         date = dmy(paste(str_remove(`Date(s)Conducted`, ".*[:punct:]"),
                          as.character(year))),
         na = is.na(nsize),
         nsize = if_else(na == T, 0, nsize),
         Lab = as.numeric(str_remove(Lab, "%")),
         Con = as.numeric(str_remove(Con, "%")),
         LD = as.numeric(str_remove(`Lib Dem`, "%")),
         Other = as.numeric(str_remove(Others, "%")),
         rcon = 36.9,
         rlab = 29.7, rld = 23.6, roth = 9.8,
         time = as_date(2010-05-06) - date) %>% 
  select(c(Pollster, Lab, Con, LD, Other, nsize, date, na,
           rcon, rlab, rld, roth, time))

# 2010-2015

path15 <- "wiki/Opinion_polling_for_the_2015_United_Kingdom_general_election"

path12 <- "wiki/Opinion_polling_for_the_2015_United_Kingdom_general_election_(2010-2012)"

r15 <- list(scrape_clean(path15,
                         '//*[@id="mw-content-text"]/div[1]/table[3]',
                         2015),
            scrape_clean(path15,
                         '//*[@id="mw-content-text"]/div[1]/table[4]',
                         2014),
            scrape_clean(path15,
                         '//*[@id="mw-content-text"]/div[1]/table[5]',
                         2013),
            scrape_clean(path12,
                         '//*[@id="mw-content-text"]/div[1]/table[1]',
                         2012),
            scrape_clean(path12,
                         '//*[@id="mw-content-text"]/div[1]/table[2]',
                         2011),
            scrape_clean(path12,
                         '//*[@id="mw-content-text"]/div[1]/table[3]',
                         2010)) %>% bind_rows %>% 
  filter(str_detect(`Polling organisation/client`,
                    "Polling|Result|Prime|election|leader|Question|Debate|Parliament|referendum|result") == F) %>%
  mutate(Pollster = str_remove(`Polling organisation/client`,
                               "/.*|\\[.*"),
         nsize = as.numeric(str_remove_all(`Sample size`,
                                           ",|\\[.*")),
         date = dmy(paste(str_remove(`Date(s)conducted`, ".*[:punct:]"),
                          as.character(year))),
         Lab = as.numeric(str_remove(Lab, "%")),
         Con = as.numeric(str_remove(Con, "%")),
         LD = as.numeric(str_remove(`Lib Dem`, "%")),
         Other = as.numeric(str_remove(Others, "%")) + 
           as.numeric(str_remove(UKIP, "%")) +
           as.numeric(str_remove(Green, "%")),
         na = is.na(nsize),
         nsize = ifelse(na == T, 0, nsize), rcon = 37.8,
         rlab = 31.2, rld = 8.1, roth = sum(12.9, 3.8, 6.3),
         time = as_date(2015-05-07) - date) %>% 
  select(c(Pollster, Lab, Con, LD, Other, nsize, date, na,
           rcon, rlab, rld, roth, time))

# 2015-2017

path17 <- "wiki/Opinion_polling_for_the_2017_United_Kingdom_general_election"

r17 <- list(scrape_clean(path17,
                         '//*[@id="mw-content-text"]/div[1]/table[2]',
                         2017),
            scrape_clean(path17,
                         '//*[@id="mw-content-text"]/div[1]/table[3]',
                         2016),
            scrape_clean(path17,
                         '//*[@id="mw-content-text"]/div[1]/table[4]',
                         2015)) %>% bind_rows %>% 
  filter(str_detect(`Polling organisation/client`,
                    "Polling|Result|Prime|election|leader|Question|Debate|Parliament|referendum|result|campaigning|Leader") == F) %>%
  mutate(Pollster = str_remove(`Polling organisation/client`,
                               "/.*|\\[.*"),
         nsize = as.numeric(str_remove_all(`Sample size`,
                                           ",|\\[.*")),
         date = dmy(paste(str_remove(`Date(s)conducted`, ".*[:punct:]"),
                          as.character(year))),
         Lab = as.numeric(str_remove(Lab, "%.*")),
         Con = as.numeric(str_remove(Con, "%.*")),
         LD = as.numeric(str_remove(`Lib Dem`, "%.*")),
         Other = 100 - Con - Lab - LD,
         na = is.na(nsize),
         nsize = if_else(na == T, 0, nsize), rcon = 43.5,
         rlab = 41, rld = 7.6, roth = sum(1.9, 3.1, 1.7, 1.2),
         time = as_date(2017-06-08) - date) %>% 
  select(c(Pollster, Lab, Con, LD, Other, nsize, date, na,
           rcon, rlab, rld, roth, time))

# 2017-2019

path19 <- "wiki/Opinion_polling_for_the_2019_United_Kingdom_general_election"

r19 <- list(scrape_clean(path19,
                         '//*[@id="mw-content-text"]/div[1]/table[2]',
                         2019),
            scrape_clean(path19,
                         '//*[@id="mw-content-text"]/div[1]/table[3]',
                         2018),
            scrape_clean(path19,
                         '//*[@id="mw-content-text"]/div[1]/table[4]',
                         2017)) %>% bind_rows %>% 
  filter(str_detect(`Pollster/client(s)`,
                    "Pollster|Result|Prime|election|leader|Question|Debate|Parliament|referendum|result|campaigning|Leader|MRP") == F &
           `Pollster/client(s)` != "") %>%
  mutate(Pollster = str_remove(`Pollster/client(s)`, "/.*|\\[.*"),
         nsize = as.numeric(str_remove_all(`Samplesize`,
                                           ",|\\[.*")),
         date = dmy(paste(str_remove(`Date(s)conducted`, ".*[:punct:]"),
                          as.character(year))),
         Lab = as.numeric(str_remove(Lab, "%.*")),
         Con = as.numeric(str_remove(Con, "%.*")),
         LD = as.numeric(str_remove(`Lib Dem`, "%.*")),
         Other = 100 - Con - Lab - LD,
         na = is.na(nsize),
         nsize = ifelse(na == T, 0, nsize), rcon = 44.7,
         rlab = 33, rld = 11.8,
         roth = sum(4, 0.5, 2.8, 2.1, 0.1, 1),
         time = as_date(2019-12-12) - date) %>% 
  select(c(Pollster, Lab, Con, LD, Other, nsize, date, na,
           rcon, rlab, rld, roth, time))

# remove last anomalies

dat <- bind_rows(r05, r10, r15, r17, r19) %>% 
  filter(Pollster != "" & !is.na(date)) %>%
  mutate(Pollster = case_when(
  Pollster %in% c("Angus Reid Public Opinion",
                  "Angus Reid Strategies") ~ "Angus Reid",
  Pollster == "BMG Research" ~ "BMG",
  Pollster %in% c("SavantaComRes", "ComRes-", "Communicate") ~ "ComRes",
  Pollster == "Harris Interative" ~ "Harris Interactive",
  Pollster %in% c("ICM Research", "ICM[3]") ~ "ICM",
  Pollster %in% c("Ipsos-MORI", "Ipsos Mori", "MORI",
                  "MORI-Political-Monitor-August-2013.aspx Ipsos MORI",
                  "MORIEvening-Standard-Political-Monitor-March-2012.aspx Ipsos MORI",
                  "Marketing Sciences", "Ipsos MOR") ~ "Ipsos MORI",
  Pollster %in% c("Kantar Public", "TNS", "TNS-BMRB", "TNS BMRB") ~ "Kantar",
  Pollster == "Lord Ashcroft Polls" ~ "Lord Ashcroft",
  Pollster == "Populus Online" ~ "Populus",
  TRUE ~ Pollster))

# write to CSV
write_csv(dat, "training.csv")
