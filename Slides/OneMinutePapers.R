library(googlesheets4)
library(tidyverse)

sheet_url <- 'https://docs.google.com/spreadsheets/d/19GAXKkE7VikWAypnNrVPnKfyCWDp27vwOwSD54Czvf0/edit?resourcekey#gid=1204644888'

omp <- read_sheet(sheet_url)

table(omp$`Your name:`) %>% as.data.frame()

