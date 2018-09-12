library(tidyverse)
library(lubridate)
library(magrittr)
library(gsubfn)

supplier <-'wssc'
wd <- getwd()
datapath <- wd%>%(function(x) file.path(x,"data"))
forecastfiles <- datapath%>%list.files()%>%(function(x) grep(paste ('verification_',supplier,sep = "", collapse = NULL),x,value=TRUE))
observedfiles <- datapath%>%list.files()%>%(function(x) grep(paste ('actual_withdrawal_data_',supplier,sep = "", collapse = NULL),x,value=TRUE))
outputfile <- wd%>%(function(x) file.path(x,"output"))%>%(function(x) file.path(x,paste('output_',supplier,sep = "", collapse = NULL)))

colClasses = c("Date", "Date", "numeric")
col.names = c("date", "submitted", "forecast")
forecast.df <- read.table(text = "",colClasses = colClasses,col.names = col.names)

colClasses = c("Date", "numeric")
col.names = c("date", "observed")
observed.df <- read.table(text = "",colClasses = colClasses,col.names = col.names)

i=1
while (i < length(forecastfiles)) {
  filepath <- forecastfiles[i]%>%(function(x) file.path(datapath,x))
  filedate <- strapplyc(filepath, "\\d+-\\d+-\\d+", simplify = TRUE)
  mydata.df <- filepath %>% read.csv(header=TRUE, sep=",")
  mutate.df <- mydata.df %>% dplyr::mutate(submitted = as.Date(paste(filedate))) 
  mutate.df <- mutate.df %>% dplyr::mutate(date=as.Date(date))
  select.df <- mutate.df %>% dplyr::select(date, submitted, forecast) 
  forecast.df <- select.df %>% full_join(final.df)
  i=i+1
}

i=1
while (i < length(observedfiles)) {
  filepath <- observedfiles[i]%>%(function(x) file.path(datapath,x))
  filedate <- strapplyc(filepath, "\\d+-\\d+-\\d+", simplify = TRUE)
  mydata.df <- filepath %>% read.csv(header=TRUE, sep=",")
  mydata.df[is.na(mydata.df)] <- 0
  mutate.df <- mydata.df %>% dplyr::mutate(secondary = is.na(secondary) <-0) 
  mutate.df <- mydata.df %>% dplyr::mutate(observed = main_potomac + secondary) 
  mutate.df <- mutate.df %>% dplyr::mutate(date=as.Date(date))
  select.df <- mutate.df %>% dplyr::select(date, observed) 
  observed.df <- select.df %>% full_join(observed.df)
  i=i+1
}

# Merge the observed withdrawals with the forecasts
merged.df <- merge(forecast.df, observed.df, by = "date", all.x = TRUE)
merged.df <- merged.df %>% arrange(date) %>% arrange(desc(submitted))

write(merged.df, sep = " ")
