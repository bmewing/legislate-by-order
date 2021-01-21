library(readr)
library(dplyr)
library(plotly)
library(lubridate)
library(magrittr)

#pre-1994 data source: https://www.archives.gov/federal-register/executive-orders/disposition
#data source: https://www.federalregister.gov/executive-orders
#trump data is being sourced from the federal register through their api

nixon = read_csv("data/nixon.csv")
ford = read_csv("data/ford.csv")
carter = read_csv("data/carter.csv")
reagan = read_csv("data/reagan.csv")
ghw_bush = read_csv("data/george h w bush.csv")
clinton = read_csv("data/documents_signed_by_william_j_clinton_of_type_presidential_document_and_of_presidential_document_type_executive_order.csv")
gw_bush = read_csv("data/documents_signed_by_george_w_bush_of_type_presidential_document_and_of_presidential_document_type_executive_order.csv")
obama = read_csv("data/documents_signed_by_barack_obama_of_type_presidential_document_and_of_presidential_document_type_executive_order.csv")
trump = read_csv("data/documents_signed_by_donald_trump_of_type_presidential_document_and_of_presidential_document_type_executive_order.csv")
biden = httr::GET("https://www.federalregister.gov/api/v1/documents.json?conditions%5Bcorrection%5D=0&conditions%5Bpresident%5D=joe-biden&conditions%5Bpresidential_document_type%5D=executive_order&conditions%5Btype%5D%5B%5D=PRESDOCU&fields%5B%5D=citation&fields%5B%5D=document_number&fields%5B%5D=end_page&fields%5B%5D=html_url&fields%5B%5D=pdf_url&fields%5B%5D=type&fields%5B%5D=subtype&fields%5B%5D=publication_date&fields%5B%5D=signing_date&fields%5B%5D=start_page&fields%5B%5D=title&fields%5B%5D=disposition_notes&fields%5B%5D=executive_order_number&fields%5B%5D=full_text_xml_url&fields%5B%5D=body_html_url&fields%5B%5D=json_url&order=executive_order&per_page=1000") %>%
  httr::content(as="text") %>%
  jsonlite::fromJSON() %>%
  `$`("results")

#There's a delta between when executive orders are signed and when they're published meaning the chart created later has a chance of under-reporting the number of orders signed. This is a simple correction to account for that.
timeToPublish = difftime(lubridate::as_date(trump$publication_date),lubridate::as_date(biden$signing_date),unit="days") %>%
  as.numeric() %>%
  median()
#Now when we calculate how long Trump has been in office, we can apply the time it takes the regsiter to publish orders and adjust the view to reflect reality.
bidenDaysInOffice = difftime(Sys.Date(),lubridate::as_date("2021-01-20"),units="days") %>%
  as.numeric() %>%
  `-`(timeToPublish)

write_csv(biden,"data/documents_signed_by_joe_biden_of_type_presidential_document_and_of_presidential_document_type_executive_order.csv")

nixon %<>%
  mutate(start_date = lubridate::as_date("1969-01-20"),
         signing_date = lubridate::as_date(signing_date,format="%m/%d/%Y")) %>%
  mutate(daysInOffice = as.integer(difftime(signing_date,start_date,units="days"))) %>%
  group_by(daysInOffice) %>%
  summarise(nixonOrders = n())

ford %<>%
  mutate(start_date = lubridate::as_date("1974-08-09"),
         signing_date = lubridate::as_date(signing_date,format="%m/%d/%Y")) %>%
  mutate(daysInOffice = as.integer(difftime(signing_date,start_date,units="days"))) %>%
  group_by(daysInOffice) %>%
  summarise(fordOrders = n())

carter %<>%
  mutate(start_date = lubridate::as_date("1977-01-20"),
         signing_date = lubridate::as_date(signing_date,format="%m/%d/%Y")) %>%
  mutate(daysInOffice = as.integer(difftime(signing_date,start_date,units="days"))) %>%
  group_by(daysInOffice) %>%
  summarise(carterOrders = n())

reagan %<>%
  mutate(start_date = lubridate::as_date("1981-01-20"),
         signing_date = lubridate::as_date(signing_date,format="%m/%d/%Y")) %>%
  mutate(daysInOffice = as.integer(difftime(signing_date,start_date,units="days"))) %>%
  group_by(daysInOffice) %>%
  summarise(reaganOrders = n())

ghw_bush %<>%
  mutate(start_date = lubridate::as_date("1989-01-20"),
         signing_date = lubridate::as_date(signing_date,format="%m/%d/%Y")) %>%
  mutate(daysInOffice = as.integer(difftime(signing_date,start_date,units="days"))) %>%
  group_by(daysInOffice) %>%
  summarise(ghw_bushOrders = n())

clinton %<>%
  mutate(start_date = lubridate::as_date("1993-01-20"),
         signing_date = lubridate::as_date(signing_date,format="%m/%d/%y")) %>%
  mutate(daysInOffice = as.integer(difftime(signing_date,start_date,units="days"))) %>%
  group_by(daysInOffice) %>%
  summarise(clintonOrders = n())

gw_bush %<>%
  mutate(start_date = lubridate::as_date("2001-01-20"),
         signing_date = lubridate::as_date(signing_date,format="%m/%d/%Y")) %>%
  mutate(daysInOffice = as.integer(difftime(signing_date,start_date,units="days"))) %>%
  group_by(daysInOffice) %>%
  summarise(gw_bushOrders = n())

obama %<>%
  mutate(start_date = lubridate::as_date("2009-01-20"),
         signing_date = lubridate::as_date(signing_date,format="%m/%d/%Y")) %>%
  mutate(daysInOffice = as.integer(difftime(signing_date,start_date,units="days"))) %>%
  group_by(daysInOffice) %>%
  summarise(obamaOrders = n())

trump %<>%
  mutate(start_date = lubridate::as_date("2017-01-20"),
         signing_date = lubridate::as_date(signing_date,format="%m/%d/%Y")) %>%
  mutate(daysInOffice = as.integer(difftime(signing_date,start_date,units="days"))) %>%
  group_by(daysInOffice) %>%
  summarise(trumpOrders = n())

orders = data_frame(daysInOffice = seq(0,8*365.25,1)) %>%
  left_join(nixon) %>%
  left_join(ford) %>%
  left_join(carter) %>%
  left_join(reagan) %>%
  left_join(ghw_bush) %>%
  left_join(clinton) %>%
  left_join(gw_bush) %>%
  left_join(obama) %>%
  left_join(trump)

orders[is.na(orders)] = 0

for(i in 2:ncol(orders)){
  orders[[i]] = cumsum(orders[[i]])
}

ymax = orders %>%
  filter(daysInOffice <= ceiling(bidenDaysInOffice*1.1)) %>%
  select(-daysInOffice) %>%
  lapply(max) %>%
  unlist() %>%
  max() %>%
  `*`(1.1) %>%
  ceiling()

orders %>%
  # filter(daysInOffice <= ceiling(trumpDaysInOffice*1.1)) %>%
  plot_ly(x = ~daysInOffice, mode='lines') %>%
  add_trace(y = ~nixonOrders,name="Nixon",line=list(color='rgb(128,0,0)')) %>%
  add_trace(y = ~fordOrders,name="Ford",line=list(color='rgb(250,128,114)')) %>%
  add_trace(y = ~carterOrders,name="Carter",line=list(color='rgb(30,144,255)')) %>%
  add_trace(y = ~reaganOrders,name="Reagan",line=list(color='rgb(255,69,0)')) %>%
  add_trace(y = ~ghw_bushOrders,name="G.H.W. Bush",line=list(color='rgb(219,112,147)')) %>%
  add_trace(y = ~clintonOrders,name="Clinton",line=list(color='rgb(0,0,205)')) %>%
  add_trace(y = ~gw_bushOrders,name="G.W. Bush",line=list(color='rgb(220,20,60)')) %>%
  add_trace(y = ~obamaOrders,name="Obama",line=list(color='rgb(0,191,255)')) %>%
  add_trace(y = ~trumpOrders,name="Trump",line=list(color='rgb(255,0,0)')) %>%
  layout(shapes=list(type='line', x0= 1460, x1= 1460, y0=0, y1=400, line=list(dash='dash', width=1)),
         xaxis = list(title="Days in Office"),
         yaxis = list(title="# of Executive Orders Signed"),
         title = 'How quickly did presidents sign Executive Orders over the course of their terms?')
