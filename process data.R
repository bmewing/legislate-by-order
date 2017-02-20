library(readr)
library(dplyr)
library(plotly)
library(lubridate)
library(magrittr)

#data source: https://www.federalregister.gov/executive-orders
#trump data is being sourced from the federal register through their api

clinton = read_csv("data/documents_signed_by_william_j_clinton_of_type_presidential_document_and_of_presidential_document_type_executive_order.csv")
bush = read_csv("data/documents_signed_by_george_w_bush_of_type_presidential_document_and_of_presidential_document_type_executive_order.csv")
obama = read_csv("data/documents_signed_by_barack_obama_of_type_presidential_document_and_of_presidential_document_type_executive_order.csv")
trump = httr::GET("https://www.federalregister.gov/documents/search.json?conditions%5Bcorrection%5D=0&conditions%5Bpresident%5D=donald-trump&conditions%5Bpresidential_document_type_id%5D=2&conditions%5Btype%5D=PRESDOCU&fields%5B%5D=citation&fields%5B%5D=document_number&fields%5B%5D=end_page&fields%5B%5D=executive_order_notes&fields%5B%5D=executive_order_number&fields%5B%5D=html_url&fields%5B%5D=pdf_url&fields%5B%5D=publication_date&fields%5B%5D=signing_date&fields%5B%5D=start_page&fields%5B%5D=title&fields%5B%5D=full_text_xml_url&fields%5B%5D=body_html_url&fields%5B%5D=json_url&order=executive_order_number&per_page=1000") %>% 
  httr::content(as="text") %>% 
  jsonlite::fromJSON() %>% 
  `$`("results")

write_csv(trump,"data/documents_signed_by_donald_trump_of_type_presidential_document_and_of_presidential_document_type_executive_order.csv")

clinton %<>%
  mutate(start_date = lubridate::as_date("1993-01-20"),
         signing_date = lubridate::as_date(signing_date,format="%m/%d/%Y")) %>% 
  mutate(daysInOffice = difftime(signing_date,start_date,units="days")) %>% 
  group_by(daysInOffice) %>% 
  summarise(clintonOrders = n())

bush %<>%
  mutate(start_date = lubridate::as_date("2001-01-20"),
         signing_date = lubridate::as_date(signing_date,format="%m/%d/%Y")) %>% 
  mutate(daysInOffice = difftime(signing_date,start_date,units="days")) %>% 
  group_by(daysInOffice) %>% 
  summarise(bushOrders = n())

obama %<>%
  mutate(start_date = lubridate::as_date("2009-01-20"),
         signing_date = lubridate::as_date(signing_date,format="%m/%d/%Y")) %>% 
  mutate(daysInOffice = difftime(signing_date,start_date,units="days")) %>% 
  group_by(daysInOffice) %>% 
  summarise(obamaOrders = n())

trump %<>%
  mutate(start_date = lubridate::as_date("2017-01-20"),
         signing_date = lubridate::as_date(signing_date)) %>% 
  mutate(daysInOffice = difftime(signing_date,start_date,units="days")) %>% 
  group_by(daysInOffice) %>% 
  summarise(trumpOrders = n())

orders = data_frame(daysInOffice = seq(0,8*365.25,1)) %>% 
  left_join(clinton) %>% 
  left_join(bush) %>% 
  left_join(obama) %>% 
  left_join(trump)

orders[is.na(orders)] = 0

for(i in 2:nrow(orders)){
  if(orders$clintonOrders[i] == 0) orders$clintonOrders[i] = orders$clintonOrders[i-1] else orders$clintonOrders[i] = orders$clintonOrders[i]+orders$clintonOrders[i-1]
  if(orders$bushOrders[i] == 0) orders$bushOrders[i] = orders$bushOrders[i-1] else orders$bushOrders[i] = orders$bushOrders[i]+orders$bushOrders[i-1]
  if(orders$obamaOrders[i] == 0) orders$obamaOrders[i] = orders$obamaOrders[i-1] else orders$obamaOrders[i] = orders$obamaOrders[i]+orders$obamaOrders[i-1]
  if(orders$trumpOrders[i] == 0) orders$trumpOrders[i] = orders$trumpOrders[i-1] else orders$trumpOrders[i] = orders$trumpOrders[i]+orders$trumpOrders[i-1]
}

orders %>% 
  plot_ly(x = ~daysInOffice) %>% 
  add_lines(y = ~clintonOrders,name="Clinton") %>% 
  add_lines(y = ~bushOrders,name="Bush") %>% 
  add_lines(y = ~obamaOrders,name="Obama") %>% 
  add_lines(y = ~trumpOrders,name="Trump") %>% 
  layout(xaxis = list(title="Days in Office"),yaxis = list(title="# of Executive Orders Signed"))
