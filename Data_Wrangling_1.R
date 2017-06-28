
# 0 Load data
refine <- read_csv("C:/Users/Michael/Desktop/SpringBoard/refine_original.csv")
View(refine)

#load files
library(readr)
library(tidyr)
library(dplyr)

# 1 clean up brand names
lowercase <- mutate (refine, company = tolower(company))

#find all unique names
unique_company <- distinct(select(lowercase, company))

philipsgroup <- c( "phillips", "philips", "phllips", "phillps", "fillips", "phlips")
akzogroup <- c("akzo", "akz0", "ak zo")
vanhatongroup <- c("van houten")
unlivergroup <- c( "unilver", "unilever")

phillips <- "philips"
akzo <- "akzo"
Vanhaton <- "van houten"
unilever <- "unilever"

#change company names

clean_company <- lowercase %>% mutate (company = case_when(
  .$company %in% philipsgroup ~ phillips,
  .$company %in% akzogroup ~ akzo,
  .$company %in% vanhatongroup ~ Vanhaton,
  .$company %in% unlivergroup ~ unilever
))



#2 seperate product code and number
Split_product <- clean_company %>% separate( "Product code / number", c("product_code", "product_number"), sep ="-" )


#3 product Categories
product_categories <- Split_product %>%mutate(product_code = case_when (
  .$product_code %in% c("p") ~ 'Smartphone',
  .$product_code %in% c('v') ~ 'TV',
  .$product_code %in% c('x') ~ 'Laptop',
  .$product_code %in% c('q') ~ 'Tablet'
)) 

#4 Add address for Geocoding
Address <- product_categories %>% unite (Geoaddress, address:country, sep = ",", remove = TRUE)

#5 Creating Variables
dummy_variables <- Address %>%
  mutate(company_binary = 1, product_binary = 1) %>%
  mutate(company = paste0("company_", company), product_code = paste0("product_", product_code)) %>%
  spread(company, company_binary, fill = 0) %>%
  spread(product_code, product_binary, fill = 0)

## save as .csv
write.csv (dummy_variables, "refine_clean.csv")