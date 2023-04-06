

library(tidyverse)
library(gt)

#GETTING THE FINAL LIST OF ITEMS ###############

#├ Loading master file with all the FCTs information -----

output1 <- readxl::read_excel(here::here("data",
                                         "food-items-data-collection.xlsx"),
                              sheet = "outcome1") 
#├ Loading master file with all the items and references for all the FCTs -----

#removing two "milk items from LSOFCT (check)
#removing dried Freshwater fish from MW0020, EA0006, KE0018
#Excluding oil crops
fct.item <- readxl::read_excel(here::here("data", 
                                          "food-items-data-collection.xlsx"),
                               sheet = "outcome2") %>% 
  filter(!food.id.original %in% c("030007", "030019", 
                                  "MW03_0027", "123",
                                  "8002") & 
           food.category != "Oilcrops, Other and products") %>% 
  mutate_at("food.name.original", str_squish) %>% 
  naniar::replace_with_na_all(condition = ~.x == "NA")

#checking that the 17 
#(SW0022 is not included bc no items were selected) are loaded
fct.item %>% filter(!is.na(food.name.original)) %>% count(fct.id)

#fixing a typos
fct.item$fct.id[fct.item$fct.id == "NG0022"] <- "NG0021"
fct.item$fct.ref_io[fct.item$food.id.original == "4352"] <- 15
#Fixing issing one reference! for MW01_0011
fct.item$fct.ref[fct.item$food.id.original == "MW01_0011"] <- "10"
#Rice in LS0010
fct.item$food.id.original[fct.item$food.id.original == "0111022"] <- "011022"

fct.item %>% filter(food.id.original == "0111022") %>% pull(comments)

