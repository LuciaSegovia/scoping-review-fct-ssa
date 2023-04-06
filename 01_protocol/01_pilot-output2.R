
#Data collection pilot. 

library(tidyverse)

test.wa <- read.csv(here::here("data", "food-items-data-collection.csv")) %>% 
  distinct()

#wafct processed for MAPS
wafct <- read_csv(here::here("data", "MAPS_WAFCT_v1.1.csv"))
#%>% 
 # rename(original_food_id = "ï..original_food_id" )

#biblio (ref.) from the original wafct (change source)

biblio <- read.csv(here::here("data","WAFCT_biblio.csv")) %>% 
  mutate_at("BiblioID", str_to_lower)


test.wa <- test.wa %>% left_join(., wafct, by=c("food.id" = "original_food_id")) %>% 
  select(food.id, food.name, data_reference_original_id ) %>% 
  mutate(line = 1:n())

#write.csv(test.wa, here::here("data", "wafct_item-list.csv"), row.names = F)


test.wa <- test.wa %>% tidytext::unnest_tokens(biblio, data_reference_original_id) %>% 
  left_join(., biblio, by = c("biblio" = "BiblioID")) 

#count of ref. per food item (preliminary food list)
test.wa %>% filter(!is.na(Reference)) %>% group_by(food.id) %>% count(food.id) %>% 
  arrange(desc(n))

#count of total ref. from wafct (preliminary food list)
test.wa %>% filter(!is.na(Reference)) %>% count()

#unique ref. from wafct (preliminary food list)
test.wa %>% filter(!is.na(Reference)) %>% distinct(Reference) %>%  count()

#checking info for fortifided maize flour
test.wa %>% filter(food.id == "01_093")

#count references w/ matches (clean) and from preliminary food list 
#from coming from WAFCT, 2012

test.wa %>% filter(str_detect(biblio, "^[:digit:]{2}\\_") | !is.na(Reference) )%>%
 distinct(Reference) %>% count()

#List of the location of each ref. 

ref.location <- c("Australia", 
                  "US",
                  "Guinea",
                  "India",
                  "Nigeria",
                  "Africa?",
                  "UK",
                  "Mali",
                  "Cameroon, Togo",
                  "Ghana",
                  "Kenya",
                  "Ghana",
                  "Ghana",
                  "Benin",
                  "Africa?",
                  "Ethiopia",
                  "Africa?",
                  "South Africa",
                  "South Africa",
                  "Cameroon?",
                  "Burkina Faso",
                  "Burkina Faso",
                  "East-Africa",
                  "Benin",
                  "Benin",
                  "Ghana",
                  "Nigeria",
                  "Nigeria",
                  "Nigeria",
                  "Nigeria",
                  "Nigeria",
                  "Nigeria",
                  "Ghana",
                  "Nigeria",
                  "South Africa",
                  "Global?",
                  "Afica?",
                  "Ghana",
                  "Global?",
                  "Nigeria",
                  "Burkina Faso",
                  "Benin",
                  "Africa?",
                  "Africa?",
                  "East-Africa, Tanzania", 
                  "West-Africa",
                  "Kenya", 
                  "Burkina Faso",
                  "Sudan",
                  "Burkina Faso",
                  "Brazil",
                  "Ghana",
                  "Benin",
                  "Senegal",
                  "Ivory Coast",
                  "Nigeria",
                  "Nigeria",
                  "Nigeria",
                  "Nigeria",
                  "Nigeria",
                  "Ghana",
                  "Burkina Faso?",
                  "Ghana",
                  "Nigeria",
                  "Nigeria",
                  "Nigeria",
                  "Southern Africa?",
                  "South Africa",
                  "Gambia",
                  "Nigeria",
                  "Nigeria",
                  "West-Africa?",
                  "Mali",
                  "Benin",
                  "Cameroon",
                  "Ghana",
                  "Nigeria",
                  "Benin?",
                  "Nigeria",
                  "Nigeria",
                  "Nigeria",
                  "Global?",
                  "Uganda",
                  "Uganda",
                  "South Africa",
                  "Senegal",
                  "Burkina Faso",
                  "Burkina Faso?",
                  "Cameroon",
                  "Ghana",
                  "Germany",
                  "Cameroon",
                  "Mauritania",
                  "ASEAN?",
                  "Ghana",
                  "Ghana",
                  "Senegal",
                  "Ghana",
                  "Nigeria",
                  "Nigeria",
                  "Nigeria",
                  "Nigeria",
                  "Nigeria?",
                  "Nigeria",
                  "Nigeria",
                  "Nigeria",
                  "Nigeria?",
                  "Sudan",
                  "Nigeria",
                  "Global?",
                  "Egypt",
                  "Egypt",
                  "Algeria",
                  "Morocco",
                  "Denmark",
                  "Global?",
                  "Nigeria",
                  "Ethiopia",
                  "Ghana",
                  "South Africa",
                  "South Africa")


wa.ref_location <- test.wa  %>% filter(!is.na(Reference)) %>% distinct(Reference, .keep_all = T) %>% 
  cbind(ref.location) %>% select(biblio, ref.location)
