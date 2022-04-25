
library(tidyverse)

#0) Loading the data

#food balance sheet (FAO, 2020)

read_csv(here::here("data", "FBS-supply-SSA-2014-2018.csv"))
read_csv(here::here("data", "FBS-energy-SSA-2014-2018.csv")) 

#regional FCDB: regional food composition data for SSA (Joy et al, 2014)

read.csv(here::here("data", "regional-SSA-fct-fbs-codes.csv"))

fct <- read.csv(here::here("data", "regional-SSA-fct-fbs-codes.csv")) %>% #select variables of interest
  select(Region:Zn_mg, ID_1, FoodName_1) %>% 
  relocate(c(ID_1, FoodName_1), .before = "Region") %>% distinct() #re-arrenging columns

#Concentration (100 g-1 fresh weight)

readxl::excel_sheets(here::here("data", "ppl12144-sup-0002-tables2.xlsx"))

fct <- readxl::read_excel(here::here("data", "ppl12144-sup-0002-tables2.xlsx"),
                          sheet = "S Table 2")

mn <- c("Energy", "Ca", "Cu", "Fe", "I", "Mg", "Se", "Zn")

fct <- fct %>% select(2:4, 8) %>%  filter(`...3` %in% mn) %>% 
  pivot_wider(
    names_from = `...3`, 
    values_from = "...8") %>% 
  rename(region = "Supplementary Table 2. Food mineral composition data from literature sources, used in conjunction with Food Balance Sheets (FBSs) to estimate dietary mineral availability",
         food.name.original = "...4")

#countries and regions in Africa  

africa <- raster::ccodes() %>%  filter(continent == "Africa")

#Fixing names in NAME_FAO to be the same as in food balance sheet
raster::ccodes() %>% filter(str_detect(NAME_FAO, "Tanzania"))

africa$NAME_FAO[africa$NAME_FAO == "Cape Verde"] <-  "Cabo Verde"
africa$NAME_FAO[africa$NAME_FAO == "Congo, Republic of"] <-  "Congo"
africa$NAME_FAO[africa$NAME_FAO == "Swaziland"] <-  "Eswatini"
africa$NAME_FAO[africa$NAME_FAO == "Tanzania, United Rep of"] <-  "United Republic of Tanzania"

#1) Calculating MN supply in SSA

#loading food supply in kg/cap/year to estimate MN supply
qty <- read_csv(here::here("data", "FBS-supply-SSA-2014-2018.csv")) %>%
  janitor::clean_names() %>% filter(element_code != "511") #filter out pop. count

#Preparing food supply for merging w/ regional FCDB

#1.1) Generating the variable Region per country 
#checking country allocation per region
qty %>% 
  left_join(., africa %>% select(NAME_FAO, UNREGION1),
            by = c("area" = "NAME_FAO")) %>%
  filter(is.na(UNREGION1)) %>% pull(area)

qty <- qty %>% 
  left_join(., africa %>% select(NAME_FAO, UNREGION1),
            by = c("area" = "NAME_FAO")) %>% 
  mutate(Region = case_when(
  str_detect(UNREGION1, "Eastern") ~ "E", 
  str_detect(UNREGION1, "Western") ~ "W",
  str_detect(UNREGION1, "Southern") ~ "S", 
  str_detect(UNREGION1, "Middle") ~ "M",
  TRUE ~ UNREGION1))  
  
#1.2) Unit conversion and mean food supply per food category and region (year, country)   

mean_qty <- qty %>% group_by(item_code, item, area_code, area, Region) %>% 
  summarise(mean_value = mean(value)) %>%  #mean food supply per country 
  ungroup() %>%  arrange(desc(mean_value))
 
total_qty <- mean_qty %>%  group_by(item_code, item, Region) %>% 
  summarise(total_value = sum(mean_value)*10/365) %>% #total supply per region (100g/capita/day)
  arrange(desc(total_value)) 
  

fct_fao_codes <- fct %>% select(food.name.original) %>% distinct() %>% 
  left_join(total_qty %>% select(item_code, item) %>% distinct(), 
            by = c("food.name.original" = "item"))

fct_fao_codes <- fct_fao_codes %>% 
  mutate(food.name.original = ifelse(is.na(item_code), paste(food.name.original, "and products"), 
                            food.name.original)) %>%
  select(-item_code) %>% 
  left_join(total_qty %>% select(item_code, item) %>% distinct(), 
            by = c("food.name.original" = "item"))

#Meat, Aquatic Mammals not found in Africa (2768)
#Sugar beet
#Sugar cane
#Sugar non-cen
#sunflowerseeds

fao_codes <- c("2620", "2552", "2768",  #Meat, Aquatic Mammals not found in Africa took from world list
  "2563", "2807", "2561", "2537" , #Sugar beet
  "2536", #Sugar cane
  "2541", #Sugar non-cen
  "2557", #sunflowerseeds
 "2533", 
 "2635" )


fao_na <- fct_fao_codes %>% filter(is.na(item_code)) %>% 
  cbind(fao_codes) %>% select(-item_code) %>% 
  rename(item_code = fao_codes)

fct_fao_codes <- fct_fao_codes %>% filter(!is.na(item_code)) %>% 
  rbind(fao_na) %>% 
  mutate(food.name.original = str_replace(food.name.original, " and products", ""))

fct <- fct %>% filter(region == "S") %>% mutate(region = "M") %>% 
  rbind(fct) %>% left_join(., fct_fao_codes) %>% 
  relocate(item_code, .before = food.name.original)

total_qty$item_code <- as.character(total_qty$item_code)

fao_fct <- left_join(total_qty , fct, by = c("item_code", 
                            "Region" = "region"), keep = TRUE) %>% 
  filter(!is.na(Energy)) %>% 
  mutate_at(mn, as.numeric)
  
#%>% 
  filter(is.na(Energy), total_value >0.1) %>% 
  distinct(item)


#NOT reported in the FCT
# Miscellaneous
# Infant food (below 0.1*100g/cap/day)
# Alcohol, non-food -> remove bc is not food
# Rape and Mustardseed (below 0.1*100g/cap/day)
# Palm kernels -> remove because is < 0

for(i in 1:length(mn)){
  
fao_fct[15+i] <-  (fao_fct$total_value * fao_fct[mn[i]])

colnames(fao_fct)[15+i] <- paste0(mn[i], "_cap_day")

}
  
fao_fct %>% 
  ggplot(aes(Zn_cap_day, region)) + geom_boxplot()

fao_fct %>% filter(Zn_cap_day >20)

# 1.5) cleaning the dataset w/ only the needed variables

glimpse(fao_fct)

#TOP 10 - w/ energy calculated (compare w/ Energy from FAOSTAT)

top10 <-  fao_fct %>% dplyr::select(c(item_code.x:total_value, ends_with("day"))) %>% 
  pivot_longer(
    cols = c(total_value:Zn_cap_day), 
    names_to = "element", 
    values_to = "value"
  ) %>% rename(item_code = "item_code.x") %>% 
  group_by(element, item_code, item) %>% 
  summarise(ave = mean(value), 
            total = sum(value)) %>% 
  ungroup() %>% 
  group_by(element) %>% 
  slice_max(ave, n= 10) 

#list of foods
top10 %>% arrange(element) %>% distinct(item) %>% pull(item) %>% unique()

#Table with the FAO food categories to be included

x <- top10 %>% ungroup() %>% select(1:4) %>% filter(element != "total_value") %>% 
  pivot_wider(names_from = element, 
              values_from = ave) %>% 
  relocate(Energy_cap_day, .after = item) %>% arrange(desc(Energy_cap_day))

# Need to compare Energy from FAOSTAT and pivot table to see items and quantities
# Same items but slight different order. We have found discrepancies: 
# we need to include plantain (from the energy both methods) and potatoes
# remove sesame seeds and oilcrops, 

x %>% filter(str_detect(item, "oil|Oil"))

#binding the two dataset together

fbs <- read_csv(here::here("data", "FBS-supply-SSA-2014-2018.csv")) %>% 
  rbind(.,read_csv(here::here("data", "FBS-energy-SSA-2014-2018.csv"))) %>% 
  janitor::clean_names()

#Food supply: top-10 food categories by kcal and kg
#calculating the yearly and region mean % of supply

fbs <- fbs %>% 
  group_by(element_code, element, item_code, item) %>% 
  summarise(ave = mean(value), 
            total = sum(value))
  
top <- fbs %>% ungroup() %>% 
  group_by(element_code, element) %>% 
  slice_max(ave, n= 10) 

top %>% distinct(item) %>% pull(item)

rm(top)

