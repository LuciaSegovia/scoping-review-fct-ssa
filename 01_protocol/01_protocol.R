
#Packages dependencies
#install.packages("tidyverse")
#install.packages("gt")


library(tidyverse)
library(gt)
#library(gtExtras)


# 1) Loading the data -----

#food balance sheet (FAO, 2020)

#loading path as downloaded from FAOSTAT

supply <- "FBS_supply-regional-SSA_2014-2018.csv"

fbs <- read_csv(here::here("data", supply))

#regional FCDB: regional food composition data for SSA (Joy et al, 2014)
#Concentration (100 g-1 fresh weight)

readxl::excel_sheets(here::here("data", "ppl12144-sup-0002-tables2.xlsx"))

fct <- readxl::read_excel(here::here("data", "ppl12144-sup-0002-tables2.xlsx"),
                          sheet = "S Table 2")

#Units information
fct %>% select(3, 7) %>% distinct()

#cleaning and preparing fct

mn <- c("Energy", "Ca",  "Fe", "I",  "Se", "Zn")

fct <- fct %>% select(2:4, 8) %>%  filter(`...3` %in% mn) %>% 
  pivot_wider(
    names_from = `...3`, 
    values_from = "...8") %>% 
  rename(region = "Supplementary Table 2. Food mineral composition data from literature sources, used in conjunction with Food Balance Sheets (FBSs) to estimate dietary mineral availability",
         food.name.original = "...4")


#2) Calculating mineral supply in SSA ----

# 2.1) Cleaning the dataset w/ only the needed variables


years <- c(2014:2018)

#Filtering food supply in kg/cap/year to estimate MN supply
qty <- fbs %>%
  janitor::clean_names() %>% 
  filter(year %in% years, element_code != "664") #filter out years from after 2019 & supply in kcal 
                                                      #for reproducibility

#Preparing food supply for merging w/ regional FCDB

qty <- qty %>% 
  mutate(Region = case_when(
  str_detect(area, "Eastern") ~ "E", 
  str_detect(area, "Western") ~ "W",
  str_detect(area, "Southern") ~ "S", 
  str_detect(area, "Middle") ~ "M",
  TRUE ~ area))  
  
# 2.2) Unit conversion and mean food supply per food category and region (year)   

#Mean supply of the years 
total_qty <- qty %>% group_by(item_code, item, area_code, area, Region) %>% 
  summarise(total_value = mean(value)*10/365) %>%  #mean food supply per region 
  ungroup() %>%  arrange(desc(total_value)) %>% 
  mutate_at("item", str_to_lower)
  
#Binding food supply with fct by name and code
fct_fao_codes <- fct %>% select(food.name.original) %>% distinct() %>% 
  mutate_at("food.name.original", str_to_lower) %>% 
  left_join(total_qty %>% select(item_code, item) %>% distinct(), 
            by = c("food.name.original" = "item"))

fct_fao_codes <- fct_fao_codes %>% 
  mutate(food.name.original = ifelse(is.na(item_code), 
                                     paste(food.name.original, "and products"), 
                            food.name.original)) %>%
  select(-item_code) %>% 
  left_join(total_qty %>% select(item_code, item) %>% distinct(), 
            by = c("food.name.original" = "item"))

#items w/o matching codes
fct_fao_codes %>% filter(is.na(item_code)) 

#Meat, Aquatic Mammals not found in Africa (2768)

fao_codes <- c(
  "2620",  #grapes
  "2552", #groudnuts
  "2768",  #Meat, Aquatic Mammals not found in Africa took from world list
  "2563", #olives
  "2807",  #rice 
  "2561", #sesameseed
# "2537" , #Sugar beet
#  "2536", #Sugar cane
  "2541", #Sugar non-cen
  "2557", #sunflowerseeds
#  "2533",   #sweet potato
 "2635"  ) #tea

qty %>% filter(item_code %in% fao_codes ) %>% distinct(item, item_code)

fao_na <- fct_fao_codes %>% filter(is.na(item_code)) %>% 
  cbind(fao_codes) %>% select(-item_code) %>% 
  rename(item_code = fao_codes)

fct_fao_codes <- fct_fao_codes %>% filter(!is.na(item_code)) %>% 
  rbind(fao_na) %>% 
  mutate(food.name.original = str_replace(food.name.original, " and products", "")) 

fct_fao_codes %>% filter(is.na(item_code))

#adding middle region to fct (acc. Joy et al., (2015))
fct <- fct %>% filter(region == "W") %>% mutate(region = "M") %>% 
  rbind(fct) %>% 
  mutate_at("food.name.original", str_to_lower) %>% 
  left_join(., fct_fao_codes) %>% 
  relocate(item_code, .before = food.name.original)

total_qty$item_code <- as.character(total_qty$item_code)
fct$item_code <- as.character(fct$item_code)

fao_fct <- left_join(total_qty , fct, by = c("item_code", 
                            "Region" = "region"), keep = TRUE) %>% 
  filter(!is.na(Energy)) %>% 
  mutate_at(mn, as.numeric)

#Food items NOT covered in the FCT  
(not_reported <- left_join(total_qty , fct, by = c("item_code", 
                                  "Region" = "region"), keep = TRUE) %>%
  filter(is.na(Energy)) %>% 
  group_by(item) %>% summarise(mean = mean(total_value, na.rm =T)))


#Calculating energy and mineral supply per food item per region
n <- length(fao_fct)

for(i in 1:length(mn)){
  
fao_fct[n+i] <-  (fao_fct$total_value * fao_fct[mn[i]])

colnames(fao_fct)[n+i] <- paste0(mn[i], "_cap_day")

}
  
fao_fct %>% 
  ggplot(aes(Zn_cap_day, region)) + geom_boxplot()


# 1.5) cleaning the dataset w/ only the needed variables

glimpse(fao_fct)

#TOP 10 - w/ energy calculated 
#(compare w/ Energy from FAOSTAT below)

top10 <-  fao_fct %>% dplyr::select(c(item_code.x:total_value, ends_with("day"))) %>% 
  pivot_longer(
    cols = c(Energy_cap_day:Zn_cap_day), 
    names_to = "element", 
    values_to = "value"
  ) %>% rename(item_code = "item_code.x") %>% 
  group_by(element, item_code, item) %>% 
  summarise(ave = mean(value), 
            total = sum(value)) %>% 
  ungroup()  %>%   group_by(element) %>% 
  slice_max(ave, n= 10) 

## Checking category list ----

#Checking cummulative Energy supply (80%)
(data <- fao_fct %>% dplyr::select(c(item_code.x:total_value, ends_with("day"))) %>% 
  pivot_longer(
    cols = c(Energy_cap_day:Zn_cap_day), 
    names_to = "element", 
    values_to = "value"
  ) %>% rename(item_code = "item_code.x") %>% 
  group_by(element, item_code, item) %>% 
  summarise(ave = mean(value, na.rm =T), 
            total = sum(value)) %>% 
  ungroup()  %>%   group_by(element) %>% 
  mutate(perc = ave/sum(ave)*100) %>%
  pivot_wider(names_from = element, 
              values_from = c(ave,total, perc)) %>% 
  relocate(perc_Energy_cap_day, .after = item) %>% 
  arrange(desc(perc_Energy_cap_day)) %>% 
  mutate(cum_Energy_cap_day = cumsum(perc_Energy_cap_day)) %>% 
  relocate(cum_Energy_cap_day, .before = perc_Energy_cap_day)) 

## OUTPUTS ------

#Table with the FAO food categories to be included (excel)

top10 %>% ungroup() %>% select(1:4) %>%
  #filter(element != "total_value") %>% 
  pivot_wider(names_from = element, 
              values_from = ave) %>% 
  relocate(Energy_cap_day, .after = item) %>% arrange(desc(Energy_cap_day)) %>% 
  write.csv(., here::here("output", "pre-selected-food-list-ssa.csv"),
            row.names = F)


#Table with the FAO food categories to be included (gt formatted)

elements <- c("Energy_cap_day", "Ca_cap_day" ,
                "Fe_cap_day","I_cap_day", "Se_cap_day", "Zn_cap_day" )


top10 %>% ungroup() %>% select(1:4) %>% filter(element != "total_value") %>% 
  pivot_wider(names_from = element, 
              values_from = ave) %>% 
  relocate(Energy_cap_day, .after = item) %>% arrange(desc(Energy_cap_day)) %>%
  rename_at(elements, ~gsub("_cap_day", "", elements)) %>% 
  gt()  %>% 
  tab_header(title = "Energy and mineral supplies in sub-Saharan Africa from 2014 to 2018") %>% 
  tab_spanner(
    label = "FAO Food Balance - Food Categories",
    columns = c("item_code", "item")) %>%
  tab_spanner(
    label = "Supply per capita per day",
    columns = mn)  %>%
  fmt_number(
    columns = mn ,
    decimals = 2,
    use_seps = TRUE) %>%
  fmt_missing(
    columns = everything(),
    missing_text = "")   %>% 
  text_transform(
      locations = cells_body(columns = item),
      fn = function(x) str_to_sentence (x)
    ) %>% 
  cols_label(
    item_code = "Id.",
    item = "Desc.",
    Energy = html("Energy,<br>kcal"),
    Ca = html("Ca,<br>mg"),
   # Cu = html("Cu,<br>mg"),
    Fe = html("Fe,<br>mg"),  
    I = html("I,<br>mcg"),
  #  Mg = html("Mg,<br>mg"),
    Se = html("Se,<br>mcg"),
    Zn = html("Zn,<br>mg"))  %>%
  tab_options(
    column_labels.font.size = "smaller",
    table.font.size = "smaller",
    data_row.padding = px(3)
  ) %>% 
  tab_source_note(
    source_note = html("Data on supply (quantity in kg/cap/year) was obtained 
    from FAOSTAT (FAO, 2020) and,<br>
    food composition data was obtained from Joy et al., 2015")) 
  
  
  
