
library(tidyverse)
library(gt)
#library(gtExtras)


#0) Loading the data

#food balance sheet (FAO, 2020)

#loading path as downloaded from FAOSTAT

supply <- paste0("FAOSTAT_data_", gsub("^0", "", format(Sys.Date(), "%m-%d-%Y")), ".csv")

read_csv(here::here("data", supply))

#regional FCDB: regional food composition data for SSA (Joy et al, 2014)
#Concentration (100 g-1 fresh weight)

readxl::excel_sheets(here::here("data", "ppl12144-sup-0002-tables2.xlsx"))

fct <- readxl::read_excel(here::here("data", "ppl12144-sup-0002-tables2.xlsx"),
                          sheet = "S Table 2")

#Units information
fct %>% select(3, 7) %>% distinct()

#cleaning and preparing fct

mn <- c("Energy", "Ca", "Cu", "Fe", "I", "Mg", "Se", "Zn")

fct <- fct %>% select(2:4, 8) %>%  filter(`...3` %in% mn) %>% 
  pivot_wider(
    names_from = `...3`, 
    values_from = "...8") %>% 
  rename(region = "Supplementary Table 2. Food mineral composition data from literature sources, used in conjunction with Food Balance Sheets (FBSs) to estimate dietary mineral availability",
         food.name.original = "...4")

##Getting countries and regions in Africa  
#
#africa <- raster::ccodes() %>%  filter(continent == "Africa")
#
##Fixing names in NAME_FAO to be the same as in food balance sheet
#raster::ccodes() %>% filter(str_detect(NAME_FAO, "Congo"))
#
#africa$NAME_FAO[africa$NAME_FAO == "Cape Verde"] <-  "Cabo Verde"
#africa$NAME_FAO[africa$NAME_FAO == "Congo, Republic of"] <-  "Congo"
#africa$NAME_FAO[africa$NAME_FAO == "Congo, Dem Republic of"] <-  "Democratic Republic of the Congo"
#africa$NAME_FAO[africa$NAME_FAO == "Swaziland"] <-  "Eswatini"
#africa$NAME_FAO[africa$NAME_FAO == "Tanzania, United Rep of"] <-  "United Republic of Tanzania"
#
#1) Calculating MN supply in SSA

years <- c(2014:2018)
#year < 2019

#loading food supply in kg/cap/year to estimate MN supply
qty <- read_csv(here::here("data", supply))%>%
  janitor::clean_names() %>% 
  filter(year %in% years, element_code != "664") #filter out years from after 2019 & supply in kcal 
                                                      #for reproducibility

#Preparing food supply for merging w/ regional FCDB

#1.1) Generating the variable Region per country 
#checking country allocation per region

# qty %>% 
#   left_join(., africa %>% select(NAME_FAO, UNREGION1),
#             by = c("area" = "NAME_FAO")) %>%
#   filter(is.na(UNREGION1)) %>% pull(area)



qty <- qty %>% 
  mutate(Region = case_when(
  str_detect(area, "Eastern") ~ "E", 
  str_detect(area, "Western") ~ "W",
  str_detect(area, "Southern") ~ "S", 
  str_detect(area, "Middle") ~ "M",
  TRUE ~ area))  
  
#1.2) Unit conversion and mean food supply per food category and region (year, country)   

#Mean supply of the years 
total_qty <- qty %>% group_by(item_code, item, area_code_fao, area, Region) %>% 
  summarise(total_value = mean(value)*10/365) %>%  #mean food supply per country 
  ungroup() %>%  arrange(desc(total_value)) %>% 
  mutate_at("item", str_to_lower)

#Total supply in each region (100g/capita/day)
#total_qty <- mean_qty %>%  group_by(item_code, item, Region) %>% 
#  summarise(total_value = sum(mean_value)*10/365) %>% 
#  arrange(desc(total_value)) %>% 
#  mutate_at("item", str_to_lower)
  
#Binding food supply with fct by name and code
fct_fao_codes <- fct %>% select(food.name.original) %>% distinct() %>% 
  mutate_at("food.name.original", str_to_lower) %>% 
  left_join(total_qty %>% select(item_code, item) %>% distinct(), 
            by = c("food.name.original" = "item"))

fct_fao_codes <- fct_fao_codes %>% 
  mutate(food.name.original = ifelse(is.na(item_code), paste(food.name.original, "and products"), 
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
left_join(total_qty , fct, by = c("item_code", 
                                  "Region" = "region"), keep = TRUE) %>%
  filter(is.na(Energy)) %>% 
  distinct(item)


#NOT reported in the FCT
# Miscellaneous
# Infant food (below 0.1*100g/cap/day)
# Alcohol, non-food -> remove bc is not food
# Rape and Mustardseed (below 0.1*100g/cap/day)
# Palm kernels -> remove because is < 0

n <- length(fao_fct)

for(i in 1:length(mn)){
  
fao_fct[n+i] <-  (fao_fct$total_value * fao_fct[mn[i]])

colnames(fao_fct)[n+i] <- paste0(mn[i], "_cap_day")

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
food_list <- top10 %>% arrange(element) %>% distinct(item) %>% pull(item) %>% unique()
ener_list1 <- top10 %>% filter(element == "Energy_cap_day") %>% arrange(item) %>%  pull(item) 

#Table with the FAO food categories to be included

top10 %>% ungroup() %>% select(1:4) %>% filter(element != "total_value") %>% 
  pivot_wider(names_from = element, 
              values_from = ave) %>% 
  relocate(Energy_cap_day, .after = item) %>% arrange(desc(Energy_cap_day)) %>% 
  write.csv(., here::here("output", "pre-selected-food-list-ssa.csv"), row.names = F)

#Food supply: top-10 food categories by kcal to compare with our Energy calculations
#calculating the yearly and region mean % of supply

ener <- read_csv(here::here("data", supply)) %>%
  janitor::clean_names() %>% filter(year %in% years, element_code == "664") %>% 
  group_by(element_code, element, item_code, item) %>% 
  summarise(ave = mean(value), 
            total = sum(value))
  
ener_list2 <- ener %>% ungroup() %>% 
  group_by(element_code, element) %>% 
  slice_max(ave, n = 10) %>%  distinct(item) %>% 
  arrange(item) %>% pull(item)


tolower(ener_list1) == tolower(ener_list2)

all.equal(ener_list1, tolower(ener_list2))

#Difference between our Energy calculations and the one reported by the FAO
setdiff(tolower(ener_list2),ener_list1)
#setdiff(ener_list1, tolower(ener_list2))

#Checking that they are all included in the final selection
setdiff(tolower(ener_list2), food_list)

#Table with the FAO food categories to be included

elements <- c("Energy_cap_day", "Ca_cap_day" ,"Cu_cap_day"  ,"Fe_cap_day","I_cap_day","Mg_cap_day", "Se_cap_day", "Zn_cap_day" )



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
    columns = c("Energy", "Ca" ,"Cu",
                "Fe","I","Mg", "Se", "Zn" ))  %>%
  fmt_number(
    columns = c("Energy", "Ca" ,"Cu",
                "Fe","I","Mg", "Se", "Zn" ),
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
    Cu = html("Cu,<br>mg"),
    Fe = html("Fe,<br>mg"),  
    I = html("I,<br>mcg"),
    Mg = html("Mg,<br>mg"),
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
  
  
  
#write.csv(., here::here("output", "pre-selected-food-list-ssa.csv"), row.names = F)