
# 0) Loading packages

library(tidyverse)


# 1) Getting the list of FCTs found from our screening (studies) ----

# Loading data: List of FCTs from online databases

readxl::read_excel(here::here("data", "01_screening4.xlsx")) %>% 
  janitor::clean_names() %>%
  tidyr::separate_rows(fct_location, sep = ",") %>%
  mutate_all(., str_squish) %>% 
  mutate(fct_location = ifelse(fct_location == "US", "USA", fct_location))

sfct.df <- readxl::read_excel(here::here("data", "01_screening4.xlsx")) %>% 
  janitor::clean_names() 

dim(sfct.df)
names(sfct.df) 
tail(sfct.df)

n0 <- nrow(sfct.df)

# Checking unnamed column (are empty?)
sfct.df$x14

#Adding location entry in a wrong location
sfct.df[148, c("location"   , "x14")]
sfct.df[148,   "location"] <- sfct.df[148, "x14"]

#Removing unnecesary column
sfct.df <- sfct.df[, 1:13]

#Checking variables
unique(sfct.df$fct_ref[sfct.df$is_fct == "y"])
subset(sfct.df, fct_ref == "NA" & is_fct == "y")[, c(6, 8:10)]

#Checking fct w/o description
sfct.df$id[sfct.df$is_fct == "y" & sfct.df$fct_description == "NA"]
#Checking fct "own-fct"
own <- sfct.df$id[sfct.df$is_fct == "y" & sfct.df$fct_description == "own-fct"]
subset(sfct.df, id %in% own, select = c(fct_ref, fct_location))

#Fixing a missing fct description
sfct.df$fct_description[sfct.df$id == "S0178"] <- "Africa, regional NCT, 2014"
sfct.df$fct_description[sfct.df$id == "S0187"] <- "Tanzania FCD, 2015"
sfct.df$fct_description[sfct.df$id == "S0234"] <- "Tanzania, Kilimanjaro NCT, 2019; Kenya, Western NCT, 2019"
sfct.df$fct_description[sfct.df$id == "S0235"] <- "Senegal NCT, 2019,"

#Making all lower case 
sfct.df$is_fct <- tolower(sfct.df$is_fct)

#No. of studies included:
length(unique(sfct.df$id))

#No. of unique FCTs from published studies (all locations included)
unique(sfct.df$fct_ref[sfct.df$is_fct == "y"])
sort(unique(sfct.df$fct_description[sfct.df$is_fct == "y"]))

# Getting observation that are FCTs
sfct.df <- subset(sfct.df, is_fct == "y")

n0-nrow(sfct.df)

#Checking some locations spelling
unique(sfct.df$fct_location)

sfct.df <- sfct.df %>%
  tidyr::separate_rows(fct_location, sep = ",") %>%
  mutate_all(., str_squish)

subset(sfct.df, fct_location == "NA" & is_fct == "y")

#Fixing a missing fct, location, description
sfct.df$fct_location[sfct.df$id == "S0184"] <- "Malawi"

names(raster::ccodes())
unique(raster::ccodes()$UNREGION1)
raster::ccodes()$UNREGION1

sfct.df$fct_location[sfct.df$fct_location == "East Africa"] <- "Eastern Africa"  
sfct.df$fct_location[sfct.df$fct_location %in% c("West Africa", "West-Africa")] <- "Western Africa"   

# Checking regional FCTs
subset(sfct.df , 
       str_detect(fct_location, "Tanzania"), 
       select =c(id, fct_location, fct_ref))

# Re-naming regional FCTs
sfct.df$fct_location[sfct.df$fct_location == "Central-Eastern Uganda"] <- "Uganda, Central Eastern"
sfct.df$fct_location[sfct.df$id %in% c("S0251", "S0252")] <- "Uganda, South Western"

# Identifying FCTs for use in SSA

#sfct.df %>% left_join(., raster::ccodes() %>% 
#                       select(NAME,UNREGION2, continent),
#                    by = c("fct_location" = "NAME"))



sfct.df %>% filter(fct_description == "West Africa FCT, 2012") %>% knitr::kable()
sfct.df %>% filter(fct_description == "NA", is_fct == "y")
sfct.df %>% filter(fct_citation == "NA", is_fct == "y", fct_ref != "NA") %>%  select(fct_ref, id)
sfct.df %>% filter(id == "S0178") 

#Fixing a citation mismatch between WAFCT, 2010 and WAFCT, 2012
sfct.df$fct_citation[sfct.df$id == "S0170"] <- "Stadlmayr, Barbara,Ruth Charrondiere,U.,Enujiugha,Victor N.,Bayili,Romaric G., Fagbohoun, Etel G.,Samb,Babacar,et al.(2012). West African food composition table/table de composition des aliments d’Afrique de I’Ouest . Rome,Italy:Food and Agriculture Organization of the United Nations" 
#Adding citation
sfct.df$fct_citation[sfct.df$id == "S0184"] <- "Joy, E.J.M., Broadley, M.R., Young, S.D., Black, C.R., Chilimba, A.D.C., Ander, E.L., Barlow, T.S., Watts, M.J., 2015. Soil type influences crop mineral composition in Malawi. Science of the Total Environment 505, 587–595. https://doi.org/10.1016/j.scitotenv.2014.10.038"
sfct.df$fct_citation[sfct.df$id == "S0178"] <- "Joy, E.J.M., Ander, E.L., Young, S.D., Black, C.R., Watts, M.J., Chilimba, A.D.C., Chilima, B., Siyame, E.W.P., Kalimbira, A.A., Hurst, R., Fairweather-Tait, S.J., Stein, A.J., Gibson, R.S., White, P.J., Broadley, M.R., 2014. Dietary mineral supplies in Africa. Physiol Plantarum 151, 208–229. https://doi.org/10.1111/ppl.12144"
sfct.df$fct_citation[sfct.df$id == "S0187"] <- "Kulwa, K.B.M., Mamiro, P.S., Kimanya, M.E., Mziray, R., Kolsteren, P.W., 2015. Feeding practices and nutrient content of complementary meals in rural central Tanzania: implications for dietary adequacy and nutritional status. BMC Pediatr 15, 171. https://doi.org/10.1186/s12887-015-0489-2"
sfct.df$fct_citation[sfct.df$id == "S0235"] <- "Yoo, Y.M., Atkin, R.A., Pachón, H., 2019. Short Communication: Development of a food composition table to analyze Senegalese food expenditure data. African Journal of Food, Agriculture, Nutrition and Development 19."
sfct.df$fct_citation[sfct.df$fct_ref == "Watts et al., 2019"] <- "Watts, M.J., Middleton, D.R.S., Marriott, A.L., Humphrey, O.S., Hamilton, E.M., Gardner, A., Smith, M., McCormack, V.A., Menya, D., Munishi, M.O., Mmbaga, B.T., Osano, O., 2019. Source apportionment of micronutrients in the diets of Kilimanjaro,Tanzania and Counties of Western Kenya. Scientific reports 9, 14447–14447. https://doi.org/10.1038/s41598-019-51075-2"
sfct.df$fct_citation[sfct.df$fct_ref == "Scarpa et al., 2021"] <- "Scarpa, G., Berrang-Ford, L., Bawajeeh, A.O., Twesigomwe, S., Kakwangire, P., Peters, R., Beer, S., Williams, G., Zavaleta-Cortijo, C., Namanya, D.B., Lwasa, S., Nowembabazi, E., Kesande, C., Rippin, H., IHACC Team, Cade, J.E., 2021. Developing an online food composition database for an Indigenous population in south-western Uganda. Public Health Nutr. 24, 2455–2464. https://doi.org/10.1017/S1368980021001397"


#Extracting year of publication for each FCT

sfct.df <- sfct.df %>% 
  mutate(fct_year = str_extract(fct_description, "[:digit:]{4}"), 
         fct_year = ifelse(is.na(fct_year), str_extract(fct_ref, "[:digit:]{4}"), fct_year),
         fct_year = ifelse((fct_year > 2021), NA, fct_year)
  ) %>% 
  mutate_at("fct_year", as.numeric)


#list of FCT with the list and count of studies where they were mentioned
sfct_list.df <- sfct.df %>% filter(is_fct == "y") %>% 
  group_by(fct_ref, fct_citation, fct_location, fct_year, fct_description) %>%
  summarise(count = length(id), 
            study_id = paste0(id, collapse = ";"))  %>% 
  rename(fct_desc = "fct_description") 


#Adding an Id for unique fct

for(i in 1:length(sfct_list.df$fct_desc)){
  
  id <-  if(i<10){
    paste0("SF00", i)
  }else {
    paste0("SF0", i)
  }
  
  sfct_list.df$fct_id[i] <- id
  
  print(i)
}

#Checking completeness of all variables

sfct_list.df %>% ungroup() %>%  filter(is.na(fct_year)) %>% select(fct_ref, fct_desc, fct_id)
sfct_list.df %>% filter(fct_citation == "NA")

head(sfct_list.df)

## Need to separate regional FCT to country FCTs

sfct_list.df$fct_location[sfct_list.df$fct_location == "East Africa"] <- "Eastern Africa"
sfct_list.df$fct_location[sfct_list.df$fct_location == "West Africa"] <- "Western Africa"
sfct_list.df$fct_location[sfct_list.df$fct_location == "UK"] <- "United Kingdom"
sfct_list.df$fct_location[sfct_list.df$fct_location == "USA"] <- "United States"


#By running this we will have matched the FCT (of each study) 
#by all the countries in the region (only for regional FCTs)  

sfct_list.df %>% 
  left_join(., raster::ccodes() %>% 
              mutate(NAME = if_else(NAME == "Côte d'Ivoire", "Ivory Coast", 
                                    NAME)), by = c("fct_location" = "UNREGION1"))%>% 
  arrange(desc(fct_year)) %>% 
  arrange(NAME) 

#With the geographic location by countries (ISO), region, continent

sfct_geolist.df <- sfct_list.df %>% 
  left_join(., raster::ccodes() %>% 
              mutate(NAME = if_else(NAME == "Côte d'Ivoire", "Ivory Coast", 
                                    NAME)), by = c("fct_location" = "NAME")) %>% 
  filter(!fct_location %in% c("Eastern Africa","Western Africa" )) %>% 
  rbind(sfct_list.df %>% 
          left_join(., raster::ccodes() %>% 
                      dplyr::select(UNREGION1, UNREGION2, continent), 
                    by = c("fct_location" = "UNREGION1"), keep = T) %>% 
          filter(!is.na(UNREGION2)) %>% distinct()) 


sfct_geolist.df <- sfct_geolist.df %>% filter(!str_detect(fct_location, "Uganda")) %>% 
  rbind(., sfct_list.df %>% filter(str_detect(fct_location, "Uganda")) %>%  
          cbind(raster::ccodes() %>% filter(NAME == "Uganda")) %>% 
          dplyr::select(-NAME)) %>% 
  mutate(continent = ifelse(fct_location == "Africa", "Africa", continent)) 

sfct_geolist.df %>% group_by(ISO3) %>%  count() %>% arrange(desc(n))
sfct_geolist.df %>% group_by(fct_location) %>%  count() %>% arrange(desc(n)) %>% knitr::kable()

#FCTs from Africa (SSA)
count(unique(subset(sfct_geolist.df, continent == "Africa", select = fct_id)))
#FCTs from national 
count(unique(subset(sfct_geolist.df, continent == "Africa" & !is.na(ISO3), select = fct_id)))

#Checking sub-national FCTs
unique(subset(sfct_geolist.df, continent == "Africa" & !is.na(ISO3), select =  fct_location))
sfct_geolist.df$fct_ref[sfct_geolist.df$fct_location == "Kenya"]
sfct_geolist.df$fct_id[sfct_geolist.df$fct_ref == "Watts et al., 2019"]

#Fixing sub-national FCTs location
sfct_geolist.df$fct_location[sfct_geolist.df$fct_id == "SF066" & sfct_geolist.df$fct_location == "Tanzania"] <- "Tanzania, Kilimanjaro"
sfct_geolist.df$fct_location[sfct_geolist.df$fct_id == "SF065" & sfct_geolist.df$fct_location == "Kenya"] <- "Kenya, Western"

sfct_ssa.df <- subset(sfct_geolist.df, continent == "Africa")



# 2) Getting the list of FCTs via other methods (grey lit.) -----

# Loading data from FCT found in online resources

#├  2.1) INFOODS ----

# Loading FAO/ INFOODS - FCT list

infoods.df <- read.csv(here::here("data", "screening-infoods-all.csv")) %>% 
  mutate_at("fct_location", str_squish) %>% 
  mutate(fct_location = str_replace_all(fct_location, 
                                        c("Africa, West" = "West Africa", 
                                          "Africa, East" = "East Africa")))


infoods.df$fct_location[infoods.df$fct_location == "Congo, Dem. Rep"] <- "Democratic Republic of the Congo"

dim(infoods.df)


#Extracting year of publication per each FCT

infoods.df <-  infoods.df %>% 
  mutate(fct_year = str_extract(fct_desc, "[:digit:]{4}"), 
         fct_year = ifelse((fct_year > 2020), NA, fct_year), 
         fct_year = case_when(
           fct_id == "I0012" ~ "1966",
           fct_id == "I0015" ~ "1983",
           fct_id == "I0026" ~ "1993",
           fct_id == "I0042" ~ "1988",
           TRUE ~ fct_year)) %>%
  mutate_at("fct_year", as.numeric)

#This 2 entries should be excluded as they are not FCTs, and 3 from outside SSA 
infoods.df %>% 
  filter(fct_id %in% c("I0025", "I0039"))

infoods_ssa.df <- infoods.df %>% 
  filter(!fct_id %in% c("I0025", "I0039"),
         !fct_location %in% c("Egypt", "Sudan", "Tunisia"))

dim(infoods_ssa.df)


#├  2.2) LANGUAL ----

langual.df <- read.csv(here::here("data", "langual_fct_raw.csv"))%>%
  dplyr::select(-X) %>% filter(str_detect(fct_name, "\\b")) 

langual.df %>% distinct(fct_location)

#Check those fct that the fct_location doesn't provide good info

dim(langual.df)

langual.df %>% 
  filter(fct_location %in% c("FAO",  #only one for Africa
                             "Biodiversity for Food and Nutrition", #International
                             "ILSI", #International crop compo
                             "INFOODS", #no ftc INFOOD website?
                             "International", #International 
                             "World Food Programme"))%>%  #International
  arrange(fct_location)

#Fixing location 
langual.df$fct_location[langual.df$fct_name =="Food Composition Tables for Use in Africa, FAO, Rome, 1968"] <-  "Africa"

#Selecting only SSA FCT (plus FAO)

langual_ssa.df <- langual.df %>% 
  filter(fct_location %in% c(  "Ethiopia", 
                               "Kenya", "Malawi", "Mozambique", 
                               "Nigeria", "South Africa", "Tanzania", "Uganda", 
                               "West Africa", "Africa"))%>%
  arrange(fct_location) %>% 
  mutate(fct_source = "langual") %>% 
  rename(fct_desc = "fct_name")

for(i in 1:length(langual_ssa.df$fct_desc)){
  
  id <-  if(i<10){
    paste0("L000", i)
  }else {
    paste0("L00", i)
  }
  
  langual_ssa.df$fct_id[i] <- id
  
  print(i)
}

langual_ssa.df$fct_id <- as.character(langual_ssa.df$fct_id)

dim(langual_ssa.df)

#├  2.3) NUTRITOOL ----

nutri.df <- read.csv(here::here("data", "nutritool_fct_raw.csv")) %>%
  filter(str_detect(fct_name, "\\b"))

dim(nutri.df)

fct_location <- c("Australia", "Australia", "Austria", "Belgium", "Canada",
                  "Czech Republic", "Denmark", "Finland", "France", "Germany",
                  "Germany", "Greece", "Greece",  "Iceland", "Irland",
                  "Italy", "Latvia", "Belgium", "Norway", "Poland", "Latin-America",
                  "Serbia", "Slovakia", "Spain", "Sweden", "Switzerland", "Turkey",
                  "Gambia", "UK", "USA")

nutri.df <- nutri.df %>% dplyr::select(-fct_location) %>%  
  bind_cols(., "fct_location" = fct_location)


nutri_ssa.df <- nutri.df %>% filter(fct_location == "Gambia") %>% 
  mutate(fct_id = "N0001", 
         fct_source = "nutritool") %>% 
  rename(fct_comment = "fct_desc",
         fct_desc = "fct_name")

#├ 2.4) WNDDS - Data not available ----

# 3) Combining online resources ----

grey_ssa.df <- infoods_ssa.df %>% 
  bind_rows(., langual_ssa.df, nutri_ssa.df)  %>% 
  mutate(fct_year = str_extract(fct_desc, "[:digit:]{4}"),
         fct_year = ifelse((fct_year > 2021), NA, fct_year),
         fct_year = case_when(
           fct_id == "I0012" ~ "1966",
           fct_id == "I0004" ~ "1987",
           fct_id == "N0001" ~ "2011",
           fct_id == "I0022" ~ "1983",
           fct_id == "I0026" ~ "1993",
           fct_id == "L0005" ~ "2011",
           fct_id == "L0006" ~ "2019",
           fct_id == "I0038" ~ "2018",
           fct_id == "L0007" ~ "2018",
           fct_id == "I0042" ~ "1988",
           fct_id == "L0009" ~ "2008",
           fct_id == "I0045" ~ "1957",
           fct_id == "L0010" ~ "2012",
           TRUE ~ fct_year)) %>%
  mutate_at("fct_year", as.numeric) %>% 
  relocate(., fct_year, .after = fct_desc) %>% 
  arrange(fct_location) 

dim(grey_ssa.df)


unique(grey_ssa.df$fct_source)

#Checking duplicates in source

grey_ssa.df %>% group_by(fct_location, fct_year, fct_source) %>% count() %>%
  filter(n>1) %>% arrange(desc(n))

#L0007 == L0008
subset(grey_ssa.df, fct_location == "South Africa" & fct_year == 2018 & fct_source == "langual")
#L0011 == L0012
subset(grey_ssa.df, fct_location == "West Africa" & fct_year == 2019 & fct_source == "langual")
#I0008 != I0009
subset(grey_ssa.df, fct_location == "West Africa" & fct_year == 1965 & fct_source == "infoods")


grey_ssa.df %>% group_by(fct_location, fct_year) %>% count() %>%
  filter(n>1) %>% arrange(desc(n))

subset(grey_ssa.df, fct_location == "South Africa" & fct_year == 2018, select = c(fct_desc, fct_id))


#De-duplication 

#Manual de-duplication (check)
#write.csv(grey_ssa.df, here::here("inter-output",                                                  
#                              "fct_grey-list-final-screening.csv"), row.names = F)

#Same results, only we found one FCT w/o a fct_year that it seemed duplicated
#I0015 == L0002

grey_ssa.df$fct_year[grey_ssa.df$fct_id == "I0015"] <- 1981

grey_ssa.df <- grey_ssa.df %>% group_by(fct_location, fct_year) %>%
  summarise(count = length(fct_id), 
            fct_id = paste0(fct_id, collapse = "; "),
            fct_desc = paste0(fct_desc, collapse = "; "),
            fct_source = paste0(fct_source, collapse = "; ")) %>% 
  mutate(fct_ref = paste0(fct_location," FCT, ", fct_year)) %>% arrange(desc(count))

#No. of unique FCTs de-dupli, plus the merged that it's not dupli
dim(grey_ssa.df)[1]+1

#By running this we will have matched the FCT (of each study) 
#by all the countries in the region (only for regional FCTs)  

#Fixing location name
grey_ssa.df$fct_location[grey_ssa.df$fct_location == "West Africa"] <- "Western Africa"
grey_ssa.df$fct_location[grey_ssa.df$fct_location == "East Africa"] <- "Eastern Africa"

names(raster::ccodes())

grey_ssa.df %>% 
  left_join(., raster::ccodes() %>% 
              mutate(NAME = if_else(NAME == "Côte d'Ivoire", "Ivory Coast", 
                                    NAME)), by = c("fct_location" = "NAME")) %>% 
  arrange(desc(fct_year)) %>% 
  arrange(UNREGION2) 

#With the geographic location by countries (ISO), region, continent

grey_geolist.df <- grey_ssa.df %>% 
  left_join(., raster::ccodes() %>% 
              mutate(NAME = if_else(NAME == "Côte d'Ivoire", "Ivory Coast", 
                                    NAME)), by = c("fct_location" = "NAME")) %>% 
  filter(!fct_location %in% c("Eastern Africa","Western Africa" )) %>% 
  rbind(grey_ssa.df %>% 
          left_join(., raster::ccodes() %>% 
                      dplyr::select(UNREGION1, UNREGION2, continent), 
                    by = c("fct_location" = "UNREGION1"), keep = T) %>% 
          filter(!is.na(UNREGION2)) %>% distinct()) %>% 
  mutate(continent = ifelse(fct_location == "Africa", "Africa", continent)) 


grey_geolist.df %>% group_by(ISO3) %>%  count() %>% arrange(desc(n))
grey_geolist.df %>% group_by(fct_location) %>%  count() %>% 
  arrange(desc(n)) %>% knitr::kable()

#FCTs from Africa (SSA)
count(unique(subset(grey_geolist.df, continent == "Africa", select = fct_id)))

#FCTs from national 
count(unique(subset(grey_geolist.df, continent == "Africa" & !is.na(ISO3), select = fct_id)))

#Checking sub-national FCTs
unique(subset(grey_geolist.df, continent == "Africa" & !is.na(ISO3), select =  fct_location))
grey_geolist.df$fct_ref[grey_geolist.df$fct_location == "Kenya"]


# 4) List of FCTs in SSA for de-duplication (combining all resources) -----

# Combining FCTs from studies (electronic databases screening) and stand-alone (grey literature)
fct_list <- sfct_ssa.df %>% bind_rows(., grey_geolist.df) %>% 
  arrange(fct_location)

names(fct_list)

#Checking rows
dim(sfct_ssa.df)
dim(grey_geolist.df)
dim(fct_list)

#Checking countries (only SSA are included, no NA, correct spelling)
unique(fct_list$fct_location)

#Fixing locations
fct_list$fct_location[fct_list$fct_location == "East Africa"] <- "Eastern Africa"
fct_list$fct_location[fct_list$fct_location == "West Africa"] <- "Western Africa"

#Checking Tanzania FCTs
subset(fct_list, str_detect(fct_location, "Tanzania"), 
       select = c(fct_id, fct_ref, fct_location))
fct_list$fct_id[fct_list$fct_ref == "Watts et al., 2019"]

subset(fct_list, str_detect(fct_location, "Uganda"), 
       select = c(fct_id, fct_ref, fct_location))

fct_list$fct_location[fct_list$fct_id ==   "I0047; L0010"] <- "Uganda, Central Eastern" 

#Manual de-duplication (check)
write.csv(fct_list, here::here("inter-output",                                                  
                               "fct_list-final-screening.csv"), row.names = F)
