

#Results: Output 2a: Mineral data availability for the pre-selected 
#                food categories in the FCTs/FCDBs included


library(tidyverse)

#GETTING THE FINAL LIST OF ITEMS ###############
#├ Loading FCDB and generating the items list -----

source("03_results-o2.R")

#Changing KT0025 into two FCTs -
#Tanzania to KT0025
#Kenya to KT0025

fct.item <- fct.item %>% 
  mutate(fct.id = 
           case_when(
             str_detect(food.id.original, "SP10_TZ") ~ "KT0025",
             str_detect(food.id.original, "SP11_KN") ~ "KT0025",
             TRUE ~ fct.id)) 

#ADDING MN values (16 FCTs):
#2 FCT excluded Mali and South Western Uganda.

#1-4) FCTs manually collated  ----
# Loaded values from in speadsheet

# see manual.fct
manual.fct <- c("EA006", "ZW009", "SN0027", "TZ0011")

#excluding dried Freshwater fish from EA0006 & Oilcrops

mn_values <- readxl::read_excel(here::here("data", 
                           "food-items-data-collection.xlsx"),
                                sheet = "outcome2_values") %>% 
        filter(food.id.original !=  "123" & 
                 food.category != "Oilcrops, Other and products") 
  

#Checking non-reported/included items
mn_values %>% filter(food.id.original == "NA") %>% count(fct.id)

#Filtering out non-reported items

mn_values <- mn_values %>% filter(food.id.original != "NA")

#Processing 11 FCDB

# 5) Lesotho [LS0010] ----

#need to convert to numeric otherwise ids did not match (extra 0)

#fct.item$food.id.original[fct.item$food.id.original == "0111022"] <- "11022"

ls0010 <- fct.item %>% filter(fct.id == "LS0010", !is.na(food.name.original)) %>% 
  mutate_at("food.id.original", as.numeric) %>% 
  left_join(., read.csv(here::here("data", "MAPS_LSOFCTv.1.0.csv")) %>% 
              mutate_at("original_food_id", as.numeric),
            by = c("food.id.original" = "original_food_id")) %>% 
  dplyr::select(1:4, 
                starts_with(c("moisture", "ca_", 
                              "fe", "se"))) %>% 
  rename(
    water = "moisture_in_g", 
    ca = "ca_in_mg",
    fe = "fe_in_mg", 
    se = "se_in_mcg")


#6) Zambia [ZM0012] ###############

read.csv(here::here("data", "ZM0013_Zambia-2009_v2.csv")) %>% slice(71:n()) %>% glimpse()

xl_data <- here::here("data", "ZM0013_Zambia-2009.xlsx")

#list_all <- lapply(5:38, function(x) readxl::read_excel(path = xl_data, sheet = x))

#str(list_all)

list_zm <- lapply(5:38, function(x) readxl::read_excel(path = xl_data, sheet = x))


for(i in 1:34){
  
  if(length(list_zm[[i]]) > 17){
    print(i)
  }
}


str(list_zm)

list_zm[[34]] <- list_zm[[34]][1:11,]

df_plyr <- plyr::ldply(list_zm, data.frame)
str(df_plyr)


x <- reduce(list_zm, bind_rows) %>% janitor::clean_names() %>% 
  mutate(food_and_description_latin_name = ifelse(is.na(food_and_description_latin_name) == T,
                                                  food_and_description_latin_name_2,
                                                  food_and_description_latin_name)) %>% 
  dplyr::select(-food_and_description_latin_name_2)


nutrient <- as.character(x[1,3:17]) %>% str_squish()

names(x)[3:17] <- nutrient


zm_fct <-  x %>% 
  janitor::clean_names() %>% 
  mutate(food.id.original = str_extract(food_and_description_latin_name, "[:digit:]{1,3}"))

#Number of foods
zm_fct %>%  filter(!is.na(food_and_description_latin_name),!is.na(moisture_g) )


zm0012 <- fct.item %>% filter(fct.id == "ZM0012", !is.na(food.name.original)) %>% 
  dplyr::select(1:4) %>% 
  left_join(., zm_fct %>% dplyr::select(food.id.original, 
                                        starts_with(c("moisture", 
                                                      "calcium", "iron",
                                                      "zinc")))) %>% 
  rename(
    water = "moisture_g",
    ca = "calcium_mg", 
    fe = "iron_mg", 
    zn = "zinc_mg"  )

#7) Uganda [CEU0015] ###############

#No of total items in the FCDB

readxl::read_excel(here::here("data", "2012_UGAFCT.xlsx")) %>% filter(!is.na(food_code)) %>% count()

ceu0015 <- fct.item %>% filter(fct.id == "CEU0015", !is.na(food.name.original)) %>% 
  left_join(., readxl::read_excel(here::here("data", "2012_UGAFCT.xlsx")) %>% 
              mutate_at("food_code", as.character),  
            by = c("food.id.original" = "food_code")) %>% 
  dplyr::select(1:4,  
                starts_with(c("water_g", 
                              "calcium", "iron",
                              "zinc"))) %>% 
  rename(
    water = "water_g",
    ca = "calcium_mg", 
    fe = "iron_mg", 
    zn = "zinc_mg"  )


#8) Cameroon [CM0016] ###############

#├ No of total items in the FCDB -----

read.csv(here::here("data", "CM0016.csv")) %>% filter(!is.na(Code)) %>% count()

#├ Selected food items -----

cm0016 <- fct.item %>% filter(fct.id == "CM0016", !is.na(food.name.original)) %>% 
  left_join(.,read.csv(here::here("data", "CM0016.csv"))%>% 
              mutate_at("Code", as.character),
            by = c("food.id.original" = "Code")) %>% 
  dplyr::select(1:4,  
                starts_with(c("Water", 
                              "Ca", "Fe", "Se", "Zn"))) %>% 
  rename_all(., tolower) %>%                        #changing variable names to lower case
  mutate_all(., ~str_replace(.,"n.d.", "NA") ) #replacing "not determined" with NA

#cm0016 <- gsub("n.d.", "NA", cm0016)

#9) Kenya [KE0018] ###############

read.csv(here::here("data", "MAPS_KENFCT1_v1.1.csv")) %>% 
  filter(!is.na(original_food_id)) %>% count()

#checking items - found two duplicates
read.csv(here::here("data", "MAPS_KENFCT1_v1.1.csv")) %>% 
   count(original_food_id) %>% arrange(desc(n))

#checking the dupli
read.csv(here::here("data", "MAPS_KENFCT1_v1.1.csv")) %>% 
  filter(original_food_id %in% c("1025", "4018"))


#├ Selected food items -----

ke0018 <- fct.item %>% filter(fct.id == "KE0018", !is.na(food.name.original)) %>% 
  left_join(.,read.csv(here::here("data", "MAPS_KENFCT1_v1.1.csv")),
            by = c("food.id.original" = "original_food_id")) %>% 
  dplyr::select(1:4,  
                starts_with(c("moisture", 
                              "ca_", "fe",
                              "se", "zn"))) %>% 
  rename(
    water = "moisture_in_g",
    ca = "ca_in_mg", 
    fe = "fe_in_mg",
    se = "se_in_mcg",
    zn = "zn_in_mg"  ) %>% distinct()


#10) Malawi [MW0020] ###############

read.csv(here::here("data", "MAPS_MAFOODS_v1.3.csv")) %>% count()

#no.of items reporting iodine

read.csv(here::here("data", "MAPS_MAFOODS_v1.3.csv")) %>% 
  filter(!is.na(i_in_mcg)) %>% count()

#├ Selected food items -----

mw0020 <- fct.item %>% filter(fct.id == "MW0020", !is.na(food.name.original)) %>% 
  left_join(.,read.csv(here::here("data", "MAPS_MAFOODS_v1.3.csv")),
            by = c("food.id.original" = "original_food_id")) %>% 
  dplyr::select(1:4,  
                starts_with(c("moisture", 
                              "ca_", "fe", "i", 
                              "se", "zn"))) %>% 
rename(
  water = "moisture_in_g", 
  ca = "ca_in_mg",
  fe = "fe_in_mg", 
  se = "se_in_mcg",
  i = "i_in_mcg",
  zn = "zn_in_mg")

mw0020 %>% filter(is.na(i))



#11) Nigeria [NG0021] ###############

#Total no. of items
readxl::read_excel(here::here("data", "NG0021.xls.xlsx")) %>% distinct(Id) %>% pull()


#├ Selected food items -----

ng0021 <- fct.item %>% filter(fct.id == "NG0021", !is.na(food.name.original)) %>% 
  left_join(., 
            readxl::read_excel(here::here("data", "NG0021.xls.xlsx")), 
            by = c("food.name.original" = "EnglishName")) %>% 
  relocate( "Code", .after =  "food.category") %>% 
  dplyr::select(1:3,5, starts_with(c("WATER", "Ca_", "Fe", "Zn"))) %>% 
  rename(
    food.id.original = "Code",
    water = "WATER_g", 
    ca = "Ca_mg",
    fe = "Fe_mg", 
    zn = "Zn_mg")


#12) West Africa [WA0001] ###############

read.csv(here::here("data", "MAPS_WAFCT_v1.1.csv")) %>% distinct(ï..original_food_id) %>%
  #pull() %>% 
  count()

#├ Selected food items -----

wa0001 <- fct.item %>% filter(fct.id == "WA0001", !is.na(food.name.original)) %>% 
  left_join(.,read.csv(here::here("data", "MAPS_WAFCT_v1.1.csv")),
            by = c("food.id.original" = "ï..original_food_id")) %>% 
  dplyr::select(1:4,  
                starts_with(c("moisture", 
                              "ca_", "fe",  
                              "zn")))  %>% 
  rename(
    water = "moisture_in_g", 
    ca = "ca_in_mg",
    fe = "fe_in_mg",
    zn = "zn_in_mg")

#13) Gambia [GM0013]  ###############

read.csv(here::here("data", "MAPS_GMBFCT.csv")) %>% str()

#No of items
read.csv(here::here("data", "MAPS_GMBFCT.csv")) %>% filter(!is.na(code)) %>% count()

fct.item %>% filter(fct.id == "GM0013", !is.na(food.name.original))

#├ Selected food items -----
#loading and filtering (by merging) only pre-selected items
gm0013 <- fct.item %>% filter(fct.id == "GM0013", !is.na(food.name.original)) %>% 
  left_join(., read.csv(here::here("data", "MAPS_GMBFCT.csv")) %>% 
              mutate_at("code", as.character),
            by = c("food.id.original" = "code")) %>%  #selecting only minerals
  dplyr::select(1:4,  
                starts_with(c("WATER", 
                              "CA", "FE",  
                              "ZN")), -CARTBEQ)  %>% #renaming to merge all
  rename(
    water = "WATER", 
    ca = "CA", 
    fe = "FE", 
    zn = "ZN")



#14) Mozambique [MZ0014]  ###############

#Minerals
readxl::read_excel(here::here("data", "MZ0014.xlsx"), sheet = "Micro") %>% str()

#Water
readxl::read_excel(here::here("data", "MZ0014.xlsx"), sheet = "Macro") %>% str()

mzfct <- readxl::read_excel(here::here("data", "MZ0014.xlsx"), 
                            sheet = "Macro") %>% 
  left_join(., readxl::read_excel(here::here("data", "MZ0014.xlsx"), 
                                  sheet = "Micro")) %>% 
  rename(food.id = "Beverages") 

# No. food items (total)
mzfct %>% filter(str_detect(food.id, "MZF")) %>% pull(food.id) %>% length()

mzfct %>% filter(!is.na(food.id))

fct.item %>% filter(fct.id == "MZ0014", !is.na(food.name.original))

#├ Selected food items -----

mz0014 <- fct.item %>% filter(fct.id == "MZ0014", !is.na(food.name.original)) %>% 
  left_join(., mzfct, by = c("food.id.original" = "food.id")) %>% 
  dplyr::select(1:4,  c("Wat", 
                        "Ca", "Fe",  
                        "Zn")) %>% 
  rename(
    water = "Wat", 
    ca = "Ca", 
    fe = "Fe", 
    zn = "Zn")

#15) Sub-Saharan Africa FCT [AF0023] ###############

#Concentration (100 g-1 fresh weight)

readxl::excel_sheets(here::here("data", "ppl12144-sup-0002-tables2.xlsx"))

ssa <- readxl::read_excel(here::here("data", "ppl12144-sup-0002-tables2.xlsx"),
                          sheet = "S Table 2")

#mn <- c("Ca", "Cu", "Fe", "I", "Mg", "Se", "Zn")
mn <- c("Ca",  "Fe", "I",  "Se", "Zn")

ssa <- ssa %>% dplyr::select(2:4, 8) %>%  filter(`...3` %in% mn) %>% 
  pivot_wider(
    names_from = `...3`, 
    values_from = "...8") %>% 
  rename(region = "Supplementary Table 2. Food mineral composition data from literature sources, used in conjunction with Food Balance Sheets (FBSs) to estimate dietary mineral availability",
         food.name.original = "...4")

ssa %>% count(region)

fct.item %>% filter(fct.id == "AF0023", !is.na(food.name.original))

#├ Selected food items -----

af0023 <-  fct.item %>% filter(fct.id == "AF0023", !is.na(food.name.original)) %>% 
  left_join(., ssa, by = c("food.id.original" = "region", "food.name.original")) %>% 
  dplyr::select(1:4, mn) %>% rename_all(., tolower)

#16) [KT0025] ###############

readxl::excel_sheets(here::here("data", "KT0025.xlsx"))

#Units: moisture content

WATER <- readxl::read_excel(here::here("data", "KT0025.xlsx"),
                            sheet = "Supp. Table_5") %>% 
  dplyr::select(1, 3) %>% 
  rename_all(~c("food.name.original", "water")) %>% 
  filter(!is.na(food.name.original))

#├ 16a) Kilimanjaro, TZ -----

#Units: mg kg-1 DW 

readxl::read_excel(here::here("data", "KT0025.xlsx"),
                   sheet = "Supp. Table_10") %>% .[1,1]

TZ <- readxl::read_excel(here::here("data", "KT0025.xlsx"),
                         sheet = "Supp. Table_10") 

#Filtering out results and summary stats

TZ <- TZ %>%  filter(!is.na(...2)) %>% slice(3:nrow(TZ))


colnames(TZ)[1] <- "food.name.original"

mn <- c("value_type", "Ca", "Cu", "Fe", "Mg",    "Se",  "Zn" , "I", "Mo")



#Published composition data, East Africa

TZ[,3] <- "published"

for(i in 1:length(mn)){
  
  colnames(TZ)[(2+i)] <- paste(mn[i])
  
  print(i)
}

#Own composition data, non-calcareous, median

TZ[,12] <- "av1_non-cal"

for(i in 1:length(mn)){
  
  colnames(TZ)[(11+i)] <- paste(mn[i])
  
  print(i)
}

#Own composition data, calcareous, median

TZ[,21] <- "av1_cal"

for(i in 1:length(mn)){
  
  colnames(TZ)[(20+i)] <- paste(mn[i])
  
  print(i)
}

#rbind(TZ[c(1,3:11)], TZ[c(1,12:20)])

TZ <- bind_rows(TZ[c(1,3:11)], TZ[c(1,12:20)], TZ[c(1,21:29)])
      


fct.item %>% filter(fct.id == "KT0025", !is.na(food.name.original),
                    str_detect(food.id.original, "SP10_TZ"))

SP10_TZ <- fct.item %>% filter(fct.id == "KT0025", !is.na(food.name.original),
                               str_detect(food.id.original, "SP10_TZ")) %>% 
  left_join(., TZ) %>% 
  dplyr::select(1:4, starts_with(mn))


#Checking that all matches were done
SP10_TZ %>% filter(is.na(value_type))

##├ 16b) Kenya -----

readxl::read_excel(here::here("data", "KT0025.xlsx"),
                   sheet = "Supp. Table_11") %>% .[1,1]

KE <- readxl::read_excel(here::here("data", "KT0025.xlsx"),
                         sheet = "Supp. Table_11") 

KE <- KE %>% filter(!is.na(...2)) %>% slice(3:nrow(KE))

#Renaming col

colnames(KE)[1] <- "food.name.original"
#colnames(KE)[3] <- "value.type"

mn <- c("value_type", "Ca", "Cu", "Fe", "Mg",    "Se",  "Zn" , "I", "Mo")

#Published composition data, East Africa

KE[,3] <- "published"

for(i in 1:length(mn)){
  
  colnames(KE)[(2+i)] <- paste(mn[i])
  
  print(i)
}

#Own composition data, non-calcareous, median

KE[,12] <- "av1_non-cal"

for(i in 1:length(mn)){
  
  colnames(KE)[(11+i)] <- paste(mn[i])
  
  print(i)
}

#Own composition data, calcareous, median

KE[,21] <- "av1_cal"

for(i in 1:length(mn)){
  
  colnames(KE)[(20+i)] <- paste(mn[i])
  
  print(i)
}


KE <- bind_rows(KE[c(1,3:11)], KE[c(1,12:20)], KE[c(1,21:29)])


fct.item %>% filter(fct.id == "KT0025", !is.na(food.name.original),
                    str_detect(food.id.original, "SP11_KN"))

SP11_KN <- fct.item %>% filter(fct.id == "KT0025", !is.na(food.name.original),
                               str_detect(food.id.original, "SP11_KN")) %>% 
  left_join(., KE) %>% 
  dplyr::select(1:4, starts_with(mn))


#Checking that all matches were done
SP11_KN %>% filter(is.na(value_type))

#pasting the two country datasets together
kt0025 <- rbind(SP10_TZ, SP11_KN)

kt0025 %>% left_join(., WATER) %>% filter(is.na(water))

kt0025 <- kt0025 %>% left_join(., WATER) 

kt0025 <- kt0025 %>% mutate_at(6:ncol(kt0025), as.numeric)

#two items are missing water content
kt0025 %>% filter(food.id.original %in% c("SP10_TZ_93", "SP11_KN_34"))

kt0025$water[kt0025$food.id.original == "SP10_TZ_93"] <- 92.18
kt0025$water[kt0025$food.id.original == "SP11_KN_34"] <- 78.08

kt0025 %>% filter(is.na(water))

#removing "value_type" of the string of variables
mn <- mn[2:9]
#MN <-  str_to_lower(mn)


#looping over water-content adjustment for the entries of each MN 

for(i in 1:length(mn)){
  
  kt0025[ncol(kt0025)+1] <- (kt0025[5+i]*(100-kt0025$water)/100)/10
  
  colnames(kt0025)[ncol(kt0025)] <- paste(str_to_lower(mn[i]))
  
  print(i)
  
}


#├ Selected food items -----
#summarisinng (median) all values type 
#(we are not evaluating the actual value here) - only presence or absence

kt0025 <- kt0025 %>% dplyr::select(1:5,water, str_to_lower(mn), -c(cu, mo, mg)) %>% 
  group_by(fct.id, food.category, food.id.original, food.name.original) %>% 
  summarise_if(is.numeric, median, na.rm = T)

#EXCLUDED FOR THIS ANALYSIS

#17) South Western Uganda FCT [SWU0022]
#18) Mali FCT [ML0017]

#MERGING THE FINAL LIST OF ITEMS ###############

#fct <- c("ls0010", "zm0012", "mz0014", "gm0013", "ceu0015", "cm0016",
 #        "af0023", "ke0018", "kt0025" )

fct <- list(ls0010, zm0012, ceu0015, cm0016, ke0018, mw0020, ng0021, 
            wa0001,  gm0013, mz0014, af0023,  kt0025)

min_values <- mn_values

min_values  <- min_values  %>% rename(i = "io")

for(i in 1:length(fct)){
min_values <- merge(min_values, fct[[i]] ,  all = T)
print(i)
}

#Changing "NA" and "-" to NA

min_values %>% 
  naniar::replace_with_na_all(., condition = ~.x %in% c("NA", "-"))

#Changing KT0025 into two FCTs -
#Tanzania to KT0025
#Kenya to WK0025

min_values <- min_values %>% 
  mutate(fct.id = 
  case_when(
  str_detect(food.id.original, "SP10_TZ") ~ "KT0025",
  str_detect(food.id.original, "SP11_KN") ~ "WK0025",
  TRUE ~ fct.id)) %>% 
  naniar::replace_with_na_all(., condition = ~.x %in% c("NA", "-"))


#Total no. food entries

nrow(min_values)

#├ Summary: Total no. food entries per fct ----
#with exception of the South Western Uganda, 
#And Mali. 

min_values %>% count(fct.id) %>% arrange(desc(n))

mean_n <- min_values %>% count(fct.id) %>% pull(n) %>% as.numeric()

mean_n[18] <- 0

summary(mean_n)
sd(mean_n)

#Total no. food entries per food category
min_values %>% count(food.category) %>% arrange(desc(n))


compo <- c("water", "zn", "se", "i", "fe", "ca")

min_values %>% 
  mutate_at(c("water", "zn", "se", "i", "fe", "ca"),
                         as.numeric) %>% 
  filter(!is.na(i)) %>% count()

##├ Summary: Values per mineral -----

#Checking that the items per FCTs are the same
min_values %>% 
  pivot_longer(cols = c("zn"  ,  "se"   , "i"   ,  "fe"   , "ca" ),
               names_to = "mn", 
               values_to = "values") %>% 
  filter(!is.na(food.id.original)) %>% #there are items included w/ all min "NA
  distinct(fct.id, food.id.original, food.name.original) %>% 
   count(fct.id) %>% arrange(desc(n))

# Count and perc of min. values per item ----
names(min_values)
min_values %>% 
  pivot_longer(cols = c("zn"  ,  "se"   , "i"   ,  "fe"   , "ca" ),
               names_to = "mn", 
               values_to = "values") %>% 
 # naniar::replace_with_na(replace = 
  #                          list(values = c("NA", "-"))) %>% 
  filter(!is.na(values)) %>% 
  count(mn) %>%
  mutate(perc = n/nrow(min_values))


#Adding count food items per FCT ----

output1_n <- output1 %>% 
  left_join(min_values %>%  count(fct.id)) %>%  
  dplyr::select(fct.id, fct.name, n) %>% 
  mutate(fct.name_n = paste0(fct.name, " (", n, ")"))

names(output1_n)


##├ Viz: Total missing values per fct -----
min_values %>% mutate_at(compo, as.numeric) %>% naniar::vis_miss() 

 

##├ Viz lollipop: Total missing values per food category

#fct.id faceting didn't provide much information bc we couldn't diff.
#NA from not reporting.

min_values %>% 
  mutate_at(compo, as.numeric) %>% 
  dplyr::select(compo[2:6], 
        # fct.id
         food.category
         ) %>% 
  rename_all(., str_to_sentence) %>% 
  naniar::gg_miss_var(facet = #Fct.id 
                        Food.category
                      ) + labs( x = "") 


##├ Viz heatmap: Total missing values per fct ----

min_values %>% left_join(., output1_n) %>% 
  mutate_at(compo, as.numeric) %>% 
  dplyr::select(compo[2:6],
                fct.name_n,
         #food.category 
         ) %>% 
  rename_at(compo[2:6], str_to_sentence) %>% 
  naniar::gg_miss_fct(.,  fct = fct.name_n
                    # fct = food.category
                      )  + 
 # theme(axis.text.y = element_text(size = 20)) +
  labs( x = "", y = "") + coord_flip() + 
  scale_fill_continuous(name = "% Missing values") + # changing legend
  theme(axis.text.x = element_text(angle = 0, size =22), 
        axis.text.y = element_text(angle = 0, size =20),
        legend.key.size = unit(1.5, 'cm'),
        legend.title = element_text(size=25),
        legend.text = element_text(size=20))

#ggsave(here::here("plots", "heatmap-of-min-FCT.png"), width = 15, height = 7)


##├ Viz heatmap: Total missing values per food category ----
min_values %>% left_join(., min_values %>% count(food.category)) %>% 
  mutate_at(compo, as.numeric) %>% 
  dplyr::select(compo[2:6],
                food.category, n 
  ) %>% 
  rename_at(compo[2:6], str_to_sentence) %>% 
  mutate(
    food.category = case_when(
      str_detect(food.category, "Milk") ~ "Milk and products*",
      str_detect(food.category, "Groundnuts") ~ "Groundnuts and products",
      str_detect(food.category, "Sugar") ~ "Sugar and products",
      TRUE ~         food.category)) %>%
  mutate(food.category_n = paste0(food.category, " (", n, ")")) %>% 
  select(-food.category) %>% 
  naniar::gg_miss_fct(.,  
                      fct = food.category_n
  )  + 
  labs( x = "", y = "", title = "b)") + coord_flip() + 
  theme(axis.text.x = element_text(angle = 0, size =22), 
        axis.text.y = element_text(angle = 0, size =20),
        legend.key.size = unit(1.5, 'cm'),
        legend.title = element_text(size=25),
        legend.text = element_text(size=20)) +
  labs(caption = "*Excl. butter") 

#ggsave(here::here("plots", "heatmap-of-min-food.png"), width = 15, height = 7)


#Count: Total nutrient values
min_values %>%
  pivot_longer(cols = c("zn"  ,  "se"   , "i"   ,  "fe"   , "ca" ),
               names_to = "components", 
               values_to = "values") %>% 
  naniar::replace_with_na(replace =list(values = "NA")) %>% 
  filter(!is.na(values)) %>% count(fct.id)

min_values %>%
  pivot_longer(cols = c("zn"  ,  "se"   , "i"   ,  "fe"   , "ca" ),
               names_to = "components", 
               values_to = "values") %>% 
  naniar::replace_with_na(replace = list(values = c("NA", "-"))) %>% 
  mutate_at("values", as.numeric) %>% 
  ggplot(aes(components, values)) + geom_boxplot() +
  facet_wrap(vars(food.category), scales = "free")

min_values %>%
  pivot_longer(cols = c("zn"  ,  "se"   , "i"   ,  "fe"   , "ca" ),
               names_to = "components", 
               values_to = "values") %>% 
  naniar::replace_with_na(replace = list(values = c("NA", "-"))) %>% 
  mutate_at("values", as.numeric) %>% 
  filter(components == "ca") %>% 
  ggplot(aes(food.category, values)) + geom_boxplot() +
  coord_flip()

#Checking freshwater fish values as they are extremely high
#Exceptions to the protocol were made in other to include more values
#we are removing them from the analysis
#Check value for AF0023 - W

min_values %>% filter(food.category == "Freshwater Fish")


#We are getting the selection of NVs w/ values for all the FCTs 
#included to filter out the ref. w/o values for the reporting
#FCTs reporting at item level only one ref. 
#MW0020 - "loosing" one item (MW08_0006 - sugar) - because all NVs
#were NA

filter.data <-  min_values  %>%
  pivot_longer(cols = c("zn"  ,  "se"   , "i"   ,  "fe"   , "ca" ),
               names_to = "mn", 
               values_to = "values") %>% 
  naniar::replace_with_na(replace = 
                            list(values = c("NA", "-"))) %>%
  filter(!is.na(values)) %>% 
  select(food.id.original, fct.id, mn)

write.csv(filter.data, 
          here::here("inter-output", "filtering-NV-output2b.csv"),
          row.names = F)

# Writing Suppl. table 2 ----

min_value <- min_values %>% arrange(fct.id) %>% relocate(water, .before = ca) 

names(min_value)[5:11] <- str_to_sentence(names(min_value)[5:11])
  
write.csv(min_value, here::here("output", 
                          "Supp.Table2.Food-items-and-mineral-content.csv"),
row.names = F)
  
  