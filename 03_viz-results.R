
# Library loading

library(dplyr)
library(raster) # This is needed for coord. of countries (object world)
library(tmap)    # for static and interactive maps

## MAP (fig-2): Number of minerals reported per each FCTs -----

##├ Loading master file with all the FCTs information -----

output1 <- readxl::read_excel(here::here("data",
                                         "food-items-data-collection.xlsx"),
                              sheet = "outcome1") 

names(output1)
unique(output1$MNreported)

output1$MNreported <- stringr::str_to_title(output1$MNreported)
output1$MNreported <- stringr::str_replace(output1$MNreported, "Io", "I")
output1$MNreported[output1$MNreported == "Ca, Fe,"] <- "Ca, Fe"

output1 %>% filter(inclusion == "Y" & country != "NA") %>% 
  filter(fct.id != "KT0025") %>%  #Filter out sub-reg KT0025
  left_join(., raster::ccodes(), by = c("country" = "NAME_FAO")) %>% 
  dplyr::select(ISO2, country, MN_total, MNreported) 

#Creating dataset w/ the geo-data for africa
africa <- world %>% filter(continent == "Africa")

#Disolving boundries for Africa, Middle, Southern, Eastern and Western Africa


world_easta = world[world$subregion == "Eastern Africa", ]
easta = st_union(world_easta)

world_westa = world[world$subregion == "Western Africa", ]
westa = st_union(world_westa)

world_middlea = world[world$subregion == "Middle Africa", ]
middlea = st_union(world_middlea)

world_southa = world[world$subregion == "Southern Africa", ]
southa = st_union(world_southa) 

world_northa = world[world$subregion == "Northern Africa", ]
northa = st_union(world_northa) 

#Binding the four region a in a geo data frame (sf dataframe)

sub.africa = rbind(st_as_sf(easta), st_as_sf(westa), 
                   st_as_sf(middlea), st_as_sf(southa)) 

#adding region name and mineral 

sub.africa[1,2] <- "Eastern Africa"
sub.africa[2,2] <- "Western Africa"
sub.africa[3,2] <- "Middle Africa"
sub.africa[4,2] <- "Southern Africa"
sub.africa[1,3] <- 2
sub.africa[2,3] <- 3
sub.africa[3,3] <- NA
sub.africa[4,3] <- NA
sub.africa[1,4] <- "Ca, Fe," 
sub.africa[2,4] <- "Ca, Fe, Zn"
sub.africa[3,4] <- NA
sub.africa[4,4] <- NA

#Renaming col
colnames(sub.africa) <- c("geom", "subregion", "MN_total", "MNreported")

#needed to keep the geom functoning
st_geometry(sub.africa) <- "geom"

plot(sub.africa[1,])

#Dissolving boundries for SSA


africa_ssa = africa[africa$subregion != "Northen Africa", ]
africa_ssa = africa_ssa[africa_ssa$subregion != "Middle Africa", ]
africa_ssa = st_union(africa_ssa)

world_middlea = world[world$subregion == "Middle Africa", ]
middlea = st_union(world_middlea)

ssa = rbind(st_as_sf(africa_ssa), st_as_sf(middlea)) 

#adding region name and mineral 
ssa[1,2] <- "SS Africa"
ssa[2,2] <- "Middle Africa"
ssa[1,3] <- 5
ssa[2,3] <- NA
ssa[1,4] <- "Ca, Fe, I, Se, Zn"
ssa[2,4] <- NA

#Renaming col
colnames(ssa) <- c("geom", "subregion", "MN_total", "MNreported")

#needed to keep the geom functoning
st_geometry(ssa) <- "geom"

levels <- stringr::str_to_title(c("ca, fe,", "ca, fe, zn", 
                                  "fe, I, zn", "ca, fe, se, zn", 
                                  "ca, fe, I, se, zn"))

fct_africa <- africa %>% 
  left_join(., output1 %>% filter(inclusion == "Y" & country != "NA") %>% 
              filter(fct.id != "KT0025") %>%  #Filter out sub-reg KT0025
              left_join(., raster::ccodes() %>% dplyr::select(NAME, ISO2), by = c("country" = "NAME")) %>%
              filter(!is.na(ISO2)),
            by = c("iso_a2" = "ISO2")) %>% 
  mutate(MNreported = factor(MNreported, levels =levels))

RColorBrewer::display.brewer.pal(n = 8, name = 'YlOrRd')
RColorBrewer::display.brewer.pal(n = 5, name = 'YlOrRd')
RColorBrewer::brewer.pal(n = 5, name = 'YlOrRd')


#cbp1 <- c("#FFFFB2", "#FECC5C", "#FD8D3C", "#E31A1C")
cbp1 <- c("#FFFFB2", "#FECC5C", "#FD8D3C", "#F03B20", "#BD0026")

map1 <- tm_shape(fct_africa) + 
  tm_polygons(col = "MNreported",
              title = "Minerals \nReported", 
              palette = cbp1,
              colorNA = "lightgrey",
              style = "cat",
              n = 5
  ) +
  tm_shape(northa) +
  tm_polygons(col = "white") +
  tm_shape(fct_africa) + 
  tm_borders() +
  tm_shape(sub.africa) +
  tm_borders(lwd = 2,
             col = "black", 
             alpha = 0.9)  +
  tm_layout(
    #  panel.show = T, 
    # panel.labels = "a) National FCTs/ NCTs",
    title = "a)",
    title.position = c("left", "top"), 
    legend.title.size=0.8,
    legend.width = 30,
    legend.text.size = 0.60)


cbp2 <- c("#FFFFB2", "#FECC5C")

map2 <- tm_shape(fct_africa) + 
  tm_borders() +
  tm_shape(sub.africa) + 
  tm_polygons(col = "MNreported",
              # title = "Minerals reported", 
              palette = cbp2,
              colorNA = "lightgrey",
              style = "cat",
              n = 2, 
              legend.show = FALSE)  +
  tm_shape(northa) +
  tm_polygons(col = "white") +
  tm_shape(fct_africa) + 
  tm_borders() + tm_shape(sub.africa) +
  tm_borders(lwd = 2,
             col = "black", 
             alpha = 0.9) +
  tm_layout(
    # panel.show = T, 
    #  panel.labels = "b) Regional FCTs",
    title = "b)",
    title.position = c("left", "top"))

#cbp3 <- c("#E31A1C")
cbp3 <- c("#BD0026")

map3 <- tm_shape(fct_africa) + 
  tm_borders() +
  tm_shape(ssa) + 
  tm_polygons(col = "MNreported",
              #  title = "No. of minerals", 
              palette = cbp3,
              colorNA = "lightgrey",
              style = "cat",
              n = 1,
              legend.show = FALSE) +
  tm_shape(northa) +
  tm_polygons(col = "white") +
  tm_shape(fct_africa) + 
  tm_borders() + tm_shape(sub.africa) +
  tm_borders(lwd = 2,
             col = "black", 
             alpha = 0.9)  +
  tm_layout(
    # panel.show = T, 
    #panel.labels = "c) Regional Africa NCT"
    title = "c)",
    title.position = c("left", "top") )


map_minerals <- tmap_arrange(map1, map2, map3)


tmap_save(map_minerals, "output/geo-minerals_option2.png", width=1920, height=1080) 
