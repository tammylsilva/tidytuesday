##TidyTuesday 8/4/2020
#Tammy Silva
##European energy


library(tidytuesdayR)
library(tidyverse)
library(maps)
library(viridis)

#load in data with tidytues R package
tuesdata <- tidytuesdayR::tt_load('2020-08-04')

#get datasets from the list
energy_types <- tuesdata$energy_types
country_totals <- tuesdata$country_totals

str(energy_types)
str(country_totals)

#which countries are included
unique(energy_types$country_name)
 
#country name as a factor
energy_types$country_name <- as.factor(energy_types$country_name)
 
#remove Namibia and Nauru (very far away from other countries geographically)
energy_types <- energy_types %>% filter(country_name!='Namibia')
energy_types <- energy_types %>% filter(country_name!='Nauru')

#sum each energy type across all three years
energy_types <- energy_types %>%
  mutate(all_years=rowSums(.[5:7]))

#get the highest energy type for each country
max_energy<-energy_types %>% 
  group_by(country_name) %>%
  filter(all_years==max(all_years))

#get list of countries
countries <- unique(energy_types$country_name)

#get map data of these countries
eu_countries_map <- map_data("world", region = countries)

#make the middle of the map the mean lat lon of the countries
#Compute the centroid as the mean longitude and lattitude
# Used as label coordinate for country's names
region_data <- eu_countries_map %>%
  group_by(region) %>%
  summarise(long = mean(long), lat = mean(lat))

colnames(eu_countries_map) <- c("long","lat","group","order","country_name","subregion")

#join with eu_countries_map
data<-full_join(max_energy,eu_countries_map,by="country_name")

#make the map
map<-ggplot(data, aes(x = long, y = lat)) +
  geom_polygon(aes( group = group, fill = type)) +
  ylim(30,75) +
  geom_text(aes(label = region), data = region_data, size = 4) +
  scale_fill_viridis_d(name="Energy Type") +
  theme_void(base_size=16) +
  ggtitle("European Energy Use",
    subtitle="Energy type with the highest combined energy use (2016-2018) by country")
  
ggsave("european_energy.png",map,height = 8, width = 8, dpi=300)

