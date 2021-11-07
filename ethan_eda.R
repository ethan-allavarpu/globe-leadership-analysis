library(tidyverse)
library(rworldmap)
source("iso_codes")

leadership <- read.csv("GLOBE-Phase-2-Aggregated-Leadership-Data.csv")
society <- read.csv("GLOBE-Phase-2-Aggregated-Societal-Culture-Data.csv")

combined_data <- full_join(leadership, society,
                           by = c("Country", "Country.Name"))

View(combined_data)

autocratic <- leadership %>% select(Country, Country.Name, Autocratic)

mapped_data <- joinCountryData2Map(autocratic, joinCode = "NAME", nameJoinColumn = "Country")

mapCountryData(mapped_data, nameColumnToPlot = "Autocratic")
heatmap_world(autocratic)
