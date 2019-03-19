#Selecting study organisms
### libraries
library(tidyverse)
library(Rcompadre)
library(Rage)
library(popbio)
library(popdemo)
library(gridExtra)


### load compadre data
compadre <- cdb_fetch("data/COMPADRE_v.X.X.X.RData")


### find long time-series for climate analysis
# first subset to wild, unmanipulated populations with 1-yr periodicity
comp_sub <- compadre %>% 
  filter(MatrixComposite == "Individual",
         # MatrixTreatment == "Unmanipulated",
         AnnualPeriodicity == "1",
         MatrixCaptivity == "W",
         !is.na(Lon) & !is.na(Lat)) %>% 
  mutate(site_id = cdb_id(., c("Lat", "Lon"))) %>% 
  mutate(Ecoregion = case_when(
    Ecoregion == "TMB" ~ "trop/subtrop moist broadleaf forest",
    Ecoregion == "TDB" ~ "trop/subtrop dry broadleaf forest",
    Ecoregion == "TSC" ~ "trop/subtrop coniferous forest",
    Ecoregion == "TBM" ~ "temperate broadleaf/mixed forest",
    Ecoregion == "TCF" ~ "temperate coniferous forest",
    Ecoregion == "BOR" ~ "boreal forest/taiga",
    Ecoregion == "TGV" ~ "tropical and subtropical grasslands, savannas and shrublands",
    Ecoregion == "TGS" ~ "temperate grasslands, savannas, and shrublands",
    Ecoregion == "FGS" ~ "flooded grasslands and savannas",
    Ecoregion == "MON" ~ "montane grasslands and shrublands",
    Ecoregion == "TUN" ~ "tundra",
    Ecoregion == "MED" ~ "Mediterranean forests/woodlands/scrubs",
    Ecoregion == "DES" ~ "deserts and xeric shrublands",
    Ecoregion == "MAN" ~ "mangrove",
    Ecoregion == "POE" ~ "marine polar ecosystems",
    TRUE ~ Ecoregion
  ))


# find populations with time-series >= 5 years
comp_ts <- comp_sub %>% 
  as_tibble() %>% 
  group_by(SpeciesAuthor, site_id) %>% 
  summarize(n_year = length(unique(MatrixStartYear)),
            Lon = unique(Lon),
            Lat = unique(Lat),
            Ecoregion = unique(Ecoregion),
            OrganismType = unique(OrganismType),
            Authors = unique(Authors)) %>% 
  ungroup()

length(which(comp_ts$n_year >= 5)) # time series >= 5 years
nrow(comp_ts)                      # total species-sites

# ecoregion counts
comp_ts %>% 
  filter(n_year >= 5) %>% 
  count(Ecoregion) %>% 
  arrange(desc(n)) %>% 
  print(n = "all")

# organism type counts
comp_ts %>% 
  filter(n_year >= 5) %>% 
  count(OrganismType) %>% 
  arrange(desc(n)) %>% 
  filter(n > 0)

comp_ts %>% 
  filter(n_year >= 5) %>% 
  count(Authors) %>% 
  arrange(desc(n)) %>% 
  print(n = "all")



# plot sites on world map
ggplot(filter(comp_ts, n_year >= 5), aes(Lon, Lat)) +
  borders(database = "world", fill = "grey80", col = NA) +
  geom_point(aes(size = n_year), col = "darkblue", shape = 1, alpha = 0.6) +
  coord_cartesian(ylim = c(-20, 80))



treatments_check <- comp_sub %>% 
  filter(SpeciesAuthor %in% comp_ts$SpeciesAuthor,
         site_id %in% comp_ts$site_id) %>% 
  as_tibble() %>% 
  filter(MatrixTreatment != "Unmanipulated") %>% 
  group_by(SpeciesAuthor, site_id, MatrixTreatment) %>% 
  summarize(n_year = length(unique(MatrixStartYear)),
            Authors = unique(Authors),
            DOI.ISBN = unique(DOI.ISBN)) %>% 
  ungroup()

write.csv(treatments_check, "output/treatment_check.csv", row.names = FALSE)

