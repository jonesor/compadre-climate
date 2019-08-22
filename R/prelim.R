
### libraries
library(tidyverse)
library(Rcompadre)
library(Rage)
library(popbio) 
library(popdemo)
library(gridExtra)
library(maps)



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
  mutate(site_pop_id = cdb_id(., c("Lat", "Lon", "MatrixPopulation"))) %>% 
  mutate(EcoregionFull = case_when(
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



# find SpeciesAuthor with time-series >= 5 years
comp_ts <- comp_sub %>% 
  as_tibble() %>% 
  group_by(SpeciesAuthor, site_id) %>% 
  summarize(n_year = length(unique(MatrixStartYear)),
            Lon = unique(Lon),
            Lat = unique(Lat),
            Population = paste(unique(MatrixPopulation), collapse = ", "),
            Ecoregion = unique(Ecoregion),
            Continent = unique(Continent),
            Country = unique(Country),
            OrganismType = unique(OrganismType),
            Authors = unique(Authors)) %>% 
  ungroup() %>% 
  filter(n_year >= 5)

# counts by Country
comp_ts %>% 
  count(Country, sort = TRUE)

# counts by Ecoregion
comp_ts %>% 
  count(Ecoregion, sort = TRUE)

# counts by OrganismType
comp_ts %>% 
  count(OrganismType, sort = TRUE)

# counts by Authors
comp_ts %>% 
  count(Authors, sort = TRUE)

# time series >= 5 years
length(which(comp_ts$n_year >= 5))



### file to correct coords
sites_out <- comp_sub %>% 
  as_tibble() %>% 
  filter(SpeciesAuthor %in% comp_ts$SpeciesAuthor) %>% 
  select(SpeciesAuthor, Authors, Journal, YearPublication, DOI.ISBN, Continent,
         Country, MatrixPopulation, Lat, Lon, Ecoregion) %>% 
  arrange(Continent, Country, SpeciesAuthor) %>% 
  unique()
  
write.csv(sites_out, "output/site_data.csv", row.names = FALSE)



### variance in lambda over time
comp_lambda <- comp_sub %>% 
  filter(SpeciesAuthor %in% comp_ts$SpeciesAuthor, MatrixPopulation %in% comp_ts$Population) %>% 
  cdb_flag("check_NA_A") %>% 
  filter(check_NA_A == FALSE) %>% 
  mutate(lambda = map_dbl(matA(mat), popbio::lambda)) %>% 
  mutate(pop = paste(SpeciesAuthor, MatrixPopulation, sep = "--"))

comp_lambda %>% 
  as_tibble() %>% 
  group_by(Authors, MatrixPopulation) %>% 
  summarize(var_lambda = var(log(lambda))) %>% 
  ungroup() %>% 
  arrange(var_lambda)

df_varlam <- comp_lambda %>% 
  group_by(pop) %>% 
  mutate(var_lambda = var(log(lambda))) %>% 
  ungroup() %>% 
  arrange(var_lambda) %>% 
  filter(pop %in% c(head(unique(pop), n = 10), tail(unique(pop), n = 10))) %>% 
  mutate(pop = reorder(pop, var_lambda))

ggplot(df_varlam, aes(MatrixStartYear, lambda)) +
  geom_point() +
  geom_hline(yintercept = 1, alpha = 0.4, size = 0.4) +
  scale_y_log10() +
  facet_wrap(~ pop, scales = "free_x") +
  theme(panel.grid = element_blank())



### plot sites on world map
ggplot(filter(comp_ts, n_year >= 5), aes(Lon, Lat)) +
  borders(database = "world", fill = "grey80", col = NA) +
  geom_point(aes(size = n_year), col = "darkblue", shape = 1, alpha = 0.6) +
  coord_cartesian(ylim = c(-20, 80))



### check treatments
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



### transform coordinates from Menges
# library(sp)
# hc <- read_csv("Rosemary_Hc_patches_2009map.csv")
# coordinates(hc) <- c("X", "Y")
# proj4string(hc) <- "+proj=utm +zone=17 +ellps=GRS80 +datum=NAD83 +units=m +no_defs "
# 
# hc_ll <- spTransform(hc, "+proj=longlat") %>%
#   as.data.frame() %>%
#   select(BALD_, lat = Y, long = X)
# 
# write.csv(hc_ll, "Rosemary_Hc_patches_ll.csv", row.names = F)

# ec <- read_csv("Rosemary_Ec_patches_2009map.csv")
# coordinates(ec) <- c("X", "Y")
# proj4string(ec) <- "+proj=utm +zone=17 +ellps=GRS80 +datum=NAD83 +units=m +no_defs "
# 
# ec_ll <- spTransform(ec, "+proj=longlat") %>%
#   as.data.frame() %>%
#   select(BALD_, lat = Y, long = X)
# 
# write.csv(ec_ll, "Rosemary_Ec_patches_ll.csv", row.names = F)
