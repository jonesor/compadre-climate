# Selecting study organisms


### libraries
library(tidyverse)
library(Rcompadre)
library(Rage)
library(popbio)
library(popdemo)
library(gridExtra)
library(maps)
library(ggplot2)


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
  filter(MatrixTreatment != "Protected" &
         MatrixTreatment != "Herbicide picloram" &
         MatrixTreatment != "Transplanted plants" &
         MatrixTreatment != "Mown"&
         MatrixTreatment != "Fire" &
         MatrixTreatment != "After burn" &
         MatrixTreatment != "Burned" &
         MatrixTreatment != "Fruit harvesting and lantana cover" &
         MatrixTreatment != "Fruit harvesting and mistletoe and lantana cover" &
         MatrixTreatment != "Fruit harvesting and mistletoe cover" &
         MatrixTreatment != "Fruit harvesting and no cover" &
         MatrixTreatment != "Lantana cover" &
         MatrixTreatment != "Mistletoe and lantana cover" &
         MatrixTreatment != "Mistletoe cover" &
         MatrixTreatment != "Fruit harvesting and Mistletoe cover" &
         MatrixTreatment != "Fruit harvesting and no cover" &
         MatrixTreatment != "Mistletoe cover") %>%
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
  ungroup() %>% 
  filter(n_year >= 5)

length(which(comp_ts$n_year >= 5)) # time series >= 5 years

# ecoregion counts and plot
comp_ts %>% 
  count(Ecoregion) %>% 
  arrange(desc(n)) %>% 
  print(n = "all")%>% 
  ggplot(aes(x=reorder(Ecoregion, -n), y=n))+
  geom_bar(stat = 'identity', size=2, fill="#b7bc25")+ 
  geom_text(aes(label = paste("n=", n, sep=""), y = n+1, x=Ecoregion), size = 3, position = position_dodge(width=0.9))+
  #ggtitle("number of sites per ecoregion")+
  theme_minimal()+
  xlab("Ecoregion")+
  ylab("Number of sites")+
  coord_flip()
#ggsave("output/ecoregion_freq.png", a, height=7, width=10, dpi=300)


# organism type counts and plot
comp_ts %>% 
  count(OrganismType) %>% 
  arrange(desc(n)) %>% 
  filter(n > 0)%>% 
  ggplot(aes(x=reorder(OrganismType, -n), y=n))+
  geom_bar(stat = 'identity', size=2, fill="#b7bc25")+ 
  geom_text(aes(label = paste("n=", n, sep=""), y = n+1, x=OrganismType), size = 3, position = position_dodge(width=0.9))+
  #ggtitle("number of sites per ecoregion")+
  theme_minimal()+
  xlab("Ecoregion")+
  ylab("Number of sites")+
  coord_flip()
# ggsave("output/organismType_freq.png", b, height=7, width=7, dpi=300)

comp_ts %>% 
  count(Authors) %>% 
  arrange(desc(n)) %>% 
  print(n = "all")


# plot sites on world map
ggplot(filter(comp_ts, n_year >= 5), aes(Lon, Lat)) +
  borders(database = "world", fill = "grey80", col = "grey90") +
  geom_point(aes(size = n_year), col = "darkblue", shape = 1, alpha = 0.6) +
  coord_cartesian(ylim = c(-20, 80))
# ggsave("output/map_allSites.png", c, height=7, width=14, dpi=300)


# check which treatments the subset includes
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

