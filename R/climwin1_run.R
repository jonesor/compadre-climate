
### libraries
library(tidyverse)
library(Rcompadre)
library(Rage)
library(popbio) 
library(climwin)
library(lubridate)
source("R/functions.R")



### load compadre and climate data
dat_keep <- read_csv("data/teated_sites_decisions.csv") %>% 
  select(-DOI.ISBN, -Comment)

dat_coords <- read_csv("data/clim/species_clim.csv") %>% 
  select(SpeciesAuthor, MatrixPopulation, Lon_Corr = Lon, Lat_Corr = Lat) %>% 
  unique()

compadre <- cdb_fetch("data/COMPADRE_v.X.X.X.RData") %>% 
  left_join(dat_coords) %>% 
  mutate(Lat = ifelse(!is.na(Lat_Corr), Lat_Corr, Lat),
         Lon = ifelse(!is.na(Lon_Corr), Lon_Corr, Lon))

dat_clim <- read_csv("data/clim/species_clim.csv") %>% 
  filter(Authors != "Dalgleish; Koons; Adler") %>% 
  select(Lon, Lat, year, month, tmp, ppt) %>% 
  unique() %>% 
  mutate(date = paste("01", formatC(month, width = 2, flag = "0"), year, sep = "/")) %>% 
  group_by(month, Lon, Lat) %>% 
  mutate(tmp_scale = as.numeric(scale(tmp)),
         ppt_scale = as.numeric(scale(ppt))) %>% 
  ungroup()
  
adler_ppt <- read_csv("data/clim/adler_monthly_ppt.csv") %>% 
  filter(YEAR >= 1930) %>%
  setNames(c("year", 1:12)) %>% 
  gather(month, ppt, -year) %>% 
  mutate(month = as.integer(month)) %>% 
  arrange(year, month) %>% 
  mutate(date = paste("01", formatC(month, width = 2, flag = "0"), year, sep = "/"))

adler_clim <- read_csv("data/clim/adler_monthly_temp.csv") %>% 
  filter(YEAR >= 1930) %>%
  setNames(c("year", 1:12)) %>% 
  gather(month, tmp, -year) %>% 
  mutate(month = as.integer(month)) %>% 
  arrange(year, month) %>% 
  left_join(adler_ppt) %>% 
  group_by(month) %>% 
  mutate(tmp_scale = as.numeric(scale(tmp)),
         ppt_scale = as.numeric(scale(ppt))) %>% 
  ungroup() %>% 
  mutate(tmp_scale = ifelse(is.na(tmp_scale), 0, tmp_scale),
         ppt_scale = ifelse(is.na(ppt_scale), 0, ppt_scale)) %>% 
  add_column(Lon = -99.3, Lat = 38.8, .before = 1)

clim_full <- bind_rows(dat_clim, adler_clim)



### subset and prepare dataset for analysis
comp_sub <- compadre %>% 
  filter(MatrixComposite == "Individual",
         AnnualPeriodicity == "1",
         MatrixCaptivity == "W",
         !is.na(Lon) & !is.na(Lat),
         Continent %in% c("Europe", "N America")) %>% 
  left_join(dat_keep) %>%
  mutate(vote = ifelse(is.na(vote), "keep", vote)) %>%
  filter(vote != "out") %>%
  cdb_flag("check_NA_A") %>%
  mutate(all_zero = map_lgl(matA(.), ~ all(.x == 0))) %>%
  filter(check_NA_A == FALSE, all_zero == FALSE) %>%
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
  )) %>% 
  group_by(SpeciesAuthor, site_id) %>% 
  mutate(n_year = length(unique(MatrixStartYear))) %>% 
  ungroup() %>% 
  filter(n_year >= 5) %>% 
  mutate(loglam = map_dbl(matA(mat), ~ log(popbio::lambda(.x)))) %>%
  mutate(MatrixEndMonth = ifelse(MatrixEndMonth == "8/9", "9", MatrixEndMonth)) %>% 
  mutate(MatrixEndMonth = ifelse(is.na(MatrixEndMonth), "8", MatrixEndMonth)) %>%
  mutate(date = paste("01", MatrixEndMonth, MatrixEndYear, sep = "/"))

spp_summary <- comp_sub %>% 
  as_tibble() %>% 
  mutate(SpeciesAuthor = sapply(SpeciesAuthor, spp_short)) %>% 
  group_by(SpeciesAuthor) %>% 
  summarize(n_year = length(unique(MatrixStartYear)),
            n_pops = length(unique(MatrixPopulation)),
            n_site = length(unique(paste(Lon, Lat))),
            endmon = unique(MatrixEndMonth),
            ecoreg = unique(Ecoregion)) %>% 
  as.data.frame() %>% 
  mutate(ecoreg = ifelse(ecoreg %in% c("DES", "MED"), "Desert/Med", "Temperate")) %>% 
  arrange(ecoreg)

# write.csv(spp_summary, "data/end_months.csv", row.names = FALSE)



### run climwin analysis for each species in spp_summary
# takes ~12 hours

# temperature
tmp_list <- list()

for (i in 1:nrow(spp_summary)) {
  dat <- filter(comp_sub, SpeciesAuthor == spp_summary$SpeciesAuthor[i])
  tmp_list[[i]] <- climwin_summary(dat, var = "tmp_scale")
}

# precipitation
ppt_list <- list()

for (i in 1:nrow(spp_summary)) {
  dat <- filter(comp_sub, SpeciesAuthor == spp_summary$SpeciesAuthor[i])
  ppt_list[[i]] <- climwin_summary(dat, var = "ppt_scale")
}


# # convert p-values to character
# for (i in 1:nrow(spp_summary)) {
#   tmp_list[[i]]$p_a <- as.character(tmp_list[[i]]$p_a)
#   ppt_list[[i]]$p_a <- as.character(ppt_list[[i]]$p_a)
# }


### write results to file
tmp_out <- bind_rows(tmp_list)
ppt_out <- bind_rows(ppt_list)

write.csv(tmp_out, "output/climwin_tmp.csv", row.names = FALSE)
write.csv(ppt_out, "output/climwin_ppt.csv", row.names = FALSE)

