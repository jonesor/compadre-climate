
### libraries
library(tidyverse)
library(rgdal)
library(raster)


### create shell scripts to download chelsa climate rasters
# year <- 1979:2013
# month <- formatC(1:12, width = 2, flag = "0")
# ym <- expand.grid(month = month, year = year)
# 
# tmp <- "wget https://www.wsl.ch/lud/chelsa/data/timeseries/tmean/CHELSA_tmean"
# ppt <- "wget https://www.wsl.ch/lud/chelsa/data/timeseries/prec/CHELSA_prec"
# end <- "V1.2.1.tif"
# 
# cat(c("#!/usr/bin/env bash", paste(tmp, ym$year, ym$month, end, sep = "_")),
#     file = "bash/fetch_chelsa_tmp.sh", sep = "\n")
# cat(c("#!/usr/bin/env bash", paste(ppt, ym$year, ym$month, end, sep = "_")),
#     file = "bash/fetch_chelsa_ppt.sh", sep = "\n")


### function to extract clim data from raster file for given set of coordinates
fetch_chelsa <- function(file_tmp, file_ppt, year, month, spp) {
  # year <- as.integer(strsplit(file_tmp, "_")[[1]][3])
  # month <- as.integer(strsplit(file_tmp, "_")[[1]][4])
  chelsa_tmp <- raster(x = file_tmp)
  chelsa_ppt <- raster(x = file_ppt)
  if (!is.data.frame(spp)) spp <- spp[[1]]
  coordinates(spp) <- c("Lon", "Lat")
  pbase <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
  proj4string(spp) <- pbase
  spp <- spTransform(spp, crs(chelsa_tmp))
  spp$tmp <- chelsa_tmp[cellFromXY(chelsa_tmp, spp)] / 10 - 273.15 # Kelvin -> C
  spp$ppt <- chelsa_ppt[cellFromXY(chelsa_ppt, spp)]
  spp$ppt[spp$ppt > 65000] <- NA_real_
  spp$year <- year
  spp$month <- month
  return(as_tibble(spp))
}


### chelsa raster files (via network drive)
netdr_t <- "/Volumes/PlantAnimalBiodemography/Patrick/chelsa/temp"
netdr_p <- "/Volumes/PlantAnimalBiodemography/Patrick/chelsa/prec"
files_tmp <- paste(netdr_t, list.files(netdr_t), sep = "/")
files_ppt <- paste(netdr_p, list.files(netdr_p), sep = "/")

df <- tibble(files_tmp, files_ppt) %>% 
  mutate(year = map_int(files_tmp, ~ as.integer(strsplit(.x, "_")[[1]][3]))) %>% 
  mutate(month = map_int(files_tmp, ~ as.integer(strsplit(.x, "_")[[1]][4])))


### coordinates for each species/pop of interest
spp_df <- read_csv("data/site_data.csv") %>% 
  mutate(Lat = ifelse(!is.na(Lat_Corr), Lat_Corr, Lat),
         Lon = ifelse(!is.na(Lon_Corr), Lon_Corr, Lon))


### test fetch_chelsa function
# fetch_chelsa(df$files_tmp[1], df$files_ppt[1], df$year[1], df$month[1], spp_df)


### loop to get climate data from all raster files for all species of interest
clim_out <- list()

for(i in 1:nrow(df)) {
  clim_out[[i]] <- fetch_chelsa(df$files_tmp[i],
                                df$files_ppt[i],
                                df$year[i],
                                df$month[i],
                                spp_df)
  print(i)
}

# bind into data.frame
df_clim <- bind_rows(clim_out) %>% 
  arrange(SpeciesAuthor, MatrixPopulation, year, month)
  

### get climate data from all raster files for all species of interest
# df_clim <- tibble(files_tmp, files_ppt, spp = list(spp_df)) %>% 
#   mutate(Year = map_int(files_tmp, ~ as.integer(strsplit(.x, "_")[[1]][3]))) %>% 
#   mutate(Month = map_int(files_tmp, ~ as.integer(strsplit(.x, "_")[[1]][4]))) %>% 
#   group_by(Year, Month) %>% 
#   do(fetch_chelsa(.$file_tmp, .$file_ppt, .$spp)) %>% 
#   ungroup() %>% 
#   arrange(SpeciesAuthor, MatrixPopulation, Year, Month)

# write to file
write.csv(df_clim, "data/clim/species_clim.csv", row.names = FALSE)




### climatogram figure via plotly
library(plotly)

df_clim <- read_csv("data/clim/species_clim.csv")

out <- df_clim %>% 
  mutate(site = as.integer(as.factor(paste(Lat, Lon)))) %>% 
  select(site, Authors, Continent, Country, Lat, Lon, year, month, tmp, ppt) %>% 
  group_by(site, Country, month, Authors) %>% 
  summarize(tmp = mean(tmp)) %>% 
  ungroup() %>% 
  unique() %>% 
  mutate(site = paste0(site, " (", Authors, ")"))

p <- ggplot(out, aes(as.factor(month), tmp, group = site)) +
  geom_line(aes(text = Authors), size = 1.2, alpha = 0.3, col = "darkred") +
  geom_hline(yintercept = 0, linetype = 2) +
  facet_wrap(~ Country) +
  labs(x = "Month", y = "Temperature") +
  theme_minimal() +
  theme(panel.border = element_rect(fill = NA),
        panel.grid = element_blank())

pp <- ggplotly(p, tooltip = c("site")) %>% 
  layout(margin = list(l = 80, b = 80))

# requires plots api key
# api_create(pp, filename = "climatograms", overwrite = T)
