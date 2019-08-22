
# libraries
library(Rcompadre)
library(tidyverse)
library(rgdal)
library(rgeos)
library(maptools)
library(sp)
library(viridis)


# # download ecoregions shapefile to temp directory, and unzip
# tmp <- tempfile()
# tmpdir <- tempdir()
# download.file("https://c402277.ssl.cf1.rackcdn.com/publications/15/files/original/official_teow.zip?1349272619",
#               destfile = tmp)
# unzip(tmp, exdir = tmpdir)
# 
# # read ecoregions shapefile
# ecoregions <- readOGR(dsn = paste0(tmpdir, "/official"),
#                       layer = "wwf_terr_ecos")
# 
# # aggregate by biome (takes a few minutes)
# biomes <- ecoregions %>%
#   unionSpatialPolygons(ecoregions@data$BIOME)
# 
# # write to file
# save(biomes, file = "data/biomes.RData")

# load biomes shapefile
load("data/biomes.RData")


# simplify polygons
biomes_simple <- gSimplify(biomes, tol = 0.1)

# read df with biome names
biome_names <- read.csv("data/biome_names.csv") %>% 
  mutate(biome = factor(biome, levels = biome))

# fortify for plotting
biomes_tidy <- fortify(biomes_simple) %>% 
  as_tibble() %>% 
  mutate(id = as.integer(id)) %>% 
  left_join(biome_names, by = "id") %>% 
  filter(lat > -56) %>% 
  filter(biome != "Lake")

# plot colors
biome_cols <- c(
  viridis(10),
  # med, desert
  "#ff7f00",
  "#b15928",
  # mangrove
  "#fb9a99",
  # tundra, ice
  "grey75",
  "grey50"
)

# plot
p <- ggplot(biomes_tidy) +
  geom_polygon(aes(long, lat, group = group, fill = biome), alpha = 0.9) +
  scale_fill_manual(name = NULL, values = biome_cols, guide = guide_legend(nrow = 5)) +
  labs(x = NULL, y = NULL) +
  theme(legend.position = "bottom",
        panel.grid = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())


dev.off()
quartz(height = 5.5, width = 8, dpi = 140)
print(p)



# load compadre
compadre <- cdb_fetch("data/COMPADRE_v.X.X.X.RData")

# transform compadre to spatial
comp_coords <- compadre %>% 
  filter(!is.na(Lat), !is.na(Lon)) %>% 
  as_tibble() %>% 
  select(SpeciesAuthor, Authors, MatrixPopulation, Lat, Lon, Ecoregion) %>% 
  unique()

coordinates(comp_coords) <- c("Lon", "Lat")
proj4string(comp_coords) <- CRS("+proj=longlat")

comp_coords <- spTransform(comp_coords, proj4string(biomes))


# get biome id for given lat/lon
comp_coords$id <- as.integer(row.names(biomes)[over(comp_coords, biomes)])


# find instances where ecoregion in Compadre doesn't match shapefile
comp_nomatch <- comp_coords %>%
  as_tibble() %>% 
  left_join(biome_names) %>% 
  mutate(abbrev = as.character(abbrev)) %>% 
  filter(Ecoregion != abbrev)
