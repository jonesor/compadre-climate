

cdb_glimpse <- function(db, cols = NULL) {
  db <- as_tibble(db)
  db <- rename(db, StartYear = MatrixStartYear, EndYear = MatrixEndYear)
  db[,c("SpeciesAuthor", "MatrixPopulation", "MatrixComposite",
        "MatrixTreatment", "StartYear", "EndYear", cols)]
}


spp_short <- function(x) {
  gsub("subsp\\._|var\\._", "", x)
}


climwin_summary <- function(dat, var = "tmp_scale") {
  
  unique_groups <- dat %>% 
    as_tibble() %>% 
    select(MatrixPopulation, Lon, Lat) %>% 
    unique()
  
  clim <- clim_full %>%
    filter(Lon %in% dat$Lon, Lat %in% dat$Lat) %>% 
    left_join(unique_groups)
  
  # sliding window
  MassWin <- slidingwin(xvar = list(Temp = clim[[var]]),
                        cdate = clim$date,
                        bdate = dat$date,
                        baseline = lm(loglam ~ 1, data = dat),
                        cinterval = "month",
                        range = c(24, 0),
                        type = "relative",
                        stat = "mean",
                        func = "lin",
                        spatial = list(dat$MatrixPopulation, clim$MatrixPopulation))
  
  # randomization
  RandWin <- randwin(repeats = 1000,
                     xvar = list(Temp = clim[[var]]),
                     cdate = clim$date,
                     bdate = dat$date,
                     baseline = lm(loglam ~ 1, data = dat),
                     cinterval = "month",
                     range = c(24, 0),
                     type = "relative",
                     stat = "mean",
                     func = "lin",
                     spatial = list(dat$MatrixPopulation, clim$MatrixPopulation))
  
  # arrange outputs
  p_c <- pvalue(dataset = MassWin[[1]]$Dataset,
                datasetrand = RandWin[[1]],
                metric = "C",
                sample.size = length(unique(dat$MatrixEndYear)))
  
  p_a <- pvalue(dataset = MassWin[[1]]$Dataset,
                datasetrand = RandWin[[1]],
                metric = "AIC")
  
  win_open <- MassWin[[1]]$Dataset$WindowOpen[1]
  win_clos <- MassWin[[1]]$Dataset$WindowClose[1]
  beta <- MassWin[[1]]$Dataset$ModelBeta[1]
  se_beta <- MassWin[[1]]$Dataset$Std.Error[1]
  
  return(tibble(SpeciesAuthor = unique(dat$SpeciesAuthor),
                end_month = unique(dat$MatrixEndMonth),
                win_open, win_clos, beta, se_beta,
                p_c, p_a))
}

