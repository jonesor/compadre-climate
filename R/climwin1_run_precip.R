#climwin_run_precip.R

# precipitation
for (i in 1:nrow(spp_summary)) {
  dat <- filter(comp_sub, SpeciesAuthor == spp_summary$SpeciesAuthor[i])
  climwinOut_ppt <- climwin_summary(dat, var = "ppt_scale")
  
  if(i == 1) write(paste(as.character(names(climwinOut_ppt)),collapse=", "), file="output/climwin_ppt.csv", append=FALSE)
  if(i == 1) write(paste(as.data.frame(climwinOut_ppt)[1,],collapse=", "), file="output/climwin_ppt.csv", append=TRUE)
  
  if(i > 1) write(paste(as.data.frame(climwinOut_ppt)[1,],collapse=", "), file="output/climwin_ppt.csv", append=TRUE)
  }
