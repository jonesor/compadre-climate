#climwin1_run_temp

# temperature
for (i in 1:nrow(spp_summary)) {
  dat <- filter(comp_sub, SpeciesAuthor == spp_summary$SpeciesAuthor[i])
  climwinOut_tmp <- climwin_summary(dat, var = "tmp_scale")
  
  if(i == 1) write(paste(as.character(names(climwinOut_tmp)),collapse=", "), file="output/climwin_tmp.csv", append=FALSE)
  if(i == 1) write(paste(as.data.frame(climwinOut_tmp)[1,],collapse=", "), file="output/climwin_tmp.csv", append=TRUE)
  
  if(i > 1) write(paste(as.data.frame(climwinOut_tmp)[1,],collapse=", "), file="output/climwin_tmp.csv", append=TRUE)
}



### write results to file
tmp_out <- bind_rows(tmp_list)
write.csv(tmp_out, "output/climwin_tmp.csv", row.names = FALSE)

