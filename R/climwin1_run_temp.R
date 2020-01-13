#climwin1_run_temp

# temperature
tmp_list <- list()

for (i in 1:nrow(spp_summary)) {
  dat <- filter(comp_sub, SpeciesAuthor == spp_summary$SpeciesAuthor[i])
  tmp_list[[i]] <- climwin_summary(dat, var = "tmp_scale")
  
  if(i == 1) cat(paste0(i,"/",nrow(spp_summary), " - temperature \n"),file="/Users/jones/Dropbox/!Inbox/temp_TemperatureProgress.txt",append = FALSE)
  if(i > 1) cat(paste0(i,"/",nrow(spp_summary), " - temperature \n"),file="/Users/jones/Dropbox/!Inbox/temp_TemperatureProgress.txt",append = TRUE)
}

### write results to file
tmp_out <- bind_rows(tmp_list)
write.csv(tmp_out, "output/climwin_tmp.csv", row.names = FALSE)
