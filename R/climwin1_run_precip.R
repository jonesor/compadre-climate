#climwin_run_precip.R


# precipitation
ppt_list <- list()

for (i in 1:nrow(spp_summary)) {
  dat <- filter(comp_sub, SpeciesAuthor == spp_summary$SpeciesAuthor[i])
  ppt_list[[i]] <- climwin_summary(dat, var = "ppt_scale")
  
  if(i == 1) cat(paste0(i,"/",nrow(spp_summary), " - precipitation \n"),file="/Users/jones/Dropbox/!Inbox/temp_PrecipProgress.txt",append = FALSE)
  if(i > 1) cat(paste0(i,"/",nrow(spp_summary), " - precipitation \n"),file="/Users/jones/Dropbox/!Inbox/temp_PrecipProgress.txt",append = TRUE)
  
}


# # convert p-values to character
# for (i in 1:nrow(spp_summary)) {
#   tmp_list[[i]]$p_a <- as.character(tmp_list[[i]]$p_a)
#   ppt_list[[i]]$p_a <- as.character(ppt_list[[i]]$p_a)
# }


### write results to file
ppt_out <- bind_rows(ppt_list)
write.csv(ppt_out, "output/climwin_ppt.csv", row.names = FALSE)
