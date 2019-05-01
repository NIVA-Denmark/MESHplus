# Save Results ------------------------------------------------------------

folder<-"20190430"

# write the results to text file for plotting in ArcGIS
write.table(
  df_MESH,
  file = paste0("results/",folder,"/MESH.csv"),
  sep = ",",
  row.names = F,
  col.names = T,
  quote = F,
  na = "-999"
)


write.table(
  meshcount,
  file = paste0("results/",folder,"/MESH_count.csv"),
  sep = ",",
  row.names = F,
  col.names = T,
  quote = F,
  na = "-999"
)
#save(dfBio,dfBioGroup,file="results.Rda")