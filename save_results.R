# Save Results ------------------------------------------------------------

# write the results to text file for plotting in ArcGIS
write.table(
  df_MESH,
  file = "results/20190430/MESH.csv",
  sep = ",",
  row.names = F,
  col.names = T,
  quote = F,
  na = "-999"
)

#save(dfBio,dfBioGroup,file="results.Rda")