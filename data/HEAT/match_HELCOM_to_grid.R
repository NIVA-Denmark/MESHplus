library(tidyverse)


# table matching HELCOM shapes to EEA grid cells, with area of overlap
dfgridmatch<-read.table(file="data/HEAT/grid_match.txt", 
                        quote="",sep=";", 
                        header=T, 
                        stringsAsFactors=F) %>% 
  select(Country=COUNTRY,ID,GRIDCODE,AREA=SHAPE_AREA)

# data collected by JNCC 
dfdat<-read.table(file="data/HEAT/data_HELCOM_OSPAR.txt", 
                  quote="",
                  sep="\t", 
                  header=T,
                  comment.char = "",
                  stringsAsFactors=F) %>% 
  filter(Source=="HOLAS2EUTRO_20112016_Indicators.xlsx") %>%  # taking only HELCOM data
  filter(!is.na(Obs),!is.na(Threshold),Exclude==0) %>%
  select(Country,ID,Category,Parameter,Unit,Obs,Threshold,Response) 

df <- dfgridmatch %>% left_join(dfdat,by=c("Country","ID")) 

df <- df %>% group_by(GRIDCODE,Category,Parameter,Unit,Response) %>%
  summarise(Obs=mean(Obs,na.rm=T),Threshold=mean(Threshold,na.rm=T)) %>%
  ungroup() %>%
  filter(!is.na(Category))

write.table(df,file="data/indicators_eutro.csv",sep="\t",fileEncoding="UTF-8",row.names=F,col.names=T,quote=F)
