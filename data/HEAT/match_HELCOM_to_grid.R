library(tidyverse)

EutClass<-function(ER){
  Class<-ifelse(ER<=0.5,1,2)
  Class<-ifelse(ER>1,3,Class)
  Class<-ifelse(ER>1.5,4,Class)
  Class<-ifelse(ER>2,5,Class)
  Class<-ifelse(is.na(ER),NA,Class)
  return(Class)
}

bWrite<-F #FALSE
# --------------------------------------------------------
coastweight <- 1 # weight of indicators from coastal areas relative to offshore when aggregating withig grid cell

dfgridmatch<-read.table(file="data/grid_match.txt", 
                        quote="",sep=";", 
                        header=T, 
                        stringsAsFactors=F) %>% 
  select(Country=COUNTRY,ID,GRIDCODE,AREA=SHAPE_AREA)

dfgrid<-read.table(file="data/grid_all.txt", 
                   quote="",sep=";", 
                   header=T, 
                   stringsAsFactors=F) %>% 
  select(GRIDCODE,Region=REGION)


dfdat<-read.table(file="data/data_HELCOM_OSPAR.txt", 
                  quote="",
                  sep="\t", 
                  header=T,
                  comment.char = "",
                  stringsAsFactors=F) %>% 
  filter(!is.na(Obs),!is.na(Threshold),Exclude==0) %>%
  select(Country,ID,Category,OC,Parameter,Obs,Threshold,Response) %>%
  mutate(weight=ifelse(OC=="C",coastweight,1)) %>%
  select(-OC) #2585

df <- dfgridmatch %>% left_join(dfdat,by=c("Country","ID")) %>% 
  mutate(ER=ifelse(Response=="+",Obs/Threshold,Threshold/Obs)) %>%
  filter(!is.na(ER))

dfcat <- df %>% group_by(GRIDCODE,Category) %>%
  mutate(AREA=1) %>% # effectively removes area weighting
  summarise(sumER=sum(weight*ER*AREA,na.rm=T),sumAREA=sum(weight*AREA,na.rm=T)) %>%
  mutate(ER=sumER/sumAREA) %>%
  mutate(Class=EutClass(ER)) %>%
  ungroup()
