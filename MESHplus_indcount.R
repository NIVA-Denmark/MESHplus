library(tidyverse)

beat1<-read.table(
  file = "data/BEAT_data/BEAT_GRID_data_all_text_for_count/BEAT_DEVOTES_GRID_ALL.txt",
  quote = "", sep = "\t", header = T, stringsAsFactors = F)

beat2<-read.table(
  file = "data/BEAT_data/BEAT_GRID_data_all_text_for_count/BEAT_FAO_GSA_GRID.txt",
  quote = "", sep = "\t", header = T, stringsAsFactors = F)

beat3<-read.table(
  file = "data/BEAT_data/BEAT_GRID_data_all_text_for_count/BEAT_HELCOM_GRID_ALL.txt",
  quote = "", sep = "\t", header = T, stringsAsFactors = F)

beat4<-read.table(
  file = "data/BEAT_data/BEAT_GRID_data_all_text_for_count/BEAT_ICES_FISH_GRID_ALL.txt",
  quote = "", sep = "\t", header = T, stringsAsFactors = F)

beat5<-read.table(
  file = "data/BEAT_data/BEAT_GRID_data_all_text_for_count/BEAT_WFD_MED_GRID.txt",
  quote = "", sep = "\t", header = T, stringsAsFactors = F)


beat1 <- beat1 %>%
  group_by(GRIDCODE) %>%
  summarise(n=n())

beat2 <- beat2 %>%
  group_by(GRIDCODE) %>%
  summarise(n=n())

beat3 <- beat3 %>%
  group_by(GRIDCODE) %>%
  summarise(n=n())

beat4 <- beat4 %>%
  group_by(GRIDCODE) %>%
  summarise(n=n())

beat5 <- beat5 %>%
  group_by(GRIDCODE) %>%
  summarise(n=n())

beat <- bind_rows(beat1,beat2,beat3,beat4,beat5)

beat <- beat %>%
  rename(GridID=GRIDCODE) %>%
  group_by(GridID) %>%
  summarise(n=max(n,na.rm=T)) %>%
  mutate(QE = "Biology")


chase<-dfChem %>%
  group_by(GridID) %>%
  summarise(n=n()) %>%
  mutate(QE = "Chemistry")

heat<-dfSupp%>%
  group_by(GridID) %>%
  summarise(n=n()) %>%
  mutate(QE = "Supporting")

meshcount<-bind_rows(beat,chase,heat)

meshcountall<-meshcount %>%
  group_by(GridID) %>%
  summarise(n=sum(n,na.rm=T)) 


meshcount<-meshcount %>% 
  spread(key=QE,value=n)

meshcount<-dfGrid %>% 
  left_join(meshcount,by="GridID") %>%
  left_join(meshcountall,by="GridID")
