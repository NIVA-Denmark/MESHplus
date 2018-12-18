library(tidyverse)

dfHELCOM <- read.table(
  file = "data/JNCC/HELCOM_indicators.txt",
  quote = "",
  sep = "\t",
  header = T,
  stringsAsFactors = F
) 

dfHELCOM2 <- read.table(
  file = "data/JNCC/HELCOM_indicators_by_SAU.txt",
  quote = "",
  sep = "\t",
  header = T,
  stringsAsFactors = F
) %>%
  select(SAUID,SAU,Indicator,Bad2=Bad,Threshold2=Threshold,Reference2=Reference)

dfHELCOM <- dfHELCOM %>%
  left_join(dfHELCOM2,by=c("SAUID","Indicator","SAU_Name"="SAU")) %>%
  mutate(Bad=ifelse(is.na(Bad),Bad2,Bad),
         Threshold=ifelse(is.na(Threshold),Threshold2,Threshold),
         Reference=ifelse(is.na(Reference),Reference2,Reference)) %>%
  select(-c(Bad2,Threshold2,Reference2))

# dfHELCOM %>% filter(is.na(Bad))
# dfHELCOM %>% filter(is.na(Threshold))
# dfHELCOM %>% filter(is.na(Reference))

write.table(dfHELCOM,file="data/indicators_biodiv.txt",sep="\t",fileEncoding="UTF-8",row.names=F,col.names=T,quote=F)

