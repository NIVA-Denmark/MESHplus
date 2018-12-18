# 


library(tidyverse)

# 3 Quality Elements (QE)
# Biology
# Chemistry
# Supporting

# we apply one-out all-out between these 3 QEs
# all other aggregation is done by averaging

# Biodiversity (NEAT) already uses a 0.0-1.0 scale
# conversion for HEAT+ Eutrophication Ratio (ER) and CHASE+ Contamination Ratio (CR) is done using the following fixed points:

  # EQR CHASE HEAT
  #     (CS)  (ER)
  # 0.0 50.0  2.5
  # 0.2 10.0  2.0
  # 0.4  5.0  1.5
  # 0.6  1.0  1.0
  # 0.8  0.5  0.5
  # 1.0  0.0  0.0

# Biology indicators
dfBio<-read.table(file="data/JNCC/HELCOM_indicators.txt", 
                        quote="",sep="\t",header=T,stringsAsFactors=F) %>%
  select(GRIDCODE,Category,Group,Unit,Indicator,IndType,Bad,Threshold,Reference,Status)

# Chemistry Indicators


# Supporting Indicators


# indicator group information
dfGp<-read.table(file="data/indicator_groups_bio_supporting.txt", 
               quote="",sep="\t", header=T, stringsAsFactors=F) 


