# MESH+
# Ecosystem Health Status Assessment Tool
library(tidyverse)

# 3 Quality Elements (QE)
# Biology
# Chemistry
# Supporting

# we apply one-out all-out between these 3 QEs all other aggregation is done by
# averaging

# Biodiversity (NEAT) already uses a 0.0-1.0 scale conversion for HEAT+
# Eutrophication Ratio (ER) and CHASE+ Contamination Ratio (CR) is done using
# the following fixed points:
# CS <- 0 ,0.5, 1.0, 5.0, 10.0, 50.0
# ER <- 0, 0.5, 1.0, 1.5, 2.0, 2.5 

# Read input data  ---------------------------------------------------------------

# Biology indicators -------------------------------------------------------------------------------
# results from BEAT+ by group 
#MxId	SpatialAssessmentUnit	EEA_Region	EEA_SubRegion	Benthic	Birds	Fish	Mammals	Pelagic	WorstBQR	WorstCase	AreaKm2
dfBio <- read.table(
  file = "data/biodiv_groups.txt",
  quote = "",
  sep = "\t",
  header = T,
  stringsAsFactors = F
) %>%
  select(GridID=SpatialAssessmentUnit,
         Benthic,
         Birds,
         Fish,
         Mammals,
         Pelagic) %>%
  gather(key="Indicator",value="EQR",Benthic,Birds,Fish,Mammals,Pelagic) %>%
  filter(!is.na(EQR))

#dfBio <- dfBio %>% filter(Indicator!="Pelagic")
 


# Chemistry Indicators -------------------------------------------------------------------------------
dfChem <- read.table(
  file = "data/CHASE/CHASE_input.csv",
  quote = "",
  sep = ",",
  header = T,
  stringsAsFactors = F
) %>%
  select(GridID, Group = Category, Indicator = Substance, CR)

# Supporting Indicators -------------------------------------------------------------------------------
dfSupp <- read.table(
  file = "data/indicators_eutro.txt",
  quote = "",
  sep = "\t",
  header = T,
  stringsAsFactors = F
) %>%
  select(GridID = GRIDCODE,
         Indicator = Parameter,
         Response,
         Obs,
         Threshold)

df_GR <- read.table(
  file = "C:/Data/GitHub/MESHplus/data/HEAT/indicators_GR.txt",
  quote = "",sep = "\t",header = T,stringsAsFactors = F
)  %>%
  select(GridID=GRIDCODE,Indicator,Response,Obs=Status,Threshold=Target)


#C:\Data\GitHub\ETC_ICM\Eutrophication\ETC_ICM_Eutro_2019mar\EIONET.R
df_EIONET <- read.table(
  file = "C:/Data/GitHub/MESHplus/data/HEAT/indicators_eutro_EIONET.txt",
  quote = "",sep = "\t",header = T,stringsAsFactors = F
) %>%
  select(GridID=GRIDCODE,Indicator=PARAM,Response=Resp,Obs=VALUE,Threshold=GoodMod)

df_TR <- read.table(
  file = "./data/HEAT/indicators_eutro_TR.txt",
  quote = "",sep = "\t",header = T,stringsAsFactors = F
) %>%
  select(GridID=GRIDCODE,Indicator=PARAM,Response=Resp,Obs=VALUE,Threshold=GoodMod)

dfSupp <- dfSupp %>% 
  bind_rows(df_GR,df_TR,df_EIONET)

dfSupp_Ind <- dfSupp %>% distinct(Indicator)


# ------------------------------------------------------------------------------------------------
# indicator group information for bio & supporting
# CHASE data contains information on groups for contaminants (Biota, Sediment,
# etc.)
dfGroup <- read.table(
  file = "data/indicator_groups_bio_supporting.txt",
  quote = "",
  sep = "\t",
  header = T,
  stringsAsFactors = F
)

# Load EEA Grid cell Region information and select Baltic Sea grid cells
dfGrid <- read.table(
  file = "data/HEAT/grid_all.txt",
  quote = "",
  sep = ";",
  header = T,
  stringsAsFactors = F
) %>%
  #filter(REGION == "Baltic Sea") %>%
  select(GridID = GRIDCODE)

# Conversion from CHASE+ contamination Sum (CS) and HEAT+ Eutrophication Ratio
# (ER) to EQR uses corresponding values from the 3 scales:
listEQR <- c(0, 0.2, 0.4, 0.6, 0.8, 1.0)
listCS <- c(50, 10, 5, 1, 0.5, 0)
listER <- c(2.5, 2, 1.5, 1, 0.5, 0)


# Calculate Biology -------------------------------------------------------



# Average per Grid cell
dfBioQE <- dfBio %>%
  group_by(GridID) %>%
  summarise(EQR = mean(EQR, na.rm = T)) %>%
  mutate(QE = "Biology")

# Calculate Chemistry -----------------------------------------------------

# Aggregate within Sediment, Biota, etc. using CHASE+ method
dfChemGroup <- dfChem %>%
  mutate(QE = "Chemistry") %>%
  group_by(GridID, QE, Group) %>%
  summarise(CS = sum(CR, na.rm = T) / sqrt(n()),
            CRavg = mean(CR, na.rm = T))

# for Bio. effects, use average of Contamination Ratio (CR)
dfChemGroup <- dfChemGroup %>%
  mutate(CS = ifelse(Group == "Bio.Effects", CRavg, CS)) %>%
  select(-CRavg)

#Convert CS to EQR
dfChemGroup <- dfChemGroup %>%
  mutate(CS = ifelse(CS > max(listCS), max(listCS), CS)) %>%
  mutate(CS = ifelse(CS < min(listCS), min(listCS), CS)) %>%
  rowwise() %>%
  mutate(EQR = approx(x = listCS, y = listEQR, xout = CS)[[2]]) 
  
# Average EQR per Grid cell
dfChemQE <- dfChemGroup %>%
  group_by(GridID, QE) %>%
  summarise(EQR=mean(EQR,na.rm=T))

      # alternatively do average then convert CS to EQR:
      # Average per Grid cell
      #dfChemQE <- dfChemGroup %>%
      #  group_by(GridID, QE) %>%
      #  summarise(CSavg = mean(CS, na.rm = T))
      
      # Convert Contamination Score to EQR
      #dfChemQE <- dfChemQE %>%
      #  mutate(CSavg = ifelse(CSavg > max(listCS), max(listCS), CSavg)) %>%
      #  mutate(CSavg = ifelse(CSavg < min(listCS), min(listCS), CSavg)) %>%
      #  rowwise() %>%
      #  mutate(EQR = approx(x = listCS, y = listEQR, xout = CSavg)[[2]])  %>%
      #  select(-CSavg)






# Calculate Supporting ----------------------------------------------------

# Calculate Eutrophication Ratio (ER)
dfSupp <- dfSupp %>%
  mutate(ER = ifelse(Response == "+", Obs / Threshold, Threshold / Obs))

# Add possible filtering of supporting indicators?

# Convert ER to EQR
dfSuppEQR <- dfSupp %>%
  mutate(ER = ifelse(ER > max(listER), max(listER), ER)) %>%
  mutate(ER = ifelse(ER < min(listER), min(listER), ER)) %>%
  rowwise() %>%
  mutate(EQR = approx(x = listER, y = listEQR, xout = ER)[[2]])

# Average per Grid cell
dfSuppQE <- dfSuppEQR %>%
  group_by(GridID) %>%
  summarise(EQR = mean(EQR, na.rm = T)) %>%
  mutate(QE="Supporting")


    # alternatively do average then convert ER to EQR:
    # Convert ER to EQR
    # Average per Grid cell
    #dfSuppQE <- dfSuppGroup %>%
    #  group_by(GridID, QE) %>%
    #  summarise(ER = mean(ER, na.rm = T))

    #dfSuppQE <- dfSuppQE %>%
    #  mutate(ER = ifelse(ER > max(listER), max(listER), ER)) %>%
    #  mutate(ER = ifelse(ER < min(listER), min(listER), ER)) %>%
    #  rowwise() %>%
    #  mutate(EQR = approx(x = listER, y = listEQR, xout = ER)[[2]])  %>%
    #  select(-ER)




# Combine Biology, Chemistry & Supporting ---------------------------------
df <- bind_rows(dfBioQE, dfChemQE, dfSuppQE)
df <- dfGrid %>% left_join(df, by = "GridID")

# Calculate the worst (minimum) EQR for each Grid cell
df_worst_EQR <- df %>%
  group_by(GridID) %>%
  summarise(EQR = min(EQR, na.rm = T)) %>%
  mutate(EQR=ifelse(is.infinite(EQR),NA,EQR))

# find the quality element responsible
df_worst_QE <- df_worst_EQR %>%
  left_join(df, by = c("GridID", "EQR"))

# if two QEs have the same worst EQR value in the same Grid cell, take only the first one
df_worst_QE <- df_worst_QE %>%
  group_by(GridID) %>%
  arrange(GridID, EQR) %>%
  slice(1) %>%
  ungroup() %>%
  rename(Worst = QE)

# arrange QE EQR values in columns (wide)
df_QE <- df %>%
  filter(!is.na(EQR)) %>%
  spread(key = QE, value = EQR) %>%
  mutate(
    Cat_Bio = ifelse(Biology == 1, 1, 5 - floor(5 * Biology)),
    Cat_Chem = ifelse(Chemistry == 1, 1, 5 - floor(5 * Chemistry)),
    Cat_Supp = ifelse(Supporting == 1, 1, 5 - floor(5 * Supporting))
  )

# include columns for the final overall (worst) EQR and show the worst QE
df_MESH <- df_QE %>%
  left_join(df_worst_QE, by = "GridID") %>%
  mutate(Cat_MESH = ifelse(EQR == 1, 1, 5 - floor(5 * EQR)))

df_MESH <- dfGrid %>%
  left_join(df_MESH,by="GridID")

#
