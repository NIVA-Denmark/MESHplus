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

# Read input data  ---------------------------------------------------------------

# Biology indicators
dfBio <- read.table(
  file = "data/indicators_biodiv.txt",
  quote = "",
  sep = "\t",
  header = T,
  stringsAsFactors = F
) %>%
  select(GridID = GRIDCODE,
         Indicator,
         Unit,
         Bad,
         Threshold,
         Reference,
         Status)

# Chemistry Indicators
dfChem <- read.table(
  file = "data/CHASE/CHASE_input.csv",
  quote = "",
  sep = ",",
  header = T,
  stringsAsFactors = F
) %>%
  select(GridID, Group = Category, Indicator = GROUP, CR)

# Supporting Indicators
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
  filter(REGION == "Baltic Sea") %>%
  select(GridID = GRIDCODE)

# Conversion from CHASE+ contamination Sum (CS) and HEAT+ Eutrophication Ratio
# (ER) to EQR uses corresponding values from the 3 scales:
listEQR <- c(0, 0.2, 0.4, 0.6, 0.8, 1.0)
listCS <- c(50, 10, 5, 1, 0.5, 0)
listER <- c(2.5, 2, 1.5, 1, 0.5, 0)


# Calculate Biology -------------------------------------------------------

# Calculate EQR
dfBio <- dfBio %>%
  rowwise() %>%
  mutate(EQR = approx(
    x = c(Bad, Threshold, Reference),
    y = c(0, 0.6, 1),
    xout = Status
  )[[2]])

# Add grouping information
dfBio <- dfBio %>%
  left_join(dfGroup, by = "Indicator") %>%
  filter(QE == "Biology", Exclude != 1)

# Average by SubGroup
dfBioSubgroup <- dfBio %>%
  group_by(GridID, QE, Group, SubGroup) %>%
  summarise(EQR = mean(EQR, na.rm = T))

# Average by Group
dfBioGroup <- dfBioSubgroup %>%
  group_by(GridID, QE, Group) %>%
  summarise(EQR = mean(EQR, na.rm = T))

# Average per Grid cell
dfBioQE <- dfBioGroup %>%
  group_by(GridID, QE) %>%
  summarise(EQR = mean(EQR, na.rm = T))

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

# Average per Grid cell
dfChemQE <- dfChemGroup %>%
  group_by(GridID, QE) %>%
  summarise(CSavg = mean(CS, na.rm = T))

# Convert Contamination Score to EQR
dfChemQE <- dfChemQE %>%
  mutate(CSavg = ifelse(CSavg > max(listCS), max(listCS), CSavg)) %>%
  mutate(CSavg = ifelse(CSavg < min(listCS), min(listCS), CSavg)) %>%
  rowwise() %>%
  mutate(EQR = approx(x = listCS, y = listEQR, xout = CSavg)[[2]])  %>%
  select(-CSavg)


# Calculate Supporting ----------------------------------------------------

# Calculate Eutrophication Ratio (ER)
dfSupp <- dfSupp %>%
  mutate(ER = ifelse(Response == "+", Obs / Threshold, Threshold / Obs))

# Add grouping information
dfSupp <- dfSupp %>%
  left_join(dfGroup, by = "Indicator") %>%
  filter(QE == "Supporting", Exclude != 1)

# Average by Group
dfSuppGroup <- dfSupp %>%
  group_by(GridID, QE, Group) %>%
  summarise(ER = mean(ER, na.rm = T))

# Average per Grid cell
dfSuppQE <- dfSuppGroup %>%
  group_by(GridID, QE) %>%
  summarise(ER = mean(ER, na.rm = T))

# Convert ER to EQR
dfSuppQE <- dfSuppQE %>%
  mutate(ER = ifelse(ER > max(listER), max(listER), ER)) %>%
  mutate(ER = ifelse(ER < min(listER), min(listER), ER)) %>%
  rowwise() %>%
  mutate(EQR = approx(x = listER, y = listEQR, xout = ER)[[2]])  %>%
  select(-ER)


# Combine Biology, Chemistry & Supporting ---------------------------------
df <- bind_rows(dfBioQE, dfChemQE, dfSuppQE)
df <- dfGrid %>% left_join(df, by = "GridID")

# Calculate the worst (minimum) EQR for each Grid cell
df_worst_EQR <- df %>%
  group_by(GridID) %>%
  summarise(EQR = min(EQR, na.rm = T))

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

# Save Results ------------------------------------------------------------

# write the results to text file for plotting in ArcGIS
write.table(
  df_MESH,
  file = "results/20181218/MESH_Baltic.csv",
  sep = ",",
  row.names = F,
  col.names = T,
  quote = F,
  na = ""
)

save(dfBio,dfBioGroup,file="results.Rda")
