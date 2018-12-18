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
  file = "data/JNCC/HELCOM_indicators.txt",
  quote = "",
  sep = "\t",
  header = T,
  stringsAsFactors = F
) %>%
  select(GRIDCODE,
         Group,
         Unit,
         Indicator,
         IndType,
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
)

# Supporting Indicators
dfSupp <- read.table(
  file = "data/indicators_eutro.txt",
  quote = "",
  sep = "\t",
  header = T,
  stringsAsFactors = F
)

# indicator group information for bio & supporting
# CHASE data contains information on groups for contaminants (Biota, Sediment,
# etc.)
dfGp <- read.table(
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
  filter(REGION=="Baltic Sea") %>%
  select(GRIDCODE)

# Conversion from CHASE+ contamination Sum (CS) and HEAT+ Eutrophication Ratio
# (ER) to EQR.
EQR <- c(0, 0.2, 0.4, 0.6, 0.8, 1.0)
CS <- c(50, 10, 5, 1, 0.5, 0)
ER <- c(2.5, 2, 1.5, 1, 0.5, 0)
dfConvert <- data.frame(CS = CS, ER = ER, EQR = EQR)


# Calculate Biology -------------------------------------------------------
# load function for calculating EQR from boundary values
source("assessment.R")


# Calculate Chemistry -----------------------------------------------------


# Calculate Supporting ----------------------------------------------------

