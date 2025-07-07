
# Settings
rm(list = ls()) # removing all itens of environment
invisible(gc()) # clealing the storage memory of the computer

# Packages ----------------------------------------------------------------

library(pacman)
p_load(tidyverse, TraMineR, RColorBrewer, WeightedCluster, cluster, haven, Hmisc)

# Importing data ----------------------------------------------------------

df <- read_dta(
  file.path("data","PartnerBirthbio.dta")
)

# reconding variable

df <- df |>
  mutate_all(
    ~(case_when(
      . < 3 ~ 1,            # Single
      . %in% c(3,4) ~ 2,    # LAT
      . %in% c(5,6) ~ 3,    # Cohabiting
      . > 6 ~ 4,           # Married
      TRUE ~ NA_integer_)
  )) |>
  na.omit()

# Creating data frame for sequences ---------------------------------------

sa.states <- c("S", "LAT", "COH", "MAR")
sa.labels <-  c("Single", "LAT", "Cohabiting", "Married")

# create state sequence object
df.seq <- seqdef(
  df,
  labels = sa.labels,
  states = sa.states,
  weights = df$weight40
)


# Describind the distribution of life states ------------------------------

# 1 - Time spent in different states

seqmeant(seqdss(df.seq),serr = TRUE)

# 2 - Number of transitions

wtd.mean(seqtransn(df.seq), df$weight40)

# 3 - Transition matrix between states

seqtrate(df.seq, weighted = TRUE)

# 4 - State distribution at different ages

seqstatd(df.seq)$Frequencies[,c(1, seq(24, 264, by = 48))] # ages = 18, 20, 24, 28, 32, 36, 40

# 5 - 10 most frequent sequences

seqtab(df.seq)

# 6 - Plot of sequences

seqIplot(df.seq)

# Computation of life course dissimilarity --------------------------------

# we will use a not arbitrary indel value based on the transition matrix
trcost <- seqcost(df.seq, method="TRATE")

# calculating distance matrix

dist.om = seqdist(
  df.seq,
  method="OM",
  indel=trcost$indel,
  sm=trcost$sm
)

# looking at the first values
dist.om[1:5,1:5] # reference


# Clustering  --------------------------------------------------

#...


# Describing clusters -----------------------------------------------------

#...
