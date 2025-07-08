
# Settings
rm(list = ls()) # removing all itens of environment
invisible(gc()) # clealing the storage memory of the computer

# Packages ----------------------------------------------------------------

library(pacman)
p_load(tidyverse, TraMineR, RColorBrewer, WeightedCluster, cluster, haven, Hmisc, ggdendro, patchwork)

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

dist.om <- seqdist(
  df.seq,
  method="OM",
  indel=trcost$indel,
  sm=trcost$sm
)

# looking at the first values
dist.om[1:5,1:5] # reference


# Clustering  --------------------------------------------------

# dendrogram analysis

wardClust <- hclust(as.dist(dist.om), method = "ward.D2")

# aglomerative nesting. it is a way to create a cluster based on different methods

clusterward <-  agnes(
  dist.om, # dissimilarity matrix
  diss = T, # start with dissimilarity matrix
  method = "ward" # method to calculate it
)

# statistical indicators

avgClustQual <- as.clustrange(
  clusterward,
  diss=dist.om,
  weights = df$weight40,
  ncluster=20
)

## plots

# dendro

dendro_plot <- ggdendrogram(
  data = wardClust, rotate = TRUE
) +
  theme(
    axis.text.y = element_blank()
  )

# statistics

indicators_plot <- avgClustQual$stats |>
  as_tibble() |>
  mutate(
    cluster = as.factor(str_remove(rownames(avgClustQual$stats),"cluster")),
    cluster = fct_reorder(cluster,2:20)
  ) |>
  select(any_of(c("R2sq","ASW","HC","HG","PBC","cluster"))) |>
  pivot_longer(
    1:5,
    values_to = "ind_value",
    names_to = "ind_name"
  ) |>
  ggplot() +
  aes(x = cluster, y = ind_value, group = ind_name, color = ind_name) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3) +
  scale_color_brewer(palette = "Set3") +
  labs(
    y = "Indicator"
  ) +
  theme_light() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

# first derivative

indicators2_plot <- avgClustQual$stats |>
  as_tibble() |>
  mutate(
    cluster = as.factor(str_remove(rownames(avgClustQual$stats),"cluster")),
    cluster = fct_reorder(cluster,2:20)
  ) |>
  select(any_of(c("R2sq","ASW","HC","HG","PBC","cluster"))) |>
  pivot_longer(
    1:5,
    values_to = "ind_value",
    names_to = "ind_name"
  ) |>
  arrange(ind_name, cluster) |>
  mutate(
    ind_value_lag = lead(ind_value),
    diff_value = ind_value_lag - ind_value,
    diff_value = lag(diff_value),
    .by = c(ind_name)
  ) |>
  filter(!is.na(diff_value)) |>
  ggplot() +
  aes(x = cluster, y = diff_value, group = ind_name, color = ind_name) +
  geom_line(linewidth = 1.1) +
  geom_point(size = 3) +
  scale_color_brewer(palette = "Set3") +
  labs(
    y = "First derivative of Indicators"
  ) +
  theme_light() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

plot_dend_stat <- dendro_plot + indicators_plot + indicators2_plot


# Choosing number of clusters ---------------------------------------------

clusters5 = cutree(clusterward, k = 5) # choosing the number of clusters

df$clusters <- clusters5

# Describing clusters -----------------------------------------------------

# typical trajectories
dc = disscenter(dist.om, group= clusters5, medoids.index="first")
medoid_seq <- print(df.seq[dc,], format = "SPS")
medoid_seq[[1]]

# levels for clusters

# giving a label to these clusters

levels(clusters5)=c(
  medoid_seq[[1]],
  medoid_seq[[2]],
  medoid_seq[[3]],
  medoid_seq[[4]],
  medoid_seq[[5]]
)


# distribution of each cluster over observational window
seqdplot(
  df.seq,
  group=clusters5,
  border=NA,
  space=0,
  main = levels(clusters5)
)

# all the sequences for each cluster
seqIplot(
  df.seq,
  group=clusters5,
  border=NA,
  space=0,
  main = levels(clusters5)
)

# duration of each cluster

seqmtplot(
  df.seq,
  group = clusters5,
  border=NA,
  space=0,
  main = levels(clusters5)
)


# Storing all the files ---------------------------------------------------

save(
  dist.om,
  clusterward,
  file = file.path("courses","sequence_analysis","assignment","outputs_sequence.RData")
)
