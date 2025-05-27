#'---------------------------------------
#'@date 2025-05-27
#'@program EDSD
#'@course Sequence Analysis
#'@professor Tommy, Bernhard and Carole
#'@description Lab session 1
#'---------------------------------------

# Settings
rm(list = ls()) # removing all itens of environment
invisible(gc()) # clealing the storage memory of the computer

# libraries ---------------------------------------------------------------

library(pacman)
p_load(tidyverse, TraMineR, RColorBrewer)
# -----------------------------
# PART 1: Refreshing R Syntax
# -----------------------------

# Basic arithmetic operations
2 + 2
2 - 4
2 * 3
8 / 3

# Define a simple square root function
my_sqrt <- function(x) x ^ 0.5
my_sqrt(9)

# Variable assignment
fred <- 2 + 2
fred

a <- fred  # Assign value of fred to a
a

A <- sqrt(a) * 5  # Compute sqrt(a) * 5
A <- A * 2        # Reassign A as A * 2
A

# Create vectors
x2 <- 1:13  # Sequence from 1 to 13
x2

myvec <- c(8, 13, 2, 1, 6)  # Concatenated vector
myvec

# -----------------------------
# READING AND WRITING DATA
# -----------------------------

# Check current working directory
getwd()
list.files()  # List files in directory

# Load data from a remote CSV file
data_DHS <- read_csv("http://nicolabarban.com/NCRM_sequence_analysis/DataLab1.csv")

# Explore the data
is.data.frame(data_DHS)
summary(data_DHS)

# -----------------------------
# SUBSETTING AND INDEXING
# -----------------------------

id <- data_DHS$filenw     # Access a variable
id[3]                     # Third element

# Access data frame elements
data_DHS[3, 1]            # 3rd row, 1st column
data_DHS[3, ]             # All columns of 3rd row

# -----------------------------
# SEQUENCE ANALYSIS SETUP
# -----------------------------
# We are gonna work with 'TraMineR' package in R

# Load and explore mvad dataset
data(mvad)
help(mvad)
names(mvad)

head(mvad, 3)  # Preview first rows

t1 <- table(mvad$gcse5eq, mvad$catholic)
t1
prop.table(t1, 1)
prop.table(t1, 2)

# Load and explore biofam dataset
data(biofam)
help(biofam)
names(biofam)
biofam <- biofam |> mutate(age = 2002 - birthyr) # calculating age

# Minimum and maximum age overall
biofam |>
  summarise(min_age = min(age), max_age = max(age))

# Minimum and maximum age for women only
biofam |>
  filter(sex == "woman") |>
  summarise(min_age_women = min(age), max_age_women = max(age))

t2 <- table(biofam$p02r04, biofam$sex)
prop.table(t2, 2)

# Or in tidyverse
# Create a cross-tabulation and compute column-wise proportions
biofam |>
  count(p02r04, sex) |>
  group_by(sex) |>
  mutate(prop = n / sum(n)) |>
  select(-n) |>
  pivot_wider(names_from = sex, values_from = prop, values_fill = 0)


# Define state labels and codes
mvad.labels <- c("employment", "further education", "higher education", "joblessness", "school", "training")
mvad.scode <- c("EM", "FE", "HE", "JL", "SC", "TR")

# Create sequence object
mvad.seq <- seqdef(
  mvad,
  17:86,
  states = mvad.scode,
  labels = mvad.labels
)
# extracting alphabet from the sequence object
alphabet(mvad.seq)
# stlab are the labels
stlab(mvad.seq)

# View and change sequence representation
mvad.seq[1:3, ]
print(mvad.seq[1:3, ], format = "SPS") # compact way to represent the sequences
seqdss(mvad.seq[1:3, ]) # same transitions without the duration

# Sequence plots
seqdplot(mvad.seq) # distribution plot
seqfplot(mvad.seq) # grouping the distributions and showing the first 10 common sequences

# Arrange plots with legend
par(mfrow = c(2, 2))
seqiplot(mvad.seq, with.legend = FALSE, border = NA, space = 0, main = "index plot (first ten sequences)")
seqfplot(mvad.seq, with.legend = FALSE, border = NA, space = 0, pbarw = TRUE, main = "Sequence frequency plot")
seqdplot(mvad.seq, with.legend = FALSE, border = NA, space = 0, main = "State distribution plot")
seqlegend(mvad.seq, cex = 0.75)
par(mfrow = c(1, 1))

# to have completed index plot
seqIplot(mvad.seq, with.legend = FALSE, border = NA, space = 0, main = "index plot (all the sequences)")

# Colors and palettes
display.brewer.all() # show colors
cpal(mvad.seq) <- brewer.pal(6, "Greys") # changing the palettes
seqdplot(mvad.seq)
cpal(mvad.seq) <- brewer.pal(6, "Accent") # standard one

# Grouped plots
levels(mvad$male) <- c("Women", "Men") # renaming levels
seqdplot(mvad.seq, group = mvad$male, border = NA, space = 0)
seqmtplot(mvad.seq, group = mvad$male) # average time spent in each trajectories
seqmsplot(mvad.seq, group = mvad$male) # modal state in each time
seqIplot(mvad.seq, group = mvad$male)

# Statistics
seqtab(mvad.seq)
seqstatd(mvad.seq[, 1:8])
seqtransn(mvad.seq[1:10, ]) # number of transitions
mvad.trate <- seqtrate(mvad.seq) # transition rates
mvad.trate
seqtrate(mvad.seq[,1:2]) # transition rates from first period to second
round(mvad.trate, 2)

# Frequent subsequences
mvad.seqe <- seqecreate(mvad.seq)
fsubseq <- seqefsub(mvad.seqe, pmin.support = 0.05)
plot(fsubseq[1:15], col = "green")

# Optimal Matching with transition rates
submat <- seqsubm(mvad.seq, method = "TRATE") # creating inverse transition rates
dist.om1 <- seqdist(
  mvad.seq,
  method = "OM",
  indel = 2, # it is the weigth we attribute to insertion/deletion operations. It has to be align with research question
  sm = submat # sequences matrix - it has to be align with RQ. Is worth to have penalities based on rare changes?
)

dist.om1[1:10, 1:10] |> View()
