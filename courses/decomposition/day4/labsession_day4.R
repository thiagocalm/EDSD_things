# Setup -------------------------------------------------------------------

library(here)
wd <- here()
setwd(wd)

load(file.path("courses","decomposition","day4","Exercise 4","AburtoBeltranSanchez.RData"))

library(tidyverse)
library(DemoDecomp)

source(file.path("courses","decomposition","day4","Exercise 3","Functions_D4.R"))

# Extract the needed information ------------------------------------------

# first we need the vectors of mortality rates
mx1 <- data %>%
  filter(year==2005, age>15) %>%
  pull(mx)
mx2 <- data %>%
  filter(year==2015, age>15) %>%
  pull(mx)

horiuchi

# Linear integral method --------------------------------------------------

# Let's have a look at the functions
edagger.frommx

# there are also other functions
sd.frommx
rG.frommx
edagger.frommx(mx = mx1, age = 0:100)
#Now we can perfom the decomposition
results <- horiuchi(func = edagger.frommx, pars1 = mx1, pars2 = mx2, N = 50, age=16:109)
# NB: if you use another function, remember to define the additional arguments

# some tests
sum(horiuchi(func = edagger.frommx, pars1 = mx1, pars2 = mx2, N = 50, age=16:109))
sum(horiuchi(func = edagger.frommx, pars1 = mx1, pars2 = mx2, N = 25, age=16:109))
sum(horiuchi(func = edagger.frommx, pars1 = mx1, pars2 = mx2, N = 10, age=16:109))
sum(horiuchi(func = edagger.frommx, pars1 = mx1, pars2 = mx2, N = 5, age=16:109))
sum(horiuchi(func = edagger.frommx, pars1 = mx1, pars2 = mx2, N = 1, age=16:109))

# Have a look at the results
results

# Check the results
#original
(original <- edagger.frommx(mx2, age=16:109) - edagger.frommx(mx1, age=16:109))
#with decomp
(with_decomp <- sum(results))

#error
with_decomp - original

#now graph results
age <- data %>%
  filter(year==2005, age>15) %>%
  pull(age)

ggplot()+
  ggtitle(bquote(~'Change in '~ e[15]^"\u2020" ~'2005-2015' )) +
  geom_bar(aes(x = age, y= results), stat = "identity", position = "stack", width=1) +
  geom_vline(xintercept = data$ex[1], color = "red3",linetype = "dashed")


# Stepwise replacement method ---------------------------------------------

results_step <- stepwise_replacement(edagger.frommx,pars1 = mx1, pars2 = mx2, age=16:109)

# Check the results
#original
(original <- edagger.frommx(mx2, age=16:109) - edagger.frommx(mx1, age=16:109))
#with decomp
(with_decomp_step <- sum(results_step))
#error
with_decomp_step - original

#now graph results

ggplot()+
  ggtitle(bquote(~'Change in '~ e[15]^"\u2020" ~'2005-2015' ))+
  geom_bar(aes(x = age, y= results_step), stat = "identity", position = "stack")

# Let's compare with linear integral method
results - results_step

# Decomposing using Horiuchi by cause of death ----------------------------

source(file.path("courses","decomposition","day4","Exercise 4","Functions_D4.R"))

#we need age- and cause-specific mortality rates in each period
COD1 <- data %>%
  filter(year==2005, age>15) %>%
  select(Cause_1:Cause_9) %>%
  as.matrix()

COD2 <- data %>%
  filter(year==2015, age>15) %>%
  select(Cause_1:Cause_9) %>%
  as.matrix()

#### Applying decomposition

# Let's have a look at the lifespan variation function
edagger.frommxc

# Let's decompose the lifespan variation change
results <- horiuchi(func = edagger.frommxc, pars1 = c(COD1), pars2 = c(COD2), N = 50, age=16:109)

#Go back to a matrix
dim(results) <- dim(COD1)

# Check the results
#original
(original <- edagger.frommxc(COD2, age=16:109) - edagger.frommxc(COD1, age=16:109))
#with decomp
(with_decomp <- sum(results))
#error
with_decomp - original

# Rearrange the results
results <- data.frame(results)
colnames(results) <- cause_names

results <- results %>%
  mutate(age = 16:109) %>%
  pivot_longer(cols=1:9, names_to="cause", values_to="contribution")

# And plot them

ggplot(results, aes(x=age, y=contribution, fill=cause)) +
  ggtitle(bquote(~'Change in '~ e[15]^"\u2020" ~'2005-2015' )) +
  geom_bar(stat = "identity", position = "stack") +
  geom_vline(xintercept = data$ex[1], color = "red3",linetype = "dashed")

results %>%
  mutate(age_group = case_when(age %in% 16:19 ~ "16-19",
                               age %in% 20:24 ~ "20-24",
                               age %in% 25:29 ~ "25-29",
                               age %in% 30:34 ~ "30-34",
                               age %in% 35:39 ~ "35-39",
                               age %in% 40:44 ~ "40-44",
                               age %in% 45:49 ~ "45-49",
                               age %in% 50:54 ~ "50-54",
                               age %in% 55:59 ~ "55-59",
                               age %in% 60:64 ~ "60-64",
                               age %in% 65:69 ~ "65-69",
                               age %in% 70:74 ~ "70-74",
                               age %in% 75:79 ~ "75-79",
                               age %in% 80:84 ~ "80-84",
                               age %in% 85:89 ~ "85-89",
                               age %in% 90:94 ~ "90-94",
                               age %in% 95:99 ~ "95-99",
                               age %in% 100:104 ~ "100-104",
                               age %in% 105:109 ~ "105-109"),
         age_group = factor(age_group, levels = c("16-19",
                                                  "20-24",
                                                  "25-29",
                                                  "30-34",
                                                  "35-39",
                                                  "40-44",
                                                  "45-49",
                                                  "50-54",
                                                  "55-59",
                                                  "60-64",
                                                  "65-69",
                                                  "70-74",
                                                  "75-79",
                                                  "80-84",
                                                  "85-89",
                                                  "90-94",
                                                  "95-99",
                                                  "100-104",
                                                  "105-109"))) %>%
  group_by(age_group, cause) %>%
  mutate(contribution = sum(contribution)) %>%
  ggplot(aes(x=age_group, y=contribution, fill=cause)) +
  ggtitle(bquote(~'Change in '~ e[15]^"\u2020" ~'2005-2015' )) +
  geom_bar(stat = "identity", position = "stack")
