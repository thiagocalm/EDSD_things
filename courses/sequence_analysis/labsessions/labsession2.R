#'---------------------------------------
#'@date 2025-05-28
#'@program EDSD
#'@course Sequence Analysis
#'@professor Nicola Barban
#'@description Lab session 2
#'---------------------------------------

# Settings
rm(list = ls()) # removing all itens of environment
invisible(gc()) # clealing the storage memory of the computer

# libraries ---------------------------------------------------------------

library(pacman)
p_load(tidyverse, TraMineR, RColorBrewer, WeightedCluster, cluster)

# Applying analysis for biofam dataset ------------------------------------

data(biofam)

# We are attributing to the numerical value a character

biofam.seq     <- seqdef(
  biofam, var=10:25,
  states = c("P", "L", "M", "L+M", "C", "L+C", "L+M+C", "D"),
  labels = c("Parent", "Left", "Married", "Left+Marr", "Child",
             "Left+Child", "Left+Marr+Child", "Divorced"),
  weights=biofam$wp00tbgs
)

seqdplot(biofam.seq)


# Starging again with MVAD dataset ----------------------------------------

data(mvad)

#add the labels to be used in the graphics
mvad.labels=c("employment","further education",
              "higher education", "joblessness", "school", "training")

#add abbreviate codes to be used in graphics
mvad.scode=c("EM","FE", "HE", "JL", "SC", "TR")

####################################
#create sequence object WEIGHTED
library(RColorBrewer)

mvad.seq<-seqdef(mvad,
                 15:86,
                 states=mvad.scode,
                 labels=mvad.labels,
                 weights=mvad$weight,
                 id=mvad$id,
                 cpal=brewer.pal(6,"Set1"))

# some help functions
alphabet(mvad.seq)
stlab(mvad.seq)
help(seqdef)


####################################
################## Statistics
##first ten most common sequences
seqtab(mvad.seq)

# transversal statistics

# duration list
mvad.duration<-seqistatd(mvad.seq)
mvad.duration

# Creating indication of time spent in each state

EM.duration<-mvad.duration[,1]
SC.duration<-mvad.duration[,5]
JL.duration<-mvad.duration[,4]

# Creating plots sorted by durations
seqIplot(mvad.seq)

seqIplot(mvad.seq, sortv=EM.duration) # sorting by duration on employment state
seqIplot(mvad.seq, sortv=-EM.duration) # sorting by decrease duration on employment state

seqIplot(mvad.seq, sortv=c(EM.duration,SC.duration,JL.duration)) # sorting by duration in more than one state

## transition matrix
mvad.trate = seqtrate(mvad.seq)
round(mvad.trate, 2)

#entropy of state distribution
Entropy= seqstatd(mvad.seq)$Entropy # creating object with entropy

# ploting these values
plot(Entropy,main="Entropy of the state distribution ",
     col="black", xlab="Time in months", ylab="Entropy", type="l")


#sequence turbulence
Turbulence = seqST(mvad.seq) # creating an object

hist(Turbulence, col="grey", main="Histogram of sequence turbulence")

seqIplot(mvad.seq, sortv=Turbulence) # using turbulence as a sorting value

####################################
# Compute Optimal Matching with transition rates
####################################

# we can calculate a not arbitrary to indel and transition matrix using 'seqcost'
trcost <- seqcost(mvad.seq, method="TRATE")

# calculating distance matrix

dist.om1 = seqdist(
  mvad.seq,
  method="OM",
  indel=trcost$indel,
  sm=trcost$sm
)
# alternative methods
dist.lcs = seqdist(mvad.seq, method="LCS")
dist.dhd = seqdist(mvad.seq, method="DHD")

# looking at the first values
dist.om1[1:5,1:5] # reference
dist.lcs[1:5,1:5] # it is quite close to OM1
dist.dhd[1:5,1:5] # the differences are larger than before

##################################################
#Clustering

# aglomerative nesting. it is a way to create a cluster based on different methods
mvad.clusterward = agnes(
  dist.om1, # dissimilarity matrix
  diss = T, # start with dissimilarity matrix
  method = "ward" # method to calculate it
)

plot(mvad.clusterward, ask = F, which.plots = 2) # ploting dendogram

### Choose the number of clusters

mvad.cl4 = cutree(mvad.clusterward, k = 4) # choosing the number of clusters

mvad.cl4[1:10] # we create a vector with clustership

###########

# we can include these vector to our initial plot to divide the analysis based on clusters

seqdplot(mvad.seq,
         group=mvad.cl4,
         border=NA,
         space=0)


seqmtplot(mvad.seq, group=mvad.cl4, border=NA, space=0) # time spent

# giving a label to these clusters

levels(mvad.cl4)=c("EM dominated",
                   "HE dominated",
                   "FE dominated",
                   "Joblessness dominated")

# plotting it again...
seqdplot(mvad.seq,
         group=mvad.cl4,
         border=NA,
         space=0,
         main=levels(mvad.cl4))

# plotting all the sequences with clusters
seqIplot(mvad.seq,
         group=mvad.cl4,
         border=NA,
         space=0,
         main=levels(mvad.cl4))

#### Trying different cluster solution
# a good alternative is to do sensitive analysis to the number of clusters

## Measuring alternative cluster solutions

mvad.cl5 <- cutree(mvad.clusterward, k = 5) # 5 cluster solution
mvad.cl3 <- cutree(mvad.clusterward, k = 3) # 3 cluster solution
seqdplot(mvad.seq, group=mvad.cl4, border=NA, space=0)
dev.new()
seqdplot(mvad.seq, group=mvad.cl3, border=NA, space=0)
dev.new()

seqdplot(mvad.seq, group=mvad.cl5, border=NA, space=0)

### medoids
# creating the medoid sequence to give the number of id in the center of cluster

disscenter(dist.om1,  medoids.index="first")
print(mvad.seq[176,], format = "SPS")

# doing it by group
dc= disscenter(dist.om1, group= mvad.cl4, medoids.index="first")
print(mvad.seq[dc,], format = "SPS")



############ Tables

# most common trajectories between each cluster
seqtab(mvad.seq[mvad.cl4==1,])
seqtab(mvad.seq[mvad.cl4==2,])

#######logistic regression joblesseness
# probability to end up in one of the trajectories

jobless = mvad.cl4 == 4
jobless.reglog = glm(jobless ~ male +
                       funemp +
                       gcse5eq,
                     family = binomial(link = logit),
                     data = mvad)

summary(jobless.reglog)

###Add clusters to data.frame anc export in stata
mvad$cl4<- mvad.cl4

# writting it to work with stata
write_dta(mvad, "mvad_cluster.dta")

################################################
##### Multichannel sequence analysis

# importing biofam dataset
data(biofam)

## Building one channel per type of event left, children or married
bf <- as.matrix(biofam[, 10:25])
children <-  bf==4 | bf==5 | bf==6
married <- bf == 2 | bf== 3 | bf==6
left <- bf==1 | bf==3 | bf==5 | bf==6

## Building sequence objects for each domain - they only have two states
child.seq <- seqdef(children)
marr.seq <- seqdef(married)
left.seq <- seqdef(left)

## Using transition rates to compute substitution costs on each channel

mcdist <- seqdistmc(
  channels=list(child.seq,
                marr.seq,
                left.seq),
  method="OM",
  sm =list("TRATE",
           "TRATE",
           "TRATE")
)

mcdist[1:5,1:5]

## Using a weight of 2 for children channel and specifying substitution-cost
smatrix <- list()
smatrix[[1]] <- seqsubm(child.seq, method="CONSTANT")
smatrix[[2]] <- seqsubm(marr.seq, method="CONSTANT")
smatrix[[3]] <- seqsubm(left.seq, method="TRATE")

mcdist2 <- seqdistmc(
  channels=list(child.seq, marr.seq, left.seq),
  method="OM",
  sm =smatrix,
  cweight=c(2,1,1) # attributing different weigth for each domain
)

# calculating euclidian distances to establish clusters
biofam.clusterward = agnes(
  mcdist,
  diss = T,
  method = "ward"
)
# dendogram
plot(biofam.clusterward, ask = F, which.plots = 2)
biofam.cl4 <- cutree(biofam.clusterward, k = 4)

########
par(mfrow = c(1,3))
seqdplot(child.seq, group=biofam.cl4)
seqdplot(marr.seq, group=biofam.cl4)
seqdplot(left.seq, group=biofam.cl4)
par(mfrow = c(1,1))

######### Extra: cluster quality
# Let's assessing the cluster's quality using some measures for that.
# it is based on a paper of Mathias Studer (2013, i guess...)
# Reference of these statistics: book suggested by Nicola
# the most interesting is R^2

library(WeightedCluster)

####### Statistics for cluster solution
qual_clus<-list()
R2<-c()
ASW<-c()

for(j in 1:19){
  qual_clus[[j]] <- wcClusterQuality(dist.om1, cutree(mvad.clusterward, k = j+1))
  R2[j]<-qual_clus[[j]]$stats[7]
  ASW[j]<-qual_clus[[j]]$stats[4]


}

for(j in 1:19){
}
par(mfrow=c(1,2))
plot(1:19,R2 , type="l")
plot(1:19,ASW , type="l")

# it compares some measures for each number of clusters we could choose
avgClustQual <- as.clustrange(mvad.clusterward, diss=dist.om1, ncluster=10)

summary(avgClustQual, max.rank = 2) # summaries of each cluster
par(mfrow=c(1,1))
plot(avgClustQual) # plotting indicators
# we can calculate the first derivative to get the better point related to this measures!!!


### Plotting the hierarchical cluster tree
# based on the partition tree

averageClust <- hclust(as.dist(dist.om1), method = "average")
averageTree <- as.seqtree(averageClust, seqdata = mvad.seq, diss = dist.om1,
                          ncluster = 6)

#### to plot the tree you need this software installed https://www.graphviz.org/download/
seqtreedisplay(averageTree, type = "d", border = NA, showdepth = TRUE)
