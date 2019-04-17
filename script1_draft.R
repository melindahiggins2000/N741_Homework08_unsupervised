library(dplyr)
library(readr)

# read-in abalone dataset
# abalone <- read_csv("abalone.data", col_names = FALSE)
abalone <- read_csv(url("http://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data"))

# assign variable names
names(abalone) <- c("sex","length","diameter","height",
                    "wholeWt","shuckedWt","visceraWt","shellWt",
                    "rings")

# cleanup data
# component weights should be less than whole weight
# length is defined to be the longest dimensional measurement
# and all dimensions should be > 0 - there are some 0 heights
abaloneClean <- abalone %>%
  filter(shuckedWt < wholeWt) %>%
  filter(visceraWt < wholeWt) %>%
  filter(diameter < length) %>%
  filter(height < length) %>%
  filter(height > 0)

# take a 5% random sample of 4169 cases
abaloneSample <- abaloneClean %>%
  sample_frac(size = 0.05, replace = FALSE)

# keep 3 dimensions and 3 component weights
# remove sex and number of rings
# number of rings + 1.5 = age of abalone
abaloneKeep <- abaloneSample %>%
  select(length, diameter, height, 
         shuckedWt, visceraWt, shellWt)

# ===================================
# do cluster analysis of dimensions and weights
# ===================================
  
# Scale the data before clustering
sd.data <- scale(abaloneKeep)

# Calculate Euclidean distance between each pair of points
data.dist <- dist(sd.data)

# Plot the tree, default linkage = 'complete'
plot(hclust(data.dist), 
     labels = abaloneSample$sex, 
     main = "Complete Linkage", 
     xlab = "", sub = "", ylab = "")

# Plot the tree, using linkage = 'average'
plot(hclust(data.dist), 
     method = "average", 
     labels = abaloneSample$sex, 
     main = "Average Linkage", xlab = "", 
     sub = "", ylab = "")

# Plot the tree, using default linkage = 'single'
plot(hclust(data.dist), 
     method = "single", 
     labels = abaloneSample$sex, 
     main = "Single Linkage", xlab = "", 
     sub = "", ylab = "")

# Let's use complete linkage (1st cluster run above)
# and cut into 3 clusters
# how do these align with the 3 sexes

hc.out <- hclust(dist(sd.data))
hc.clusters <- cutree(hc.out, 3)
table(hc.clusters, abaloneSample$sex)

# which cluster(s) did the best job 
# identifying the infants?
# which cluster(s) found the adults (M and F)?

# ===================================
# K-means clustering
# ===================================

# K-means clustering with K=3 (from the hierarchical clustering number)

set.seed(40523)
km.out = kmeans(sd.data, 3, nstart = 20)
km.clusters = km.out$cluster
table(km.clusters, hc.clusters)

# how does K-means clustering compare to 
# complete linkage clustering

# ===================================
# PCA
# ===================================

# Find the principal components of the normalized data
# which are computed using scale = TRUE option
# use abaloneKeep
pr.out <- prcomp(abaloneKeep, scale = TRUE)

# colors
cols=rainbow(length(unique(abaloneSample$sex)))

# mapping of colors to sex
# blue is #0000FFFF
# light green is #00FF00FF
# red is #FF0000FF
#
# see https://webtools.fineaty.com/ColorInfo/0000FFFF/en/Information-about-the-0000FFFF-color.html
# see https://webtools.fineaty.com/ColorInfo/00FF00FF/en/Information-about-the-00FF00FF-color.html
# see https://webtools.fineaty.com/ColorInfo/FF0000FF/en/Information-about-the-FF0000FF-color.html

table((unique(abaloneSample$sex)), cols)

# plot scores
plot(pr.out$x[, 1:2],
     col = cols)
legend(x = "topleft",
       legend = c("F", "I", "M"),
       col = cols, pch = 1)

# do you see any separation or clusters by sex?
# look at summary of PCA
summary(pr.out)

# what are the percentages of variance of 1st 2 PCs

# plot scree plot
plot(pr.out)

# plot cumulative PVE
pve = 100*pr.out$sdev^2/sum(pr.out$sdev^2)
plot(pve, type = "o", 
     ylab = "Cumulative PVE", 
     xlab = "Principal Component", 
     col="blue")

# and plot cumulative PVE
plot(cumsum(pve), type = "o", 
     ylab = "Cumulative PVE", 
     xlab = "Principal Component", col="brown3")

# ===================================
# MDS
# ===================================

# compute distances for abaloneKeep
d <- dist(abaloneKeep)

# run cmdscale - classicalmetric MDS
fit <- cmdscale(d, eig=TRUE, k=2)

# k is the number of principal coordinates we want
# let's use 2

# view the results
fit

# plot it

x <- fit$points[,1]
y <- fit$points[,2]

plot(x, y, xlab = "PCo1", ylab = "PCo2",
     main = "Metric MDS", type = "n")
text(x, y, labels = abaloneSample$sex, cex = 0.7)

# nonMetric MDS - skip
library(MASS)

# get fit using isoMDS
fit <- isoMDS(d, k=2)

# plot it
x <- fit$points[,1]
y <- fit$points[,2]

plot(x, y, xlab = "PCo1", ylab = "PCo2",
     main = "nonMetric MDS", type = "n")
text(x, y, labels = abaloneSample$sex, cex = 0.7)



















