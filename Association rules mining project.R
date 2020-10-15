## Victoria Silverman
##University of Ottawa 
##This is the property of Victoria Silverman and the University of Ottawa and Carleton University

##set up directory 
wd2='/Users/Victoria/Documents/Victoria/4th year/STAT4601'
data="grocery.csv"
grocery=paste(wd2,data,sep="/")

##import libraries needed
library(arules)
library(arulesViz)
library(ggplot2)
library(qlcMatrix)
library(corrplot)
library(tidyverse)
library(colorspace) # for sequential_hcl

##Q1
grocery <- read.transactions(grocery, sep=',')
summary(grocery)
par(mfrow=c(1,2))
##create frequency plots for top 10 items, relative is more useful than absolute
itemFrequencyPlot(grocery,topN=10,type="absolute",col=sequential_hcl(10))
itemFrequencyPlot(grocery,topN=10,type="relative", col=sequential_hcl(10))

par(mfrow=c(1,1))

##create frequency plot for the # of items in a transaction
groc <- as(grocery, "matrix")
groc <- as.data.frame(groc)

# A loop to convert all TRUE to 1, FALSE to 0. 
for(i in 1:dim(groc)[2]){
  groc[[i]] <- sapply(groc[[i]], function(x){ifelse(x %in% TRUE, 1, 0)})
}

groc$rowSums <- rowSums(groc)

#histogram of items per transaction
sumTable <- table(groc$rowSums)
df=as.data.frame(sumTable)
names(df)[names(df) == "Var1"] <- "Items"
p<-ggplot(data=df, aes(x=Items, y=Freq)) +
  geom_bar(stat="identity", fill="dark blue")+theme_minimal()
p

##Heirarchial clustering on items with support>0.05 using jaccaard as the dissimilarity measure and ward method for heirarchial clustering
s <- grocery[,itemFrequency(grocery)>0.05]
d_jaccard <- dissimilarity(s, which = "items")
plot(hclust(d_jaccard, method = "ward.D2"), main = "Dendrogram for items")

## create a correlation plot/matrix
groc <- as(grocery, "matrix")
corr<-corSparse(groc, Y = NULL, cov = FALSE) ## need qlcMatrix package to run this version of correlation because it is a sparse matrix
corr[lower.tri(corr,diag=TRUE)] <- NA
corr[corr == 1] <- NA
corrdf <- as.data.frame(corr)
ind <- which(upper.tri(corrdf, diag = FALSE), arr.ind = TRUE)
nn <- dimnames(corrdf)
# There are no NAs in this new dataframe, so no need to omit
newCorr <- data.frame(row = nn[[1]][ind[, 1]],
                      col = nn[[2]][ind[, 2]],
                      val = corrdf[ind])
corrSub <- subset(newCorr, abs(val) > 0.1)
#sort by highest correlation
corrSub <- corrSub[order(-abs(corrSub$val)),]
# convert corrSub to a dataframe
corrSub <- as(corrSub, "matrix")
# Use the transactions object to extract labels
itemLabels <- grocery@itemInfo$labels

# This gets the first set
for(i in 1:dim(corrSub)[1]){
  ind <- as.numeric(corrSub[i, "row"])
  corrSub[i, "row"] <- itemLabels[ind]
}

# The second set is a bit trickier because of the V
library(stringr)
# Using a nested ifelse() and str_trunc:
# If the number of characters is 2, it's V and a single-digit number,
# so replace it with the single-digit number.
# If the number of characters is 3, it's V and a double-digit number,
# so replace it with the double-digit number.
# Otherwise, it's V and a triple-digit number,
# so replace it with the triple-digit number.
corrSub[,2] <- ifelse(nchar(corrSub[,2]) == 2, str_trunc(corrSub[,2], width = 1, side = "left", ellipsis = ""),
                      ifelse(nchar(corrSub[,2]) == 3, str_trunc(corrSub[,2], width = 2, side = "left", ellipsis = ""),
                             str_trunc(corrSub[,2], width = 3, side = "left", ellipsis = "")))

# Make it a numeric vector
corrSub[,2] <- as.numeric(corrSub[,2])

# This gets the second set
for(i in 1:dim(corrSub)[1]){
  ind <- as.numeric(corrSub[i, "col"])
  corrSub[i, "col"] <- itemLabels[ind]
}

corrSub <- as.matrix(corrSub)
# Saving these for later
corrRows <- unique(corrSub[, 1])
corrCols <- unique(corrSub[, 2])

# Rename the columns
names(corrSub) <- c("row", "col", "val")
# Convert to 34 x 37 table
mtx_corr <- pivot_wider(data = as_tibble(corrSub), names_from = "col", values_from = "val", values_fill = list("val"=0))
# Drop the column of names
mtx_corr <- mtx_corr[, -1]
# Coerce to matrix
mtx_corr <- as.matrix(mtx_corr)
# Change to numeric
mtx_corr <- apply(mtx_corr, MARGIN = 2, as.numeric)
# Add in the names again
rownames(mtx_corr) <- corrRows
colnames(mtx_corr) <- corrCols
#plot correlations visually
#corrplot(mtx_corr, method = "color", cl.pos = "n", type = "upper")

corrplot(mtx_corr,type="upper", is.corr=FALSE, tl.col="black", na.label=" ")

## create plots that look at the density of the data set
set.seed(123)
subset<-sample(grocery,50)
#subset<-as(subset,"matrix")
image(grocery, aspect='fill')
image(subset, aspect='fill')

##Q2
## run apriori - this is the algorithm that generates the association rules
rules <- apriori(grocery, parameter = list(supp = 0.01, conf = 0.25, target = "rules"))

##remove redundant rules
summary(rules)
rules2 <- rules[!is.redundant(rules)]
summary(rules2)
rules<-rules2

##Q3
## inspect top 4 rules by lift
inspect(head(rules, n = 4, by = "lift"))

##Q4
##create graphics (we make 4)

#graphic 1 is a graph method, in a circle with a subset of the rules
subrules <- subset(rules, lift>1.5)
realsmol<-head(subrules,n=10, by="support", decreasing=TRUE)
plot(realsmol, method="graph", control=list(layout=igraph::in_circle()))


#graphic 2 is a scatter plot on a subset of the rules
subrules2 <- subset(rules, support<0.1 & confidence>0.4)
plot(subrules2, measure=c("support", "lift"), shading="confidence",control=list(col=sequential_hcl(100)), jitter=0)

#graphic 3 parallel coordinates graph on another subset of the rules
# the colouring default for parallel coordinate graphs in arulesViz is lift
confiru <- (subset(rules, confidence>0.3))
confiru2<- head(subrules,n=35, by="confidence")
plot(confiru2, method="paracoord", control=list(col=sequential_hcl(100)))
# you can also run
plot(confiru2, method="paracoord", shading="lift", control=list(col=sequential_hcl(100)))
#to confirm that statement, or you could change "lift" to any other arules metric for shading

#graphic 4 grouped graph on another subset of the rules
reelsmol<-head(subrules,n=50, by="lift", decreasing=TRUE)
plot(reelsmol, method="grouped", control=list(col=sequential_hcl(100)))

##end of assignment


