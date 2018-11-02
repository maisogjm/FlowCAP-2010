# Load library and functions.
source("H:/DATA/Documents and Settings/Joe/Papers/ClassificationAndDiscrimination/elith_and_leathwick_tutorial_for_running_boosted_regression_trees/brt.functions.R")
library(gbm)

# Load data.
model.data = read.csv("H:/DATA/Documents and Settings/Joe/Papers/GeneralStatistics/FlowCap/csv/Challenge4/CH4/StemCell/005.csv")
summary(model.data)
dim(model.data)

# Compute optimal number of trees.
angaus.tc5.lr01 <- gbm.step(data=model.data,  
    gbm.x = 3:14, 
    gbm.y = 2, 
    family = "bernoulli", 
    tree.complexity = 5, 
    learning.rate = 0.01, 
    bag.fraction = 0.5) 
names(angaus.tc5.lr01)
length(angaus.tc5.lr01$fitted)
class(angaus.tc5.lr01$fit)
length(angaus.tc5.lr01$fit)
class(angaus.tc5.lr01$cv.statistics)
length(angaus.tc5.lr01$cv.statistics)
angaus.tc5.lr01$cv.statistics
class(angaus.tc5.lr01$n.trees)
length(angaus.tc5.lr01$n.trees)
angaus.tc5.lr01$n.trees ; # Number of trees fitted.

# Change the Settings.
angaus.tc5.lr005 <- gbm.step(data=model.data,  
    gbm.x = 3:14, 
    gbm.y = 2, 
    family = "bernoulli", 
    tree.complexity = 5, 
    learning.rate = 0.005, 
    bag.fraction = 0.5) 
angaus.tc5.lr005$ n.trees

# Simplify the Model.
angaus.simp <- gbm.simplify(angaus.tc5.lr005, n.drops = 5)

# Make a Model With One Predictor Dropped
angaus.tc5.lr005.simp <- gbm.step(model.data, gbm.x = angaus.simp$pred.list[[1]],
    gbm.y = 2, tree.complexity = 5, learning.rate = 0.005) 

# Keep the Workspace at a Reasonable Size
dump(ls(pattern="angaus"), "scratch.R"); #this dumps up all objects with "angaus" in the name 
source("scratch.R")
save.image("C:/brt/.RData")

# Plot the functions and fitted values from the model
par(mfrow=c(3,4))
gbm.plot(angaus.tc5.lr005, n.plots=12, write.title = F)

# Plot the fitted values in relation to each of the 
# predictors used in the model.
gbm.plot.fits(angaus.tc5.lr005)

# Interrogate and plot the interactions 
# Assess the extent to which pairwise interactions exist in the data. 
find.int <- gbm.interactions(angaus.tc5.lr005) 

# The returned object, here named find.int, is a list.
# The first 2 components summarise the results,
# first as  a ranked list of the 5 most important pairwise interactions,
# and the second tabulating all pairwise interactions.
# The variable index numbers in $rank.list can be used for plotting. 
find.int

# Plot pairwise interactions
gbm.perspec(angaus.tc5.lr005,7,1,y.range = c(15,20), z.range=c(0,0.6))

###################################################################
###################################################################
# PREDICTING TO NEW DATA
###################################################################
###################################################################

# Load sample data.
eval.data <- read.csv("H:/DATA/Documents and Settings/Joe/Papers/ClassificationAndDiscrimination/elith_and_leathwick_tutorial_for_running_boosted_regression_trees/data/eval.data.csv", as.is=T)

# The "Method" column needs to be converted to a factor,
# with levels matching those in the modelling data: 
eval.data$Method <- factor(eval.data$Method, levels = levels(model.data$Method)) 

# To make predictions to sites from the BRT model use Ridgeway's code: 
library(gbm) 
preds <- predict.gbm(angaus.tc5.lr005, eval.data, 
    n.trees=angaus.tc5.lr005$gbm.call$best.trees, type="response")

# or use our code with the defaults, which means it just makes predictions to the workspace: 
gbm.predict.grids(angaus.tc5.lr005, eval.data, want.grids = F, sp.name = "preds")

# These are evaluation sites, and have observations in column 1
# (named Angaus_obs). They are independent of the model building set
# and could be used for an independent evaluation. For example 
# here is code for calculating the deviance and the AUC
# (area under the ROC curve): 
calc.deviance(eval.data$Angaus_obs,preds,calc.mean=T) 
roc(eval.data$Angaus_obs,preds) 

# One useful feature of prediction in gbm is you can predict to a varying
# number of trees. See the highlighted code below to how to predict to a
# vector of trees. The full set of code here shows how to make one of the
# graphed lines from Fig. 2 in our paper, using a model of 5000 trees
# developed with gbm.fixed 
angaus.5000 <- gbm.fixed(data=model.data, gbm.x = 3:14, gbm.y = 2, 
learning.rate = 0.005, tree.complexity = 5, n.trees = 5000) 

tree.list <- seq(100, 5000, by = 100) 

pred <- predict.gbm(angaus.5000, eval.data, n.trees = tree.list, "response")

# Note that the code above makes a matrix, with each column being the
# predictions from the model angaus.5000 to the number of trees
# specified by that element of tree.list – for example, the predictions 
# in column 5 are for tree.list[5] = 500 trees.
  
# Now to calculate the deviance of all these results, and plot them: 
 
angaus.pred.deviance <- rep(0,50) 
 
for (i in 1:50) { 
    angaus.pred.deviance[i] <- calc.deviance(eval.data$Angaus_obs, 
    pred[,i],calc.mean=T) 
} 
plot(tree.list,angaus.pred.deviance,ylim=c(0.7,1),xlim = c(-100,5000),type='l', 
    xlab = "number of trees",ylab = "predictive deviance", cex.lab = 1.5)  

# Making grids
# Import grids
theDir = "H:/DATA/Documents and Settings/Joe/Papers/ClassificationAndDiscrimination/elith_and_leathwick_tutorial_for_running_boosted_regression_trees/data/"
grid.names <- c("mask.asc", "segsumt.asc", 
    "segtseas.asc","seglowflow.asc","dsdist.asc","dsmaxslope.asc","usavgt.asc",
    "usraindays.asc","usslope.asc", "usnative.asc", "dsdam.asc", "locsed.asc") 

#here make sure the order is the same as above, if you're using different data
variable.names <- c("mask", names(model.data)[c(3:12,14)]) 
 
for(i in 1:length(grid.names)){
    filePath = sprintf("%s%s",theDir,grid.names[i]) 
    assign(variable.names[i],
        scan(filePath, skip=6, na.string = "-9999"), pos=1) 
}

# Make them into a data frame, adding a column for the Method – we will
# predict the probability of catch using electric fishing: 
preddat <- data.frame(SegSumT,USNative,DSDist,LocSed,DSMaxSlope,USSlope,
    USRainDays,USAvgT,SegTSeas,SegLowFlow,DSDam,rep("electric", length(SegSumT))) 

names(preddat)[12] <- "Method" 

preddat$Method <- factor(preddat$Method, levels = levels(model.data$Method)) 

# This data frame has 49000 rows, because there were 49000 cells in each grid.
# Whilst you could predict to all sites, for very large grids you might want
# to reduce it to the sites you are interested in: 
 
preddat<- preddat[!is.na(mask),] 

# You will now have have 8058 rows in the data.

# This is how to make a grid
# (it also returns the predictions to the R workspace too) : 
gbm.predict.grids(angaus.tc5.lr005, preddat, want.grids = T,
    sp.name = "angaus_preds",pred.vec = rep(-9999,49000),
    filepath = "c:/brt/", num.col = 250, num.row = 196, xll = 0,
    yll = 0, cell.size = 100, no.data = -9999, plot=T) 

# Dealing with large grids
# First detail how many rows you will have in each run; here we'll do ¼ at a time 
rep.rows <- c(49,49,49,49) 

# Then do the first run: 
for(i in 1:length(grid.names)){
    filePath = sprintf("%s%s",theDir,grid.names[i]) 
    assign(variable.names[i], scan(filePath, skip=6, na.string = "-9999", 
        nlines = rep.rows[1]), pos=1) 
} 

preddat <- data.frame(SegSumT,USNative,DSDist,LocSed,DSMaxSlope,USSlope,
    USRainDays,USAvgT,SegTSeas,SegLowFlow,DSDam,rep("electric", length(SegSumT))) 
names(preddat)[12] <- "Method" 
preddat$Method <- factor(preddat$Method, levels = levels(model.data$Method)) 
preddat<- preddat[!is.na(mask),]

gbm.predict.grids(angaus.tc5.lr005, preddat, want.grids = T,
    sp.name = "angaus_preds2",pred.vec = rep(-9999,250 * rep.rows[1]),
    filepath = theDir, num.col = 250, num.row = 196, xll = 0, yll = 0,
    cell.size = 100, no.data = -9999, plot=T, full.grid = F,
    part.number = 1, part.row = rep.rows[1])  

# Then do the rest in a loop: 
for(i in 2:4){ 
    for(j in 1:length(grid.names)){
        filePath = sprintf("%s%s",theDir,grid.names[j])
        assign(variable.names[j],scan(filePath ,
            skip=(6 + sum(rep.rows[1:(i-1)])), na.string = "-9999",
            nlines = rep.rows[i]), pos=1) 
    } 
 
    preddat <- data.frame(SegSumT,USNative,DSDist,LocSed,DSMaxSlope,USSlope,
        USRainDays,USAvgT,SegTSeas,SegLowFlow,DSDam,rep("electric",
        length(SegSumT))) 
    names(preddat)[12] <- "Method" 
    preddat$Method <- factor(preddat$Method, levels = levels(model.data$Method)) 
    preddat<- preddat[!is.na(mask),] 
 
    gbm.predict.grids(angaus.tc5.lr005, preddat, want.grids = T,
        sp.name = "angaus_preds2",pred.vec = rep(-9999,250 * rep.rows[i]),
        filepath = theDir, num.col = 250, full.grid = F, part.number = i,
        part.row = rep.rows[i], header = F)  
}
