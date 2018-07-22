getwd()
setwd("C:/home/pranjali")
getwd()

library(datasets)

wine <- read.csv("WINE.csv")
#Shuffle the data with set.seed
set.seed(5)
dt.Shuffled <- wine[sample(nrow(wine)),]

# Split the data into training and testing
idx <- round(0.8 * nrow(dt.Shuffled))
wine.tra <-SF.asDecisionTable(dt.Shuffled[1:idx,],decision.attr = 12, indx.nominal = 12)
wine.tst <- SF.asDecisionTable(dt.Shuffled[(idx+1):nrow(dt.Shuffled), -ncol(dt.Shuffled)])

# DISCRETIZATION
cut.values <- D.discretization.RST(wine.tra,type.method = "global.discernibility")
d.tra <- SF.applyDecTable(wine.tra, cut.values)
d.tst <- SF.applyDecTable(wine.tst, cut.values)

data(RoughSetData)
decision.table <- RoughSetData$wine.dt

## generate single superreduct
res.2 <- FS.feature.subset.computation(decision.table,method = "quickreduct.frst")
 
## generate new decision table
new.decTable <- SF.applyDecTable(decision.table, res.2)

# INSTANCE SELECTION
indx <- IS.FRIS.FRST(new.decTable,control = list(threshold.tau = 0.2, alpha = 1))

wine.tra.is <- SF.applyDecTable(new.decTable, indx)

# RULE INDUCTION (Rule-based classifiers)
control.ri <- list(type.aggregation = c("t.tnorm", "lukasiewicz"),type.relation = c("tolerance", "eq.3"),t.implicator = "kleene_dienes")

decRules.hybrid <- RI.hybridFS.FRST(wine.tra.is,control.ri)

# predicting newdata
predValues.hybrid <- predict(decRules.hybrid,new.decTable)

X.laplace(decRules.hybrid)