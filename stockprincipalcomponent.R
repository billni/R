stockA <- read.csv("D:/niyong/tools/R/projects/stock/stockA.csv", header=FALSE, stringsAsFactors=FALSE)
names(stockA) <- stockA[1,]
stockA <- stockA[-1,]
stockA <- na.omit(stockA)
stockA[,3:18] <- apply(stockA[,3:18], 2, as.numeric)
####################
# Principal components analysis
# parallel anaylze with scree
# to confirm how many principal components in case?
# scale data
stockA.scale <- data.frame(stockA[, c(1,2,19)], scale(stockA[,c(-1,-2,-19)], scale=T))
require(psych)
fa.parallel(stockA.scale[,c(-1,-2,-3)], fa="PC", n.iter=100, main="stock A Rating")
# 碎石图
scree(stockA.scale[,c(-1,-2,-3)])
# extract principal components
pc <- principal(stockA.scale[,c(-1,-2,-3)], nfactors=3, rotate="cluster", scores=T)
# get scores with principal component(PC) in case
stockA.pc.score <- data.frame(stockA, pc$scores)

# 根据方差贡献度加权计算总分值
pc.weights <- c(pc1=0.45, pc2=0.13, pc3=0.12)
stockA.pc.score$totalscore <- pc$scores %*% pc.weights
head(stockA.pc.score[order(stockA.pc.score$totalscore),])


####################
# Exploratory factor analysis
# scale data
stockA.scale <- data.frame(stockA[, c(1,2,19)], scale(stockA[,c(-1,-2,-19)], scale=T))
fa.parallel(stockA.scale[,c(-1,-2,-3)], fa="FA", n.iter=100, main="stock A Rating")

# extract public factors
fa <- fa(stockA.scale[,c(-1,-2,-3)], nfators=4, rotate="varimax", scores=T, fm="minchi")
# factor loadings plot 1
factor.plot(fa, labels=rownames(fa$loadings))
# factor loadings plot 2
fa.diagram(fa, simple=F)
fa