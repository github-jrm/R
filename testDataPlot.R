library(gplots)
require(graphics)

rm(list=ls(all=TRUE))

multi.sapply = function(...) {
      arglist = match.call(expand.dots = FALSE)$...
      var.names = sapply(arglist, deparse)
      has.name = (names(arglist) != "")
      var.names[has.name] = names(arglist)[has.name]
      arglist = lapply(arglist, eval.parent, n = 2)
      x = arglist[[1]]
      arglist[[1]] = NULL
      result = sapply(arglist, function (FUN, x) sapply(x, FUN), x)
      colnames(result) = var.names[-1]
      return(result)
}

#fixme: Stack obervations e.g. multiple LTR verification runs
#fixme: text(x=8.0,labels="blah blah blah",cex=1.0)
#fixme: Calculate mean and standard deviation confidence intervals, use these to calculate proportion of runs that would violate spec limits
#fixme: Auto t test matrix
#fixme: Autocorrelation spectrum

boxplotMain="Temperature [*C]"
testGrepStr = "^T10.2D"
setTestNamesOveride = T
testNamesOveride = c("T10.2C.T90.900s.Max","T10.2D.T90.900s.Max.S1.S5","T10.2D.T90.900s.Max.S6.S10","T10.2D.T90.900s.Max.S11.S15")
summaryStatsRound = 1
setAbline = T
ablineMin = 55
ablineMax = 57
ablineNom = 56

marDefault = c(5.1,4.1,4.1,2.1)
layoutHeights = c(2,1)
cexHist = 1.0

#Create graphic windows
dev.new()
dev.1 = as.integer(dev.cur())
dev.new()
dev.2 = as.integer(dev.cur())
dev.new()
dev.3 = as.integer(dev.cur())

dev.set(dev.1)

testDatabase = read.table(file.choose(),header=T,sep=",")

namesData = names(testDatabase)

if (setTestNamesOveride) {
  testNames = testNamesOveride
} else {
  testNames = namesData[grepl(testGrepStr,names(testDatabase))]
}

Ntests = length(testNames)
testData = testDatabase[testNames]

if (setAbline) {
  ylimMin = min(testData,ablineMin,na.rm=T)
  ylimMax = max(testData,ablineMax,na.rm=T)  
} else {
  ylimMin = min(testData,na.rm=T)
  ylimMax = max(testData,na.rm=T)
}

layoutMatrix = matrix(1,2,Ntests+1)
layoutMatrix[2,] = layoutMatrix[2,] + 1:(Ntests+1)
layout(layoutMatrix,heights=layoutHeights)

boxplot(testData,main=boxplotMain,notch=T,varwidth=T,col="lightblue",outpch=1,outcex=1.5,ylim=c(ylimMin,ylimMax))
grid(nx=NA,ny=NULL)
stripchart(testData,vertical=T,method="jitter",jitter=0.25,pch=21,cex=1.25,col="maroon",bg="bisque",add=T,ylim=c(ylimMin,ylimMax)) 
if (setAbline) {
  abline(h=c(ablineMin,ablineMax),col="red",lwd=2.0)
  abline(h=ablineNom,col="green",lwd=2.0)
}

summaryStats = data.frame(sapply(testData,nobs),multi.sapply(na.omit(testData),mean,sd,min,max))
colnames(summaryStats) = c("N","Average","Standard deviation","Minimum","Maximum")
textplot(round(t(summaryStats),summaryStatsRound),cmar=0.4)

for(i in 1:Ntests){
  testDataIasNum = sapply(testData[i],as.numeric)
  shapiroTest = shapiro.test(testDataIasNum)
  xlabTxt = c("Shapiro-Wilk normality test:",ifelse(shapiroTest[2]>0.05,"Normal","NOT normal"))
  mainTxt = c("Histogram ",names(testData[i]))
  hist(testDataIasNum,xlab=xlabTxt,main=mainTxt,cex.lab=cexHist,cex.main=cexHist,cex.axis=cexHist)
}

dev.set(dev.2)
par(mfrow=c(Ntests,1))

for(i in 1:Ntests){
  testDataIasNum = sapply(testData[i],as.numeric)
  mainTxt = c("Normal Q-Q Plot: ",names(testData[i]))
  qqnorm(testDataIasNum,main=mainTxt)
  qqline(testDataIasNum)
}

dev.set(dev.3)
textplot(round(t(summaryStats),summaryStatsRound),cmar=0.4)


