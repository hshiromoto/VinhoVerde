# Setting directory
rm(list=ls(all=TRUE))
setwd("C:/Users/Humberto/Dropbox/Work/Python/Wine")

#install.packages(c("CarletonStats", "devtools", "epanetReader", "fmsb", "ggplot2", "ggthemes", 
#                   "latticeExtra", "MASS", "PerformanceAnalytics", "psych", 
#                   "plyr", "prettyR", "plotrix", "proto", "RCurl", "reshape", "reshape2"))

# Load the data frame
df <- read.csv ("./Datasets/winequality-red.csv", sep=';', header=TRUE)
#df <- read.csv ("./Datasets/winequality-white.csv", sep=';', header=TRUE)
pc <- prcomp( df )
plot(pc)
pcx <- prcomp( df, scale=TRUE )
table( df$quality )
summary( pcx )

numel = length( as.matrix( df )) / length( df )
biplot(pcx, xlabs = rep( 'o', numel ))

hist(df$quality, main="White Wine Quality", xlab="Quality index", ylab="Frequency (wine samples)", 
     breaks=seq(min(df$quality)-0.5, max(df$quality)+0.5, by=1))


x <- df$quality
y <- df$alcohol
boxplot(y ~ x, main = "", axes = FALSE, xlab="Quality", ylab="Alcohol content",
        pars = list(boxcol = "transparent", medlty = "blank", medpch=16, whisklty = c(1, 1),
                    medcex = 0.7,  outcex = 0, staplelty = "blank"))
axis(1, at=1:length(unique(x)), label=sort(unique(x)), tick=F, family="serif")
axis(2, las=2, tick=F, family="serif")
text(min(x)/3, max(y)/1.1, pos = 4, family="serif",
     "Box plot of alcohol \ncontent according \nto wine quality")

x <- df$quality
y <- df$fixed.acidity
boxplot(y ~ x, main = "", axes = FALSE, xlab="Quality", ylab="Fixed Acidity",
        pars = list(boxcol = "transparent", medlty = "blank", medpch=16, whisklty = c(1, 1),
                    medcex = 0.7,  outcex = 0, staplelty = "blank"))
axis(1, at=1:length(unique(x)), label=sort(unique(x)), tick=F, family="serif")
axis(2, las=2, tick=F, family="serif")
text(min(x)/3, max(y)/1.1, pos = 4, family="serif",
     "Box plot of Fixed Acidity \naccording \nto wine quality")


library(ggplot2)
library(ggExtra)
library(ggthemes)

p <- ggplot(df, aes(alcohol, fixed.acidity, colour=quality)) + geom_point(alpha = 0.5) + theme_tufte(ticks=F) +
  theme() + scale_colour_gradient(low = "blue", high = "red", na.value = "grey50")
ggMarginal(p, type = "histogram", fill="transparent")