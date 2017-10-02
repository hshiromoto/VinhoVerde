# Setting directory
rm(list=ls(all=TRUE))
setwd("C:/Users/Humberto/Dropbox/Work/Python/Wine")

#install.packages(c("CarletonStats", "devtools", "epanetReader", "fmsb", "ggplot2", "ggthemes", 
#                   "latticeExtra", "MASS", "PerformanceAnalytics", "psych", 
#                   "plyr", "prettyR", "plotrix", "proto", "RCurl", "reshape", "reshape2"))

# Loading the data frame
df_ww <- read.csv ("./Datasets/winequality-white.csv", sep=';', header=TRUE)

# What the data looks like?
summary(df_ww)

pc_ww <- prcomp( df_ww )
plot(pc_ww)
pcx_ww <- prcomp( df_ww, scale=TRUE )
print(pcx_ww)

numel = length( as.matrix( df_ww )) / length( df_ww )
biplot(pcx_ww, xlabs = rep( '.', numel ))

hist(df_ww$quality, main="White Wine Quality", xlab="Quality index", ylab="Frequency (wine samples)", 
     breaks=seq(min(df_ww$quality)-0.5, max(df_ww$quality)+0.5, by=1))

boxplot(alcohol~quality,data=df_ww, main="Box plot of alcohol content per quality index", 
        xlab="Quality index", ylab="Alcohol content")

boxplot(fixed.acidity~quality,data=df_ww, main="Box plot of fixed acidity per quality index", 
        xlab="Quality index", ylab="Fixed acidity")


library(ggplot2)
library(ggExtra)
library(ggthemes)

p_r <- ggplot(df_ww, aes(alcohol, fixed.acidity, colour=quality)) + geom_point(alpha = 0.5) + theme_tufte(ticks=F) +
  theme() + scale_colour_gradient(low = "blue", high = "green", na.value = "grey50")
ggMarginal(p_r, type = "histogram", fill="transparent")
