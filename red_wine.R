# Setting directory
rm(list=ls(all=TRUE))
setwd("C:/Users/Humberto/Dropbox/Work/Python/Wine")

#install.packages(c("CarletonStats", "devtools", "epanetReader", "fmsb", "ggplot2", "ggthemes", 
#                   "latticeExtra", "MASS", "PerformanceAnalytics", "psych", 
#                   "plyr", "prettyR", "plotrix", "proto", "RCurl", "reshape", "reshape2"))

# Loading the data frame
df_rw <- read.csv ("./Datasets/winequality-red.csv", sep=';', header=TRUE)

# What the data looks like?
summary(df_rw)

pc_rw <- prcomp( df_rw )
plot(pc_rw)
pcx_rw <- prcomp( df_rw, scale=TRUE )

numel = length( as.matrix( df_rw )) / length( df_rw )
biplot(pcx_rw, xlabs = rep( 'o', numel ))

hist(df_rw$quality, main="Red Wine Quality", xlab="Quality index", ylab="Frequency (wine samples)", 
     breaks=seq(min(df_rw$quality)-0.5, max(df_rw$quality)+0.5, by=1))

boxplot(alcohol~quality,data=df_rw, main="Box plot of alcohol content per quality index", 
        xlab="Quality index", ylab="Alcohol content")

boxplot(fixed.acidity~quality,data=df_rw, main="Box plot of fixed acidity per quality index", 
        xlab="Quality index", ylab="Fixed acidity")


library(ggplot2)
library(ggExtra)
library(ggthemes)

p_r <- ggplot(df_rw, aes(alcohol, fixed.acidity, colour=quality)) + geom_point(alpha = 0.5) + theme_tufte(ticks=F) +
  theme() + scale_colour_gradient(low = "blue", high = "red", na.value = "grey50")
ggMarginal(p_r, type = "histogram", fill="transparent")
