# Setting directory
rm(list=ls(all=TRUE))
setwd("C:/Users/Humberto/Dropbox/Work/Python/Wine")

#install.packages(c("CarletonStats", "devtools", "epanetReader", "fmsb", "ggplot2", "ggthemes", 
#                   "latticeExtra", "MASS", "PerformanceAnalytics", "psych", 
#                   "plyr", "prettyR", "plotrix", "proto", "RCurl", "reshape", "reshape2"))

# Load the data frame
df_rw <- read.csv ("./Datasets/winequality-red.csv", sep=';', header=TRUE)
df_ww <- read.csv ("./Datasets/winequality-white.csv", sep=';', header=TRUE)
pc_rw <- prcomp( df_rw )
pc_ww <- prcomp( df_ww )
par(mfrow=c(2,1))
plot(pc_rw)
plot(pc_ww)
par(mfrow=c(1,1))
pcx_rw <- prcomp( df_rw, scale=TRUE )
pcx_ww <- prcomp( df_ww, scale=TRUE )

numel = length( as.matrix( df_rw )) / length( df_rw )
biplot(pcx_rw, xlabs = rep( 'o', numel ))

hist(df_rw$quality, main="Red Wine Quality", xlab="Quality index", ylab="Frequency (wine samples)", 
     breaks=seq(min(df_rw$quality)-0.5, max(df_rw$quality)+0.5, by=1))


x <- df_rw$quality
y <- df_rw$alcohol
boxplot(y ~ x, main = "", axes = FALSE, xlab="Quality", ylab="Alcohol content",
        pars = list(boxcol = "transparent", medlty = "blank", medpch=16, whisklty = c(1, 1),
                    medcex = 0.7,  outcex = 0, staplelty = "blank"))
axis(1, at=1:length(unique(x)), label=sort(unique(x)), tick=F, family="serif")
axis(2, las=2, tick=F, family="serif")
text(min(x)/3, max(y)/1.1, pos = 4, family="serif",
     "Box plot of alcohol \ncontent according \nto red wine quality")

x <- df_rw$quality
y <- df_rw$fixed.acidity
boxplot(y ~ x, main = "", axes = FALSE, xlab="Quality", ylab="Fixed Acidity",
        pars = list(boxcol = "transparent", medlty = "blank", medpch=16, whisklty = c(1, 1),
                    medcex = 0.7,  outcex = 0, staplelty = "blank"))
axis(1, at=1:length(unique(x)), label=sort(unique(x)), tick=F, family="serif")
axis(2, las=2, tick=F, family="serif")
text(min(x)/3, max(y)/1.1, pos = 4, family="serif",
     "Box plot of Fixed Acidity \naccording \nto red wine quality")


library(ggplot2)
library(ggExtra)
library(ggthemes)

p_r <- ggplot(df_rw, aes(alcohol, fixed.acidity, colour=quality)) + geom_point(alpha = 0.5) + theme_tufte(ticks=F) +
  theme() + scale_colour_gradient(low = "blue", high = "red", na.value = "grey50")
ggMarginal(p_r, type = "histogram", fill="transparent")

p_w <- ggplot(df_ww, aes(alcohol, fixed.acidity, colour=quality)) + geom_point(alpha = 0.5) + theme_tufte(ticks=F) +
  theme() + scale_colour_gradient(low = "blue", high = "green", na.value = "grey50")
ggMarginal(p_w, type = "histogram", fill="transparent")