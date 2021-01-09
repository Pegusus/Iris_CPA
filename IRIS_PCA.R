install.packages("stats")
install.packages("dplyr")

library(stats)
library(dplyr)

mydata = iris
mydata
summary(mydata)
str(mydata)

SL = mean(mydata$Sepal.Length)
SW = median(mydata$Sepal.Width)

head(iris)

#PCA components
myPr <- prcomp(iris[,1:4], scale = TRUE)
#prcomp(~Sepal.Length + Petal.Width, data = iris)
#scaling the length & width of the dataset
plot(scale(iris$Sepal.Length),scale(iris$Sepal.Width))
myPr
summary(myPr)
plot(myPr, type = "l")
biplot(myPr, scale = 0)

#Extracting PC scores
str(myPr)
myPr$x
iris <- cbind(iris, myPr$x[,1:2])
head(iris)

#Plot with GGPLOt
install.packages("ggplot2")
library(ggplot2)

ggplot(iris, aes(PC1,PC2, col = Species, fill = Species)) + stat_ellipse(geom = "polygon", col = "black", alpha = 0.5) + geom_point(shape = 21, col = "black")

cor(iris[1:4], iris2[,6:7])

#------------------------------
panel.cor <- function(x, y, digits=2, prefix="", cex.cor) 
{
  usr <- par("usr"); on.exit(par(usr)) 
  par(usr = c(0, 1, 0, 1)) 
  r <- abs(cor(x, y)) 
  txt <- format(c(r, 0.123456789), digits=digits)[1] 
  txt <- paste(prefix, txt, sep="") 
  if(missing(cex.cor)) cex <- 0.8/strwidth(txt) 
  
  test <- cor.test(x,y) 
  # borrowed from printCoefmat
  Signif <- symnum(test$p.value, corr = FALSE, na = FALSE, 
                   cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                   symbols = c("***", "**", "*", ".", " ")) 
  
  text(0.5, 0.5, txt, cex = cex * r) 
  text(.8, .8, Signif, cex=cex, col=2) 
}

panel.pearson <- function(x, y, ...) {
  horizontal <- (par("usr")[1] + par("usr")[2]) / 2; 
  vertical <- (par("usr")[3] + par("usr")[4]) / 2; 
  text(horizontal, vertical, format(abs(cor(x,y)), digits=2)) 
}

library(ggfortify)

data(iris)
df <- iris
print(head(df))

# Define colors
colors <- c('blue', 'red', 'green3')[unclass(df$Species)]

# Pairs plot
pairs(df[1:4], pch=21, bg=colors, upper.panel=panel.pearson, oma=c(3,3,3,15), cex.labels=1)
par(xpd = TRUE)
legend("bottomright", fill=unique(colors), legend=c(levels(iris$Species)), text.width = strwidth(c(levels(iris$Species)))[1] * 3)
