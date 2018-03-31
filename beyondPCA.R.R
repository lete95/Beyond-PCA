### HOMEWORK III BY POL SERRA LIDON

##1
##Read again the Russet data set and impute the missing values. 
##Define as X the matrix formed by the standardized continuous variables.

require(nipals)#to realize the NIPALS algorithm
require(ggplot2)#plottings
require(VIM)#kNN imputation 
require(prcomp)#to PCA
russet <- read.table("russet.txt", header=TRUE, sep="\t") #read the data set
data <- kNN(russet,variable=c("Rent","ecks"),k=6) #realize the imputation
CountryStatus<-data[,10] #get the contry status of the demo variable to plot with colours
X <- data[,2:9] #get the continuous data
X <- data.matrix(X, rownames.force = NA) #put X to matrix
X <- scale(X) #scale X

##2
#Obtain the Principals Components till the significant dimension you stated in Homework 2, 
#using the NIPALS algorithm 

nipals <- nipals(X,3,it=400,tol=1e-04) #apply the NIPALS algorithm

##3
#With the results of the NIPALS, obtain the biplot of Rp. Interpret the results.

plot(nipals$T[,1], nipals$T[,2], col=CountryStatus, pch=19,
 xlab= "PC1" ,ylab="PC2") #scores
grid() #add grid
text(nipals$T[,1],nipals$T[,2]-0.1, labels=as.character(data[,1]),
     cex=0.5,pos=3,col="black",font=4) #Give names to countries
arrows(0,0,nipals$P[,1]*4,nipals$P[,2]*4,
       col="black",length=0.1) #plot the arrows
text(nipals$P[,1]*4,nipals$P[,2]*4+0.3, labels=colnames(X),
     cex=0.9,col="red") #add names of variables
abline(h=0,col="black",lwd=2) #introduce axis
abline(v=0,col="black",lwd=2) #introduce axis

##4
#Perform the Varimax rotation and plot the rotated variables. 
#Interpret the new rotated components.
x <- runif(10)
y <- runif(10)

plot(x, y, type="n", xlim=c(-0.8,0.8), ylim=c(-0.8,0.8),xlab= "PC1" ,ylab="PC2") #new plot
arrows(0,0,nipals$P[,1],nipals$P[,2],pch=19,
     xlab= "PC1" ,ylab="PC2",
     cex=0.9,col="black") #introduce arrows
text(nipals$P[,1],nipals$P[,2], labels=colnames(X),
     cex=0.9,col="red") #add names of variables
grid() #add grid
abline(h=0,col="black",lwd=2) #introduce axis
abline(v=0,col="black",lwd=2) #introduce axis

rotatedLoadings <- varimax(nipals$P, normalize = F, eps = 1e-5) #realize varimax rotation

plot(x, y, type="n", xlim=c(-0.8,0.8), ylim=c(-0.8,0.8),xlab= "PC1" ,ylab="PC2") #newplot
arrows(0,0,rotatedLoadings$loadings[,1],rotatedLoadings$loadings[,2],pch=19,
       xlab= "PC1" ,ylab="PC2",
       cex=0.9,col="black") #introduce arrows
text(rotatedLoadings$loadings[,1],rotatedLoadings$loadings[,2], labels=colnames(X),
     cex=0.9,col="green") #add names of variables
grid() #add grid
abline(h=0,col="black",lwd=2) #introduce axis
abline(v=0,col="black",lwd=2) #introduce axis

##5
#Compute the scores of individuals in the rotated components Psi.rot. 
#Interpret them (xxxx$ind$coord[,1:nd] = Psi.rot; dimdesc(xxxx,axes=1:nd).

Psi <- X%*%rotatedLoadings$loadings; #compute the projections in varimax vars
Demos = as.factor(CountryStatus) #get the demo variable to act as color
plot(Psi[,1],Psi[,2],xlab="PC1",ylab="PC2",
     pch=19,col=Demos,ylim=range(-3.5:2.5),
     xlim=range(-4.5:3)) #plot the projections
text(Psi[,1],Psi[,2]-0.1, labels=as.character(data[,1]),
     cex=0.5,pos=3,col="black") #Give names to countries
grid() #add grid
abline(h=0,col="black",lwd=2) #introduce axis
abline(v=0,col="black",lwd=2) #introduce axis

##6
#Read the PCA_quetaltecaen data. 

quetaltecaen <- read.table("PCA_quetaltecaen.txt", header=TRUE, sep="\t") #read the data set
Y <- as.matrix(quetaltecaen[1:8,2:9]) #get the cont. variables

##7
#Symmetrize the data matrix, expressing the joint feeling between CCAA.

D <- (Y+t(Y))/2 #symmetrize
rownames(D) <- c("AND","CAST","CAT","VAL","GAL","MAD","EUZ","REST")
colnames(D) <- quetaltecaen[,1]


##8
#Transform the similarity matrix into a dissimilarity (notice that max. similarity = 10).

C <- matrix(10,nrow=8,ncol=8) #define max. similarity matrix 
Z <- C-D #substract

##9
#Perform the PCA upon the formed dissimilarity matrix.

PCA <- nipals(Z,3,it=400,tol=1e-04) #apply the NIPALS algorithm

##10
#Plot the first two components.

plot(PCA$T[,1], PCA$T[,2], pch=19,
     xlab= "PC1" ,ylab="PC2" ) #scores
grid() #add grid
text(PCA$T[,1],PCA$T[,2]-0.1, labels=as.character(quetaltecaen[,1]),
     cex=1,pos=3,col="black",font=4) #Give names to countries
abline(h=0,col="black",lwd=2) #introduce axis
abline(v=0,col="black",lwd=2) #introduce axis

##11
#Further analysis

f <- quetaltecaen[,2:9]
rownames(f) <- quetaltecaen[,1]
quetaltecaen
colMeans(f) #que piensan de ti:
rowMeans(f) #que piensas del resto:

