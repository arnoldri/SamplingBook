
set.seed(123)



knitr::include_graphics("include-files/fig/StatisticalInference.png")



runif(5)



runif(5)



hist(runif(100), xlab="x", ylab="Frequency")



hist(runif(100000), xlab="x", ylab="Frequency")



set.seed(111)
runif(5)
runif(5)
runif(5)



set.seed(8)
runif(5)



set.seed(111)
runif(5)



set.seed(123)



n <- 10
u <- runif(n)
u
y <- ifelse(u<=0.5,"Heads","Tails")
y
table(y)



table(ifelse(runif(10000)<=0.5,"Heads","Tails"))



pp <- c(Red=1/2, Green=1/3, Blue=1/6)
sum(pp)



m <- length(pp) # Number of categories
cp <- cumsum(pp)
cp



set.seed(112)
u <- runif(8)
u
y <- case_when(u <= cp[1] ~ "Red",
               u <= cp[2] ~ "Green",
               .default="Blue")   # here .default means "otherwise"
data.frame(round(u,5),y)
table(y)
table(y)/length(y)



y <- factor(y,levels=names(pp))
table(y)



i <- 1
plot(0:m,c(0,cp),type="s",ylim=c(0,1),axes=FALSE,
     xlab="Category",ylab="U")
axis(1,at=1:m,labels=names(pp)); axis(2); box()
axis(4,at=c(0,cp),lab=sprintf("%.3f",c(0,cp)),las=2)
text(0,u[i],label=bquote(u[.(i)]),pos=2,xpd=TRUE)
points(0,u[i],pch=16,cex=1.7,col="red")
arrows(0,u[i], as.numeric(y[i]),u[i], col="red", lwd=2)
arrows(as.numeric(y[i]),u[i], as.numeric(y[i]),0, col="red", lwd=2)
title(bquote(u[.(i)]==.(sprintf("%.3f",u[i]))*"; "*y[.(i)]*"="*.(as.character(y[i]))))



i <- 2
plot(0:m,c(0,cp),type="s",ylim=c(0,1),axes=FALSE,
     xlab="Category",ylab="U")
axis(1,at=1:m,labels=names(pp)); axis(2); box()
axis(4,at=c(0,cp),lab=sprintf("%.3f",c(0,cp)),las=2)
text(0,u[i],label=bquote(u[.(i)]),pos=2,xpd=TRUE)
points(0,u[i],pch=16,cex=1.7,col="red")
arrows(0,u[i], as.numeric(y[i]),u[i], col="red", lwd=2)
arrows(as.numeric(y[i]),u[i], as.numeric(y[i]),0, col="red", lwd=2)
title(bquote(u[.(i)]==.(sprintf("%.3f",u[i]))*"; "*y[.(i)]*"="*.(as.character(y[i]))))



set.seed(112)
n <- 10000
u <- runif(n)
y <- case_when(u <= cp[1] ~ "Red",
               u <= cp[2] ~ "Green",
               .default="Blue")
y <- factor(y, levels=names(pp))
table(y)



n <- 10000
y <- sample(m, size=n, prob=pp, replace=TRUE)
table(y)



y <- sample(1:m, size=n, prob=pp, replace=TRUE)
table(y)



y <- sample(c("Red","Green","Blue"), size=n, prob=pp, replace=TRUE)
y <- factor(y,levels=names(pp))
table(y)



y <- sample(names(pp), size=n, prob=pp, replace=TRUE)
y <- factor(y,levels=names(pp))
table(y)



m <- 10
p <- 1/6
pp <- dbinom(0:m, m, p)
pp



barplot(pp, names=0:m,
        xlab="y", ylab="Pr(Y=y)",
        main=bquote("Bin("*.(m)*","*.(sprintf(" %.4f",p))*")"))



cp <- pbinom(0:m, m, p)



plot(0:m, cp, type="s",
        xlab="y", ylab="Pr(Y<=y)",
        main=bquote("Bin("*.(m)*","*.(sprintf(" %.4f",p))*")"))



u <- 0.511
cp <- pbinom(0:m, m, p)
y <- min((0:m)[u<cp])
plot(c(0,0:m),c(0,cp),type="s",ylim=c(0,1),
     xlab="Y",ylab="U")
text(0,u,label=expression(u),pos=2,xpd=TRUE)
points(0,u,pch=16,cex=1.7,col="red")
arrows(0,u, y,u, col="red", lwd=2)
arrows(y,u, y,0, col="red", lwd=2)
title(bquote(u==.(sprintf("%.3f",u))*"; "*y*"="*.(y)))



n <- 10000
pp <- dbinom(0:m, m, p)
y <- sample(0:m, n, prob=pp, replace=TRUE)
barplot(table(y))



n <- 10000
y <- rbinom(n, m, p)
barplot(table(y))



par(mfrow=c(1,2))
np <- 101
x <- seq(from=-3, to=+3, length=np)
y <- dnorm(x)
plot(x,y, type="l",main="Normal density",xlab="y",ylab="f(y)")
z <- 1
x <- seq(from=-3, to=z, length=np)
y <- dnorm(x)
polygon(c(x[1],x,x[np]),c(0,y,0),col="grey")
x <- seq(from=-3, to=+3, length=np)
y <- pnorm(x)
plot(x,y, type="l",xlab="y",ylab="F(y)")
title(bquote("F(1)=Pr(Y<"*.(z)*")="*.(round(pnorm(z),4))))
x <- c(x[1],z,z); y <- c(rep(pnorm(z),2),0)
lines(x,y,col="red")



lambda <- 0.5
np <- 101
yvec <- seq(from=0, to=10, length=np)
fvec <- lambda*exp(-lambda*yvec)
plot(yvec, fvec, type="l", 
     xlab="y", ylab="f(y)", main="Probability density of Exp(0.1)")



lambda <- 0.5
np <- 101
yvec <- seq(from=0, to=10, length=np)
cdfvec <- 1-exp(-lambda*yvec)
plot(yvec, cdfvec, type="l", 
     xlab="y", ylab="f(y)", main="CDF of Exp(0.1)")



n <- 10000
u <- runif(n)
lambda <- 0.5
y <- -(1/lambda)*log(1-u)
mean(y)
hist(y, main="Histogram of draws from Exp(0.5)")



bigN <- 10 # population size
frame <- data.frame(house=1:bigN, selected=FALSE)
n <- 5  # sample size
nselected <- 0
while(nselected < n) {
  i <- sample(bigN, 1)  # sample a single number at random from 1:bigN with equal probability
  if(!frame$selected[i]) {
    frame$selected[i] <- TRUE
    nselected <- nselected + 1 
  }
}
frame$house[frame$selected]



bigN <- 10 # population size
frame <- data.frame(house=1:bigN, selected=FALSE)
n <- 5  # sample size
frame$u <- runif(bigN)
frame <- frame[order(frame$u),]
frame$selected[1:n] <- TRUE
frame
frame <- frame[order(frame$house),]
frame$house[frame$selected]



bigN <- 10 # population size
n <- 5  # sample size
sample(bigN, n)



frame <- data.frame(Name=c("Mark","Chloe","Sam","Caitlin","Harry","Nina"),
                    Age=c(20,19,18,19,19,21),
                    Major=c("STAT","STAT","DATA","COMP","MATH","STAT"))
frame



n <- 3
samplenames <- sample(frame$Name, n)
samplenames



samplemembers <- frame[frame$Name%in%samplenames,]
samplemembers



samplemembers <- frame |> 
  filter(Name%in%samplenames)
samplemembers



match(c('Mark','Harry'),frame$Name)



set.seed(111)
bigN <- 10
n <- 5
replicate(n, sample(bigN, 1))



set.seed(122)
bigN <- 10
n <- 5
frame <- data.frame(farm=1:10, ncows=c(1000,5000,2000,500,500,
                                       5000,500,5000,30000,100))
frame
sample(frame$farm, n, prob=frame$ncows, replace=TRUE)


