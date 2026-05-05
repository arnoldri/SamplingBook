
surf <- read.csv("data/surf.csv")[,1:8]



set.seed(111)
msurf <- surf
msurf$Marital[sample(nrow(surf),10)] <- NA
msurf$Income[sample(nrow(surf),10)] <- NA



table(msurf$Marital,exclude=NULL) |> 
  as.data.frame() |>
  rename(Marital=Var1) |>
  kbl() |> kable_styling(full_width=FALSE)



library(mice)
library(lattice)



summary(msurf)



md.pattern(msurf)



mnhanes <- mice::nhanes
mnhanes$age <- factor(mnhanes$age)
mnhanes$hyp <- factor(mnhanes$hyp)
levels(mnhanes$age) <- c("20-39","40-59","60+")
levels(mnhanes$hyp) <- c("No","Yes")
mnhanes <- data.frame(id=factor(1:nrow(mnhanes)),mnhanes)



mnhanes[1:10,] |> kbl() |> kable_styling(full_width=FALSE)



md.pattern(mnhanes)



mnhanes1 <- mnhanes
mnhanes1$bmi.imp <- is.na(mnhanes1$bmi)
mnhanes1$bmi[is.na(mnhanes1$bmi)] <- mean(mnhanes1$bmi, na.rm=TRUE)



mnhanes1[1:10,] |> kbl() |> kable_styling(full_width=FALSE)



mhnanes1 <- impute(mnhanes, varname="bmi", method="mean")



mnhanes1 <- mnhanes1 |>
  impute(varname="hyp", method="mode") |>
  impute(varname="chl", method="median") 



mnhanes1[1:10,] |> kbl() |> kable_styling(full_width=FALSE)



barplot(cbind(table(mnhanes$hyp),
              table(mnhanes1$hyp)),beside=TRUE,
        legend=TRUE, args.legend=list(x="topleft",cex=1.0),
        names=c("Before imputation","After imputation"))



par(mfrow=c(1,2))
hist(mnhanes1$bmi[!mnhanes1$bmi.imp],breaks=10,xlim=c(15,40),
     main="BMI distribution\nbefore imputation", xlab="BMI (kg/m2)")
hist(mnhanes1$bmi,breaks=10,xlim=c(15,40),
     main="BMI distribution\nafter imputation", xlab="BMI (kg/m2)")



set.seed(222)
mnhanes2 <- mnhanes |> 
  impute(varname="bmi", method="sample") |>
  impute(varname="hyp", method="sample") |>
  impute(varname="chl", method="sample")



barplot(cbind(table(mnhanes$hyp),
              table(mnhanes2$hyp)),beside=TRUE,
        legend=TRUE, args.legend=list(x="topleft",cex=1.0),
        names=c("Before imputation","After imputation"))



par(mfrow=c(1,2))
hist(mnhanes2$bmi[!mnhanes2$bmi.imp],breaks=10,xlim=c(15,40),
     main="BMI distribution\nbefore imputation", xlab="BMI (kg/m2)")
hist(mnhanes2$bmi,breaks=10,xlim=c(15,40),
     main="BMI distribution\nafter imputation", xlab="BMI (kg/m2)")



set.seed(333)
mnhanes3 <- mnhanes |>
  impute(varname="bmi", method="mean", formula=~age) |>
  impute(varname="hyp", method="sample", formula=~age) |>
  impute(varname="chl", method="median", formula=~age) 



par(mfrow=c(2,2))
boxplot(bmi~age, data=mnhanes, main="Original data set")
boxplot(bmi~age, data=mnhanes1, main="Mean imputation")
boxplot(bmi~age, data=mnhanes2, main="Hot deck imputation")
boxplot(bmi~age, data=mnhanes3, main="Cell mean imputation")



plot(msurf$Hours, msurf$Income, 
     pch=ifelse(msurf$Gender=="male",16,1),
     xlab="Weekly Hours Worked", ylab="Weekly Income")
legend("topleft", pch=c(16,1), legend=c("Male","Female"))
abline(lm(Income~Hours, data=msurf[msurf$Gender=="male",]))
abline(lm(Income~Hours, data=msurf[msurf$Gender!="male",]),lty=2)



msurf1 <- msurf |>
  impute(varname="Income", method="mean") 
msurf2 <- msurf |>
  impute(varname="Income", method="lm.mean", formula=~Hours+Gender) 
msurf3 <- msurf |>
  impute(varname="Income", method="lm.sim", formula=~Hours+Gender) 



plot(msurf$Hours, msurf$Income, 
     pch=ifelse(msurf$Gender=="male",16,1),
     xlab="Weekly Hours Worked", ylab="Weekly Income")
legend("topleft", pch=c(16,1), legend=c("Male","Female"))
abline(lm(Income~Hours, data=msurf[msurf$Gender=="male",]))
abline(lm(Income~Hours, data=msurf[msurf$Gender!="male",]),lty=2)
datf <- msurf1[msurf1$Income.imp,]
points(datf$Hours, datf$Income, pch=ifelse(datf$Gender=="male",16,1),col="green")
datf <- msurf2[msurf1$Income.imp,]
points(datf$Hours, datf$Income, pch=ifelse(datf$Gender=="male",16,1),col="blue")
datf <- msurf3[msurf1$Income.imp,]
points(datf$Hours, datf$Income, pch=ifelse(datf$Gender=="male",16,1),col="red")


