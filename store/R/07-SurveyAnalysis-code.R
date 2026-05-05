
data(api)



apivarnames <- read.csv("data/apivarnames.csv")



apiclus2$stype <- factor(apiclus2$stype, levels=c("E","M","H"))
levels(apiclus2$stype) <- c("Elementary","Middle","High")

apiclus2 |>
  slice_head(n=10) |>
  select(cds, stype, name, dnum, cnum, api99, api00) |>
  kbl() |>
  kable_styling()



apivarnames |>
  kbl(col.names=c("Variable","Description")) |>
  kable_styling(full_width=FALSE)



apiclus2.des <- svydesign(ids=~dnum+snum, fpc=~fpc1+fpc2, data=apiclus2)



summary(apiclus2.des)



apiclus2 |>
  group_by(dnum,dname) |>
  summarise(Mi=first(fpc2),mi=n()) |>
  ungroup() |>
  arrange(desc(Mi)) |>
  slice_head(n=15) |>
  kbl(col.names=c("dnum","District Name","$M_i$","$m_i$"),escape=FALSE) |>
  kable_styling(full_width=FALSE)



svyhist(~api00, design=apiclus2.des, prob=FALSE,
        xlab="API Score (Year 2000)", 
        main="Weighted distribution of API (2000) scores")



hist(apiclus2$api00, 
        xlab="API Score (Year 2000)", 
        main="Unweighted distribution of API (2000) scores")



svyboxplot(api00~1, design=apiclus2.des, 
           ylab="API (2000)", main="API (2000) Scores")



svyboxplot(api00~stype, design=apiclus2.des, 
           xlab="School Type",
           ylab="API (2000)", main="API (2000) Scores")



svyplot(api00~api99, design=apiclus2.des, style="bubble",
        xlab="API (1999)", ylab="API (2000)")



svymean(~api00, design=apiclus2.des)



library(jtools)
svysd(~api00, design=apiclus2.des)




svymean(~api99+api00, design=apiclus2.des)



svyby(~api00, by=~stype, svymean, design=apiclus2.des)



ss <- svyby(~api00, by=~stype, svymean, design=apiclus2.des)
sc <- confint(ss)
ns <- nrow(sc)
dotchart(ss, xlim=range(sc), xlab="Mean API (2000) Score")
arrows(sc[,1], 1:ns, sc[,2], 1:ns, angle=90, code=3, length=0.1)



summary(svyglm(api00~stype, design=apiclus2.des))



summary(svyglm(api00~stype+meals, design=apiclus2.des))



svytotal(~enroll, design=apiclus2.des, na.rm=TRUE)



confint(svymean(~api99+api00, design=apiclus2.des), level=0.95)



confint(svyby(~api00, by=~stype, svymean, design=apiclus2.des))



SE(svyby(~api00, by=~stype, svymean, design=apiclus2.des))



cv(svyby(~api00, by=~stype, svymean, design=apiclus2.des))



ss <- svymean(~api00, design=apiclus2.des)
cc <- cv(ss)



cv(svymean(~api00, design=apiclus2.des))



barplot(svymean(~stype, design=apiclus2.des),
        names=levels(apiclus2$stype),
        xlab="School Type", ylab="Proportion")



svymean(~stype, design=apiclus2.des)



svytotal(~stype, design=apiclus2.des)



confint(svytotal(~stype, design=apiclus2.des))



ss <- svymean(~stype, design=apiclus2.des)
ci <- confint(ss)
bp <- barplot(ss, xlab="School Type", ylab="Estimated Size",
              names=levels(apiclus2$stype),
              ylim=c(0,max(ci)))
arrows(bp, ci[,1], bp, ci[,2], angle=90, length=0.2, code=3)



names(ss) <- levels(apiclus2$stype)
cbind(as.numeric(ss),ci) |>
  kbl(col.names=c("Estimate","2.5%","97.5%"), 
      digits=3,
      caption="Symmetric confidence interval estimates") |>
  kable_styling(full_width=FALSE)



svyciprop(~I(stype=="Elementary"), design=apiclus2.des)



ss <- svymean(~stype, design=apiclus2.des)
ci <- t(sapply(levels(apiclus2$stype),
       function(stypelevel) {
          form <- eval(parse(text=paste0("formula(~I(stype==\"",stypelevel,"\"))")))
          confint(svyciprop(form, design=apiclus2.des))    
       }))
bp <- barplot(ss, xlab="School Type", ylab="Estimated Size",
              names=levels(apiclus2$stype),
              ylim=c(0,max(ci)))
arrows(bp, ci[,1], bp, ci[,2], angle=90, length=0.2, code=3)



names(ss) <- levels(apiclus2$stype)
cbind(as.numeric(ss),ci) |>
  kbl(col.names=c("Estimate","2.5%","97.5%"), 
      digits=3,
      caption="Asymmetric confidence interval estimates") |>
  kable_styling(full_width=FALSE)



svytable(~stype+sch.wide, design=apiclus2.des)



st <- svytable(~sch.wide+stype, design=apiclus2.des)
bp <- barplot(st, beside=TRUE, col=c("light blue","dark red"),
              legend=TRUE, args.legend=list(title="Met targets"), 
              xlab="School Type", ylab="Frequency")



svychisq(~stype+sch.wide, design=apiclus2.des)



summary(svyglm(I(sch.wide=="Yes")~stype, family=quasibinomial(link="logit"), 
               design=apiclus2.des))



sr <- svyglm(I(sch.wide=="Yes")~stype, family=quasibinomial(link="logit"), 
               design=apiclus2.des)
regTermTest(sr, ~stype)



mil <- read.csv("data/military-govt-gdp-2023.csv")

gdpdata <- mil
gdpdata$GDP <- gdpdata$GDP/1e9
gdpdata$Population <- gdpdata$Population/1e6
gdptotal <- sum(gdpdata$GDP)
poptotal <- sum(gdpdata$Population)



plot(gdpdata$Population, gdpdata$GDP, 
     xlab="Population (Millions)", ylab="GDP ($Billions)")
idx <- gdpdata$Population>300
text(gdpdata$Population[idx], gdpdata$GDP[idx], lab=gdpdata$Country[idx], pos=2, cex=0.7)



set.seed(113)
gdp.sample <- gdpdata |> select.srswor(n=40)
row.names(gdp.sample) <- NULL
gdp.sample |>
  slice_head(n=10) |>
  select(Country,Population,GDP,bigN,weight) |>
  kbl() |> kable_styling(full_width=FALSE)



plot(gdp.sample$Population, gdp.sample$GDP, 
     xlab="Population (Millions)", ylab="GDP ($Billions)")
idx <- gdp.sample$Population>300
if(sum(idx)>0) text(gdp.sample$Population[idx], gdp.sample$GDP[idx], lab=gdp.sample$Country[idx], pos=2, cex=0.7)



gdp.sample.des <- svydesign(id=~1, fpc=~bigN, data=gdp.sample)



ss1 <- svytotal(~GDP, gdp.sample.des)
ss1



ci1 <- confint(ss1)
ci1



cv(ss1)



lmfit <- svyglm(GDP ~ -1 + Population, design=gdp.sample.des)
summary(lmfit)



confint(lmfit)



cv(lmfit)



ci2 <- confint(lmfit)*poptotal
ci2



plot(gdp.sample$Population, resid(lmfit),
     xlab="Population (millions)", ylab="GDP residuals ($Billion)",
     ylim=c(-1,1)*1.2*max(abs(resid(lmfit))))
abline(h=0)



ss3 <- svyratio(~GDP, ~Population, design=gdp.sample.des)
ss3



confint(ss3)



ci3 <- confint(ss3)*poptotal
ci3



cimat <- round(rbind(ci1,ci2,ci3))
sdat <- data.frame(Method=c("Simple","Regression","Ratio"),
                   Estimate=round(c(ss1,coef(lmfit)*poptotal,as.numeric(ss3)[1]*poptotal)),
                   RSE=round(c(cv(ss1),cv(lmfit),cv(ss3)),2),
                   CI=paste0("(",cimat[,1],",",cimat[,2],")"))
rownames(sdat) <- NULL
sdat |> kbl() |> kable_styling(full_width=FALSE)


