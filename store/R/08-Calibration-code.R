
nzpop <- read.csv("data/year-age-sex.csv")
nzpop <- nzpop |>
  filter(Year==2018)
nzpop <- nzpop[nzpop$Age>=15 & nzpop$Age<=45,]
idx <- nzpop$Age==45
nzpop$count[idx] <- (1/5)*nzpop$count[idx]
nzpop <- nzpop |>
  mutate(AgeGrp=case_when(Age<=20 ~ "15-24",
                          Age<=30 ~ "25-34",
                          Age<=45 ~ "35-45"))
nzpop <- nzpop |>
  mutate(count=round(count)) |>
  group_by(AgeGrp,Sex) |>
  summarise(count=sum(count)) |>
  as.data.frame()
nzpop <- nzpop |> rename(Gender=Sex)
nzpop$Gender <- factor(nzpop$Gender,levels=c("Male","Female"))
levels(nzpop$Gender) <- tolower(levels(nzpop$Gender))
total <- sum(nzpop$count)
total.Gender <- tapply(nzpop$count, nzpop$Gender, sum)
total.AgeGrp <- tapply(nzpop$count, nzpop$AgeGrp, sum)
total.AgeGrp <- data.frame(AgeGrp=names(total.AgeGrp),Nh=total.AgeGrp)



surf <- read.csv("data/surf.csv")[,1:8]
bigN <- total
n <- nrow(surf)
surf$Gender <- factor(surf$Gender, levels=c("male","female"))
surf <- surf |>
  mutate(AgeGrp=case_when(Age<=24 ~ "15-24",
                          Age<=34 ~ "25-34",
                          Age<=45 ~ "35-45"))
surf$bigN <- bigN
surf$weight <- bigN/n
n.Gender <- tapply(surf$Gender, surf$Gender, length)
sampweight <- bigN/n



surf |> slice_head(n=10) |>
  kbl() |>
  kable_styling(full_width=FALSE)



surf.des <- svydesign(id=~1, fpc=~bigN, data=surf)



svymean(~Income, design=surf.des)



svyby(~Income, by=~Gender, design=surf.des, svymean)



svyboxplot(Income~Gender, design=surf.des, main="Income by Gender", ylab="Weekly income")



nzpoptab <- nzpop |> 
  arrange(Gender) |>
  pivot_wider(names_from="Gender", values_from="count") |>
  as.data.frame()
nzpoptab$Total <- apply(nzpoptab[,-1],1,sum)
nzpoptab <- rbind(nzpoptab, 
                  data.frame(AgeGrp="Total", rbind(apply(as.matrix(nzpoptab[,-1]),2,sum))))
nzpoptab.pct <- as.data.frame(lapply(nzpoptab,
  function(x) {
    if(is.numeric(x)) return(sprintf("%.1f%%",100*x/total)) else (return(x))
  }))
nzpoptab |>
  kbl() |>
  kable_styling(full_width=FALSE)



nzpoptab.pct |>
  kbl() |>
  kable_styling(full_width=FALSE)



svymean(~Gender, design=surf.des)



table(surf$Gender)



pstab <- data.frame(Gender=c("male","female"),
                    SampWeight=sampweight,
                    nh=n.Gender,
                    Nh=total.Gender,
                    PostStratWeight=total.Gender/n.Gender)
rownames(pstab) <- NULL
pstab |>
  kbl() |>
  kable_styling(full_width=FALSE)



Gender.benchmarks <- pstab[,c("Gender","Nh")]



Gender.benchmarks |> kbl() |> kable_styling(full_width=FALSE)



surf.des.ps <- postStratify(surf.des, strata=~Gender, population=Gender.benchmarks)



svymean(~Gender, design=surf.des)



svymean(~Gender, design=surf.des.ps)



svymean(~Income, surf.des)



svymean(~Income, surf.des.ps)



set.seed(234)
surf.nonresp <- surf
surf.nonresp$Responds <- rbinom(nrow(surf),1,prob=ifelse(surf$Gender=="male",0.8,1.0))
surf.nonresp <- surf.nonresp[surf.nonresp$Responds==1,]
surf.nonresp$weight <- bigN/nrow(surf.nonresp)
table(surf.nonresp$Gender)



surf.nonresp.des <- svydesign(id=~1, fpc=~bigN, data=surf.nonresp)
svymean(~Income, design=surf.nonresp.des)



surf.nonresp.des.ps <- postStratify(surf.nonresp.des, strata=~Gender, 
                                    population=Gender.benchmarks)



svymean(~Income, design=surf.nonresp.des.ps)



AgeGrp.Gender.benchmarks <- nzpop



AgeGrp.Gender.benchmarks |> kbl() |> kable_styling(full_width=FALSE)



surf.nonresp.des.ps2 <- postStratify(surf.nonresp.des, strata=~AgeGrp*Gender, 
                                     population=AgeGrp.Gender.benchmarks)
svymean(~Income, design=surf.nonresp.des.ps2)



AgeGrp.benchmarks <- total.AgeGrp



AgeGrp.benchmarks |> kbl() |> kable_styling(full_width=FALSE)



Gender.benchmarks |> kbl() |> kable_styling(full_width=FALSE)



surf.nonresp.des.rake <- rake(surf.nonresp.des, 
                              sample=list(~AgeGrp, ~Gender), 
                              population=list(AgeGrp.benchmarks, Gender.benchmarks))



svymean(~Income, design=surf.nonresp.des.rake)



nzpoptab |> kbl() |> kable_styling(full_width=FALSE)



svyby(~Gender, by=~AgeGrp, FUN=svytotal, design=surf.nonresp.des) |>
  kbl(col.names=c("AgeGrp","male","female","SE(male)","SE(female)")) |> 
  kable_styling(full_width=FALSE)



svyby(~Gender, by=~AgeGrp, FUN=svytotal, design=surf.nonresp.des.ps)  |>
  kbl(col.names=c("AgeGrp","male","female","SE(male)","SE(female)")) |> 
  kable_styling(full_width=FALSE)



svyby(~Gender, by=~AgeGrp, FUN=svytotal, design=surf.nonresp.des.ps2)  |>
  kbl(col.names=c("AgeGrp","male","female","SE(male)","SE(female)")) |> 
  kable_styling(full_width=FALSE)



svyby(~Gender, by=~AgeGrp, FUN=svytotal, design=surf.nonresp.des.rake)  |>
  kbl(col.names=c("AgeGrp","male","female","SE(male)","SE(female)")) |> 
  kable_styling(full_width=FALSE)



ff <- function(x) c(as.numeric(x), SE(x))



dd <- data.frame(Method=c("True","No adjustment","PS(Gender)","PS(AgeGrp*Gender)","Rake(Age+Gender)"),
                 rbind(c(mean(surf$Income), 0),
                       ff(svymean(~Income, design=surf.nonresp.des)),
                       ff(svymean(~Income, design=surf.nonresp.des.ps)),
                       ff(svymean(~Income, design=surf.nonresp.des.ps2)),
                       ff(svymean(~Income, design=surf.nonresp.des.rake))))
dd |>
  rename(Estimate=X1, SE=X2) |> 
  kbl() |> kable_styling(full_width=FALSE)



par(mar=c(5.1,8.1,4.1,2.1))
sc <- cbind(dd$X1-1.96*dd$X2, dd$X1+1.96*dd$X2)
ns <- nrow(sc)
dotchart(dd$X1, xlim=range(sc), xlab="Mean Income")
arrows(sc[,1], 1:ns, sc[,2], 1:ns, angle=90, code=3, length=0.1)
axis(2, at=1:ns, lab=dd$Method, las=2, cex=0.45)



apipopc <- apipop[!is.na(apipop$enroll),]
apipopc$stype <- factor(as.character(apipopc$stype),levels=c("E","M","H"))
levels(apipopc$stype) <- c("Elementary","Middle","High")
apipopc$yr.rnd[is.na(apipopc$yr.rnd)] <- "Yes"



stype.benchmarks <- as.data.frame(table(apipopc$stype))
names(stype.benchmarks) <- c("stype","Freq")
stype.benchmarks |> kbl() |> kable_styling(full_width=FALSE)
#sum(stype.benchmarks$Freq) # 6157



yr.rnd.benchmarks <- as.data.frame(table(apipopc$yr.rnd))
names(yr.rnd.benchmarks) <- c("yr.rnd","Freq")
yr.rnd.benchmarks |> kbl() |> kable_styling(full_width=FALSE)
#sum(yr.rnd.benchmarks$Freq) # 6157



stype.yr.rnd.benchmarks <- as.data.frame(table(apipopc$stype, apipopc$yr.rnd))
names(stype.yr.rnd.benchmarks) <- c("stype","yr.rnd","Freq")
stype.yr.rnd.benchmarks |> kbl() |> kable_styling(full_width=FALSE)



set.seed(111)
api2sc <- apipopc |> 
  select.2sc(n=40, cfraction=0.30, mmin=2, clustervar="dnum") 
api2sc.des <- svydesign(ids=~dnum+snum, fpc=~bigN1+bigN2, data=api2sc) 
table(api2sc$stype)/nrow(api2sc)
svytable(~stype, design=api2sc.des)
svytable(~stype, design=api2sc.des)/sum(api2sc$weight)
svymean(~enroll, design=api2sc.des)
svyby(~enroll, by=~stype, api2sc.des, svymean)



api2sc.des.ps <- postStratify(api2sc.des, strata=~stype, population=stype.benchmarks)
svymean(~enroll, api2sc.des.ps)



api2sc.des.ps2 <- postStratify(api2sc.des, strata=~stype+yr.rnd, 
                               population=stype.yr.rnd.benchmarks)
svymean(~enroll, api2sc.des.ps2)



api2sc.des.rake <- rake(api2sc.des, 
                        sample=list(~stype, ~yr.rnd), 
                        population=list(stype.benchmarks, yr.rnd.benchmarks))
svymean(~enroll, api2sc.des.rake)



ff <- function(x) c(as.numeric(x), SE(x))



ss <- rbind(
  c(mean(apipopc$enroll), 0),
  ff(svymean(~enroll, api2sc.des)), 
  ff(svymean(~enroll, api2sc.des.ps)), 
  ff(svymean(~enroll, api2sc.des.ps2)), 
  ff(svymean(~enroll, api2sc.des.rake))) 
dimnames(ss) <- list(c("Cenus","2SC","2SC+PS(stype)","2SC+PS(stype x yr.rnd)","2SC+Rake(stype+yr.rnd)"),
                     c("Estimate","SE"))
ss |> kbl() |> kable_styling(full_width=FALSE)



knitr::include_graphics('include-files/fig/SamplingFromPopulation.png')



knitr::include_graphics('include-files/fig/PseudoSampling.png')



exdat <- data.frame(id=1:12, psu=rep(LETTERS[1:4],c(3,4,2,3)),
                    weight=rep(c(100,80,120,100),c(3,4,2,3)))
ss <- sum(exdat$weight)
exdat$rep01 <- rep(c(0,1,1,1),c(3,4,2,3))
exdat$rep02 <- rep(c(1,0,1,1),c(3,4,2,3))
exdat$rep03 <- rep(c(1,1,0,1),c(3,4,2,3))
exdat$rep04 <- rep(c(1,1,1,0),c(3,4,2,3))
exdat$rep01 <- exdat$rep01*exdat$weight*ss/sum(exdat$rep01*exdat$weight)
exdat$rep02 <- exdat$rep02*exdat$weight*ss/sum(exdat$rep02*exdat$weight)
exdat$rep03 <- exdat$rep03*exdat$weight*ss/sum(exdat$rep03*exdat$weight)
exdat$rep04 <- exdat$rep04*exdat$weight*ss/sum(exdat$rep04*exdat$weight)
exdat$col01 <- ifelse(exdat$rep01==0, "#FDD","")
exdat$col02 <- ifelse(exdat$rep02==0, "#FDD","")
exdat$col03 <- ifelse(exdat$rep03==0, "#FDD","")
exdat$col04 <- ifelse(exdat$rep04==0, "#FDD","")



#exdat$rep01 <- cell_spec(exdat$rep01, color = ifelse(exdat$rep01==0, "light blue", ""))
exdat <- rbind(exdat,
               data.frame(id="Total",
                          psu="",
                          weight=sum(exdat$weight),
                          rep01=sum(exdat$rep01),
                          rep02=sum(exdat$rep01),
                          rep03=sum(exdat$rep01),
                          rep04=sum(exdat$rep01),
                          col01="",col02="",col03="",col04=""
                          ))
tab.caption <- "Example of Jacknife replicate weights: a sample of 4 clusters (PSUs) with sets of replicate weights, each one deleting the weight of a single PSU, and rescaling to maintain the total of the weights."
if(knitr::is_latex_output()) {
  exdat |> 
    select(id:rep04) |>
    mutate(across(rep01:rep04, \(x) round(x, digits=1))) |> 
    kbl(caption=tab.caption, escape=FALSE) |> 
    kable_styling(full_width=FALSE) 
} else {
  exdat |> 
    select(id:rep04) |>
    mutate(across(rep01:rep04, \(x) round(x, digits=1))) |> 
    kbl(caption=tab.caption, escape=FALSE) |> 
    kable_styling(full_width=FALSE) |>
    column_spec(3+0, background="#DDF") |>
    column_spec(3+1, background=exdat$col01) |>
    column_spec(3+2, background=exdat$col02) |>
    column_spec(3+3, background=exdat$col03) |>
    column_spec(3+4, background=exdat$col04) 
}



api2sc.jkdes <- as.svrepdesign(api2sc.des, type="JK1")



api2sc.jkdes$repweights$weights[1:5,1:8]



svymean(~enroll, design=api2sc.jkdes)



svymean(~enroll, design=api2sc.des)



api2sc.jkdes.ps <- postStratify(api2sc.jkdes, strata=~stype, population=stype.benchmarks)
svymean(~enroll, design=api2sc.jkdes.ps)



ss <- rbind(
  c(mean(apipopc$enroll), 0),
  ff(svymean(~enroll, api2sc.des)), 
  ff(svymean(~enroll, api2sc.des.ps)), 
  ff(svymean(~enroll, api2sc.des.ps2)), 
  ff(svymean(~enroll, api2sc.des.rake)),
  ff(svymean(~enroll, api2sc.jkdes)),
  ff(svymean(~enroll, api2sc.jkdes.ps))
  ) 
dimnames(ss) <- list(c("Cenus","2SC","2SC+PS(stype)","2SC+PS(stype x yr.rnd)","2SC+Rake(stype+yr.rnd)",
                       "JK1","JK1+PS(stype)"),
                     c("Estimate","SE"))
ss |> kbl() |> kable_styling(full_width=FALSE)



api2sc.jkdes.ps$repweights$weights[1:5,1:8]



fwmat <- api2sc.jkdes.ps$repweights$weights[api2sc.jkdes.ps$repweights$index,]
colnames(fwmat) <- paste0("repweight",sprintf("%0.3d",1:ncol(fwmat)))
pweight <- api2sc.jkdes.ps$pweights
fwmat <- diag(pweight)%*%fwmat
api2sc.withrep.ps <- cbind(api2sc, pweight, fwmat)
row.names(api2sc.withrep.ps) <- NULL
api2sc.withrep.ps[1:10 ,c(1,47,48:55),] |> kbl() |> kable_styling(full_width=FALSE)



pweightcol <- match("pweight",names(api2sc.withrep.ps)) # which column name is exactly equal to pweight
repweightcols <- grep("^repweight",names(api2sc.withrep.ps)) # which columnnames start with repweight?
varcols <- (1:ncol(api2sc.withrep.ps))
varcols <- varcols[!(varcols%in%c(pweightcol,repweightcols))]
n <- length(unique(api2sc$dnum))
api2sc.repdes.ps <- svrepdesign(variables=api2sc.withrep.ps[,varcols],
                                weights=api2sc.withrep.ps[,pweightcol],
                                repweights=api2sc.withrep.ps[,repweightcols],
                                combined.weights=TRUE,
                                scale=(n-1)/n,
                                type="JK1")
svymean(~enroll, api2sc.repdes.ps)



ss <- rbind(
  c(mean(apipopc$enroll), 0),
  ff(svymean(~enroll, api2sc.des)), 
  ff(svymean(~enroll, api2sc.des.ps)), 
  ff(svymean(~enroll, api2sc.des.ps2)), 
  ff(svymean(~enroll, api2sc.des.rake)),
  ff(svymean(~enroll, api2sc.jkdes)),
  ff(svymean(~enroll, api2sc.jkdes.ps)),
  ff(svymean(~enroll, api2sc.repdes.ps))
  ) 
dimnames(ss) <- list(c("Cenus","2SC","2SC+PS(stype)","2SC+PS(stype x yr.rnd)","2SC+Rake(stype+yr.rnd)",
                       "JK1","JK1+PS(stype)","JK1+PS(stype); weights only"),
                     c("Estimate","SE"))
ss |> kbl() |> kable_styling(full_width=FALSE)



set.seed(999)
apist2sc <- apipopc |>
  select.st2sc(n=40, stratumvar="awards", cfraction=0.20, clustervar="dnum")
apist2sc.des <- svydesign(id=~dnum+snum, strata=~awards, fpc=~bigN1+bigN2, 
                          data=apist2sc, nest=TRUE)
apist2sc.des.ps <- postStratify(apist2sc.des, strata=~stype, population=stype.benchmarks)



svymean(~enroll, design=apist2sc.des)
svymean(~enroll, design=apist2sc.des.ps)
apist2sc.jkdes <- as.svrepdesign(apist2sc.des, type="JKn")
svymean(~enroll, design=apist2sc.jkdes)
apist2sc.jkdes.ps <- postStratify(apist2sc.jkdes, strata=~stype, population=stype.benchmarks)
svymean(~enroll, design=apist2sc.jkdes.ps)


