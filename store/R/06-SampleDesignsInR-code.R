
#schools <- read.csv("data/nzschools-list.csv")
#names(schools)[c(22,28,39)] <- c("Takiwā","Māori_Electorate","Māori")
#  schools[schools$Regional_Council=="Area Outside Region",]
#schools <- schools[schools$Territorial_Authority!="",]
#schools <- schools[!is.na(schools$Total) & schools$Total>0,]
#schools <- schools |> 
#             mutate(Region=case_when(Territorial_Authority=="Chatham Islands Territory" ~ "Chatham Islands",
#                                 Territorial_Authority=="Rotorua District" ~ "Bay of Plenty Region",
#                                 Territorial_Authority=="Stratford District" ~ "Taranaki Region",
#                                 Territorial_Authority=="Taupo District" ~ "Waikato Region",
#                                 Territorial_Authority=="Waitaki District" ~ "Otago Region",
#                                 .default=Regional_Council))
#write.csv(schools,"data/nzschools.csv",row.names=FALSE)
#is.nested(schools, "Territorial_Authority", "Regional_Council")
#is.nested(schools, "Territorial_Authority", "Region")



mil <- read.csv("data/military-govt-gdp-2023.csv")



schools <- read.csv("data/nzschools.csv")



# Census
schools.census <- schools |> select.srswor(n=nrow(schools))
# SRSWOR
schools.srswor <- schools |> select.srswor(n=20)
# PPSWR
schools.ppswr <- schools |> select.ppswr(n=20, sizevar="Total")
# STSRS
schools.stsrs <- schools |> select.stsrs(n=60, stratumvar="Education_Region")
# 1SC
schools.1sc <- schools |> select.1sc(n=10, clustervar="Territorial_Authority")
# 2SC
schools.2sc <- schools |> select.2sc(n=10, cfraction=0.20, mmin=2, 
                                     clustervar="Territorial_Authority")
# ST2SC
schools.st2sc <- schools |> select.st2sc(n=30, stratumvar="Region", allocation="proportional",
                                cfraction=0.20, clustervar="Territorial_Authority")



ybar <- mean(schools$Total)
ybar



schools.census <- schools |> select.srswor(n=nrow(schools))



schools.census.des <- svydesign(id=~1, data=schools.census, fpc=~bigN)



svymean(~Total, design=schools.census.des, deff=TRUE)



schools.census.des <- svydesign(id=~1, data=schools.census)



svymean(~Total, design=schools.census.des, deff=TRUE)



sy <- sd(schools.census$Total)
n <- nrow(schools.census)
se <- sy/sqrt(n)



set.seed(111)



schools.srswor <- schools |> select.srswor(n=60)



schools.srswor |>
  slice_head(n=5) |>
  select(School_Id,Org_Name,Add2_City,Regional_Council,bigN,weight) |>
  kbl() |>
  kable_styling(full_width=FALSE)



schools.srswor.des <- svydesign(id=~1, data=schools.srswor, fpc=~bigN)



svymean(~Total, design=schools.srswor.des, deff=TRUE)



bigN <- first(schools.srswor$bigN) # take the entry in the first row: they're all the same
sy <- sd(schools.srswor$Total)
n <- nrow(schools.srswor)
se <- sqrt(1-n/bigN)*sy/sqrt(n)



se <- sy/sqrt(n)



schools.srswor.des <- svydesign(id=~1, data=schools.srswor)
svymean(~Total, design=schools.srswor.des, deff=TRUE)



mil |>
  slice_head(n=10) |>
  select(-Year) |>
  select(-Prob) |>
  kbl() |>
  kable_styling()



mil.ppswr <- mil |> select.ppswr(n=30, sizevar="Population")



mil.ppswr.des <- svydesign(id=~1, data=mil.ppswr, weight=~weight)



svymean(~MilSpend, design=mil.ppswr.des, deff=TRUE)



svytotal(~MilSpend, design=mil.ppswr.des, deff=TRUE)



set.seed(112)



schools.stsrs <- schools |> select.stsrs(n=60, stratumvar="Education_Region")



schools.stsrs.des <- svydesign(id=~1, strata=~Education_Region, fpc=~Nh,
                               data=schools.stsrs,
                               survey.lonely.psu="remove")



svymean(~Total, design=schools.stsrs.des, deff=TRUE)



#(SE(svymean(~Total, design=schools.stsrs.des, deff=TRUE))/
#  SE(svymean(~Total, design=schools.srswor.des, deff=TRUE)))^2



set.seed(221)



schools.1sc <- schools |> select.1sc(n=10, clustervar="Territorial_Authority")



schools.1sc.des <- svydesign(id=~Territorial_Authority, data=schools.1sc, 
                             fpc=~bigN)



svymean(~Total, design=schools.1sc.des, deff=TRUE)



set.seed(222)



schools.2sc <- schools |> select.2sc(n=10, cfraction=0.20, mmin=2, 
                                     clustervar="Territorial_Authority")



schools.2sc.des <- svydesign(id=~Territorial_Authority+School_Id, data=schools.2sc, 
                             fpc=~bigN1+bigN2,
                             survey.lonely.psu="remove")



svymean(~Total, design=schools.2sc.des, deff=TRUE)



schools.2sc.ppswr <- schools |> select.2sc(n=10, 
                                           method1="PPSWR", sizevar1="Total",
                                           cfraction=0.20, mmin=2, 
                                     clustervar="Territorial_Authority")
schools.2sc.ppswr.des <- svydesign(id=~Territorial_Authority+School_Id, data=schools.2sc.ppswr, 
                                   weight=~weight,
                                   survey.lonely.psu="remove")
svymean(~Total, design=schools.2sc.ppswr.des, deff=TRUE)



set.seed(223)



schools.st2sc <- schools |> select.st2sc(n=30, stratumvar="Region", allocation="proportional",
                                         cfraction=0.20, clustervar="Territorial_Authority")



schools.st2sc.des <- svydesign(id=~Territorial_Authority+School_Id, strata=~Region, 
                               data=schools.st2sc, 
                               fpc=~bigN1+bigN2,
                               survey.lonely.psu="remove")



svymean(~Total, design=schools.st2sc.des, deff=TRUE)


