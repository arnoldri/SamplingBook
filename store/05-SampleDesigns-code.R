
knitr::include_graphics("include-files/fig/Census.png")



choose(87,20)



bigN <- nrow(sframe)
samp <- sframe[sample(bigN,n),]
samp$weight <- bigN/n



bigN <- nrow(sframe)
IDset <- sample(sframe$ID,n)
samp <- sframe[sframe$ID %in% IDset,]
samp$weight <- bigN/n



select.srswor <- function(df, n) {
  # SRSWOR
  bigN <- nrow(df)
  if(n<=bigN) {
    samp <- df[sample(bigN,n,replace=FALSE),]
    samp$bigN <- bigN
    samp$weight <- bigN/n
  } else {
    stop("Cannot have a SRSWOR with n>N")
  }
  return(samp)
} 



knitr::include_graphics("include-files/fig/SRS.png")



airports <- read.csv("data/airports_w_headers.csv",encoding="UTF8")
names(airports) <- c("ID","Name","City","Country","IATA","ICAO",
                     "Latitude","Longitude","Altitude","TimeZoneHours","DST","TimeZoneName","Type","Source")
airports <- airports[order(airports$IATA),]
airports <- airports[airports$IATA!="",]
airports$ID <- 1:nrow(airports)
row.names(airports) <- NULL



airports[1:10,c("ID","IATA","Name","City","Country","Latitude","Longitude")] |>
  kbl(row.names=FALSE) |>
  kable_styling()  



samp <- airports |> select.srswor(n=5)



samp[,c("ID","IATA","Name","City","Country","Latitude","Longitude","bigN","weight")] |>
  kbl(caption="Sample of airports (SRSWOR)",row.names=FALSE) |>
  kable_styling()



bigN <- nrow(airports)
n <- 5
samp <- airports[sample(bigN,n,replace=TRUE),]
samp$weight <- bigN/n
samp <- samp |>
  group_by(ID) |>
  summarise(across(-weight, first), nsampled=n(), weight=sum(weight))



select.srswr <- function(df, n) {
  # SRSWR
  bigN <- nrow(df)
  df$rownames <- 1:bigN
  samp <- df[sample(bigN,n,replace=TRUE),]
  samp$bigN <- bigN
  samp$weight <- bigN/n
  samp <- samp |>
    group_by(rownames) |>
    summarise(across(-weight, first), nsampled=n(), weight=sum(weight)) |>
    as.data.frame() |>
    ungroup() |>
    select(-rownames)
  return(samp)
}



samp <- airports |> select.srswr(n=5)



samp[,c("ID","IATA","Name","City","Country","Latitude","Longitude","nsampled","weight")] |>
  kbl(caption="Sample of airports (SRSWOR)") |>
  kable_styling()



pop <- read.csv("data/population.csv")
pop <- pop |> filter(Year==2023) |> 
  rename(Country=Entity, Population=all.years) |>
  arrange(Country) 
pop <- pop[!grepl("^OWID_",pop$Code),]
pop <- pop[!grepl("^UN_",pop$Code),]
pop <- pop[pop$Code!="",]
pop$Prob <- pop$Population/sum(pop$Population)
row.names(pop) <- NULL
bigN <- nrow(pop)



pop[1:10,] |>
  kbl() |>
  kable_styling()



set.seed(111)
bigN <- nrow(pop)
n <- 10
samp <- pop[sample(bigN,n,prob=pop$Prob,replace=TRUE),]
samp$weight <- 1/(n*samp$Prob)
samp <- samp |>
  group_by(Country) |>
  summarise(across(-weight, first), nsampled=n(), weight=sum(weight))



select.ppswr <- function(df, n, sizevar) {
  # PPSWR
  bigN <- nrow(df)
  df$rownames <- 1:bigN
  samp <- df[sample(bigN,n,prob=df[,sizevar],replace=TRUE),]
  samp$bigN <- bigN
  samp$weight <- bigN/n
  samp <- samp |>
    group_by(rownames) |>
    summarise(across(-weight, first), nsampled=n(), weight=sum(weight)) |>
    as.data.frame() |>
    ungroup() |>
    select(-rownames)
  return(samp)
}



set.seed(111)



samp <- pop |> select.ppswr(n=10, sizevar="Population")



samp |>
  kbl() |>
  kable_styling()



knitr::include_graphics("include-files/fig/LSRS.png")



bigN <- nrow(airports)
airports <- airports[order(airports$Latitude),]
n <- 5
k <- floor(bigN/n)
cat("Skip size is k =",k,"\n")
i0 <- sample(k,1) # first selected unit
cat("Starting sampling at unit",i0,"\n")
n <- (bigN-i0)%/%k + 1 # recalculate the sample size
cat("Actual sample size is",n,"\n")
samp <- airports[i0 + (0:(n-1))*k,]
samp$weight <- bigN/n



select.lsrs <- function(df, n) {
  # LSRS
  bigN <- nrow(df)
  k <- floor(bigN/n)
  i0 <- sample(k,1) # first selected unit
  n <- (bigN-i0)%/%k + 1 # recalculate the sample size
  samp <- df[i0 + (0:(n-1))*k,]
  samp$bigN <- bigN
  samp$weight <- bigN/n
  return(samp)
}

samp <- airports |> select.lsrs(5)



rownames(samp) <- NULL
samp[,c("ID","IATA","Name","City","Country","Latitude","Longitude","weight")] |>
  kbl(caption="Sample of airports (SRSWOR)") |>
  kable_styling()



knitr::include_graphics("include-files/fig/STSRS.png")



schools <- read.csv("data/nzschools.csv")
bigN <- nrow(schools)



stratumproperties <- schools |> 
  group_by(Education_Region) |>
  summarise(Nh=n()) |>
  arrange(Education_Region) |>
  as.data.frame() 
stratumproperties$h <- 1:nrow(stratumproperties)
stratumproperties$Fh <- stratumproperties$Nh/sum(stratumproperties$Nh)
stratumproperties |>
  select(h,Education_Region,Nh,Fh) |>
  mutate(Fh=sprintf("%.4f",Fh)) |>
  kbl(col.names=c("$h$","Education Region","$N_h$","$F_h$"),escape=FALSE) |>
  kable_styling(full_width=FALSE)



select.stsrs <- function(df, n, stratumvar, 
                         allocation="equal",
                         nmin=2, # minimum number of units to select per stratum 
                         method="SRSWOR", sizevar=NULL) {
  bigN <- nrow(df)
  if(!stratumvar%in%names(df)) {
    stop("Stratum variable is not present in the data frame")
  }
  stratumproperties <- as.data.frame(table(df[,stratumvar]))
  names(stratumproperties) <- c(stratumvar,"Nh")
  if(allocation=="equal") {
    stratumproperties$nh <- pmin(stratumproperties$Nh,pmax(nmin,
                                 round(n/nrow(stratumproperties))))
  } else if(allocation=="proportional") {
    stratumproperties$nh <- pmin(stratumproperties$Nh,pmax(nmin,                                                                                   round(n*stratumproperties$Nh/sum(stratumproperties$Nh))))
  } else {
    stop("Invalid allocation")
  }
  df <- merge(df,stratumproperties,by=stratumvar)

  if(method=="SRSWOR") {
    samp <- do.call(rbind, by(df, df[,stratumvar], 
                              function(dfx) select.srswor(dfx,first(dfx$nh))))
  } else if(method=="PPSWR") {
    samp <- do.call(rbind, by(df, df[,stratumvar], 
                              function(dfx) select.ppswr(dfx,first(dfx$nh),sizevar=sizevar)))
  } else {
    stop("Sampling method invalid")
  }
  return(samp)
}



samp <- schools |> select.stsrs(n=120,  
                                stratumvar="Education_Region", allocation="equal")



samp |>
  group_by(Education_Region) |>
  arrange(School_Id) |>
  summarise(SampleSize=n(), Nh=first(Nh), weight=first(weight),
            SelectedSchools=paste(School_Id,collapse=";")) |>
  kbl(col.names=c("Education Region, $h$","$N_h$","$n_h$","Weight, $w_h$","Selected Schools"),escape=FALSE) |>
  kable_styling(full_width=FALSE)



samp <- schools |> select.stsrs(n=120, 
                                stratumvar="Education_Region", 
                                allocation="proportional", sizevar="Total")



samp |>
  group_by(Education_Region) |>
  arrange(School_Id) |>
  summarise(SampleSize=n(), Nh=first(Nh), weight=first(weight),
            SelectedSchools=paste(School_Id,collapse=";")) |>
  kbl(col.names=c("Education Region, $h$","$N_h$","$n_h$","Weight, $w_h$","Selected Schools"),escape=FALSE) |>
  kable_styling(full_width=FALSE)



samp <- schools |> select.stsrs(n=120,  
                                stratumvar="Education_Region", 
                                allocation="equal", 
                                method="PPSWR", sizevar="Total")



samp |>
  group_by(Education_Region) |>
  arrange(School_Id) |>
  summarise(SampleSize=n(), Nh=first(Nh), 
            SelectedSchools=paste(paste0(School_Id,ifelse(nsampled>1,"*","")),collapse=";")) |>
  kbl(col.names=c("Education Region, $h$","$N_h$","$n_h$","Selected Schools"),escape=FALSE) |>
  kable_styling(full_width=FALSE)



knitr::include_graphics("include-files/fig/SRS1SC.png")



select.1sc <- function(df, n, clustervar, 
                       method="SRSWOR", sizevar=NULL) {
  bigM <- nrow(df)
  if(!clustervar%in%names(df)) {
    stop("Cluster ID variable is not present in the data frame")
  }
  clusterproperties <- as.data.frame(table(df[,clustervar]))
  names(clusterproperties) <- c(clustervar,"Mi")
  if(method=="SRSWOR") {
     sampcluster <- clusterproperties |> 
                       select.srswor(n=n)
  } else if(method=="PPSWR") {
     sampcluster <- clusterproperties |>
                       select.ppswr(n=n, sizevar="Mi")
  } else {
    stop("Sampling method invalid")
  }
  samp <- merge(df,sampcluster,by=clustervar)

  return(samp)
}



samp <- schools |> select.1sc(n=10, clustervar="Territorial_Authority")



samp |>
  group_by(Territorial_Authority) |>
  arrange(School_Id) |>
  summarise(Mi=n(), 
            SelectedSchools=paste(paste0(School_Id[1:min(n(),8)],collapse=";"),ifelse(n()>8,"...",""))) |>
  kbl(col.names=c("Territorial Authority, $i$","$M_i$","Selected Schools"),escape=FALSE) |>
  kable_styling(full_width=FALSE)



knitr::include_graphics("include-files/fig/SRS2SC.png")



select.2sc <- function(df, n,  # number of first stage clusters to select 
                       cfraction, clustervar, 
                       mmin=2, # minimum number of 2nd stage clusters to select
                       method1="SRSWOR", sizevar1=NULL,
                       method2="SRSWOR", sizevar2=NULL) {
  bigM <- nrow(df)
  if(!clustervar%in%names(df)) {
    stop("Cluster ID variable is not present in the data frame")
  }
  clusterproperties <- as.data.frame(table(df[,clustervar]))
  bigN <- nrow(clusterproperties)
  names(clusterproperties) <- c(clustervar,"Mi")
  if(method1=="SRSWOR") {
     sampcluster <- clusterproperties |> 
                       select.srswor(n=n)
  } else if(method1=="PPSWR") {
     sampcluster <- clusterproperties |>
                       select.ppswr(n=n, sizevar="Mi")
  } else {
    stop("First stage sampling method invalid")
  }
  sampcluster$mi <- pmin(sampcluster$Mi,pmax(mmin,round(cfraction*sampcluster$Mi)))
  samp1 <- merge(df,sampcluster,by=clustervar)
  samp1 <- samp1 |> rename(bigN1=bigN,weight1=weight)
  samp1$n1 <- n

  # Second stage selection
  if(method2=="SRSWOR") {
    samp2 <- do.call(rbind, 
                     by(samp1, samp1[,clustervar],
                        function(ds) select.srswor(ds,n=first(ds$mi))
                               ))
  } else if(method2=="PPSWR") {
    samp2 <- do.call(rbind, 
                     by(samp2, samp2[,clustervar],
                        function(ds) select.ppswr(ds,n=first(ds$mi),sizevar=sizevar2)
                               ))
  } else {
    stop("Second stage sampling method invalid")
  }
  samp2 <- samp2 |> rename(bigN2=bigN,weight2=weight)
  samp2$n2 <- samp2$mi
  samp2$weight <- samp2$weight1*samp2$weight2

  return(samp2)
}



samp <- schools |> select.2sc(n=10, cfraction=0.20, mmin=2, 
                              clustervar="Territorial_Authority")



samp |>
  group_by(Territorial_Authority) |>
  arrange(School_Id) |>
  summarise(N=first(bigN1),n=first(n1),Mi=first(bigN2),mi=n(), 
            weight1=first(weight1),weight2=first(weight2),weight=first(weight),
            SelectedSchools=paste(paste0(School_Id[1:min(n(),4)],collapse=";"),ifelse(n()>4,"...",""))) |>
  ungroup() |>
  mutate(weight1=sprintf("%.2f",weight1)) |>
  mutate(weight2=sprintf("%.2f",weight2)) |>
  mutate(weight=sprintf("%.2f",weight)) |>
  kbl(col.names=c("Territorial Authority, $i$",
                  "$N$","$n$","$M_i$","$m_i$",
                  "$w^{(1)}_i$","$w^{(2)}_{ij}$","Weight, $w_{ij}$",
                  "Selected Schools"),escape=FALSE) |>
  kable_styling(full_width=FALSE)



select.st2sc <- function(df, 
                         n,      # total number of 1st stage clusters to select over all strata
                         stratumvar, allocation="equal",
                         nmin=2, # minimum number of 1st stage clusters to select per stratum
                         cfraction, clustervar, 
                         mmin=2, # minimum number of 2nd stage clusters to select
                         method1="SRSWOR", sizevar1=NULL,
                         method2="SRSWOR", sizevar2=NULL) {

  # stratum selection  
  bigN <- nrow(df)
  if(!stratumvar%in%names(df)) {
    stop("Stratum variable is not present in the data frame")
  }
  if(!clustervar%in%names(df)) {
    stop("Clustring variable is not present in the data frame")
  }
  xx <- tapply(df[,clustervar], df[,stratumvar], function(x) length(unique(x)))
  stratumproperties <- data.frame(Var1=names(xx),Freq=xx)
  names(stratumproperties) <- c(stratumvar,"Nh")
  rownames(stratumproperties) <- NULL
  if(allocation=="equal") {
    stratumproperties$nh <- pmin(stratumproperties$Nh,pmax(nmin,
                                 round(n/nrow(stratumproperties))))
  } else if(allocation=="proportional") {
    stratumproperties$nh <- pmin(stratumproperties$Nh,pmax(nmin,
                                 round(n*stratumproperties$Nh/sum(stratumproperties$Nh))))
  } else {
    stop("Invalid allocation")
  }
  df <- merge(df,stratumproperties,by=stratumvar)
  
  samp <- do.call(rbind, by(df, df[,stratumvar], 
                            function(dfx) {
                               select.2sc(dfx, n=first(dfx$nh), 
                                          cfraction=cfraction, 
                                          clustervar=clustervar, 
                                          mmin=mmin, 
                                          method1=method1, sizevar1=sizevar1,
                                          method2=method2, sizevar2=sizevar2)
                            }))
  return(samp)
}



samp <- schools |> select.st2sc(n=30, stratumvar="Region", allocation="proportional",
                                cfraction=0.20, clustervar="Territorial_Authority")



samp |>
  group_by(Region,Territorial_Authority) |>
  arrange(School_Id) |>
  summarise(N=first(bigN1),n=first(n1),Mi=first(bigN2),mi=n(), 
            weight1=first(weight1),weight2=first(weight2),weight=first(weight),
            SelectedSchools=paste(paste0(School_Id[1:min(n(),4)],collapse=";"),ifelse(n()>4,"...",""))) |>
  ungroup() |>
  slice_head(n=10) |>
  mutate(weight1=sprintf("%.2f",weight1)) |>
  mutate(weight2=sprintf("%.2f",weight2)) |>
  mutate(weight=sprintf("%.2f",weight)) |>
  kbl(col.names=c("Region, $h$","Territorial Authority, $i$",
                  "$N_h$","$n_h$","$M_{hi}$","$m_{hi}$",
                  "$w^{(1)}_i$","$w^{(2)}_{ij}$","Weight, $w_{ij}$",
                  "Selected Schools"),escape=FALSE) |>
  kable_styling(full_width=FALSE)


