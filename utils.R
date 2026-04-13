is.unique <- function(x) length(x)==length(unique(x))

is.nested <- function(df, x, y) {
  # do all the levels of x nest within levels of y?
  all(tapply(df[,y], df[,x], function(yvals) length(unique(yvals)))==1)
}

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

select.2sc <- function(df, n, # number of first stage clusters to select
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

