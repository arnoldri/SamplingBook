##############################################################
# Useful functions

is.unique <- function(x) {
  # are the elements of x all distinct?
  length(x)==length(unique(x))
}

is.nested <- function(df, x, y) {
  # do all the levels of x nest within levels of y?
  all(tapply(df[,y], df[,x], function(yvals) length(unique(yvals)))==1)
}

modevalue <- function(x) {
  # mode of vector x
  # if the vector has multiple modes it returns the first
  xvals <- unique(x)
  xvals[which.max(tabulate(match(x, xvals)))]
}


##############################################################
# Drawing samples from a data frame df

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
  # Stratified SRS
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
  # One stage cluster design with SRSWOR or PPSWR
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
  # Two stage cluster design with SRSWOR or PPSWR at each stage
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

select.st2sc <- function(df, 
                         n,      # total number of 1st stage clusters to select over all strata
                         stratumvar, allocation="equal",
                         nmin=2, # minimum number of 1st stage clusters to select per stratum
                         cfraction, clustervar, 
                         mmin=2, # minimum number of 2nd stage clusters to select
                         method1="SRSWOR", sizevar1=NULL,
                         method2="SRSWOR", sizevar2=NULL) {
  # Stratified two stage cluster design with SRSWOR or PPSWR at each stage
  
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

#####################################################################
# Impute variable varname in data frame df
# Methods are
# sample: hot deck imputation
#   if a formula is supplied then this creates cells 
# mean, median, mode: mean, median or mode imputation
#   if a formula is supplied then this creates cells 
# lm.mean: linear regression imputation
#   a formula *must* be supplied: should be the RHS of the regression model
#   formula.  Imputes the mean value predicted by the model
# lm.sim: linear regression imputation with added error
#   a formula *must* be supplied: should be the RHS of the regression model
#   formula.  Imputes the mean value predicted by the model with added error

impute <- function(df, varname, 
                   method=NULL,
                   formula=NULL, 
                   impute.suffix=".imp") {
  missing <- is.na(df[,varname])
  df[,paste0(varname,impute.suffix)] <- missing
  
  if(method=="sample") {
    # Hot Deck imputation - method is the same for any type of variable
    if(is.null(formula)) {
      # overall
      df[missing,varname] <- sample(df[!missing,varname],sum(missing),replace=TRUE)
      
    } else if(!is.null(formula)) {
      # in cells defined by the formula
      mmat <- model.matrix(formula, data=df)
      df$.tmp.rownames <- 1:nrow(df)
      df$.tmp.cellnames <- apply(mmat,1,paste,collapse=";")
      df <- do.call(rbind, by(df, df$.tmp.cellnames,
                              function(dfx) {
                                midx <- dfx[,paste0(varname,impute.suffix)]
                                if(sum(!midx)==0) {
                                  dfx[midx,paste0(varname,impute.suffix)] <- FALSE
                                  warning("Cell with missing data but no donors: data not imputed")
                                } else {
                                  dfx[midx,varname] <- sample(dfx[!midx,varname],sum(midx),replace=TRUE)
                                }
                                return(dfx)
                              }))
      df <- df[order(df$.tmp.rownames),]
      df$.tmp.rownames <- NULL
      df$.tmp.cellnames <- NULL
    } else {
      # not recognised
      stop("Method not recognised")
    }
    
  } else if(is.numeric(df[,varname])) {
    # Methods for numeric variables
    
    if(method=="median") {
      # overall median imputation
      df[missing,varname] <- median(df[!missing,varname])
      
    } else if(method=="mean") {
      # overall mean imputation
      df[missing,varname] <- mean(df[!missing,varname])
      
    } else if(method=="lm.mean" && !is.null(formula)) {
      # linear model mean imputation with cells defined by the formula
      formula <- as.formula(paste0(varname,
                                   paste0(as.character(formula),collapse=" ")))
      lm1 <- lm(formula, data=df[!missing,])
      df[missing,varname] <- predict(lm1,newdata=df[missing,])
      
    } else if(method=="lm.sim" && !is.null(formula)) {
      # linear model imputation with cells defined by the formula with added error
      formula <- as.formula(paste0(varname,
                                   paste0(as.character(formula),collapse=" ")))
      lm1 <- lm(formula, data=df[!missing,])
      err <- rnorm(sum(missing),0,sigma(lm1))
      df[missing,varname] <- predict(lm1,newdata=df[missing,]) + err
      
    } else {
      # not recognised
      stop("Method not recognised")
    }
  } else if(is.character(df[,varname]) || is.factor(df[,varname])) {
    # Methods for categorical variables
    
    if(method=="mode") {
      if(is.null(formula)) {
        # overall mode imputation
        df[missing,varname] <- modevalue(df[!missing,varname])
        
      } else {
        # mode imputation in cells defined by the formula
        mmat <- model.matrix(formula, data=df)
        df$.tmp.rownames <- 1:nrow(df)
        df$.tmp.cellnames <- apply(mmat,1,paste,collapse=";")
        df <- do.call(rbind, by(df, df$.tmp.cellnames,
                                function(dfx) {
                                  midx <- dfx[,paste0(varname,impute.suffix)]
                                  if(sum(!midx)==0) {
                                    dfx[midx,paste0(varname,impute.suffix)] <- FALSE
                                    warning("Cell with missing data but no donors: data not imputed")
                                  } else {
                                    dfx[midx,varname] <- modevalue(dfx[!midx,varname])
                                  }
                                  return(dfx)
                                }))
        df <- df[order(df$.tmp.rownames),]
        df$.tmp.rownames <- NULL
        df$.tmp.cellnames <- NULL
      }
      
    } else {
      # not recognised
      stop("Method not recognised")
    }
  } else {
    stop("Variable type is not numeric or factor/character - cannot be imputed")
  }
  return(df)
}


#####################################################################
