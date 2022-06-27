# Test

###################################################################
# PANEL FUNCTIONS:

###################################################################
# Function to draw from a truncated gamma distribution; sourced from: https://www.r-bloggers.com/2020/08/generating-data-from-a-truncated-distribution/
rgammat <- function(n, range, shape, rate) {
  # range is a vector of two values
  
  F.a <- pgamma(min(range), shape = shape, rate = rate)
  F.b <- pgamma(max(range), shape = shape, rate = rate)
  
  u <- runif(n, min = F.a, max = F.b)
  qgamma(u, shape = shape, rate = rate)
}

###################################################################
# Function to draw from a truncated normal distribution; sourced from: https://www.r-bloggers.com/2020/08/generating-data-from-a-truncated-distribution/
rnormt <- function(n, range, mean, sd) {
  # range is a vector of two values
  
  F.a <- pnorm(min(range), mean = mean, sd = sd)
  F.b <- pnorm(max(range), mean = mean, sd = sd)
  
  u <- runif(n, min = F.a, max = F.b)
  
  qnorm(u, mean = mean, sd = sd)
  
}

###################################################################
# Function to draw from a truncated Poisson distribution; sourced from: https://www.r-bloggers.com/2020/08/generating-data-from-a-truncated-distribution/
rpoist <- function(n, range, lambda) {
  # range is a vector of two values
  
  F.a <- ppois(min(range), lambda = lambda)
  F.b <- ppois(max(range), lambda = lambda)
  
  u <- runif(n, min = F.a, max = F.b)
  
  qpois(u, lambda = lambda)
  
}

###################################################################
# function to add row to a dataframe
f.pushRow <- function(df = NULL, row){
  if(is.null(df)){
    df <- row
  }else if(is.data.frame(df) & nrow(df)>0){
    df <- rbind(df, row)
  }
  return(df)
}

###################################################################
# function to add a column to a dataframe
f.pushColumn <- function(df = NULL, column){
  if(is.null(df)){
    df <- data.frame(column)
  }else if(is.data.frame(df) & ncol(df)>0){
    df <- cbind(df, column)
  }
  return(df)
}

###################################################################
# function to scale continuous variable:

f.scale.cont <- function(variable = NULL, scale = 1, basevariable = NULL, mean.bv = NULL, sd.bv = NULL){
  #use scale=2 to divide by 2*sd (as proposed by Gelman 2008)
  if(is.null(basevariable)) basevariable <- variable
  if(!is.null(mean.bv) & !is.null(sd.bv)){
    return((variable - mean.bv)/(scale*sd.bv))
  }else{
    return((variable - mean(basevariable))/(scale*sd(basevariable)))
  }
}


###################################################################
# function to backscale continuous variable:

f.backscale.cont <- function(variable.scaled = NULL, scale = 1, basevariable = NULL, mean.bv = NULL, sd.bv = NULL){
  if(is.null(basevariable)) basevariable <- variable
  if(!is.null(mean.bv) & !is.null(sd.bv)){
    return(variable.scaled*scale*sd.bv + mean.bv) 
  }else{
    return(variable.scaled*scale*sd(basevariable) + mean(basevariable)) 
  }
  
}


###################################################################
# function to draw dominant individual
f.draw.dominant <- function(sex, new.group = TRUE, df.candidates, dom.male.dead = FALSE){
  
  # df.candidates <-  data.frame(idDog, sex, ageMonth, dominant)
  
  if(nrow(df.candidates)==0) return(NA)
  
  # function to randomly assign dominance among individuals aged ≥ 15 months:
  f.random.dom <- function(Sex, df){
    df <- subset(df, sex==Sex & ageMonth>=15 & dominant==FALSE)
    n <- nrow(df)
    if(length(n)==0) return(NA)
    
    row <- sample(x = c(1:n), size = 1, replace = FALSE)
    
    # return ID of randomly drawn dominant individual:
    return(df$idDog[row])
  }
  
  # if it is a newly formed group, dominance is assigned randomly among individuals aged ≥ 15 months:
  if(new.group==TRUE) return(f.random.dom(Sex = sex, df = df.candidates))
  
  # if the dominant female of an existing group dies, dominance will be randomly assigned among founding individuals aged ≥ 15 months:
  if(sex=="F") return(f.random.dom(Sex = sex, df = df.candidates))
  
  # if the dominant male of an existing group dies, dominance will be randomly assigned among founding individuals aged ≥ 15 months:
  if(sex=="M" & dom.male.dead==TRUE) return(f.random.dom(Sex = sex, df = df.candidates))
  
  # does the dominant male loose dominance?
  if(sex=="M" & dom.male.dead==FALSE){
    
    dom.lost <- FALSE
    
    # probability to loose dominance:
    df <- subset(df.candidates, sex=="M" & dominant==FALSE & ageMonth>=15)
    
    # if there are candidate males for the dominant position:
    if(nrow(df)>0){
      df$probDom <- as.numeric(NA)
      
      # probability of being dominant, taken from Creel & Creel Book, Fig. 7.9:
      df.probDom.m <- data.frame(sex = "M",
                                 ageMonth = c(1:11)*12,
                                 p = c(0, 0.04, 0.2, 0.22, 0.42, 0.38, 0.28, 0.22, 0, 0, 0 )
      )
      
      # linear interpolation to get p for each individual
      df$p <- as.numeric(approx(x = df.probDom.m$ageMonth, y = df.probDom.m$p, xout = df$ageMonth, method = "linear")$y)
      df$p. <- 1-df$p
      
      
      for(i in 1:nrow(df)){
        df$probDom[i] <- df$p[i]*prod(df$p.[-i])
      }
      
      df <- na.omit(df)
      if(nrow(df)==0){
        dom.lost <- FALSE
      }else{
        p.dom.loose <- sum(df$probDom)
        
        # from a binomial distribution we draw, if currently dominant male looses its dominance or not:
        dom.lost <- as.logical(rbinom(n = 1, size = 1, prob = p.dom.loose))
      }
    }
    
    
    # if dominance is not lost, the same male stays dominant:
    if(dom.lost==FALSE) return(df.candidates$idDog[which(df.candidates$sex=="M" & df.candidates$dominant==TRUE)])
    
    # if dominance is lost, dominance will be randomly assigned among founding individuals aged ≥ 15 months:
    if(dom.lost==TRUE) return(f.random.dom(Sex = sex, df = df.candidates))
    
  }
}


###################################################################
# function to create new dogs (either pack founders or new borns within a pack):

f.createDog <- function(df.individuals, nMales, nFemales, maleAgeMonths=0, femaleAgeMonths=0, year, month, run, groupID, packFounder, immigrant = FALSE, nAdults = NA, idMother = NA, idFather = NA, idLitter = NA){
  
  nDogs <- sum(nMales, nFemales)
  
  if(length(maleAgeMonths)==1 & length(femaleAgeMonths)==1){
    ageMonths <- c(rep(maleAgeMonths, nMales), rep(femaleAgeMonths, nFemales))
  }else{
    ageMonths <- c(maleAgeMonths, femaleAgeMonths)
  }
  
  sex <- c(rep("M", nMales), rep("F", nFemales))
  
  dominant <- as.logical(rep(FALSE, nDogs))
  
  idMother <- as.numeric(rep(idMother, nDogs))
  idFather <- as.numeric(rep(idFather, nDogs))
  
  idLitter <- as.numeric(rep(idLitter, nDogs))
  
  birthdate <- rep(as.Date(paste0("01.",month,".",year), format="%d.%m.%Y"), nDogs)
  if(packFounder==TRUE){
    birthdate <- as.Date(rep(NA, nDogs), format="%Y-%m-%d")
  }
  
  nAdults <- rep(nAdults, nDogs)
  if(packFounder==TRUE){
    nAdults <- rep(nDogs, nDogs)
  }
  
  nPups <- rep(nDogs, nDogs)
  if(packFounder==TRUE){
    nPups <- rep(0, nDogs)
  }
  
  inNatalPack <- FALSE
  if(packFounder==FALSE & immigrant==FALSE) inNatalPack <- TRUE
  
  for(j in 1:nDogs){
    
    row <- data.frame(id = ifelse(test = is.null(df.individuals), yes = 1, no = max(df.individuals$id)+1),
                      year = year,
                      month = month,
                      run = run,
                      #date = as.Date(-1,ceiling_date(x = as.Date(paste0(year,"-",month,"-01")), unit = "month")),
                      idGroup = groupID,
                      idDog = ifelse(test = is.null(df.individuals), yes = 1, no = max(df.individuals$idDog)+1),
                      sex = sex[j],
                      ageMonth = ageMonths[j],
                      inNatalPack = as.logical(inNatalPack),
                      packFounder = as.logical(packFounder),
                      dominant = as.logical(dominant[j]),
                      movement.state = factor(x = "resident", levels = c("resident","dispersing")),
                      reproductive.stage = factor(x = ifelse(test = sex[j]=="F", yes = "non-reproductive", no = NA), levels = c("non-reproductive", "pregnant", "lactating")),
                      packImmigrant = as.logical(immigrant),
                      disp.durationMonths.cum = as.numeric(NA), # how long has individual been dispersing
                      disp.durationMonths.max = as.numeric(NA), # how long should the individual be dispersing at max
                      allIndOppSexRelated = as.logical(ifelse(test = packFounder==TRUE, yes = FALSE, no = NA)),
                      survProb = as.numeric(1),
                      alive = as.logical(TRUE),
                      emProb = as.numeric(NA),
                      emigrated = as.logical(FALSE),
                      settled = as.logical(NA),
                      nPups = as.numeric(nPups[j]),
                      nAdults = as.numeric(nAdults[j]),
                      birthdate = as.Date(birthdate[j], format="%Y-%m-%d"),
                      idMother = as.numeric(idMother[j]),
                      idFather = as.numeric(idFather[j]),
                      idLitter = as.numeric(idLitter[j]),
                      stringsAsFactors = FALSE)
    
    df.individuals <- pushRow(df = df.individuals, row = row)
    
  }
  
  # assign dominance / pregnancy:
  if(packFounder==TRUE){
    Year <- year
    Month <- month
    
    # assign dominance
    dogID.f <- f.draw.dominant(sex = "F", new.group = TRUE, df.candidates = subset(df.individuals, idGroup==groupID & year==Year & month==Month & sex=="F" & packFounder==TRUE, select = c(idDog, sex, ageMonth, dominant)))
    dogID.m <- f.draw.dominant(sex = "M", new.group = TRUE, df.candidates = subset(df.individuals, idGroup==groupID &  year==Year & month==Month & sex=="M" & packFounder==TRUE, select = c(idDog, sex, ageMonth, dominant)), dom.male.dead = FALSE)
    df.individuals$dominant[which(df.individuals$idDog%in%c(dogID.f, dogID.m) & df.individuals$idGroup==groupID & df.individuals$year==Year & df.individuals$month==Month & df.individuals$packFounder==TRUE)] <- TRUE
    
    # assign pregnancy
    if(Month==3) df.individuals$reproductive.stage[which(df.individuals$idDog==dogID.f & df.individuals$sex=="F" & df.individuals$idGroup==groupID & df.individuals$year==Year & df.individuals$month==Month & df.individuals$packFounder==TRUE)] <- "pregnant"
    
  }
  
  
  return(df.individuals)
}


###################################################################
# function to draw random litter size based on packsize:
f.drawLitterSize <- function(nDraws = 1, adults, femaleAge.yrs){
  # parameters and intercept are taken from fitting a poisson model (without random effect) of litter size (see Chapter 4 - Reproduction file litterSize.R)
  # --> in paper just cite McNutt & Silk (2008)
  adults.scaled <- f.scale.cont(variable = adults, scale = 1, mean.bv = 9.994012, sd.bv = 5.505197)
  femaleAge.yrs.scaled <- f.scale.cont(variable = femaleAge.yrs, scale = 1, mean.bv = 4.54491, sd.bv = 1.871494)
  femaleAge.yrs2.scaled <- f.scale.cont(variable = femaleAge.yrs^2, scale = 1, mean.bv = 24.13772, sd.bv = 20.14463)
  
  # max litter size should be 16 (from empirical observations)
  # min litter size should be 1 (from empirical observations)
  litterSizes <- as.numeric(rep(NA, nDraws))
  for(i in 1:nDraws){
    litterSize <- 0
    while(litterSize<1 | litterSize>16){
      
      lambda <- -1
      
      while(lambda<0){
        lambda <- exp(rnorm(n = 1, mean = 2.25569, sd = XY) + #intercept
                      rnorm(n = 1, mean = 0.31503, sd = XY)*femaleAge.yrs.scaled + # effect of female age (years)
                      rnorm(n = 1, mean = -0.35512, sd = XY)*femaleAge.yrs2.scaled + # effect of female age (years) squared
                      rnorm(n = 1, mean = 0.14416, sd = XY)*adults.scaled # effect of number of adults (i.e. pack size)
        )
      }
      
      litterSize <- rpois(n = 1, lambda = lambda) # we draw litter size 
    }
    
    litterSizes[i] <- litterSize
  }
  
  return(litterSizes)
}


###################################################################
# function to draw random litter sex ratio (proportion of males) based on litterNR:
f.drawLitterSexRatio <- function(nDraws = 1, litterSize, femaleAge.yrs){
  
  # # parameters and intercept are taken from fitting a binomial model (without random effect) of proporiton of males (see Chapter 4 - Reproduction file sexRatio.R)
  # # --> I wasn't really able to find a significant effect of female age but nevertheless included it... even though the effect is very weak
  # # --> in paper just cite McNutt & Silk (2008)
  # 
  # femaleAge.yrs.scaled <- f.scale.cont(variable = femaleAge.yrs, scale = 1, mean.bv = 4.493902, sd.bv = 1.746151)
  # #femaleAge.yrs2.scaled <- f.scale.cont(variable = femaleAge.yrs^2, scale = 1, mean.bv = 23.22561, sd.bv = 17.78444)
  # 
  # #x <- 0.1255 -0.1644*femaleAge.yrs.scaled + 0.1521*femaleAge.yrs2.scaled
  # x <- 0.1258  -0.0147*femaleAge.yrs.scaled #
  # p <- exp(x)/(1+exp(x))
  
  # we just use average proportion of males in our study population
  p <- 0.54
  
  propM <- as.numeric(rep(NA, nDraws))
  
  for(i in 1:nDraws){
    propM[i] <- sum(rbinom(n = litterSize, size = 1, prob = p))/litterSize
  }
  
  return(propM)
}


###################################################################
# calculate probability of survival from age1 to age2:
# a0, a1, c, b0, b1 are parameters of the Siler model

f.survProbInterval <- function(state, ageMonths1, ageMonths2, Sex, df.coeffs, df.scale.cov, df.values.cov, ageMax = 133){
  
  # we assume maximum lifespan of 133 months (i.e. 11.1 years, the maximum observed age in our study population)
  if(ageMonths2>ageMax) return(0)
  
  # # resident mortality:
  # df.values.cov <- data.frame(adults = 3, pups = 4, dominant = 1, temp = 34, rain = 29, denning = 0, habitat = NA)
  # 
  # # dispersing mortality:
  # df.values.cov <- data.frame(coalSize = 3, dispDuration = 3, temp = 34, rain = 200)
  
  # we remove columns with NA's:
  df.values.cov <- df.values.cov[,!as.logical(colSums(is.na(df.values.cov)))]
  if(ncol(df.values.cov)==0) stop("no covariate values provided")
  # let's transpose df.values.cov:
  df.values.cov2 <- data.frame(label = colnames(df.values.cov), value = as.numeric(df.values.cov[1,]))
  
  # # if dispersing, we take mean value of dispDuration:
  # df.values.cov2$value[which(df.values.cov2$label=="dispDuration")] <- mean(c(df.values.cov2$value[which(df.values.cov2$label=="dispDuration")], df.values.cov2$value[which(df.values.cov2$label=="dispDuration")]+1))
  # 
  # ages are in years:
  age1 <- ageMonths1/12
  age2 <- ageMonths2/12
  
  # resident or dispersing mortality?
  Rate <- switch(state, resident=("mortality.res"), dispersing=("mortality.dis"))
  
  a0 <- rnorm(n = 1, mean = df.coeffs$mean[which(df.coeffs$rate==Rate & df.coeffs$label=="a0" & df.coeffs$sex==Sex)], sd = df.coeffs$SD[which(df.coeffs$rate==Rate & df.coeffs$label=="a0" & df.coeffs$sex==Sex)])
  a1 <- rnorm(n = 1, mean = df.coeffs$mean[which(df.coeffs$rate==Rate & df.coeffs$label=="a1" & df.coeffs$sex==Sex)], sd = df.coeffs$SD[which(df.coeffs$rate==Rate & df.coeffs$label=="a1" & df.coeffs$sex==Sex)])
  c <- rnorm(n = 1, mean = df.coeffs$mean[which(df.coeffs$rate==Rate & df.coeffs$label=="c" & df.coeffs$sex==Sex)], sd = df.coeffs$SD[which(df.coeffs$rate==Rate & df.coeffs$label=="c" & df.coeffs$sex==Sex)])
  b0 <- rnorm(n = 1, mean = df.coeffs$mean[which(df.coeffs$rate==Rate & df.coeffs$label=="b0" & df.coeffs$sex==Sex)], sd = df.coeffs$SD[which(df.coeffs$rate==Rate & df.coeffs$label=="b0" & df.coeffs$sex==Sex)])
  b1 <- rnorm(n = 1, mean = df.coeffs$mean[which(df.coeffs$rate==Rate & df.coeffs$label=="b1" & df.coeffs$sex==Sex)], sd = df.coeffs$SD[which(df.coeffs$rate==Rate & df.coeffs$label=="b1" & df.coeffs$sex==Sex)])
  
  # we select coefficients of covariates
  df.betas <- subset(df.coeffs, rate==Rate & type=="coefficient" & sex==Sex, select = -c(CI_low, CI_high, significant))
  df.betas$cov.label <- unlist(strsplit(x = df.betas$label, split = "_"))[seq(2,2*nrow(df.betas),2)]
  # we add covariate values:
  df.betas <- merge(x = df.betas, y = df.values.cov2, by.x="cov.label", by.y="label")
  
  # we add scale parameters
  df.betas <- merge(x = df.betas, y = subset(df.scale.cov, sex==Sex & rate==Rate, select = c(label,mean,SD)), by.x="cov.label", by.y="label")
  colnames(df.betas)[which(colnames(df.betas)=="mean.x")] <- "coeff.mean"
  colnames(df.betas)[which(colnames(df.betas)=="mean.y")] <- "cov.mean"
  colnames(df.betas)[which(colnames(df.betas)=="SD.x")] <- "coeff.SD"
  colnames(df.betas)[which(colnames(df.betas)=="SD.y")] <- "cov.SD"
  colnames(df.betas)[which(colnames(df.betas)=="value")] <- "cov.value"
  
  # we scale all covariate values:
  scale <- 1
  df.betas$cov.scaled <- (df.betas$cov.value - df.betas$cov.mean)/(scale*df.betas$cov.SD)
  
  # we calculate the product of scaled parameter and coefficients
  #df.betas$cov.prod <- df.betas$coeff.mean*df.betas$cov.scaled
  df.betas$cov.prod <- df.betas$cov.scaled*apply(X = cbind(df.betas$coeff.mean, df.betas$coeff.SD), MARGIN = 1, FUN = function(X) rnorm(n = 1, mean = X[1], sd = X[2]))
  
  
  if(state=="resident"){
    cov.a.sum1 <- sum(df.betas$cov.prod[which(df.betas$ageClass=="juveniles")])
    cov.a.sum2 <- cov.a.sum1 # could be used if we were to calculate mean of temp and rain from values at beginning and end of month
    
    cov.b.sum1 <- sum(df.betas$cov.prod[which(df.betas$ageClass=="adults")])
    cov.b.sum2 <- cov.b.sum1 # could be used if we were to calculate mean of temp and rain from values at beginning and end of month
    
    integral1 <- (exp(b1*age1 + b0 + cov.b.sum1)/b1 - exp(a0 - a1*age1 + cov.a.sum1)/a1 + c*age1)
    integral2 <- (exp(b1*age2 + b0 + cov.b.sum2)/b1 - exp(a0 - a1*age2 + cov.a.sum2)/a1 + c*age2)
    
  }else if(state=="dispersing"){
    cov.sum1 <- sum(df.betas$cov.prod)
    cov.sum2 <- cov.sum1 # could be used if we were to calculate mean of temp and dispDuration from values at beginning and end of month
    
    integral1 <- (exp(b1*age1 + b0)/b1 - exp(a0 - a1*age1)/a1 + c*age1)*exp(cov.sum1)
    integral2 <- (exp(b1*age2 + b0)/b1 - exp(a0 - a1*age2)/a1 + c*age2)*exp(cov.sum2)
    
  }
  
  return(exp(-(integral2-integral1)))
}


###################################################################
# function to calculate natal emigration probability from age1 to age2:
f.pred.emigration <- function(ageMonths1, ageMonths2, Sex, df.coeffs, df.scale.cov, df.values.cov){
  
  # we remove columns with NA's:
  df.values.cov <- df.values.cov[,!as.logical(colSums(is.na(df.values.cov)))]
  if(ncol(df.values.cov)==0) stop("no covariate values provided")
  # let's transpose df.values.cov:
  df.values.cov2 <- data.frame(label = colnames(df.values.cov), value = as.numeric(df.values.cov[1,]))
  
  # ages are in years (starting with 0 at age 1 year):
  t1 <- ageMonths1/12-1
  t2 <- ageMonths2/12-1
  
  if(t1<0 | t2<0) return(NA)
  
  alpha <- rnorm(n = 1, mean = df.coeffs$mean[which(df.coeffs$rate=="natal.dis" & df.coeffs$label=="alpha" & df.coeffs$sex==Sex)], sd = df.coeffs$SD[which(df.coeffs$rate=="natal.dis" & df.coeffs$label=="alpha" & df.coeffs$sex==Sex)])
  kappa <- rnorm(n = 1, mean = df.coeffs$mean[which(df.coeffs$rate=="natal.dis" & df.coeffs$label=="kappa" & df.coeffs$sex==Sex)], sd = df.coeffs$SD[which(df.coeffs$rate=="natal.dis" & df.coeffs$label=="kappa" & df.coeffs$sex==Sex)])
  
  # we select coefficients of covariates
  df.betas <- subset(df.coeffs, rate=="natal.dis" & type=="coefficient" & sex==Sex, select = -c(CI_low, CI_high, significant))
  colnames(df.betas)[which(colnames(df.betas)=="label")] <- "cov.label"
  # we add covariate values:
  df.betas <- merge(x = df.betas, y = df.values.cov2, by.x="cov.label", by.y="label")
  
  # we add scale parameters
  df.betas <- merge(x = df.betas, y = subset(df.scale.cov, sex==Sex & rate=="natal.dis", select = c(label,mean,SD)), by.x="cov.label", by.y="label")
  colnames(df.betas)[which(colnames(df.betas)=="mean.x")] <- "coeff.mean"
  colnames(df.betas)[which(colnames(df.betas)=="mean.y")] <- "cov.mean"
  colnames(df.betas)[which(colnames(df.betas)=="SD.x")] <- "coeff.SD"
  colnames(df.betas)[which(colnames(df.betas)=="SD.y")] <- "cov.SD"
  colnames(df.betas)[which(colnames(df.betas)=="value")] <- "cov.value"
  
  # we scale all covariate values:
  scale <- 1
  df.betas$cov.scaled <- (df.betas$cov.value - df.betas$cov.mean)/(scale*df.betas$cov.SD)
  
  # we calculate the product of scaled parameter and coefficient
  #df.betas$cov.prod <- df.betas$coeff.mean*df.betas$cov.scaled
  df.betas$cov.prod <- df.betas$cov.scaled*apply(X = cbind(df.betas$coeff.mean, df.betas$coeff.SD), MARGIN = 1, FUN = function(X) rnorm(n = 1, mean = X[1], sd = X[2]))
  
  # frailty
  u <- 1
  
  Ht1 <- log(1+exp(alpha)*t1^kappa)
  Ht2 <- log(1+exp(alpha)*t2^kappa)
  
  ht1 <- (exp(alpha)*kappa*t1^(kappa-1))/(1+exp(alpha)*t1^kappa)
  ht2 <- (exp(alpha)*kappa*t2^(kappa-1))/(1+exp(alpha)*t2^kappa)
  
  sum.covariates <- sum(df.betas$cov.prod)
  
  prob.no.event <- exp(-(Ht2-Ht1)*u*exp(sum.covariates))
  emProb <- 1-prob.no.event
  return(emProb)
  
}


###################################################################
# function to draw size of dispersing coalition:
f.drawDispCoalSize <- function(nDraws = 1, pups, SSsiblings){
  lambda <- exp(1.07433 + 0.13486*f.scale.cont(variable = SSsiblings, scale = 1, mean.bv = 4.851852, sd.bv = 2.695842) -0.13574*f.scale.cont(variable = pups, scale = 1, mean.bv = 6.62037, sd.bv = 4.471546))
  return(rpois(n = nDraws, lambda = lambda))
}


###################################################################
# Function to draw dispersal duration from gamma distribution:

f.drawDispDuration <- function(nDraws = 1, unit = "months", rangeDays = c(0,130), gammaParams = gammaParams_dispersal_duration){
  # unit = c("days", "months", "years")
  
  # we draw dispersal duration from a right-truncated gamma distribution (to avoid drawing values that exceed empirically observed values, i.e. the max. observed dispersal duration of 130 days):
  dispDuration <- rgammat(n = nDraws, 
                          range = rangeDays,
                          # shape and rate parameters are drawn from a left-truncated normal distribution (as the shape and rate parameters both need to have positive values):
                          shape = rnormt(n = nDraws, range = c(5e-324, Inf), mean = gammaParams[which(rownames(gammaParams)=="shape"),which(colnames(gammaParams)=="Mean")], sd = gammaParams[which(rownames(gammaParams)=="shape"),which(colnames(gammaParams)=="SD")]), 
                          rate = rnormt(n = nDraws, range = c(5e-324, Inf), mean = gammaParams[which(rownames(gammaParams)=="rate"),which(colnames(gammaParams)=="Mean")], sd = gammaParams[which(rownames(gammaParams)=="rate"),which(colnames(gammaParams)=="SD")])) 
  # we obtained shape and rate parameters from fitting a gamma distribution to observed dispersal duration times (see below code that is commented out)
  
  if(unit=="days") return(dispDuration)
  if(unit=="months") return(dispDuration/(365/12))
  if(unit=="years") return(dispDuration/365)
}

# # check output of the draw by comparing to the fitted PDF for 5000 draws:
# ggplot()+
#     geom_histogram(aes(y = ..density.., x = f.drawDispDuration(nDraws = 5000, unit = "days", rangeDays = c(0,130))), binwidth = 5, fill = "grey70", color = "black")+
#     geom_line(data = data.frame(x = x, y = dgamma(x = x, shape = summary(dispDur.fitted)$statistics[which(rownames(summary(dispDur.fitted)$statistics)=="shape"),1], rate = summary(dispDur.fitted)$statistics[which(rownames(summary(dispDur.fitted)$statistics)=="shape"),1]/exp(summary(dispDur.fitted)$statistics[which(rownames(summary(dispDur.fitted)$statistics)=="intercept"),1]))), aes(x = x, y = y), color = "red")+
#     ylab("Density")+xlab("Data")+
#     ggtitle("Empirical vs. fitted PDF")+
#     theme_general()


# we obtained shape and rate parameters from fitting a gamma distribution to observed dispersal duration times:
# output of model fit is saved as gammaParams_dispersal_duration.RData and imported using the file "01 - Start.R"
# below code needs to be run again in case gammaParams_dispersal_duration is not yet loaded into the work space

# # get empirical data of dispersal duration:
# df.dispEvents <- read.xlsx(xlsxFile = "Data/Dogs_traj_segmentation.xlsx", sheet = 1, startRow = 11, colNames = TRUE, rowNames = FALSE, skipEmptyRows = FALSE, detectDates = TRUE)
# str(df.dispEvents)
# df.dispEvents$Start_phase_date <- as.Date(df.dispEvents$Start_phase_date)
# df.dispEvents$Stop_phase_date <- as.Date(df.dispEvents$Stop_phase_date)
# df.dispEvents <- subset(df.dispEvents, Phase%in%c("Disp","Trans"), select = c("Ind","CollarID","Phase","Start_phase_date","Stop_phase_date","Phase_duration"))
# 
# # how many dispersers?
# length(unique(df.dispEvents$Ind))
# 
# # get dispersal duration in days:
# df.dispEvents$Phase_duration <- as.numeric(difftime(time2 = df.dispEvents$Start_phase_date, time1 = df.dispEvents$Stop_phase_date, units = "days"))
# summary(df.dispEvents$Phase_duration)
# hist(df.dispEvents$Phase_duration)
# 
# 
# # nimble code to fit Gamma GLMM for dispersal duration:
# dispDur.code <- nimbleCode({
# 
#   ## PRIORS ##
# 
#   # Prior for intercept
#   intercept ~ dnorm(mean = 0, sd = 20)  ## uniform prior per Gelman (2006)
# 
#   #Prior for random intercept effect of individual:
#   # ind_sd ~ dunif(0, 20)
#   # for(i in 1:Ninds){ # we loop over all individuals
#   #   ind_effect[i] ~ dnorm(mean = 0, sd = ind_sd)
#   # }
# 
#   # Prior for shape parameter:
#   shape ~ dgamma(shape = 0.01, rate = 0.01)
# 
# 
#   ## LIKELIHOOD ##
# 
#   for(i in 1:N){ # we loop over all observations
#     linear_predictor[i] <- intercept #+ ind_effect[individuals[i]]
#     y[i] ~ dgamma(shape = shape, rate = shape/exp(linear_predictor[i]))
#   }
# 
# })
# 
# N <- nrow(df.dispEvents) # number of observations
# Ninds <- length(unique(df.dispEvents$Ind))  # number of individuals
# individuals <- as.integer(as.factor(df.dispEvents$Ind))
# y <- df.dispEvents$Phase_duration # response variable
# head(y)
# 
# # set up the model:
# dispDur.constants <- list(N = N, Ninds = Ninds, individuals = individuals)
# 
# dispDur.data  <- list(y = y)
# 
# # we generate initial data
# dispDur.inits <- list(intercept = rnorm(n = 1, mean = 0, sd = 20),
#                       ind_sd = runif(n = 1, min = 0, max = 20),
#                       ind_effect = rnorm(n = dispDur.constants$Ninds, mean = 0, sd = 1),
#                       shape = rgamma(n = 1, shape = 0.01, rate = 0.01))
# 
# dispDur.model <- nimbleModel(code = dispDur.code, constants = dispDur.constants, inits = dispDur.inits, data = dispDur.data)
# # ignore initialize warning: NA's in y and X wont be used
# 
# # fit the model:
# dispDur.fitted <- nimbleMCMC(model = dispDur.model, thin = 1, nchains = 4, nburnin = 500, niter = 5000, samplesAsCodaMCMC = TRUE) #, monitors = c("shape","intercept","rate"))
# 
# # let's have a visual look at convergence:
# # mcmcplot(dispDur.fitted)
# 
# # let's have quantitative look at convergence by looking at R-hat statistics:
# # values below < 1.1 are regarded as indicating convergence
# gelman.diag(x = dispDur.fitted)
# 
# 
# # get summary of model fit:
# summary(dispDur.fitted)
# 
# # combine the posterior values of all chains into a single dataframe:
# df.dispDur.fitted <- as.data.frame(combine.mcmc(mcmc.objects = dispDur.fitted))
# head(df.dispDur.fitted)
# 
# # Calculate rate parameter based on shape and intercept:
# df.dispDur.fitted$rate <- df.dispDur.fitted$shape/exp(df.dispDur.fitted$intercept)
# df.dispDur.fitted <- subset(df.dispDur.fitted, select = c("shape","rate"))
# 
# # save mean and sd of model fit:
# output <- summary(dispDur.fitted)$statistics
# output <- output[,which(colnames(output)%in%c("Mean","SD"))]
# output <- rbind(output, c(mean(df.dispDur.fitted$rate), sd(df.dispDur.fitted$rate)))
# rownames(output)[nrow(output)] <- "rate"
# 
# saveRDS(object = output, file = "Data/gammaParams_dispersal_duration.RData")
# 
# 
# x <- seq(0, max(df.dispEvents$Phase_duration),0.1)
# 
# 
# # diagnostic plots:
# 
# # compare empirical and fitted PDF:
# p1 <- ggplot()+
#   geom_histogram(data = df.dispEvents, aes(y = ..density.., x = Phase_duration), binwidth = 20, fill = "grey70", color = "black")+
#   geom_line(data = data.frame(x = x, y = dgamma(x = x, shape = summary(dispDur.fitted)$statistics[which(rownames(summary(dispDur.fitted)$statistics)=="shape"),1], rate = summary(dispDur.fitted)$statistics[which(rownames(summary(dispDur.fitted)$statistics)=="shape"),1]/exp(summary(dispDur.fitted)$statistics[which(rownames(summary(dispDur.fitted)$statistics)=="intercept"),1]))), aes(x = x, y = y), color = "red")+
#   ylab("Density")+xlab("Data")+
#   ggtitle("Empirical vs. fitted PDF")+
#   theme_general()
# 
# # compare empirical and fitted CDF
# p2 <- ggplot()+
#   stat_ecdf(data = df.dispEvents, aes(x = Phase_duration), geom = "step", color = "black")+
#   #geom_histogram(data = df.dispEvents, aes(y = ..density.., x = Phase_duration), binwidth = 20, fill = "grey70", color = "white")+
#   geom_line(data = data.frame(x = x, y = pgamma(q = x, shape = summary(dispDur.fitted)$statistics[which(rownames(summary(dispDur.fitted)$statistics)=="shape"),1], rate = summary(dispDur.fitted)$statistics[which(rownames(summary(dispDur.fitted)$statistics)=="shape"),1]/exp(summary(dispDur.fitted)$statistics[which(rownames(summary(dispDur.fitted)$statistics)=="intercept"),1]))), aes(x = x, y = y), color = "red")+
#   ylab("CDF")+xlab("Data")+
#   ggtitle("Empirical vs. fitted CDF")+
#   theme_general()
# 
# # Q-Q Plot
# percentile.empirical <- ecdf(df.dispEvents$Phase_duration)
# percentile.fitted <- ecdf(rgamma(n = 1000, shape = summary(dispDur.fitted)$statistics[which(rownames(summary(dispDur.fitted)$statistics)=="shape"),1], rate = summary(dispDur.fitted)$statistics[which(rownames(summary(dispDur.fitted)$statistics)=="shape"),1]/exp(summary(dispDur.fitted)$statistics[which(rownames(summary(dispDur.fitted)$statistics)=="intercept"),1])))
# df.qq <- data.frame(x = sapply(X = df.dispEvents$Phase_duration, FUN = function(X) percentile.fitted(X)), y = sapply(X = df.dispEvents$Phase_duration, FUN = function(X) percentile(X)))
# 
# p3 <- ggplot(data = df.qq, aes(x = x, y = y))+
#   geom_point()+
#   geom_smooth(method = "lm", se = FALSE, color = "red")+
#   ylab("Empirical quantiles")+xlab("Fitted quantiles")+
#   ggtitle("Q-Q plot")+
#   theme_general()
# 
# # combine all 3 diagnostic plots:
# grid.arrange(p1, p2, p3, nrow = 2, ncol = 2)
# 
# 
# # Compare this to frequentist fit of a Gamma distribution to the data:
# fit.gamma <- fitdist(data = df.dispEvents$Phase_duration, distr = "gamma", method = "mle")
# summary(fit.gamma)
# plot(fit.gamma)


###################################################################
# function to calculate julian days sin or cos:
f.julianDays <- function(year, month, fun){
  # fun = c("sin","cos")
  
  # calculate julian days:
  JulianDate.month <- 6
  JulianDate.day <- 1
  date.i <- as.Date(x = paste0(year,"-",month,"-",15), format = "%Y-%m-%d")
  if(month(date.i)==JulianDate.month){
    if(day(date.i)>=JulianDate.day){
      year.i <- year(date.i)
    }else{
      year.i <- year(date.i)-1
    }
  }else if(month(date.i)<JulianDate.month){
    year.i <- year(date.i)-1
  }else{
    year.i <- year(date.i)
  }
  JulianDays <- as.numeric(difftime(time1 = date.i, time2 = as.Date(x = paste0(year.i,"-",JulianDate.month,"-",JulianDate.day), format = "%Y-%m-%d"), units = "days"))
  
  if(fun=="sin") return(sin(2*pi*JulianDays/365))
  if(fun=="cos") return(cos(2*pi*JulianDays/365))
}



