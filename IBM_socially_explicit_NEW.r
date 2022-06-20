###################################################################

# Socially explicit IBM

# we start with newly formed packs based on all possible compositions of number of founding females and males
# we start in March (when mating)
# we assume a single litter is born per pack each year in June (no failed pregnancies)
# we update pack composition on a monthly basis based on vital rates: birth, survival, emigration, (and immigration)
# dominance is assigned randomly in a newly formed pack
# dominance is inherited randomly by co-founders of same-sex
# we assume maximum lifespan of 133 months (i.e. 11.1 years, the maximum observed age in our study population)
# when all remaining dogs in a pack are related, individuals will disperse in same-sex coalitions
# age of founding dogs: either 1, 2, or 3 years
# as soon as individuals emigrate, we draw the dispersal duration from a gamma distribution and let the dispersers form a new pack at dispersal end with a coalition of 1-8 opposite-sex individuals drawn from a truncated Poisson distribution T(Pois(0,8)) 


# SIMULATION END: We let simulation run until all individuals that were born in one of the initial packs (the packs that were created at the beginning of the simulation) are dead


###################################################################

# initialize packs and simulations:

nFounders <- c(1:8) # number of pack founders of each sex
founders.ageMonths <- c(1:3)*12+9 # how old in months are founding individuals at beginning of March

monthStart <- 3 # we start in March
yearStart <- 1 # we start in year 1
monthLitter <- 6 # pups are born in June

nRuns <- 1 # number of runs (how many times should all pack compositions be simulated?)
#domNatal <- FALSE # can native born individuals become dominant once all founding individuals of the same-sex are dead and if dominant individual of opp-sex is immigrant?

# simulate all possible pack founding compositions:
df.packsStart <- expand.grid(idPack = NA, # pack identity
                             fFounders = nFounders, # number of pack founding females
                             mFounders = nFounders, # number of pack founding males
                             agefFounders = founders.ageMonths, # age of pack founding females (we assume they're all sisters of the same litter)
                             ageMFounders = founders.ageMonths) # age of pack founding males (we assume they're all brothers of the same litter)


# assign unique pack identity number:
df.packsStart$idPack <- 1:nrow(df.packsStart)

# have a look at the data frame:
head(df.packsStart)
tail(df.packsStart)
summary(df.packsStart)


# max lifespan: # we assume maximum lifespan of 133 months (i.e. 11.1 years, the maximum observed age in our study population)
ageMax <- 133 #months

# create place holder for data frames:
df.groups <- NULL
df.individuals <- NULL
df.individuals.full <- NULL
df.litters <- NULL
df.rain <- NULL
df.tempMax <- NULL

# where to save output files:
folderSim <- "IBM-Simulation/sim_01/"


###################################################################
# run simulations

# we proceed in monthly time steps:
for(run in 1:nRuns){
  
  start_time <- Sys.time()
  
  # define start date:
  year <- yearStart
  month <- monthStart
  
  # get current run
  runNow <- run 
  
  print(paste("----- RUN =",runNow,"-----"))
  
  # we initialize df.rain and df.temp with the 3 months prior to monthStart and yearStart
  dateEnd <- ceiling_date(as.Date(-1, as.Date(paste0(yearStart,"-",monthStart,"-01"))))
  dateStart <- floor_date(as.Date(-3*28, dateEnd), unit = "month")
  dateNow <- dateStart
  ##Error: object 'df.monthlyRain' not found  // couldn't find it in 01-start or 02-functions
  
  df.r <- subset(df.monthlyRain, date>=as.Date("1989-07-01"))
  while(dateNow<=dateEnd){
    row <- data.frame(id = as.integer(NA),
                      run = runNow,
                      year = year(dateNow),
                      month = month(dateNow),
                      date = as.Date(paste0(year(dateNow),"-",month(dateNow),"-15")),
                      rain = rnorm(n = 1, mean = mean(df.r$rain.mean[which(df.r$month==month(dateNow))]), sd = sd(df.r$rain.mean[which(df.r$month==month(dateNow))]))
    )
    df.rain <- f.pushRow(df = df.rain, row = row)
    remove(row)
    dateNow <- ceiling_date(dateNow, unit = "month")
  }
  
  # we initialize df.rain and df.temp with the 3 months prior to monthStart and yearStart
  dateNow <- dateStart
  df.t <- subset(df.temp, date>=as.Date("1989-07-01") & date<=as.Date("2020-06-30"))
  df.t$month <- month(df.t$date)
  while(dateNow<=dateEnd){
    row <- data.frame(id = as.integer(NA),
                      run = runNow,
                      year = year(dateNow),
                      month = month(dateNow),
                      date = as.Date(paste0(year(dateNow),"-",month(dateNow),"-15")),
                      temp = rnorm(n = 1, mean = mean(df.t$Tmax[which(df.t$month==month(dateNow))], na.rm=TRUE), sd = sd(df.t$Tmax[which(df.t$month==month(dateNow))], na.rm=TRUE))
    )
    df.tempMax <- f.pushRow(df = df.tempMax, row = row)
    remove(row)
    dateNow <- ceiling_date(dateNow, unit = "month")
  }
  remove(dateNow,dateEnd,dateStart)
  
  
  keep.running <- TRUE
  
  # we proceed in monthly time steps until we break out of the while loop
  while(keep.running==TRUE){
    
    # get current year and month:
    monthNow <- month
    yearNow <- year
    
    #if(yearNow==8) break()
    
    print(paste("year =",yearNow,"| month =",monthNow))
    
    # print number of live individuals that were born in one of the initial packs:
    nInd <- ifelse(test = is.null(df.individuals), yes = 0, no = nrow(subset(df.individuals, alive==TRUE & year==yearNow & month==monthNow & run==runNow & idLitter%in%df.litters$id[which(df.litters$idGroup%in%unique(df.groups$idGroup[which(df.groups$run==runNow & df.groups$initialPack==TRUE)]))])))
    print(paste("     n individuals alive =",nInd))
    
    # get month and year of next time step:
    month.next <- month+1
    year.next <- year
    if(month.next>12){
      month.next <- 1
      year.next <- year+1
    }
    
    # we draw monthly rainfall from historic data and add it to df.rain
    row <- data.frame(id = as.integer(NA),
                      run = runNow,
                      year = yearNow,
                      month = monthNow,
                      date = as.Date(paste0(yearNow,"-",monthNow,"-15")),
                      rain = rnorm(n = 1, mean = mean(df.r$rain.mean[which(df.r$month==monthNow)]), sd = sd(df.r$rain.mean[which(df.r$month==monthNow)]))
    )
    df.rain <- f.pushRow(df = df.rain, row = row)
    
    # we draw mean max monthly temperature from historic data and add it to df.tempMax
    row <- data.frame(id = as.integer(NA),
                      run = runNow,
                      year = yearNow,
                      month = monthNow,
                      date = as.Date(paste0(yearNow,"-",monthNow,"-15")),
                      temp = rnorm(n = 1, mean = mean(df.t$Tmax[which(df.t$month==monthNow)], na.rm=TRUE), sd = sd(df.t$Tmax[which(df.t$month==monthNow)], na.rm=TRUE))
    )
    df.tempMax <- f.pushRow(df = df.tempMax, row = row)
    remove(row)
    
    # define time interval of prior 3 months:
    int <- interval(start = floor_date(as.Date(-3*28, as.Date(paste0(yearNow,"-",monthNow,"-01"))), unit = "month"), end = ceiling_date(as.Date(-1, as.Date(paste0(yearNow,"-",monthNow,"-01")))))
    
    # get rain and temp of prior 3 months:
    rainNow <- sum(df.rain$rain[which(df.rain$date%within%int & df.rain$run==runNow)])
    tempNow <- mean(df.tempMax$temp[which(df.tempMax$date%within%int & df.tempMax$run==runNow)], na.rm=TRUE)
    
    # if this is the first time step, we have to initialize all packs and founding individuals:
    if(yearNow==yearStart & monthNow==monthStart){
      
      for(i in 1:nrow(df.packsStart)){
        
        # A) add pack to df.groups:
        groupID <- ifelse(test = is.null(df.groups), yes = 1, no = max(df.groups$idGroup)+1)
        row <- data.frame(id = ifelse(test = is.null(df.groups), yes = 1, no = nrow(df.groups)+1), #unique row ID
                          idGroup = as.integer(groupID), #unique group ID
                          run = as.integer(run),
                          year = as.integer(yearStart),
                          month = as.integer(monthStart),
                          type = factor(x = "pack", levels = c("pack","disp.coalition")),
                          nAdults = as.integer(sum(df.packsStart$mFounders[i], df.packsStart$fFounders[i])),
                          nPups = as.integer(0),
                          #temp = as.numeric(tempNow),
                          #rain = as.numeric(rainNow),
                          #habitat = as.numeric(NA),
                          denning = as.logical(FALSE),
                          initialPack = as.logical(TRUE) # label for packs that were part of simulation initialization
        )
        
        df.groups <- f.pushRow(df = df.groups, row = row)
        
        # we add the same group again with the next month:
        row$year <- year.next
        row$month <- month.next
        row$id <- max(df.groups$id)+1
        df.groups <- f.pushRow(df = df.groups, row = row)
        remove(row)
        
        # B) add associated founders to df.individuals and assign dominance:
        df.individuals <- f.createDog(df.individuals = df.individuals, 
                                      nMales = df.packsStart$mFounders[i], nFemales = df.packsStart$fFounders[i], 
                                      maleAgeMonths = df.packsStart$ageMFounders[i], 
                                      femaleAgeMonths = df.packsStart$agefFounders[i], 
                                      year = yearNow, month = monthNow, run = runNow, 
                                      groupID = groupID, packFounder = TRUE)
        
        # we add the same individuals again with the next month:
        row <- subset(df.individuals, idGroup==groupID & year==yearNow & month==monthNow & run==runNow)
        row$year <- year.next
        row$month <- month.next
        row$id <- max(df.individuals$id)+c(1:nrow(row))
        df.individuals <- f.pushRow(df = df.individuals, row = row)
        remove(row)
        
      }
      
      
      # if this is not the first time step:
    }else{ 
      
      # we select all groups that are currently existing:
      df.groupsAlive <- subset(df.groups, run==runNow & year==yearNow & month==monthNow)
      
      # if there are no groups left, we start the next run:
      if(nrow(df.groupsAlive)==0){
        keep.running <- FALSE
        next(run)
      }
      
      # if there are no initial packs left and no individuals left that were born in one of the initialized packs, we start the next run:
      if(nrow(subset(df.groupsAlive, initialPack==TRUE))==0 & nrow(subset(df.individuals, alive==TRUE & year==yearNow & month==monthNow & run==runNow & idLitter%in%df.litters$id[which(df.litters$idGroup%in%unique(df.groups$idGroup[which(df.groups$run==runNow & df.groups$initialPack==TRUE)]))]))==0 ){
        keep.running <- FALSE
        next(run)
      }
      
      # we update rain and temp with currently drawn values:
      df.groupsAlive$temp <- tempNow
      df.groupsAlive$rain <- rainNow
      df.groups$temp[which(df.groups$id%in%df.groupsAlive$id & run==runNow & year==yearNow & month==monthNow)] <- tempNow
      df.groups$rain[which(df.groups$id%in%df.groupsAlive$id & run==runNow & year==yearNow & month==monthNow)] <- rainNow
      
      
      # we loop through all groups and update state variables of associated individuals:
      for(i in 1:nrow(df.groupsAlive)){
        
        # select all individuals that are alive and present in this group at the beginning of this time step
        df <- subset(df.individuals, idGroup==df.groupsAlive$idGroup[i] & month==monthNow & year==yearNow & alive==TRUE & emigrated==FALSE)
        
        # if there are no live individuals associated with this group:
        if(nrow(df)==0) next(i)
        
        # we make a copy of this group and update variables associated with this group
        row.group <- df.groupsAlive[i,]
        
        # we loop through all individuals of this group and update state variables:
        for(j in 1:nrow(df)){
          
          # A) if individual is resident:
          if(df$movement.state[j]=="resident"){
            
            ########
            # A.1) individuals survive or die
            # resident mortality covariate values:
            df.values.cov <- data.frame(adults = df$nAdults[j], pups =  df$nPups[j], dominant = as.integer(df$dominant[j]), temp = df.groupsAlive$temp[i], rain = df.groupsAlive$rain[i], denning = as.integer(df.groupsAlive$denning[i]), habitat = NA)
            df$survProb[j] <- min(c(f.survProbInterval(state = "resident", ageMonths1 = df$ageMonth[j], ageMonths2 = df$ageMonth[j]+1, Sex = df$sex[j], df.coeffs = df.coeffs, df.scale.cov = df.scale.cov, df.values.cov = df.values.cov, ageMax = ageMax), 1))
            df$alive[j] <- as.logical(rbinom(n = 1, size = 1, prob = df$survProb[j]))
            remove(df.values.cov)
            
            ########
            # A.2) individuals emigrate from natal pack or stay:
            if(df$alive[j]==TRUE & df$inNatalPack[j]==TRUE & df$ageMonth[j]>=13){
              
              if(df$sex[j]=="F"){
                df.values.cov <- data.frame(adults = df$nAdults[j], pups =  df$nPups[j], allIndOppSexRelated = 1, JulianDaysSin = f.julianDays(year = yearNow, month = monthNow, fun = "sin"), adultsPups = df$nAdults[j]*df$nPups[j])
                colnames(df.values.cov)[which(colnames(df.values.cov)=="adultsPups")] <- "adults:pups"
              }else if(df$sex[j]=="M"){
                df.values.cov <- data.frame(adults = df$nAdults[j], pups =  df$nPups[j], allIndOppSexRelated = 1, JulianDaysCos = f.julianDays(year = yearNow, month = monthNow, fun = "cos"), SSsiblingsTot = max(c(0,nrow(subset(df, inNatalPack==TRUE & df$sex=="M" & ageMonth>=12))-1)))
              }
              df$emProb[j] <- min(c(1, f.pred.emigration(ageMonths1 = df$ageMonth[j], ageMonths2 = df$ageMonth[j]+1, Sex = df$sex[j], df.coeffs = df.coeffs, df.scale.cov = df.scale.cov, df.values.cov = df.values.cov)))
              df$emigrated[j] <- as.logical(rbinom(n = 1, size = 1, prob = df$emProb[j]))
              remove(df.values.cov)
              
              
            }
            
            # B) if individual is dispersing:
          }else if(df$movement.state[j]=="dispersing"){
            
            # B.1) individuals survive or die
            df.values.cov <- data.frame(coalSize = df$nAdults[j], dispDuration = df$disp.durationMonths.cum[j], temp = df.groupsAlive$temp[i], rain = df.groupsAlive$rain[i])
            df$survProb[j] <- min(c(f.survProbInterval(state = "dispersing", ageMonths1 = df$ageMonth[j], ageMonths2 = df$ageMonth[j]+1, Sex = df$sex[j], df.coeffs = df.coeffs, df.scale.cov = df.scale.cov, df.values.cov = df.values.cov, ageMax = ageMax), 1))
            df$alive[j] <- as.logical(rbinom(n = 1, size = 1, prob = df$survProb[j]))
            
            
            # B.2) individuals settle or continue dispersing:
            # if max dispersal duration is reached, individual settles at end of this time step
            if(df$alive[j]==TRUE & df$disp.durationMonths.cum[j]>=df$disp.durationMonths.max[j]){
              df$settled[j] <- TRUE
              
              # if max dispersal duration is not yet reached, we increment current dispersal duration by one month and individual continues dispersing:
            }else if(df$alive[j]==TRUE & df$disp.durationMonths.cum[j]<df$disp.durationMonths.max[j]){
              df$disp.durationMonths.cum[j] <- df$disp.durationMonths.cum[j]+1
              df$settled[j] <- FALSE
            }
            
          }
          
        }
        
        
        
        
        ########
        # for resident groups: 
        if(df.groupsAlive$type[i]=="pack"){
          
          #####
          # 1) update dominance status:
          
          # FEMALE:
          # check if dominant female is dead:
          if(!TRUE%in%df$dominant[which(df$sex=="F" & df$alive==TRUE)]){
            # assign dominance to a new founding female:
            id.dom.F <- f.draw.dominant(sex = "F", new.group = FALSE, df.candidates = subset(df, sex=="F" & packFounder==TRUE & alive==TRUE, select = c(idDog, sex, ageMonth, dominant)))
            if(length(id.dom.F)>0) if(!is.na(id.dom.F)) df$dominant[which(df$idDog==id.dom.F)] <- TRUE
          }
          
          # MALE:
          # check if dominant male is dead:
          if(!TRUE%in%df$dominant[which(df$sex=="M" & df$alive==TRUE)]){
            # assign dominance to a new founding male:
            id.dom.M <- f.draw.dominant(sex = "M", new.group = FALSE, df.candidates = subset(df, sex=="M" & packFounder==TRUE & alive==TRUE, select = c(idDog, sex, ageMonth, dominant)), dom.male.dead = TRUE)
            if(length(id.dom.M)>0) if(!is.na(id.dom.M)) df$dominant[which(df$idDog==id.dom.M)] <- TRUE
            
            
            # if dominant male is alive and its mating season (i.e. March), re-assign dominance:
          }else if(monthNow==3){
            # re-assign dominance:
            id.dom.M <- f.draw.dominant(sex = "M", new.group = FALSE, df.candidates = subset(df, sex=="M" & packFounder==TRUE & alive==TRUE, select = c(idDog, sex, ageMonth, dominant)), dom.male.dead = FALSE)
            if(length(id.dom.M)>0){
              if(!is.na(id.dom.M)){
                df$dominant[which(df$sex=="M" & df$alive==TRUE)] <- FALSE
                df$dominant[which(df$idDog==id.dom.M)] <- TRUE
              }
            }
          }
          
          if(nrow(subset(df, sex=="M" & dominant==TRUE & alive==TRUE))>1) stop("more than one male is dominant")
          if(nrow(subset(df, sex=="F" & dominant==TRUE & alive==TRUE))>1) stop("more than one female is dominant")
          
          #####
          # 2) update reproductive stage of dominant female:
          
          # if it is mating season (i.e. March), dominant female gets pregnant:
          # check if dominant female and dominant male are alive:
          if(monthNow==3 & TRUE%in%df$dominant[which(df$sex=="F" & df$alive==TRUE)] & TRUE%in%df$dominant[which(df$sex=="M" & df$alive==TRUE)]){
            df$reproductive.stage[which(df$sex=="F" & df$dominant==TRUE)] <- "pregnant"
          }
          
          # if its whelping time (i.e. June), dominant female gives birth:
          # check if dominant female is alive and pregnant:
          if(monthNow==6 & TRUE%in%df$dominant[which(df$sex=="F" & df$alive==TRUE)]){
            if(df$reproductive.stage[which(df$sex=="F" & df$dominant==TRUE & df$alive==TRUE)]=="pregnant"){
              
              # change reproductive stage to "lactating":
              df$reproductive.stage[which(df$sex=="F" & df$dominant==TRUE)] <- "lactating"
              
              # draw litter size:
              litterSize <- f.drawLitterSize(nDraws = 1, adults = df.groupsAlive$nAdults[i], femaleAge.yrs = df$ageMonth[which(df$sex=="F" & df$dominant==TRUE & df$alive==TRUE)]/12)
              
              # draw litter sex ratio:
              litterSexRatio <- f.drawLitterSexRatio(nDraws = 1, litterSize = litterSize, femaleAge.yrs = df$ageMonth[which(df$sex=="F" & df$dominant==TRUE & df$alive==TRUE)]/12)
              
              # ID of parents:
              idMother <- df.individuals$idDog[which(df.individuals$idGroup==df.groupsAlive$idGroup[i] & df.individuals$sex=="F" & df.individuals$dominant==TRUE & df.individuals$year==yearNow & df.individuals$month==3 & df.individuals$alive==TRUE & df.individuals$reproductive.stage=="pregnant")]
              idFather <- df.individuals$idDog[which(df.individuals$idGroup==df.groupsAlive$idGroup[i] & df.individuals$sex=="M" & df.individuals$dominant==TRUE & df.individuals$year==yearNow & df.individuals$month==3 & df.individuals$alive==TRUE)]
              # check if dominant male was dominant in March but died during that month:
              if(length(idFather)==0) idFather <- df.individuals$idDog[which(df.individuals$idGroup==df.groupsAlive$idGroup[i] & df.individuals$sex=="M" & df.individuals$dominant==TRUE & df.individuals$year==yearNow & df.individuals$month==3 & df.individuals$alive==FALSE)]
              
              # subset(df.groups, idGroup==df.groupsAlive$idGroup[i])
              # View(subset(df.individuals, idGroup==df.groupsAlive$idGroup[i]))
              
              # add litter to df.litters:
              row.litter <- data.frame(id = ifelse(test = is.null(df.litters), yes = 1, no = max(df.litters$id)+1),
                                       idGroup = df.groupsAlive$idGroup[i],
                                       litterSize = litterSize,
                                       litterSexRatio = litterSexRatio,
                                       year = yearNow,
                                       month = monthNow,
                                       run = runNow,
                                       idMother = idMother,
                                       idFather = idFather
              )
              df.litters <- f.pushRow(df = df.litters, row = row.litter)
              remove(row.litter)
              
              # add pups to df.individuals:
              malePups <- litterSexRatio*litterSize
              femalePups <- litterSize-malePups
              df.individuals <- f.createDog(df.individuals = df.individuals, 
                                            nMales = malePups, nFemales = femalePups,
                                            year = yearNow, month = monthNow, run = runNow,
                                            groupID = df.groupsAlive$idGroup[i], packFounder = FALSE,
                                            idMother = idMother, 
                                            idFather = idFather,
                                            idLitter = ifelse(test = is.null(df.litters), yes = 1, no = max(df.litters$id)+1)
              )
              
            }
          }
          
          # if pups are weaned after 3 months, dominant female becomes non-reproductive:
          if(monthNow==6+3 & TRUE%in%df$dominant[which(df$sex=="F" & df$alive==TRUE)]){
            pos <- which(df$sex=="F" & df$dominant==TRUE & df$reproductive.stage=="lactating")
            if(length(pos)>0) df$reproductive.stage[pos] <- "non-reproductive"
          }
          
        }
        
        # if at least one individual emigrated, we have to draw size of dispersing coalition, dispersal duration, and add a new group (i.e. a dispersing coalition):
        df2 <- subset(df, emigrated==TRUE & alive==TRUE)
        if(nrow(df2)>0){
          
          # we look at sexes separately:
          for(Sex in c("F","M")){
            
            df2.sex <- subset(df2, sex==Sex)
            if(nrow(df2.sex)>0){
              # check how many individuals of this sex are emigrating
              nEmi <- nrow(df2.sex)
              
              # check how many individuals of this sex are potential emigrants:
              df2.sex.p <- subset(df, sex==Sex & alive==TRUE & !is.na(emProb))
              
              # draw size of the dispersing coalition --> truncate upper limit of draw by the number of potential dispersers
              if(nrow(df2.sex.p)>nEmi){
                coalSize <- nrow(df2.sex.p)+1
                while(coalSize>nrow(df2.sex.p) | coalSize<1){
                  coalSize <- f.drawDispCoalSize(nDraws = 1, pups = df2.sex.p$nPups[1], SSsiblings = (nrow(df2.sex.p)-1))
                }
                
                # if more individuals should be dispersing simultaneously, we select dispersers based on highest emProb
                if(coalSize>nEmi){
                  df2.fp2 <- subset(df2.sex.p, emigrated==FALSE)
                  df2.fp2 <- df2.fp2[with(df2.fp2, order(emProb, decreasing = TRUE)), ]
                  df2.fp2 <- df2.fp2[c(1:(coalSize-nEmi)),]
                  df$emigrated[which(df$id%in%df2.fp2$id)] <- TRUE
                  
                  # update the number of emigrants:
                  nEmi <- coalSize
                }
                
              }
              
              # we only follow the dispersal fate of individuals that were born in one of the initial packs:
              idLitters.initialPack <- df.litters$id[which(df.litters$idGroup%in%unique(df.groups$idGroup[which(df.groups$run==runNow & df.groups$initialPack==TRUE)]))]
              
              # check if there are emigrating dogs that were born in one of the initial packs:
              rows <- which(df$emigrated==TRUE & df$alive==TRUE & df$sex==Sex & df$idLitter%in%idLitters.initialPack)
              
              if(length(rows)>0){
                
                # generate a new dispersing coalition:
                idGroup.disp <- max(df.groups$idGroup)+1
                
                # we add a new group to df.groups:
                row <- data.frame(id = ifelse(test = is.null(df.groups), yes = 1, no = nrow(df.groups)+1), #unique row ID
                                  idGroup = idGroup.disp, #unique group ID
                                  run = runNow,
                                  year = year.next,
                                  month = month.next,
                                  type = "disp.coalition",
                                  nAdults = length(rows),
                                  nPups = 0,
                                  temp = NA,
                                  rain = as.numeric(NA),
                                  #habitat = as.numeric(NA),
                                  denning = FALSE,
                                  initialPack = FALSE # label for packs that were part of simulation initialization
                )
                
                df.groups <- f.pushRow(df = df.groups, row = row)
                remove(row)
                
                # we update state variables and auxiliary variables: 
                rows <- which(df$emigrated==TRUE & df$alive==TRUE & df$sex==Sex & df$idLitter%in%idLitters.initialPack)
                df$idGroup[rows] <- idGroup.disp
                df$nPups[rows] <- 0
                df$nAdults[rows] <- nEmi
                df$movement.state[rows] <- "dispersing" 
                df$disp.durationMonths.cum <- 1
                df$inNatalPack[rows] <- FALSE 
                
                # draw dispersal duration (in months):
                df$disp.durationMonths.max[rows] <- f.drawDispDuration(nDraws = 1, unit = "months")
                
                remove(rows)
                
              }
            }
          }
        }
        remove(df2)
        
        # set id.dom.newGroup to zero for now
        id.dom.newGroup <- 0
        
        # if individuals settled, we have to create a newly formed pack:
        rows <- which(df$settled==TRUE & df$alive==TRUE)
        if(length(rows)>0){
          
          # a new pack is founded at end of this time step:
          groupID.new <- ifelse(test = is.null(df.groups), yes = 1, no = max(df.groups$idGroup)+1)
          
          # we randomly draw the number of opposite-sex founders and their age:
          nOSfounders <- sample(x = nFounders, size = 1)
          ageOSfounders <- sample(x = rep(founders.ageMonths, 2), size = 1)
          
          # get sex of opposite-sex founders based on sex of dispersing dogs
          OSsex <- ifelse(test = df$sex[rows[1]]=="F", yes = "M", no = "F")
          
          # we add a newly founded pack to df.groups
          row <- data.frame(id = ifelse(test = is.null(df.groups), yes = 1, no = nrow(df.groups)+1), #unique row ID
                            idGroup = groupID.new, #unique group ID
                            run = runNow,
                            year = year.next,
                            month = month.next,
                            type = "pack",
                            nAdults = nOSfounders+length(rows),
                            nPups = 0,
                            temp = NA,
                            rain = NA,
                            #habitat = as.numeric(NA),
                            denning = FALSE,
                            initialPack = FALSE # label for packs that were part of simulation initialization
          )
          
          df.groups <- f.pushRow(df = df.groups, row = row)
          remove(row)
          
          # we add opposite-sex founders to df.individuals and assign dominance:
          df.individuals <- f.createDog(df.individuals = df.individuals, 
                                        nMales = ifelse(test = OSsex=="M", yes = nOSfounders, no = 0), 
                                        nFemales = ifelse(test = OSsex=="F", yes = nOSfounders, no = 0), 
                                        maleAgeMonths = ifelse(test = OSsex=="M", yes = ageOSfounders, no = NA), 
                                        femaleAgeMonths = ifelse(test = OSsex=="F", yes = ageOSfounders, no = NA), 
                                        year = year.next, month = month.next, run = runNow, 
                                        groupID = groupID.new, packFounder = TRUE)
          
          # we assign dominance to one of the founding individuals of the same-sex as dispersers:
          id.dom.newGroup <- f.draw.dominant(sex = df$sex[rows[1]], new.group = TRUE, df.candidates = df[rows,])
          
          # we add dispersers as pack founders to df.individuals for the next time step:
          df2 <- df[rows,]
          df2$id <- max(df.individuals$id)+c(1:nrow(df2)) # assign unique ids
          df2$year <- year.next
          df2$month <- month.next
          df2$idGroup <- groupID.new
          df2$movement.state <- "resident"
          df2$ageMonth <- df2$ageMonth+1
          df2$nAdults <- nOSfounders+length(rows)
          df2$nPups <- 0
          df2$settled <- NA
          df2$inNatalPack <- FALSE
          df2$packFounder <- TRUE
          df2$dominant <- FALSE
          df2$dominant[which(df2$idDog==id.dom.newGroup)] <- TRUE # we assign dominance
          df2$disp.durationMonths.cum <- df2$disp.durationMonths.max <- df2$settled <- df2$survProb <- NA
          
          df.individuals <- pushRow(df = df.individuals, row = df2)
          remove(df2, rows)
          
          # we update state variables in df.individuals for the current time step:
          rows <- which(df.individuals$id%in%df$id)
          df.individuals$alive[rows] <- df$alive 
          df.individuals$survProb[rows] <- df$survProb
          df.individuals$settled[rows] <- df$settled  
          
          # we skip the rest and move on the the next group
          remove(row.group, df, rows)
          next(i)
          
          
        }
        
        
        # we  update variables associated with this group
        row.group$year <- year.next
        row.group$month <- month.next
        row.group$nAdults <- nrow(subset(df, idGroup==row.group$idGroup & year==yearNow & month==monthNow & ageMonth>=12 & alive==TRUE))
        litterSize <- df.litters$litterSize[which(df.litters$idGroup==df.groupsAlive$idGroup[i] & df.litters$year==yearNow & df.litters$month==monthNow & df.litters$run==runNow)]
        row.group$nPups <- ifelse(test = length(litterSize)>0, yes = litterSize, no = nrow(subset(df, idGroup==row.group$idGroup & year==yearNow & month==monthNow & ageMonth<12 & alive==TRUE)) ) 
        row.group$denning <- FALSE
        
        # if there are surviving pups and the next month is July or August, the pack is denning during the next month:
        if(row.group$nPups>0 & (row.group$month==7 | row.group$month==8) ) row.group$denning <- TRUE
        
        # if next month is June and dominant female is alive and pregnant at end of May, pack is denning in June:
        if(month.next==6 & monthNow==5){
          # get id of dom. female:
          id.dom.F <- df.individuals$idDog[which(df.individuals$idGroup==row.group$idGroup & df.individuals$year==yearNow & df.individuals$month==monthNow & df.individuals$sex=="F" & df.individuals$dominant==TRUE & df.individuals$alive==TRUE & df.individuals$reproductive.stage=="pregnant")]
          if(length(id.dom.F)==1) row.group$denning <- TRUE
        }
        
        #row.group$denning <- ifelse(test = row.group$nPups>0 & row.group$month>=6 & row.group$month<=6+2, yes = TRUE, no = FALSE) 
        #row.group$temp <- 
        
        # we update state variables in df.individuals for the current time step:
        rows <- which(df.individuals$id%in%df$id)
        df.individuals$alive[rows] <- df$alive 
        df.individuals$survProb[rows] <- df$survProb
        df.individuals$emigrated[rows] <- df$emigrated
        df.individuals$emProb[rows] <- df$emProb
        df.individuals$reproductive.stage[rows] <- df$reproductive.stage
        df.individuals$dominant[rows] <- df$dominant
        remove(rows)
        
        # if there are individuals that emigrated and were not born in one of the initial packs, we remove them from further simulations as we're only interested in life expectancy and LTRS of dogs born in one of the initiated packs:
        idLitters.initialPack <- df.litters$id[which(df.litters$idGroup%in%unique(df.groups$idGroup[which(df.groups$run==runNow & df.groups$initialPack==TRUE)]))]
        # check if there are emigrating dogs that were not born in one of the initial packs:
        rows <- which(df$emigrated==TRUE & df$alive==TRUE & !df$idLitter%in%idLitters.initialPack)
        if(length(rows)>0) df <- df[-rows,]
        remove(rows)
        
        # if there are no adults left, the group is no longer existing
        if(row.group$nAdults==0){
          # if there are surviving pups, they are assumed to die during this time step:
          if(row.group$nPups>0) df.individuals$alive[which(df.individuals$idGroup==row.group$idGroup & df.individuals$year==yearNow & df.individuals$month==monthNow & df.individuals$alive==TRUE & df.individuals$ageMonth<12)] <- FALSE
          
          # we skip the rest and move on the the next group
          remove(row.group, df)
          next(i)
          
          # if there are adults left, the group is still existing and moves on to the next time step:
        }else if(row.group$nAdults>0){
          row.group$id <- max(df.groups$id)+1 # assign unique ids
          df.groups <- f.pushRow(df = df.groups, row = row.group)
        }
        
        
        # we generate new entries in df.individuals for surviving dogs:
        df3 <- subset(df, alive==TRUE)
        
        # # check if pups are actually dead because all adults died (see a few lines above)
        # idsRemove <- df3$id[which(df3$ageMonth<12)][which(df.individuals$alive[which(df.individuals$id%in%df3$id[which(df3$ageMonth<12)])]==FALSE)]
        # df3 <- subset(df3, !id%in%idsRemove)
        
        # if pups were born in this month, we need to add pups:
        if(length(litterSize)>0){
          df4 <- subset(df.individuals, idGroup==df.groupsAlive$idGroup[i] & year==yearNow & month==monthNow & ageMonth==0 & alive==TRUE)
          if(nrow(df4)>0) df3 <- f.pushRow(df = df3, row = df4)
          remove(df4)
          
        }
        
        if(nrow(df3)>0){
          df3$id <- max(df.individuals$id)+c(1:nrow(df3)) # assign unique ids
          df3$year <- year.next
          df3$month <- month.next
          df3$ageMonth <- df3$ageMonth+1
          
          # for resident individuals:
          df3$nAdults[which(df3$movement.state=="resident")] <- row.group$nAdults
          df3$nPups[which(df3$movement.state=="resident")] <- row.group$nPups
          df3$settled[which(df3$movement.state=="resident")] <- NA
          
          # for dispersing individuals:
          #df3$dominant[which(df3$idDog==id.dom.newGroup & df3$movement.state=="dispersing" & df3$settled==TRUE)] <- TRUE
          df3$emigrated[which(df3$movement.state=="dispersing" & df3$emigrated==TRUE)] <- FALSE
          df3$emProb[which(df3$movement.state=="dispersing")] <- NA
          #df3$idGroup[which(df3$movement.state=="dispersing" & df3$settled==TRUE)] <- groupID.new
          #df3$movement.state[which(df3$movement.state=="dispersing" & df3$settled==TRUE)] <- "resident"
          
          df.individuals <- f.pushRow(df = df.individuals, row = df3)
          remove(df3)
        }
        
        remove(row.group, df)
        
      }
      
    }
    
    # we update time step:
    month <- month.next
    year <-  year.next
    
    # we remove row names
    rownames(df.groups) <- NULL
    rownames(df.individuals) <- NULL
    if(!is.null(df.litters)) rownames(df.litters) <- NULL
    
    remove(tempNow, rainNow)
    
  }
  
  df.rain$id <- c(1:nrow(df.rain))
  df.tempMax$id <- c(1:nrow(df.tempMax))
  
  if(TRUE%in%duplicated(df.litters$id) ) stop("non-unique ids in df.individuals") 
  if(TRUE%in%duplicated(df.individuals$id) ) stop("non-unique ids in df.individuals") 
  if(TRUE%in%duplicated(df.groups$id) ) stop("non-unique ids in df.groups") 
  
  # save copies of df's
  saveRDS(object = df.individuals, file = paste0(folderSim,"run_",runNow,"_df.individuals.RData"))
  saveRDS(object = df.groups, file = paste0(folderSim,"df.groups.RData"))
  saveRDS(object = df.litters, file = paste0(folderSim,"df.litters.RData"))
  saveRDS(object = df.rain, file = paste0(folderSim,"df.rain.RData"))
  saveRDS(object = df.tempMax, file = paste0(folderSim,"df.tempMax.RData"))
  
  # we keep one complete df of all individuals
  df.individuals.full <- f.pushRow(df = df.individuals.full, row = df.individuals)
  saveRDS(object = df.individuals.full, file = paste0(folderSim,"df.individuals.full.RData"))
  
  # reset df.individuals:
  df.individuals <- NULL
  
  end_time <- Sys.time()
  print(paste0("-----> run time of run ",runNow," = ", round(end_time - start_time, 1), " hours"))
  
}


# df.groups <- readRDS("IBM-simulation/sim_9/df.groups.RData")
# df.litters <- readRDS("IBM-simulation/sim_9/df.litters.RData")
# df.rain <- readRDS("IBM-simulation/sim_9/df.rain.RData")
# df.tempMax<- readRDS("IBM-simulation/sim_9/df.tempMax.RData")
# df.individuals<- readRDS("IBM-simulation/sim_9/df.individuals.full.RData")

# end_time <- Sys.time()
# end_time - start_time #--> 1 run with 64 initial packs lasts 3hours


