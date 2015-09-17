PRICHOD <- function(n_jizd,delka_bh,n_lidi,sd_lidi,cap,stredni_doba_prichodu,sd2,counter){
  ## n_jizd                      ... vytvori data frame o n_jizd jizdach 
  ## n_lidi (sd_lidi)            ... s prumernym poctem lidi
  ## delka_bh                    ... s delkou booking horizontu ve dnech
  ## stredni_doba_prichodu (sd2) ... se stredni dobou prichodu zakazniku ve dnech
  
#   n_jizd   <- 10
#   delka_bh <- 30
#   n_lidi   <- 40 ; sd_lidi  <- 5
#   cap      <- 50
#   stredni_doba_prichodu <- 7.5 ; sd2 <- 0.79
  
  ### ------------------------------------------------------------------
  mu    <- 1-stredni_doba_prichodu/delka_bh ; var <- sd2^2/delka_bh
  alpha <- ((1 - mu) / var - 1 / mu) * mu ^ 2
  beta  <- alpha * (1 / mu - 1)
  if(alpha < 0 | beta < 0){stop("Parameters of beta distributiuon are negative.")}
  
  #alpha <- 6
  #beta  <- 2
  #mu  <- (1-alpha/(alpha+beta))*delka_bh
  #var <- (alpha*beta)/((alpha+beta)^2*(alpha+beta+1))*delka_bh
  
  #curve(dbeta(x,alpha,beta))
  #curve(pbeta(x,alpha,beta))
  
  if(exists("tickets") == T){tickets <- tickets[,c(1,2)]}else{tickets <- data.frame(matrix(NA, nrow=0, ncol=2)) ; counter <- 1}
  
  for(i in 1 : n_jizd){
    jede_lidi <- min(floor(rnorm(1,n_lidi,sd_lidi)),cap)
    y         <- 1 - rbeta(jede_lidi, alpha, beta, ncp = 0)
    dtg       <- y * delka_bh
    hist(dtg,xlim=c(0,delka_bh),breaks=10)
    
    df1     <- data.frame(x = rep(counter,jede_lidi), y = dtg) ; names(df1) <- c("ride_id","dtg")
    tickets <- rbind( tickets , df1 )    
  }
  
  names(tickets) <- c("ride_id","dtg")
  return(tickets)    
}



BINY_DEF <- function(pocet_binu,delka_bh){
  ## definice kvanitlovych binu
  #df1 <- data.frame(matrix(NA,pocet_binu,3))
  #for(i in 1 : pocet_binu){
  #  df1[i,] <-  c(i,quan_med[i],quan_med[i+1])
  # df1[1,2] <- 0 
  #  df1[pocet_binu,3] <- delka_bh
  #}  
  
  # definice pevnych binu
  df1 <- data.frame(matrix(NA,pocet_binu,3))
  for(i in 1 : pocet_binu){
    df1[i,] <-  c(i,delka_bh/(2^(pocet_binu-i+1)),delka_bh/(2^(pocet_binu-i)))
    df1[1,2] <- 0
  } 
  return(df1)
}



QUANTILES <- function(tickets,pocet_binu,hist_length,next_ride){
  
  quan_hist <- data.frame(matrix(NA, nrow=hist_length, ncol=pocet_binu+1))
  for(j in 1 : hist_length){
    ride         <- next_ride - j
    x            <- tickets[tickets$ride_id == ride, ]
    quan_hist[j,]<- quantile(x$dtg, probs = seq(0,1,1/pocet_binu), na.rm = TRUE,  names = TRUE)
  }
  
  return(apply(quan_hist,2,median))
}




FCUSAZENI <- function(tickets,kontingent,fares){
  
  # pro kazdou ridu usadim lidi ->> priradim kazdemu ticketu poradi ->> poradi priradim cenu
  tickets$poradi <- rep(-42,dim(tickets)[1])
  tickets$fare   <- rep(fares[length(fares)],dim(tickets)[1])
  rides          <- unique(tickets$ride_id)
  
  for(i in 1 : length(rides)){
    ride <- rides[i]
    #poradi pro ridu
    tickets[tickets$ride_id == ride, ][order(tickets[tickets$ride_id == ride, ]$dtg,decreasing = TRUE), ]$poradi      <-     seq(1,length(tickets[tickets$ride_id == ride, ][order(tickets[tickets$ride_id == ride, ]$dtg,decreasing = TRUE), ]$poradi),1)
    delka <- dim(tickets[tickets$ride_id == ride, ][order(tickets[tickets$ride_id == ride, ]$dtg,decreasing = TRUE), ])[1]
    
    for(j in 1 : delka){
      pom_poradi <- tickets[tickets$ride_id == ride, ][order(tickets[tickets$ride_id == ride, ]$dtg,decreasing = TRUE), ]$poradi[j]

      pom_kontingent <- rev(cumsum(rev(kontingent)))
      for(k in 1 : length(pom_kontingent)){
        if(pom_poradi <= pom_kontingent[k]){cena <- fares[k]}  
      }   
      
      tickets[tickets$ride_id == ride, ][order(tickets[tickets$ride_id == ride, ]$dtg,decreasing = TRUE), ]$fare[j] <- cena
    } 
    
  }  
  return(tickets)
}



                              # tickets ~ kolik lidi prislo na next_ride
FCUSAZENI1 <- function(df_ystar,tickets,kontingent,fares,next_ride,pocet_binu,df1){ # usazeni next_ride

  # pro kazdou ridu usadim lidi ->> priradim kazdemu ticketu poradi ->> priradim cenu
  tickets$poradi  <- rep(-42,dim(tickets)[1])
  tickets$fare    <- rep(fares[length(fares)],dim(tickets)[1])
  tickets$usazeno <- rep(-42,dim(tickets)[1])
  
  ride            <- 1#next_ride
  
  # poradi pro ridu
  tickets[tickets$ride_id == ride, ][order(tickets[tickets$ride_id == ride, ]$dtg,decreasing = TRUE), ]$poradi      <-     seq(1,length(tickets[tickets$ride_id == ride, ][order(tickets[tickets$ride_id == ride, ]$dtg,decreasing = TRUE), ]$poradi),1)
  tickets$id <- seq(1,dim(tickets)[1])
  # pocet ticketu pro danou ridu
  delka <- dim(tickets[tickets$ride_id == ride, ][order(tickets[tickets$ride_id == ride, ]$dtg,decreasing = TRUE), ])[1]
  
  #pom_poradi <- tickets[tickets$ride_id == ride, ][order(tickets[tickets$ride_id == ride, ]$dtg,decreasing = TRUE), ]$poradi
   
  # rozradit tickets do binu
  tickets$bin_id <- rep(NA, delka) 
  for(l in 1 : pocet_binu){
    od <- df1[l,2]
    do <- df1[l,3]
    tickets$bin_id[which(tickets$dtg>od & tickets$dtg<=do)] <- rep(df1[l,1],length(tickets$bin_id[which(tickets$dtg>od & tickets$dtg<=do)]))
  }
  
  # urcit poradi v ramci binù
  for(l in 1 : pocet_binu){
    tickets[tickets$ride_id == ride & tickets$bin_id == l, ][order(tickets[tickets$ride_id == ride & tickets$bin_id == l, ]$dtg,decreasing = TRUE), ]$poradi      <-     seq(1,length(tickets[tickets$ride_id == ride & tickets$bin_id == l, ][order(tickets[tickets$ride_id == ride & tickets$bin_id == l, ]$dtg,decreasing = TRUE), ]$poradi),1)
  }
  
  df_usazeno <- data.frame(matrix(0,length(fares),pocet_binu+2))
  for(l in 1 : pocet_binu){
    usad_vys <- 0
    for(m in length(fares) : 1){
      df_l            <- tickets[with(tickets, order(tickets$poradi)), ] ; df_lm <- df_lm[df_lm$bin_id==l & usazeno!=1, ]
      
      available_seats <- df_ystar[m,l]                                                                                              # kolik mohu usadit v l, m <<- EMSRb
      needed_seats    <- dim(df_l)[1] + usad_vys                                                                                    # kolik potrebuju usadit do celeho binu
      
      kolik_usadim    <- min(available_seats,needed_seats)                                                                          # pocet jaky realne usadim v l, m
      df_usazeno[m,l] <- kolik_usadim                                 
      ids             <- df_l$id[1:kolik_usadim]                                                                                    # id usazenych
      tickets$usazeno[which(is.element(tickets$id,ids))] <- 1                                                                       # zadam je do tickets
      tickets$fare[which(is.element(tickets$id,ids))]    <- rep(fares[m],length(tickets$fare[which(is.element(tickets$id,ids))]))   # pripisu cenu
      usad_vys        <- usad_vys + max( needed_seats - available_seats , 0 )                                                       # kolik zbzva usadit do vyssi FC
      needed_seats    <- max( needed_seats - kolik_usadim , 0 )                                                                     # kolik stale zbyva usadit
      
      # presahl jsem kapacitu binu
      if(usad_vys > 0 & m == 1){                                                                                                    # kontingencni hlidani
        df_lm <- df_lm[df_lm$bin_id==l & usazeno!=1, ]                                                                              # tickety, ktera zbyvaji k usazeni
        ids   <- df_lm$id
        tickets$fare[which(is.element(tickets$id,ids))] <- rep(fares[m], length(tickets$fare[which(is.element(tickets$id,ids))]))   # EMSRb cena
        
        # poradi vsech ticketu
        tickets[tickets$ride_id == ride, ][order(tickets[tickets$ride_id == ride, ]$dtg,decreasing = TRUE), ]$poradi      <-     seq(1,length(tickets[tickets$ride_id == ride, ][order(tickets[tickets$ride_id == ride, ]$dtg,decreasing = TRUE), ]$poradi),1)
        delka <- dim(tickets[tickets$ride_id == ride, ][order(tickets[tickets$ride_id == ride, ]$dtg,decreasing = TRUE), ])[1]
        
        for(j in 1 : delka){
          pom_poradi <- tickets[tickets$ride_id == ride, ][order(tickets[tickets$ride_id == ride, ]$dtg,decreasing = TRUE), ]$poradi[j]
          
          pom_kontingent <- rev(cumsum(rev(kontingent)))
          for(k in 1 : length(pom_kontingent)){
            if(pom_poradi <= pom_kontingent[k]){cena <- fares[k]}  
          }   
          
          tickets[tickets$ride_id == ride, ][order(tickets[tickets$ride_id == ride, ]$dtg,decreasing = TRUE), ]$fare[j] <- cena
        } 
        
        
        
      }      
      
    }

  }
       
}
  
  
  
  
  
  
  
  
 
  
  
  
  
  
}  

























EOSBINS <- function(quan_med,hist_length,next_ride,pocet_binu,fares,delka_bh,df1){
  # spravna historie 
  tickets_relevant <- tickets[tickets$ride_id==next_ride-1 | tickets$ride_id==next_ride-2 | tickets$ride_id==next_ride-3,]
  df2              <- data.frame(matrix(NA,0,4))
  
  # rozrazeni ticketu do df1 binu a pak rovnou do fares
  for(i in 1 : pocet_binu){
    tickets_bin        <- tickets_relevant[df1$X2[i]<tickets_relevant$dtg & tickets_relevant$dtg<=df1$X3[i], ]
    #tickets_bin$bin_id <- rep(i,dim(tickets_bin)[1]) 
    
    for(j in 1 : length(fares)){
      tickets_bin_fares <- tickets_bin[tickets_bin$fare==fares[j],] %>% group_by(ride_id) %>% summarise(demand <- n())
      tickets_bin_fares <- data.frame(tickets_bin_fares)
      
      zeros_to_add <- hist_length-dim(tickets_bin_fares)[1]
      vec          <- c(tickets_bin_fares$demand, rep(0,zeros_to_add)) 
      
      if(dim(tickets_bin_fares)[1]==0                               ){expected <- 0}else{expected <- mean(vec)}
      if(dim(tickets_bin_fares)[1]==0 | dim(tickets_bin_fares)[1]==1){      sd <- 0}else{      sd <-   sd(vec)}
      
      row <- data.frame(t(c(i, fares[j], expected, sd)))
      df2 <- rbind(df2,row)      
      
    }
    # EO celeho binu
    tickets_bin_EO <- tickets_bin %>% group_by(ride_id) %>% summarise(demand <- n())
    
    zeros_to_add <- hist_length-dim(tickets_bin_EO)[1]
    vec          <- c(tickets_bin_EO$demand, rep(0,zeros_to_add)) 
    
    if(dim(tickets_bin_EO)[1]==0                            ){expected <- 0}else{expected <- mean(vec)}
    if(dim(tickets_bin_EO)[1]==0 | dim(tickets_bin_EO)[1]==1){      sd <- 0}else{      sd <-   sd(vec)}
    
    row <- data.frame(t(c(i, NA, expected, sd)))
    df2 <- rbind(df2,row)   
    
  }  

  return(df2)   # predikce v ramci binu pro next_ride
}




OBRAZEK <- function(fares_bin_demand,pocet_binu,fares){
  df4 <- na.omit(fares_bin_demand)
  
  df3 <- data.frame(matrix(NA,length(fares),pocet_binu))
  for(i in 1 : length(fares)){
    for(j in 1 : pocet_binu){
      df3[i,j] <- df4$expected[df4$bin_id==j & df4$fare==fares[i]]  
    }  
  }
  df3 <- data.matrix(df3) ; df3 <- df3[seq(length(fares),1,-1),]
  barplot(df3, main="FC Demand", col=rev(c("gold","gray77","red","orange","green")), legend = rownames(df3), xlab="BH bins", ylab="# tickets")  
}




BILETO_EMSRB <- function(Fare,Mean,Var,pup,cap,Mean_pred){              # pup = c("bullshit" ,P[2->1], P[3->2+], P[4->3+], P[5->4+], P[6->5+])
  
  if(sum(pup>1)>0){stop("Dave says - Probabilities pup out of range [0,1].")}
  
  ## Making Mean nonzero for pup aprroach. Else dividing by zero in "p = 1 - ... " BULLSHIT zejo
  Mean_pred[1] <- max(Mean_pred[1],1)
  Mean[1]      <- max(Mean[1],1)
  
  
  
  PL <- rep(cap,length(Fare))
  for(j in (length(PL)-1) : 1){                               # zacina se od konce prechodem z classy cjslo (length(PL)-1)
    
    # 1. moznost
    #p     = 1 - (Fare[j+1]-pup[j+1]*((sum(Mean[1:j]))^-1)*(sum((Mean*Fare)[1:j])))/( (1-pup[j+1])*((sum(Mean[1:j]))^-1)*(sum((Mean*Fare)[1:j])) )
    
    # 2. moznost 
    p     = 1 - (Fare[j+1]-pup[j+1]*Fare[j])/( (1-pup[j+1])*((sum(Mean[1:j]))^-1)*(sum((Mean*Fare)[1:j])) )
    
    # 3. moznost 
    #p     = 1 - (Fare[j+1]-pup[j+1]*Fare[j])/( (1-pup[j+1])*((sum(Mean_pred[1:j]))^-1)*(sum((Mean_pred*Fare)[1:j])) )
    
    
    if(abs(p) >= 1){p = 1 ; warning("Probability as an input to inverse standardized normal df is greater than one.")}
    Var[Var<0.001]<-0.001 # nize by mohl nasobit 0*Inf a to neumi
    PL[j] = sum(Mean[1:j]) + sqrt(sum(Var[1:j]))*qnorm(p = p, mean = 0, sd = 1) 
    PL[j] = min(PL[j],cap)
    PL[j] = min(ceiling(PL[j+1]),PL[j])
    PL[j] = max(PL[j],0)
  }
  #PL
  return(ceiling(PL))
}




OBRAZEK_PER_RIDES <- function(df5,df6,cap,nweeks_sim,fares){
  for(j in 1 : max(df5$ride_id)){
    df6[,j] <- df5[,1][which(df5$ride_id==j)]    # zmena formatu pro plot
  }  
  # cely plot protekcnich levelu do binu
  barplot(data.matrix(df6)[seq(from=dim(df6)[1],to=1),], names.arg=seq(1,nweeks_sim,1), main=paste("Bin",i,"Demand"), legend=rev(fares), col=rev(c("gold","gray77","red","orange","green")), xlab="week (spoj ride_id)", ylab="# tickets", ylim=c(0,cap))  
  
}

