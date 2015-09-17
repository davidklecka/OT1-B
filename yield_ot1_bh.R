rm(list=ls())
setwd("C:/Users/David/Desktop/Bileto/YIELD/EMSR anal/emsr simulace/booking horizon")
source('HELPERS.R')
require(dplyr)
##########################################
n_jizd   <- 10
delka_bh <- 30
n_lidi   <- 40 ; sd_lidi  <- 5
cap      <- 50
stredni_doba_prichodu <- 7.5 ; sd2 <- 0.79

pocet_binu   <- 4
hist_length  <- 3 

kontingent   <- c(10,10,10,10,10)         # first one is the highest FC
fares        <- c(500,400,300,200,100)    # first one is the highest FC
nweeks_sim   <- 52
##########################################
counter <- 1                                                                                                                                                              # cislo ridy k nasimulovani

tickets          <- PRICHOD(1,delka_bh,n_lidi,sd_lidi,cap,stredni_doba_prichodu,sd2,counter)  ; counter <- counter + 1                                                    # inkrementalni simulace prichodu n_jizd poctu ride

biny             <- BINY_DEF(pocet_binu,delka_bh)                                                                                                                         # definice binu
  
# fare dostanou podle usazeni podle spravneho algoritmu zejo
tickets          <- FCUSAZENI(tickets,kontingent,fares)                                                                                                                   # klasicke kontingentove rozhozeni do cen

next_ride        <- max(tickets$ride_id) + 1                                                                                                                              # cislo dalsi ridy, ktera nasledne pribude to tickets

quan_med         <- QUANTILES(tickets,pocet_binu,hist_length,next_ride)                                                                                                   # medianove kvantily

fares_bin_demand <- EOSBINS(quan_med,hist_length,next_ride,pocet_binu,fares,delka_bh, biny) ; names(fares_bin_demand) <- c("bin_id","fare","expected","sd")               # predikce EO pro kazdy bin pres fares pro next_ride

OBRAZEK(fares_bin_demand,pocet_binu,fares)                                                                                                                                # obrazek validujici, ze mam nyni spravne vystupy. Obrazek pro jednu predikovanou ridu

## pro kazdy bin vyhodim df_ystar vektor, tj. predikci pro next_ride
if(exists("df_ystar_all")==F){df_ystar_all <- data.frame(matrix(NA,0,pocet_binu+2))  ; names(df_ystar_all) <- c("bin 1","bin 2","bin 3","bin 4","fare","ride_id")}        # vsechny predikce najednou (jen pro vykresleni)
df_ystar <- data.frame(matrix(NA,length(fares),pocet_binu))                          ; names(df_ystar)     <- c("bin 1","bin 2","bin 3","bin 4")                          # predikce pro next_ride
for(i in 1 : pocet_binu){                                # pro kazdy bin vyhodim ystar vektor
  df4  <- fares_bin_demand[fares_bin_demand$bin_id==i,]  # relevantni historie binu
  Cap  <- round(df4$expected[which(is.na(df4$fare))])
  Mean <- df4$expected[1:(dim(df4)[1]-1)]
  Var  <- df4$sd[1:(dim(df4)[1]-1)]^2
  
  df_ystar[,i]                      <- BILETO_EMSRB(fares,Mean,Var,rep(0,length(fares)),Cap,rep(NA,length(fares))) 
  df_ystar[length(fares),i]         <- Cap               # emsr zacina prodavat z realne kapacity zejo # posledni radek rika, kolik lidi do binu ocekavam. Je tam dany na prasaka.
  if(df_ystar[length(fares),i] < df_ystar[length(fares)-1,i]){stop("Dave says - the last but one FC has higher ystar than the last one (which is equal to the bin capacity, which is bin EO).")}
  if(i == pocet_binu){df_ystar$fare <- fares ; df_ystar$ride_id <- next_ride}
}                                                                                                                                                                         # skutecna predikce pro next_ride do df_ystar
df_ystar_all <- rbind(df_ystar_all,df_ystar)             # pocita agregovane PL pro odjete tydny. Je potreba to takto udelat, kvuli plotu




opar <- par(mfrow=c(pocet_binu,1))                       # pro vykreslovani kompletniho df_ystar_all znovu a znovu
for(i in 1 : pocet_binu){
  df6 <- data.frame(matrix(0,5,nweeks_sim))
  df5 <- df_ystar_all[,c(i,pocet_binu+2)]
  OBRAZEK_PER_RIDES(df5,df6,cap,nweeks_sim,fares)        # obrazek predikce pro jeden bin pro vsechny odjete ridy. Potrebuje df_ystar_all, zejo
  }                            # !!!!!! tady ale chci vykreslovat, za jake ceny se lidi usadi, zejo























## pod novym EMSRb musi bezet klasicky kontingent OT zejo











