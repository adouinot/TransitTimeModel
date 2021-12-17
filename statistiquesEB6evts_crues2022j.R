
rm(list =ls())
library(pracma)
library(plotrix)
library(RSAGA)
###


## info
stations = c("Koedange","Heffingen", "Larochette", "Medernach", "Hessemillen", "Reisdorf")
id_station = c(16,15,14,13,12,3)
coleurs = rev(c("black", "darkcyan", "olivedrab3","navyblue", "pink3", "orangered"))
areas =  c(31.14,48.687, 69.41,79.04,92.981, 100.6)
distance_brut = list()

#####################################################
# load EVTs range
ad = "/media/audrey/8858-1E31/PRO/2019/test_dispositifs/EBgather/"
DATE_EVTS  = read.table(paste(ad, "range_evts2022j.csv",sep =""), header = T,
                        stringsAsFactors = F, sep =";")
for(i in 2:5){
  DATE_EVTS[,i] = as.POSIXct(DATE_EVTS[,i], tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
}
DATE_EVTS = DATE_EVTS[-c(1,2,27,28),]
EVTs = DATE_EVTS$EVTs

#####################################################
#### min chgt du au check -> fin P
# 07.10.2019
DATE_EVTS$FIN_P[EVTs == "2019_10_07"] = as.POSIXct("11/10/2019 06:00", tz = "UTC", format = "%d/%m/%Y %H:%M")
# 20.12.2019
DATE_EVTS$FIN_Q[EVTs == "2019_12_20"] = as.POSIXct("22/12/2019 21:00", tz = "UTC", format = "%d/%m/%Y %H:%M")
# 26.01.2020
DATE_EVTS$FIN_P[EVTs == "2020_01_26"] = as.POSIXct("29/01/2020 06:00", tz = "UTC", format = "%d/%m/%Y %H:%M")
DATE_EVTS$FIN_Q[EVTs == "2020_01_26"] = as.POSIXct("30/01/2020 12:00", tz = "UTC", format = "%d/%m/%Y %H:%M")
# 31.01.2020
DATE_EVTS$FIN_P[EVTs == "2020_01_31"] = as.POSIXct("02/02/2020 16:00", tz = "UTC", format = "%d/%m/%Y %H:%M")
# 09.02.2020
DATE_EVTS$FIN_P[EVTs == "2020_02_09"] = as.POSIXct("11/02/2020 18:00", tz = "UTC", format = "%d/%m/%Y %H:%M")
# 13.02.2020
DATE_EVTS$FIN_P[EVTs == "2020_02_13"] = as.POSIXct("14/02/2020 08:00", tz = "UTC", format = "%d/%m/%Y %H:%M")
# 29.04.2020
DATE_EVTS$FIN_P[EVTs == "2020_04_29"] = as.POSIXct("02/05/2020 20:00", tz = "UTC", format = "%d/%m/%Y %H:%M")
#### min chgt du au check -> fin Q
# 26.11.2019
#DATE_EVTS$FIN_Q[EVTs == "2019_11_26"] = as.POSIXct("30/11/2019 16:00", tz = "UTC", format = "%d/%m/%Y %H:%M")
# 07.10.2019
DATE_EVTS$FIN_Q[EVTs == "2019_10_07"] = as.POSIXct("11/10/2019 20:00", tz = "UTC", format = "%d/%m/%Y %H:%M")
# 29.04.2020
DATE_EVTS$FIN_Q[EVTs == "2020_04_29"] = as.POSIXct("03/05/2020 16:00", tz = "UTC", format = "%d/%m/%Y %H:%M")
# 12.06.2020
DATE_EVTS$FIN_Q[EVTs == "2020_06_12"] = as.POSIXct("14/06/2020 12:00", tz = "UTC", format = "%d/%m/%Y %H:%M")
# 03.06.2020
DATE_EVTS$FIN_Q[EVTs == "2020_06_03"] = as.POSIXct("05/06/2020 08:00", tz = "UTC", format = "%d/%m/%Y %H:%M")
# 17.06.2020
DATE_EVTS$FIN_Q[EVTs == "2020_06_17"] = as.POSIXct("19/06/2020 00:00", tz = "UTC", format = "%d/%m/%Y %H:%M")
# 26.06.2020
DATE_EVTS$FIN_P[EVTs == "2020_06_26"] = as.POSIXct("27/06/2020 18:00", tz = "UTC", format = "%d/%m/%Y %H:%M")
# 02.12.2020
DATE_EVTS$FIN_Q[EVTs == "2020_12_02"] = as.POSIXct("06/12/2020 08:00", tz = "UTC", format = "%d/%m/%Y %H:%M")
# 21.12.2020
DATE_EVTS$FIN_Q[EVTs == "2020_12_21"] = as.POSIXct("26/12/2020 14:00", tz = "UTC", format = "%d/%m/%Y %H:%M")
# 27.12.2020
DATE_EVTS$FIN_Q[EVTs == "2020_12_27"] = as.POSIXct("30/12/2020 20:00", tz = "UTC", format = "%d/%m/%Y %H:%M")
# 26.09.2020
DATE_EVTS$FIN_Q[EVTs == "2020_09_26"] = as.POSIXct("28/09/2020 12:00", tz = "UTC", format = "%d/%m/%Y %H:%M")
# 08.10.2020
DATE_EVTS$FIN_Q[EVTs == "2020_10_08"] = as.POSIXct("11/10/2020 02:00", tz = "UTC", format = "%d/%m/%Y %H:%M")
# 21.01.2021
DATE_EVTS$FIN_Q[EVTs == "2021_01_21"] = as.POSIXct("23/01/2021 06:00", tz = "UTC", format = "%d/%m/%Y %H:%M")
# 27.01.2021
DATE_EVTS$FIN_P[EVTs == "2021_01_27"] = as.POSIXct("02/02/2021 02:00", tz = "UTC", format = "%d/%m/%Y %H:%M")
DATE_EVTS$FIN_Q[EVTs == "2021_01_27"] = as.POSIXct("02/02/2021 15:00", tz = "UTC", format = "%d/%m/%Y %H:%M")
# 02.02.2021
DATE_EVTS$FIN_Q[EVTs == "2021_02_02"] = as.POSIXct("04/02/2021 23:00", tz = "UTC", format = "%d/%m/%Y %H:%M")
# 06.02.2021
DATE_EVTS$FIN_Q[EVTs == "2021_02_06"] = as.POSIXct("08/02/2021 00:00", tz = "UTC", format = "%d/%m/%Y %H:%M")
# 11.03.2021
DATE_EVTS$FIN_P[EVTs == "2021_03_11"] = as.POSIXct("18/03/2021 12:00", tz = "UTC", format = "%d/%m/%Y %H:%M")
# 14.05.2021
DATE_EVTS$FIN_Q[EVTs == "2021_05_14"] = as.POSIXct("19/05/2021 12:00", tz = "UTC", format = "%d/%m/%Y %H:%M")
# 24.05.2021
DATE_EVTS$FIN_Q[EVTs == "2021_05_24"] = as.POSIXct("29/05/2021 00:00", tz = "UTC", format = "%d/%m/%Y %H:%M")
# 24.06.2021
DATE_EVTS$FIN_Q[EVTs == "2021_06_24"] = as.POSIXct("25/06/2021 21:00", tz = "UTC", format = "%d/%m/%Y %H:%M")


adout = "/media/audrey/8858-1E31/PRO/2019/test_dispositifs/EBgather/"
write.table(DATE_EVTS, paste(adout, "range_evts2022jbis.csv",sep =""), row.names = F,sep =";")

#####################################################

# =======================================================
# load HUM time series
dir =  "/media/audrey/8858-1E31/PRO/2019/test_dispositifs/EBgather/"

RelHum = read.csv(paste(dir, "RelHum_raw21.csv", sep =""), header = T, sep = ",", 
                  na.strings = NA, stringsAsFactors = F)
RelHum$date = as.POSIXct(RelHum$date, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")

#####################################################
## chargement des pluies pour le timing

# load RAINFALL time series
dir =  "/media/audrey/8858-1E31/PRO/2019/test_dispositifs/EBgather/"

RainEB = read.csv(paste(dir, "DataloggerRainfall_5minC.csv", sep =""), header = T, sep = ",", 
                  na.strings = NA, stringsAsFactors = F)
RainEB$date = as.POSIXct(RainEB$date, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
names(RainEB) = c("date", "atlinster",
                  "bakesmillen" ,  "schoos", "schwanterhaff")


Raintmp = read.csv(paste(dir, "Rainfall_5minC.csv", sep =""), header = T, sep = ",", 
                   na.strings = NA, stringsAsFactors = F)
Raintmp$date = as.POSIXct(Raintmp$date, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")

Raintmp = merge( RainEB,Raintmp, by = "date", all.x =T, all.y=T)
for(sta_c in c("atlinster",
               "bakesmillen" ,  "schoos", "schwanterhaff")){
  
  idx = is.na(Raintmp[,paste(sta_c, ".x", sep ="")]) & !is.na(Raintmp[,paste(sta_c, ".y", sep ="")]) 
  Raintmp[idx,paste(sta_c, ".x", sep ="")] = Raintmp[idx,paste(sta_c, ".y", sep ="")]
}

RainEB = Raintmp[, 1:5]
names(RainEB) = c("date", "atlinster",
                  "bakesmillen" ,  "schoos", "schwanterhaff")
rm(Raintmp)
# =======================================================

# load RAINFALL thiessen area
dir =  "/media/audrey/8858-1E31/PRO/2019/GIS_data/"
ThiessenArea = read.csv(paste(dir, "EBraingauges_LIST_thiessenAire.csv", sep =""), 
                        header = T, sep = ",", 
                        na.strings = NA, stringsAsFactors = F)
for(i in 1:nrow(ThiessenArea)){
  ThiessenArea[i, -c(1,2)] = ThiessenArea[i, -c(1,2)]/ThiessenArea[i, 2]
}

# calcul des pluies moyennes sur chaque BV
pluviosN = names(ThiessenArea)[-c(1,2)]
for(sta_i in 1:length(stations)){
  
  RainEB[, stations[sta_i]] = 0
  for(p in 1:4){
    
    RainEB[, stations[sta_i]] = RainEB[, stations[sta_i]] + 
      RainEB[, pluviosN[p]]*ThiessenArea[ThiessenArea$BV == stations[sta_i], pluviosN[p] ]
  }
}


# !!! Medernach = troncon heffingen - medernach
RainEB[ , "Medernach"] = (RainEB[ , "Medernach"]*areas[4]-RainEB[ , "Heffingen"]*areas[2])/(areas[4]-areas[2])


# calcul des pluies moyennes sur chaque BV
GeologieArea = ThiessenArea
GeologieArea[GeologieArea$BV == "Koedange",c(3:6)] = c(0.53,0,0,0.47) 
#GeologieArea[GeologieArea$BV == "Koedange",c(3:6)] = c(0.0,0,0,1) 
GeologieArea[GeologieArea$BV == "Heffingen",c(3:6)] = c(0.57,0,0.08,0.35) 
#GeologieArea[GeologieArea$BV == "Heffingen",c(3:6)] = c(1,0,0.0,0.) 
GeologieArea[GeologieArea$BV == "Medernach",c(3:6)] = c(0.51,0.14,0.14,0.21) 

pluviosN = names(ThiessenArea)[-c(1,2)]
for(sta_i in 1:length(stations)){
  
  RelHum[, paste(stations[sta_i], "_SWCtop", sep ="")] = 0
  RelHum[, paste(stations[sta_i], "_SWCbottom", sep ="")] = 0
  for(p in 1:4){
    
    RelHum[, paste(stations[sta_i], "_SWCtop", sep ="")] = RelHum[, paste(stations[sta_i], "_SWCtop", sep ="")] + 
      RelHum[, paste(pluviosN[p], "_SWCtop", sep ="")]*GeologieArea[GeologieArea$BV == stations[sta_i], pluviosN[p] ]
    RelHum[, paste(stations[sta_i], "_SWCbottom", sep ="")] = RelHum[, paste(stations[sta_i], "_SWCbottom", sep ="")] + 
      RelHum[, paste(pluviosN[p], "_SWCbottom", sep ="")]*GeologieArea[GeologieArea$BV == stations[sta_i], pluviosN[p] ]
  }
}
#####################################################
# load dischareh time series
ad = "/media/audrey/8858-1E31/PRO/2019/DATABASE/HYDRO/data/HQ_made_in_home/"


TAB_discharge = read.table(paste(ad, "HQserie_",stations[1],".csv", sep =""), header =T,
                           stringsAsFactors = F, sep= ",")
names(TAB_discharge)= c(names(TAB_discharge)[1], paste(names(TAB_discharge)[-1], "_", stations[1], sep =""))
TAB_discharge$date = as.POSIXct(TAB_discharge$date, format = "%Y-%m-%d  %H:%M:%S", tz = "UTC")
for(i in 2:length(stations)){
  
  tmp =  read.table(paste(ad, "HQserie_",stations[i],".csv",sep =""), header =T,
                    stringsAsFactors = F, sep= ",")
  names(tmp)= c(names(tmp)[1], paste(names(tmp)[-1], "_", stations[i], sep =""))
  idx = !is.na(tmp[ , 3])
  
  tmp$date = as.POSIXct(tmp$date, format = "%Y-%m-%d  %H:%M:%S", tz = "UTC")
  print(range(tmp$date[idx]))
  TAB_discharge = merge(TAB_discharge, tmp, by = "date", all.x = T, all.y = T)
}



DayDischarge = aggregate(TAB_discharge[ , -1], by = list(date = format(TAB_discharge$date, "%Y%m%d")),
                         FUN = "min", na.rm=T)

DayDischarge$date = as.POSIXct(DayDischarge$date, format = "%Y%m%d", tz ="UTC")

rgx = as.POSIXct(c("01062020","01022021"),format = "%d%m%Y", tz ="UTC")
par(mar =c(4,4,2,2))
plot(DayDischarge$date, DayDischarge$Qmed_Koedange, type = "l", xlim = rgx,
     ylim = c(0.05,0.2))
#####################################################
## check rainfall range
coleurs[2] = "purple"
par(mfrow = c(2,2), mar = c(4,4,2,4))
for(evt_i in 1:length(EVTs)){

  if((evt_i %% 4) == 1){
    png(paste(ad_img, "/Runoff4_",EVTs[evt_i], "_4.png", sep=""),
        width= 6.4,height=4.5, units="in", res=400, pointsize = 8)
    par(mfrow = c(2,2), mar = c(4,4,2,4))
    
  }
  idxP = RainEB$date <= DATE_EVTS$FIN_P[evt_i]  &  RainEB$date >= DATE_EVTS$DEB_P[evt_i]
  idxQ = RainEB$date <= DATE_EVTS$FIN_Q[evt_i]  &  RainEB$date >= DATE_EVTS$DEB_Q[evt_i]
  rgx = c(DATE_EVTS$DEB_Q[evt_i],DATE_EVTS$FIN_Q[evt_i])
  plot(RainEB$date[idxQ], RainEB$Reisdorf[idxQ]*12, type = "h", col = "red3", lwd =2,
       ylim = c(30,0), xlab = "", ylab = "", axes = F, xlim = rgx)
  axis.POSIXct(1, at = seq(from = rgx[1], to = rgx[2], length.out = 4),
               labels = format(seq(from = rgx[1], to = rgx[2], length.out = 4),
                               format = "%d/%m %Hh"))
  box()
  axis(4, col.ticks = "royalblue", col ="royalblue" )
  par(new = T)
  plot(RainEB$date[idxP], RainEB$Reisdorf[idxP]*12, type = "h", col = "royalblue", lwd =2,
       xlim = rgx, ylim = c(30,0), xlab = "", ylab = "", axes = F)

  idxT = TAB_discharge$date <= DATE_EVTS$FIN_Q[evt_i]   &  TAB_discharge$date >= DATE_EVTS$DEB_Q[evt_i]
  rgY = c(0.05, 1.5*max(as.vector(TAB_discharge[idxT, paste("Qmed_",stations, sep ="")]), na.rm =T))
  idx = !is.na(TAB_discharge[, paste("Qmed_",stations[1], sep ="")])
  par(new = T)
  plot(TAB_discharge$date[idx & idxT], TAB_discharge[idx & idxT, paste("Qmed_",stations[1], sep ="")],
       type = "l", col = coleurs[1], xlim = rgx, ylim = rgY, xlab = "", ylab = "",
       axes = F)
  axis(2)
  mtext(side = 2, line = 2.2, bquote("Discharge [m"^3 ~".s"^{-1} ~"]"))
  mtext(side = 4, line = 2.2, bquote("Rainfall [mm.h"^{-1} ~"]"), col ="royalblue")
  for(i in 2:5){
    idx = !is.na(TAB_discharge[, paste("Qmed_",stations[i], sep ="")])
    lines(TAB_discharge$date[idx & idxT], TAB_discharge[idx & idxT, paste("Qmed_",stations[i], sep ="")],
          type = "l", col = coleurs[i])
 
  }
  title(main = EVTs[evt_i])
  if((evt_i %% 4) == 0){
    legend("topright", col = coleurs[c(1:5)], stations[c(1:5)], lty = 1,
           bty = "o", bg = "gray90", box.col = "black" ,cex = 1, seg.len=1)
    
    dev.off()
  }
}
#####################################################
#####################################################

#####################################################
## remove Qbase
TAB_runoff = TAB_discharge
for(evt_i in 1:length(EVTs)){
  
  idxQ = TAB_runoff$date <= DATE_EVTS$FIN_Q[evt_i] +17*3600 &  TAB_runoff$date >= DATE_EVTS$DEB_Q[evt_i]
  rgx = c(DATE_EVTS$DEB_Q[evt_i],DATE_EVTS$FIN_Q[evt_i])
  
  
  idxInit = TAB_runoff$date <= (DATE_EVTS$DEB_Q[evt_i]+10*3600)  &  TAB_runoff$date >= DATE_EVTS$DEB_Q[evt_i]
  if(evt_i > 1){
    idxInit = TAB_runoff$date <= (DATE_EVTS$DEB_Q[evt_i]+10*3600)  &  TAB_runoff$date > max(DATE_EVTS$DEB_Q[evt_i], DATE_EVTS$FIN_Q[evt_i-1] +17*3600)
    
  }  
  for(sta_i in 1:length(stations)){
    
    if(sum(!is.na(TAB_runoff[idxInit,paste("Qmed_",stations[sta_i] ,sep ="")]))>0){
      Qb = min(TAB_runoff[idxInit,paste("Qmed_",stations[sta_i] ,sep ="") ], na.rm =T)
      TAB_runoff[idxQ, paste("Qmed_",stations[sta_i] ,sep ="")]= TAB_runoff[idxQ, paste("Qmed_",stations[sta_i] ,sep ="")] - Qb
     # TAB_runoff[idxInit,paste("Qmed_",stations[sta_i] ,sep ="")] = 0
      TAB_runoff[idxQ, paste("Qmed_",stations[sta_i] ,sep ="")][TAB_runoff[idxQ, paste("Qmed_",stations[sta_i] ,sep ="")]<0]=0
    }
  }
}
## some peculiars
# 06.08.2019 
evt_i = 1
idxQ = TAB_runoff$date <= DATE_EVTS$FIN_Q[evt_i] +17*3600 &  TAB_runoff$date >= DATE_EVTS$DEB_Q[evt_i]
TAB_runoff[idxQ,] = TAB_discharge[idxQ,]
FINinit = as.POSIXct("06/08/2019 20:00", format ="%d/%m/%Y %H:%M", tz = "UTC") 
idxInit = TAB_runoff$date <= FINinit  &  TAB_runoff$date >= DATE_EVTS$DEB_Q[evt_i]
for(sta_i in 1:length(stations)){
  if(sum(!is.na(TAB_runoff[idxInit,paste("Qmed_",stations[sta_i] ,sep ="")]))>0){
    Qb = min(TAB_runoff[idxInit,paste("Qmed_",stations[sta_i] ,sep ="") ], na.rm =T)
    TAB_runoff[idxQ, paste("Qmed_",stations[sta_i] ,sep ="")]= TAB_runoff[idxQ, paste("Qmed_",stations[sta_i] ,sep ="")] - Qb
    TAB_runoff[idxQ, paste("Qmed_",stations[sta_i] ,sep ="")][TAB_runoff[idxQ, paste("Qmed_",stations[sta_i] ,sep ="")]<0]=0
    TAB_runoff[idxInit,paste("Qmed_",stations[sta_i] ,sep ="")] = 0
  }
}
# 29.02.2020 
evt_i = 16
idxQ = TAB_runoff$date <= DATE_EVTS$FIN_Q[evt_i]  +17*3600 &  TAB_runoff$date >= DATE_EVTS$DEB_Q[evt_i]
TAB_runoff[idxQ,] = TAB_discharge[idxQ,]
FINinit = as.POSIXct("29/02/2020 12:00", format ="%d/%m/%Y %H:%M", tz = "UTC") 
idxInit = TAB_runoff$date <= FINinit  &  TAB_runoff$date >= DATE_EVTS$DEB_Q[evt_i]
for(sta_i in 1:length(stations)){
  
  if(sum(!is.na(TAB_runoff[idxInit,paste("Qmed_",stations[sta_i] ,sep ="")]))>0){
    Qb = min(TAB_runoff[idxQ,paste("Qmed_",stations[sta_i] ,sep ="") ], na.rm =T)
    print(Qb)
    plot( TAB_runoff[idxQ, paste("Qmed_",stations[sta_i] ,sep ="")])
    TAB_runoff[idxQ, paste("Qmed_",stations[sta_i] ,sep ="")]= TAB_runoff[idxQ, paste("Qmed_",stations[sta_i] ,sep ="")] - Qb
    TAB_runoff[idxQ, paste("Qmed_",stations[sta_i] ,sep ="")][TAB_runoff[idxQ, paste("Qmed_",stations[sta_i] ,sep ="")]<0]=0
    TAB_runoff[idxInit,paste("Qmed_",stations[sta_i] ,sep ="")] = 0
  }
}


Hplus = rep(15, length(EVTs))
Hplus[c(5,13,15,16,18,20,24,26,27,28)] = c(10, 6,6,5,5,5,3,5,5,5) 
HplusM = rep(16, length(EVTs))
HplusM[c(5,6,9,13,15,16,18,20,24,26,27,28)] = c(10,10, 3,3,5,3,7,5,2,10,10,10) 
ad_img = paste("/media/audrey/8858-1E31/PRO/2019/DATABASE/HYDRO/IMG/" )
par(mfrow = c(1,1))
for(evt_i in 1:length(EVTs)){
  
  #png(paste(ad_img, "/Runoff_",EVTs[evt_i], ".png", sep=""),
  #    width= 6.4,height=3, units="in", res=400, pointsize = 8)
  
  idxP = RainEB$date <= DATE_EVTS$FIN_P[evt_i]  &  RainEB$date >= DATE_EVTS$DEB_P[evt_i]
  idxQ = RainEB$date <= DATE_EVTS$FIN_Q[evt_i]+Hplus[evt_i]*3600  &  RainEB$date >= DATE_EVTS$DEB_Q[evt_i]
  idxH = RelHum$date <= DATE_EVTS$FIN_Q[evt_i]  &  RelHum$date >= DATE_EVTS$DEB_Q[evt_i]
  
  rgx = c(DATE_EVTS$DEB_Q[evt_i],DATE_EVTS$FIN_Q[evt_i]+HplusM[evt_i]*3600)
  plot(RainEB$date[idxQ], RainEB$Medernach[idxQ]*12, type = "h", col = "red3", lwd =2,
       ylim = c(30,0), xlab = "", ylab = "", axes = F, xlim = rgx)
  lines(RainEB$date[idxQ], cumsum(RainEB$Medernach[idxQ]))
  axis(4, col.ticks = "royalblue", col ="royalblue")
  axis.POSIXct(1, at = seq(from = rgx[1], to = rgx[2], length.out = 4),
               labels = format(seq(from = rgx[1], to = rgx[2], length.out = 4),
                               format = "%d/%m %Hh"))
  par(new = T)
  plot(RainEB$date[idxP], RainEB$Medernach[idxP]*12, type = "h", col = "royalblue", lwd =2,
       xlim = rgx, ylim = c(30,0), xlab = "", ylab = "", axes = F)
  
  idxT = TAB_runoff$date <= DATE_EVTS$FIN_Q[evt_i]+Hplus[evt_i]*3600   &  TAB_runoff$date >= DATE_EVTS$DEB_Q[evt_i]
  rgY = c(0.0, 1.5*max(as.vector(TAB_runoff[idxT, paste("Qmed_",stations[c(1,2,4)], sep ="")]), na.rm =T))
  par(new = T)
  idxT = TAB_runoff$date <= DATE_EVTS$FIN_Q[evt_i]  &  TAB_runoff$date >= DATE_EVTS$DEB_Q[evt_i]
  idx = !is.na(TAB_runoff[, paste("Qmed_",stations[1], sep ="")])
  plot(TAB_runoff$date[idx & idxT], TAB_runoff[idx & idxT, paste("Qmed_",stations[1], sep ="")],
       type = "l", col = coleurs[1], xlim = rgx, ylim = rgY, xlab = "", ylab = "",
       axes = F)
  axis(2)
  mtext(side = 2, line = 2.1, "Discharge [m3/s]")
  box()
  for(i in c(2,4)){
    if(i == 2){
      idxT = TAB_runoff$date <= DATE_EVTS$FIN_Q[evt_i]+Hplus[evt_i]*3600   &  TAB_runoff$date >= DATE_EVTS$DEB_Q[evt_i]
      
    }
    if(i == 4){
      idxT = TAB_runoff$date <= DATE_EVTS$FIN_Q[evt_i]+HplusM[evt_i]*3600   &  TAB_runoff$date >= DATE_EVTS$DEB_Q[evt_i]
      
    }
    idx = !is.na(TAB_runoff[, paste("Qmed_",stations[i], sep ="")])
    lines(TAB_runoff$date[idx & idxT], TAB_runoff[idx & idxT, paste("Qmed_",stations[i], sep ="")],
          type = "l", col = coleurs[i])
    
  }
  title(main = EVTs[evt_i])
  #dev.off()
}


#####################################################
#####################################################
## remove Qbase
STATS_EVTS = array(dim = c(length(EVTs), length(stations), 20),
                   dimnames = list(names(EVTs), stations,
                                   c("DureeP",  "Vrain", "Intensite","I15min","I1hour",
                                     "Imoy50","Imoy75","Imoy90",
                                     "Vruiss", "QspePeak",
                                     "Train50th",  "Truiss50th",
                                     "Tpeak", "CR","deltaQpeak", "deltaQ50th",
                                     "Hum_initTop","Hum_initBot","Qbase", "dQ5min")))
CumulMax1Pluvios = vector()
Cumul30jrs = vector()
# calcul elts stats
for(evt_i in 1:length(EVTs)){
  
  idx = RainEB$date <= DATE_EVTS$FIN_P[DATE_EVTS$EVTs == EVTs[evt_i]] &
    RainEB$date >= DATE_EVTS$DEB_P[DATE_EVTS$EVTs == EVTs[evt_i]]
  idx2 = RainEB$date <= DATE_EVTS$DEB_P[DATE_EVTS$EVTs == EVTs[evt_i]] &
    RainEB$date >=( DATE_EVTS$DEB_P[DATE_EVTS$EVTs == EVTs[evt_i]] -15*24*3600)
  
  Cumul30jrs[evt_i] =sum(RainEB[idx2, c(  "Medernach")],na.rm =T)
  CumulMax1Pluvios[evt_i] =max( apply(RainEB[idx, c(  "atlinster"  ,   "bakesmillen"  , "schoos",  "schwanterhaff")],
                                      2,sum, na.rm =T))
  for(sta_i in 1:length(stations)){
    
    idx = RainEB$date <= DATE_EVTS$FIN_P[DATE_EVTS$EVTs == EVTs[evt_i]] &
      RainEB$date >= DATE_EVTS$DEB_P[DATE_EVTS$EVTs == EVTs[evt_i]]
    idxn = !is.na( RainEB[,stations[sta_i]])
    tabrain = RainEB[idx & idxn,]
    
    STATS_EVTS[ evt_i, stations[sta_i], "Vrain"] = sum(tabrain[,stations[sta_i] ],na.rm =T)
    STATS_EVTS[ evt_i, stations[sta_i], "Intensite"] = max(tabrain[,stations[sta_i] ],na.rm =T)
    
    int_rgee = sort(tabrain[tabrain[,stations[sta_i] ]>0,stations[sta_i]], decreasing = T)
    idxl = cumsum(int_rgee) < 0.75*sum(int_rgee)
    STATS_EVTS[ evt_i, stations[sta_i], "Imoy75"]=mean(int_rgee[idxl])
    idxl = cumsum(int_rgee) < 0.90*sum(int_rgee)
    STATS_EVTS[ evt_i, stations[sta_i], "Imoy90"]=mean(int_rgee[idxl])
    idxl = cumsum(int_rgee) < 0.50*sum(int_rgee)
    STATS_EVTS[ evt_i, stations[sta_i], "Imoy50"]=mean(int_rgee[idxl])
    
      
    cumucumu = cumsum(tabrain[,stations[sta_i] ])/max(cumsum(tabrain[,stations[sta_i] ]))
    d50th  = tabrain[mean(which(abs(cumucumu-0.5) == min(abs(cumucumu-0.5)))), "date"]
    STATS_EVTS[ evt_i, stations[sta_i], "Train50th"] = d50th
    
    rgx = range(tabrain$date[tabrain[,stations[sta_i]] >0])
    duree = (as.numeric(rgx[2]) - as.numeric(rgx[1]))/3600
    STATS_EVTS[ evt_i, stations[sta_i], "DureeP"] = duree
    
    
    idxT = TAB_discharge$date <=  DATE_EVTS$FIN_Q[DATE_EVTS$EVTs == EVTs[evt_i]] &
      TAB_discharge$date >= DATE_EVTS$DEB_P[DATE_EVTS$EVTs == EVTs[evt_i]]-7*24*3600
    #Qbase
    if(sum(idxT)>0 & sum(!is.na(TAB_discharge[idxT, paste("Qmed_",stations[sta_i], sep ="")]))>0){
      STATS_EVTS[ evt_i, stations[sta_i],"Qbase"] = min(TAB_discharge[idxT, paste("Qmed_",stations[sta_i], sep ="")],
                                                        na.rm =T)
      
    }
    
    idxT = TAB_runoff$date <=  DATE_EVTS$FIN_Q[DATE_EVTS$EVTs == EVTs[evt_i]] &
      TAB_runoff$date >= DATE_EVTS$DEB_P[DATE_EVTS$EVTs == EVTs[evt_i]]
    
    if(sta_i == 2){
      idxT = TAB_runoff$date <=  DATE_EVTS$FIN_Q[DATE_EVTS$EVTs == EVTs[evt_i]] +Hplus[evt_i]*3600 &
        TAB_runoff$date >= DATE_EVTS$DEB_P[DATE_EVTS$EVTs == EVTs[evt_i]]
      
    }
    if(sta_i == 4){
      idxT = TAB_runoff$date <=  DATE_EVTS$FIN_Q[DATE_EVTS$EVTs == EVTs[evt_i]] +HplusM[evt_i]*3600 &
        TAB_runoff$date >= DATE_EVTS$DEB_P[DATE_EVTS$EVTs == EVTs[evt_i]]
      
    }
    
    idx = !is.na(TAB_runoff[, paste("Qmed_",stations[sta_i], sep ="")])
    tmp_tab =TAB_runoff[idx & idxT, ]
    STATS_EVTS[ evt_i, stations[sta_i],"dQ5min"] = max(tmp_tab[-c(1,2), paste("Qmed_",stations[sta_i], sep ="")]-
                                                         tmp_tab[-c(nrow(tmp_tab),nrow(tmp_tab)-1), paste("Qmed_",stations[sta_i],
                                                                                                          sep ="")], na.rm =T)
    
    if(nrow(tmp_tab)>0){
        
        #time of Qpeak
        peaktime = tmp_tab$date[which(tmp_tab[, paste("Qmed_",stations[sta_i], sep ="")] ==
                                        max(tmp_tab[, paste("Qmed_",stations[sta_i], sep ="")]))]
        STATS_EVTS[evt_i,  stations[sta_i], "Tpeak"] = peaktime[length(peaktime)]
        STATS_EVTS[evt_i,  stations[sta_i], "QspePeak"] = max(tmp_tab[, paste("Qmed_",stations[sta_i], sep ="")])/areas[sta_i]
        
        #time of Q50th
        cumusumu = cumsum(tmp_tab[, paste("Qmed_",stations[sta_i], sep ="")]*c(5*60,diff(as.numeric(tmp_tab$date)))) 
        STATS_EVTS[ evt_i, stations[sta_i],"Vruiss"] = max(cumusumu)
        
        cumusumu = cumusumu/max(cumusumu)
        d50th  = tmp_tab[mean(which(abs(cumusumu-0.5) == min(abs(cumusumu-0.5)))), "date"]
        STATS_EVTS[evt_i,  stations[sta_i], "Truiss50th"] =d50th
        
         
        
    }
      
    idxT = RelHum$date >= (rgx[1] -5*3600)  &  RelHum$date < rgx[1]
    al = grep("SWCtop", names(RelHum))
    STATS_EVTS[evt_i,  stations[sta_i], "Hum_initTop"] = mean(as.numeric(RelHum[idxT, paste(stations[sta_i], "_SWCtop", sep ="")]), na.rm =T)
    if(is.nan(STATS_EVTS[evt_i,  stations[sta_i], c("Hum_initTop")])){
      STATS_EVTS[evt_i,  stations[sta_i], "Hum_initTop"] = mean(as.numeric(unlist(RelHum[idxT, al])), na.rm =T)
      
    }

    al = grep("SWCbot", names(RelHum))
    STATS_EVTS[evt_i,  stations[sta_i], "Hum_initBot"] = mean(as.numeric(RelHum[idxT, paste(stations[sta_i], "_SWCbottom", sep ="")]), na.rm =T)
    if(is.nan(STATS_EVTS[evt_i,  stations[sta_i], c("Hum_initBot")])){
      STATS_EVTS[evt_i,  stations[sta_i], "Hum_initBot"] = mean(as.numeric(unlist(RelHum[idxT, al])), na.rm =T)
      
    }
    
  }
}

for(sta_i in 1:length(stations)){
  STATS_EVTS[, stations[sta_i], "Vruiss"] =  STATS_EVTS[, stations[sta_i],"Vruiss"]/areas[sta_i]/1000
  STATS_EVTS[,  stations[sta_i],"CR"] =100*  STATS_EVTS[, stations[sta_i] ,"Vruiss"]/STATS_EVTS[,  stations[sta_i] ,"Vrain"]

}
STATS_EVTS[, ,"deltaQpeak"] = STATS_EVTS[, ,"Tpeak" ] - STATS_EVTS[, ,"Train50th" ]
STATS_EVTS[, ,"deltaQpeak"] = STATS_EVTS[,  ,"deltaQpeak"] / 3600
STATS_EVTS[, ,"deltaQ50th"] = STATS_EVTS[, ,"Truiss50th" ] - STATS_EVTS[, ,"Train50th" ]
STATS_EVTS[, ,"deltaQ50th"] = STATS_EVTS[,  ,"deltaQ50th"] / 3600

idx = TAB_runoff$date <= as.POSIXct("16072021", format ="%d%m%Y", tz = "UTC") &
  TAB_runoff$date > as.POSIXct("14072021", format ="%d%m%Y", tz = "UTC") 
#############################################################################################################
# compilation evt data : pluie et runoff

id_rain = vector() 
id_Q = vector() 
for(evt_i in 1:length(EVTs)){
  
  id_rain = c(id_rain, which(RainEB$date <= DATE_EVTS$FIN_Q[evt_i] & RainEB$date >= DATE_EVTS$DEB_P[evt_i] ))
  id_Q = c(id_Q, which(TAB_runoff$date <= DATE_EVTS$FIN_Q[evt_i] & TAB_runoff$date >= DATE_EVTS$DEB_P[evt_i] ))
}

#####
RainEB_evts = RainEB[id_rain,]
# calcul I15 et I1hour
tmp15min = data.frame(date= RainEB_evts[-c(1,2),1], 
                      RainEB_evts[-c(1,2),-1]+RainEB_evts[-c(1,nrow(RainEB_evts)),-1]+
                        RainEB_evts[-c(nrow(RainEB_evts)-1,nrow(RainEB_evts)),-1] )
tmp1hour = data.frame(date= RainEB_evts[-c(1:5, (nrow(RainEB_evts)-5):nrow(RainEB_evts)),1], 
                      RainEB_evts[-c(1:11),-1]+RainEB_evts[-c(1:10,nrow(RainEB_evts)),-1]+
                        RainEB_evts[-c(1:9,(nrow(RainEB_evts)-1):nrow(RainEB_evts)),-1]+
                        RainEB_evts[-c(1:8,(nrow(RainEB_evts)-2):nrow(RainEB_evts)),-1]+
                        RainEB_evts[-c(1:7,(nrow(RainEB_evts)-3):nrow(RainEB_evts)),-1]+
                        RainEB_evts[-c(1:6,(nrow(RainEB_evts)-4):nrow(RainEB_evts)),-1]+
                        RainEB_evts[-c(1:5,(nrow(RainEB_evts)-5):nrow(RainEB_evts)),-1]+
                        RainEB_evts[-c(1:4,(nrow(RainEB_evts)-6):nrow(RainEB_evts)),-1]+
                        RainEB_evts[-c(1:3,(nrow(RainEB_evts)-7):nrow(RainEB_evts)),-1]+
                        RainEB_evts[-c(1:2,(nrow(RainEB_evts)-8):nrow(RainEB_evts)),-1]+
                        RainEB_evts[-c(1,(nrow(RainEB_evts)-9):nrow(RainEB_evts)),-1]+
                        RainEB_evts[-c((nrow(RainEB_evts)-10):nrow(RainEB_evts)),-1] )

for(evt_i in 1:length(EVTs)){
  
  for(sta_i in 1:length(stations)){
    
    idx = tmp15min$date <= DATE_EVTS$FIN_P[DATE_EVTS$EVTs == EVTs[evt_i]] &
      tmp15min$date >= DATE_EVTS$DEB_P[DATE_EVTS$EVTs == EVTs[evt_i]]
    idxn = !is.na( tmp15min[,stations[sta_i]])
    STATS_EVTS[ evt_i, stations[sta_i], "I15min"] = max(tmp15min[idx & idxn,stations[sta_i] ],na.rm =T)

    idx = tmp1hour$date <= DATE_EVTS$FIN_P[DATE_EVTS$EVTs == EVTs[evt_i]] &
      tmp1hour$date >= DATE_EVTS$DEB_P[DATE_EVTS$EVTs == EVTs[evt_i]]
    idxn = !is.na( tmp1hour[,stations[sta_i]])
    STATS_EVTS[ evt_i, stations[sta_i], "I1hour"] = max(tmp1hour[idx & idxn,stations[sta_i] ],na.rm =T)
    
      }
}
#####


#############################################################################################################
# save rain + runoff data with only the evts
adout = "/media/audrey/8858-1E31/PRO/2019/test_dispositifs/EBgather/"
write.table(RainEB[id_rain,], paste(adout, "RainEB_evts2022j.csv",sep =""), row.names = F,sep =";")
write.table(TAB_runoff[id_Q,], paste(adout, "RunoffEB_evts2022j.csv",sep =""), row.names = F,sep =";")
save(STATS_EVTS, file = paste(adout, "stats_evts2022j.rda", sep = ""))
#############################################################################################################



# Rain1H = aggregate(RainEB[id_rain,-1], by = list(datesec = format(RainEB[id_rain,]$date, "%Y%m%d%H")),
#                                                FUN = "sum", na.rm =T)
### arrondi a 5 min
TAB_runoff  = aggregate(TAB_runoff, by = list(datesec = as.integer((as.numeric(TAB_runoff$date)+240)/300)*300), 
                        FUN = "mean", na.rm =T)
TAB_runoff$date = as.POSIXct(as.numeric(TAB_runoff$datesec),
                    tz = "UTC", origin = "01-01-1970", format = "%d-%m-%Y")
id_rainall = vector() 
Rel_HumInit = vector() 
id_Qall = vector()
id_date_lim = vector()
for(evt_i in 1:length(EVTs)){
  
  id_rainall = c(id_rainall, which(RainEB$date <= DATE_EVTS$FIN_Q[evt_i] & RainEB$date >= DATE_EVTS$DEB_P[evt_i] ))

  id_Qall = c(id_Qall, which(TAB_runoff$date <= DATE_EVTS$FIN_Q[evt_i] & TAB_runoff$date >= DATE_EVTS$DEB_P[evt_i] ))
  id_date_lim  = c(id_date_lim , length(id_rainall))
  
  id_hum = which(RelHum$date <= DATE_EVTS$FIN_Q[evt_i] & RelHum$date >= DATE_EVTS$DEB_P[evt_i] )
  Rel_HumInit = c(Rel_HumInit, mean(as.vector(as.matrix(RelHum[id_hum[1:10],c(2,4,6,8)])) , na.rm =T))
}


rgHuM = range(Rel_HumInit)
seqHum = seq(from = rgHuM[1], to = 94, length.out = 11)
ii = as.integer((Rel_HumInit-rgHuM[1])/diff(seqHum)[1])+1

col_evts = brewer.pal(11,"RdYlBu")[ii]

#### plot all event
png(paste(ad_img, "/Runoff_all4.png", sep=""),
    width= 6.4,height=3, units="in", res=400, pointsize = 8)
par(mfrow = c(2,1), mar = c(1,4,1,4))

#id_rainall = id_rain
#id_Qall = id_Q
id_Q = id_Qall[1:7400]
id_rain= id_rainall[1:7400]

rgx = c(200,length(id_rain)-200)
plot( RainEB$Medernach[id_rain], type = "h", col = "royalblue", lwd =2,
     ylim = c(8,-0.5), xlab = "", ylab = "", axes = F, xlim = rgx)
axis(4, at = c(0,2,4,6),col.ticks = "royalblue", col ="royalblue")
#axis.POSIXct(1, at = seq(from = rgx[1], to = rgx[2], length.out = 4),
#             labels = format(seq(from = rgx[1], to = rgx[2], length.out = 4),
#                             format = "%d/%m %Hh"))
segments(x0 = c(1:length(id_rainall))[c(1,id_date_lim)], 
         x1 = c(1:length(id_rainall))[c(1,id_date_lim)],
         y0 = -10, y1 = 50, col = "gray", lty = 2)
for(evt_i in 1:length(EVTs)){
  xx = rep(c(c(1:length(id_rainall))[c(1,id_date_lim)][ c(evt_i,evt_i+1)]),each =2)
  polygon(x =xx , y = c(0,-1,-1,0), col =col_evts[evt_i], border =NA)
}
rgY = c(0.05, 1.3*max(as.vector(TAB_runoff[id_Q, paste("Qmed_",stations[-c(6)], sep ="")]), na.rm =T))
idx = !is.na(TAB_runoff[id_Q, paste("Qmed_",stations[1], sep ="")])
par(new = T)
plot(c(1:length(id_Q))[idx], TAB_runoff[ id_Q, paste("Qmed_",stations[1], sep ="")][idx],
     type = "l", col = coleurs[1], xlim = rgx, ylim = rgY, xlab = "", ylab = "",
     axes = F )
axis(2)
mtext(side = 4, line = 2.1, "Rainfall [mm/5min]", col ="royalblue")
mtext(side = 2, line = 2.1, "Discharge [m3/s]")
box()
for(i in c(2,3,4,5)){
  idx = !is.na(TAB_runoff[id_Q, paste("Qmed_",stations[i], sep ="")])
  lines( c(1:length(id_Q))[idx],TAB_runoff[id_Q, paste("Qmed_",stations[i], sep ="")][idx],
        type = "l", col = coleurs[i])
  
}
text(x = c(1:length(id_rainall))[id_date_lim]-120,
     y = 2.1,EVTs,  srt = 90, col = "gray10", cex = 0.85)


id_Q = id_Qall[7514:14400]
id_rain= id_rainall[7514:14400]

rgx = c(100,length(id_rain)-200)
plot( RainEB$Medernach[id_rain], type = "h", col = "royalblue", lwd =2,
      ylim = c(8,-0.5), xlab = "", ylab = "", axes = F, xlim = rgx)
axis(4, at = c(0,2,4,6), col.ticks = "royalblue", col ="royalblue")
for(evt_i in 1:length(EVTs)){
  xx = rep(c(c(1:length(id_rainall))[c(1,id_date_lim)][ c(evt_i,evt_i+1)]-7514),each =2)
  polygon(x =xx , y = c(0,-1,-1,0), col =col_evts[evt_i], border =NA)
}
#axis(1, at =c(1:length(id_rainall))[id_date_lim]-8047,
#     labels = rep("", length(id_date_lim)))

segments(x0 = c(1:length(id_rainall))[c(1,id_date_lim)]-7514, 
         x1 = c(1:length(id_rainall))[c(1,id_date_lim)]-7514,
         y0 = -10, y1 = 50, col = "gray", lty = 2)
rgY = c(0.05, 1.3*max(as.vector(TAB_runoff[id_Q, paste("Qmed_",stations[-c(6)], sep ="")]), na.rm =T))
idx = !is.na(TAB_runoff[id_Q, paste("Qmed_",stations[1], sep ="")])
par(new = T)
plot(c(1:length(id_Q))[idx], TAB_runoff[ id_Q, paste("Qmed_",stations[1], sep ="")][idx],
     type = "l", col = coleurs[1], xlim = rgx, ylim = rgY, xlab = "", ylab = "",
     axes = F )
axis(2)
mtext(side = 4, line = 2.1, "Rainfall [mm/5min]", col ="royalblue")
mtext(side = 2, line = 2.1, "Discharge [m3/s]")

legend(x = -170, y = 25, col = coleurs[c(1:5)], stations[c(1:5)], lty = 1,
       bty = "o", bg = "gray", box.col = "gray" ,cex = 0.65, seg.len=0.7)
box()
for(i in c(2,3,4,5)){
  idx = !is.na(TAB_runoff[id_Q, paste("Qmed_",stations[i], sep ="")])
  lines( c(1:length(id_Q))[idx],TAB_runoff[id_Q, paste("Qmed_",stations[i], sep ="")][idx],
         type = "l", col = coleurs[i])
  
}
text(x = c(1:length(id_rainall))[id_date_lim][11:20]-7600,
     y = 12,EVTs[11:20],  srt = 90, col = "gray10", cex = 0.85)

dev.off()

################################################################################################################
### graph all order
ord_evt = sort.int(STATS_EVTS[ , 4, "QspePeak"], index.return = T)$ix

sel_1 = STATS_EVTS[ , 4, "QspePeak"] <=  quantile(STATS_EVTS[ , 4, "QspePeak"],0.5)
sel_2 = STATS_EVTS[ , 4, "QspePeak"] >  quantile(STATS_EVTS[ , 4, "QspePeak"],0.5)

id_rain_sel1 = vector() 
id_Qall_sel1 = vector()
id_date_lim_sel1 = vector()
for(evt_i in which(sel_1)){
  
  id_rain_sel1 = c(id_rain_sel1, which(RainEB$date <= DATE_EVTS$FIN_Q[evt_i] & RainEB$date >= DATE_EVTS$DEB_P[evt_i] ))
  
  id_Qall_sel1 = c(id_Qall_sel1, which(TAB_runoff$date <= DATE_EVTS$FIN_Q[evt_i] & TAB_runoff$date >= DATE_EVTS$DEB_P[evt_i] ))
  id_date_lim_sel1  = c(id_date_lim_sel1 , length(id_rain_sel1))
  
}
id_rain_sel2 = vector() 
id_Qall_sel2 = vector()
id_date_lim_sel2 = vector()
for(evt_i in which(sel_2)){
  
  id_rain_sel2 = c(id_rain_sel2, which(RainEB$date <= DATE_EVTS$FIN_Q[evt_i] & RainEB$date >= DATE_EVTS$DEB_P[evt_i] ))
  
  id_Qall_sel2 = c(id_Qall_sel2, which(TAB_runoff$date <= DATE_EVTS$FIN_Q[evt_i] & TAB_runoff$date >= DATE_EVTS$DEB_P[evt_i] ))
  id_date_lim_sel2  = c(id_date_lim_sel2 , length(id_rain_sel2))
  
}

#### plot all event with order
png(paste(ad_img, "/Runoff_all_ierdered2021.png", sep=""),
    width= 6.4,height=3, units="in", res=400, pointsize = 9.5)
par(mfrow = c(2,1), mar = c(1,4,1,4))


rgx = c(200,length(id_rain_sel1)-200)
plot( RainEB$Medernach[id_rain_sel1], type = "h", col = "royalblue", lwd =2,
      ylim = c(7,-0.5), xlab = "", ylab = "", axes = F, xlim = rgx)
axis(4, at = c(0,2,4,6),col.ticks = "royalblue", col ="royalblue")
axis(1, at = seq(from = rgx[1], to = rgx[2], by = 12*24),
             labels =rep("",length(seq(from = rgx[1], to = rgx[2], by = 12*24))))
mtext(side = 1,text = "day", line =0.5, adj = 1)
segments(x0 = c(1:length(id_rain_sel1))[c(1,id_date_lim_sel1)], 
         x1 = c(1:length(id_rain_sel1))[c(1,id_date_lim_sel1)],
         y0 = -10, y1 = 50, col = "gray", lty = 2)
i = 0
for(evt_i in which(sel_1)){
  i =i +1
  xx = rep(c(c(1:length(id_rain_sel1))[c(1,id_date_lim_sel1)][ c(i,i+1)]),each =2)
  polygon(x =xx , y = c(0,-1,-1,0), col =col_evts[evt_i], border =NA)
}
rgY = c(0.05, 1.3*max(as.vector(TAB_runoff[id_Qall_sel1, paste("Qmed_",stations[-c(6)], sep ="")]), na.rm =T))
idx = !is.na(TAB_runoff[id_Qall_sel1, paste("Qmed_",stations[1], sep ="")])
par(new = T)
plot(c(1:length(id_Qall_sel1))[idx], TAB_runoff[ id_Qall_sel1, paste("Qmed_",stations[1], sep ="")][idx],
     type = "l", col = coleurs[1], xlim = rgx, ylim = rgY, xlab = "", ylab = "",
     axes = F )
axis(2)
mtext(side = 4, line = 2.1, "Rainfall [mm/5min]", col ="royalblue")
bquote(R^2 == 3, 2)
mtext(side = 2, line = 2.1, bquote("Discharge"~"[m"^3*".s"^{-1}*"]"))
#mtext(side = 2, line = 2.1, "Discharge [m3/s]")
box()
for(i in c(2,4)){
  idx = !is.na(TAB_runoff[id_Qall_sel1, paste("Qmed_",stations[i], sep ="")])
  lines( c(1:length(id_Qall_sel1))[idx],TAB_runoff[id_Qall_sel1, paste("Qmed_",stations[i], sep ="")][idx],
         type = "l", col = coleurs[i])
  
}
text(x = c(1:length(id_rain_sel1))[id_date_lim_sel1]-120,
     y = 1.,EVTs[sel_1],  srt = 90, col = "gray10", cex = 0.8)


rgx = c(200,length(id_rain_sel2)-200)
plot( RainEB$Medernach[id_rain_sel2], type = "h", col = "royalblue", lwd =2,
      ylim = c(7,-0.5), xlab = "", ylab = "", axes = F, xlim = rgx)
axis(4, at = c(0,2,4,6),col.ticks = "royalblue", col ="royalblue")
axis(1, at = seq(from = rgx[1], to = rgx[2], by = 12*24),
     labels =rep("",length(seq(from = rgx[1], to = rgx[2], by = 12*24))))
mtext(side = 1,text = "day", line =0.05, adj = 1)
segments(x0 = c(1:length(id_rain_sel2))[c(1,id_date_lim_sel2)], 
         x1 = c(1:length(id_rain_sel2))[c(1,id_date_lim_sel2)],
         y0 = -10, y1 = 50, col = "gray", lty = 2)
i = 0
for(evt_i in which(sel_2)){
  i =i +1
  xx = rep(c(c(1:length(id_rain_sel2))[c(1,id_date_lim_sel2)][ c(i,i+1)]),each =2)
  polygon(x =xx , y = c(0,-1,-1,0), col =col_evts[evt_i], border =NA)
}


rgY = c(0.05, 1.3*max(as.vector(TAB_runoff[id_Qall_sel2, paste("Qmed_",stations[-c(6)], sep ="")]), na.rm =T))
idx = !is.na(TAB_runoff[id_Qall_sel2, paste("Qmed_",stations[1], sep ="")])
par(new = T)
plot(c(1:length(id_Qall_sel2))[idx], TAB_runoff[ id_Qall_sel2, paste("Qmed_",stations[1], sep ="")][idx],
     type = "l", col = coleurs[1], xlim = rgx, ylim = rgY, xlab = "", ylab = "",
     axes = F )
axis(2)
mtext(side = 4, line = 2.1, "Rainfall [mm/5min]", col ="royalblue")
mtext(side = 2, line = 2.1, bquote("Discharge"~"[m"^3*".s"^{-1}*"]"))
box()
for(i in c(2,4)){
  idx = !is.na(TAB_runoff[id_Qall_sel2, paste("Qmed_",stations[i], sep ="")])
  lines( c(1:length(id_Qall_sel2))[idx],TAB_runoff[id_Qall_sel2, paste("Qmed_",stations[i], sep ="")][idx],
         type = "l", col = coleurs[i])
  
}
text(x = c(1:length(id_rain_sel2))[id_date_lim_sel2]-120,
     y = 15, EVTs[sel_2],  srt = 90, col = "gray10", cex = 0.8)


legend(x =850, y = 21, col = coleurs[c(1,2,4)], stations[c(1,2,4)], lty = 1,
       bty = "n", cex = 0.7, seg.len=0.7)


dev.off()



################################################################################################################
### graph all order QSPE
#### plot all event with order
ord_evt = sort.int(STATS_EVTS[-c(1:4) , 1, "QspePeak"], index.return = T)$ix

sel_1 = STATS_EVTS[ -c(1:4), 1, "QspePeak"] < quantile(STATS_EVTS[ -c(1:4), 1, "QspePeak"],0.5)
sel_2 = STATS_EVTS[ -c(1:4), 1, "QspePeak"] >=  quantile(STATS_EVTS[ -c(1:4), 1, "QspePeak"],0.5)

id_rain_sel1 = vector() 
id_Qall_sel1 = vector()
id_date_lim_sel1 = vector()
for(evt_i in (which(sel_1)+4)){
  
  id_rain_sel1 = c(id_rain_sel1, which(RainEB$date <= DATE_EVTS$FIN_Q[evt_i] & RainEB$date >= DATE_EVTS$DEB_P[evt_i] ))
  
  id_Qall_sel1 = c(id_Qall_sel1, which(TAB_runoff$date <= DATE_EVTS$FIN_Q[evt_i] & TAB_runoff$date >= DATE_EVTS$DEB_P[evt_i] ))
  id_date_lim_sel1  = c(id_date_lim_sel1 , length(id_rain_sel1))
  
}
id_rain_sel2 = vector() 
id_Qall_sel2 = vector()
id_date_lim_sel2 = vector()
for(evt_i in (which(sel_2)+4)){
  
  id_rain_sel2 = c(id_rain_sel2, which(RainEB$date <= DATE_EVTS$FIN_Q[evt_i] & RainEB$date >= DATE_EVTS$DEB_P[evt_i] ))
  
  id_Qall_sel2 = c(id_Qall_sel2, which(TAB_runoff$date <= DATE_EVTS$FIN_Q[evt_i] & TAB_runoff$date >= DATE_EVTS$DEB_P[evt_i] ))
  id_date_lim_sel2  = c(id_date_lim_sel2 , length(id_rain_sel2))
  
}


png(paste(ad_img, "/RunoffSPE_all_ierdered2021b.png", sep=""),
    width= 6.4,height=3, units="in", res=400, pointsize = 9.5)
par(mfrow = c(2,1), mar = c(1,4,1,4))


rgx = c(200,length(id_rain_sel1)-200)
plot( RainEB$Medernach[id_rain_sel1], type = "h", col = "royalblue", lwd =2,
      ylim = c(7,-0.5), xlab = "", ylab = "", axes = F, xlim = rgx)
axis(4, at = c(0,2,4,6),col.ticks = "royalblue", col ="royalblue")
axis(1, at = seq(from = rgx[1], to = rgx[2], by = 12*24),
     labels =rep("",length(seq(from = rgx[1], to = rgx[2], by = 12*24))))
mtext(side = 1,text = "day", line =0.5, adj = 1)
segments(x0 = c(1:length(id_rain_sel1))[c(1,id_date_lim_sel1)], 
         x1 = c(1:length(id_rain_sel1))[c(1,id_date_lim_sel1)],
         y0 = -10, y1 = 50, col = "gray", lty = 2)
i = 0
for(evt_i in (which(sel_1)+4)){
  i =i +1
  xx = rep(c(c(1:length(id_rain_sel1))[c(1,id_date_lim_sel1)][ c(i,i+1)]),each =2)
  polygon(x =xx , y = c(0,-1,-1,0), col =col_evts[evt_i], border =NA)
}

rgY = c(0.0, 1.3*max(as.vector(STATS_EVTS[which(sel_1)+4, c(1,2,4), "QspePeak"]), na.rm =T))*3600/1000
idx = !is.na(TAB_runoff[id_Qall_sel1, paste("Qmed_",stations[1], sep ="")])
par(new = T)
plot(c(1:length(id_Qall_sel1))[idx], TAB_runoff[ id_Qall_sel1, paste("Qmed_",stations[1], sep ="")][idx]/areas[1]*3600/1000,
     type = "l", col = coleurs[1], xlim = rgx, ylim = rgY, xlab = "", ylab = "",
     axes = F )
axis(2)
mtext(side = 4, line = 2.1, "Rainfall [mm/5min]", col ="royalblue")
bquote(R^2 == 3, 2)
mtext(side = 2, line = 2.1, bquote("Discharge"~"[mm"*".h"^{-1}*"]"))
#mtext(side = 2, line = 2.1, "Discharge [m3/s]")
box()
for(i in c(2,4)){
  idx = !is.na(TAB_runoff[id_Qall_sel1, paste("Qmed_",stations[i], sep ="")])
  lines( c(1:length(id_Qall_sel1))[idx],TAB_runoff[id_Qall_sel1, paste("Qmed_",stations[i], sep ="")][idx]/areas[i]*3600/1000,
         type = "l", col = coleurs[i])
  
}
text(x = c(1:length(id_rain_sel1))[id_date_lim_sel1]-120,
     y = 0.075,EVTs[which(sel_1)+4],  srt = 90, col = "gray10", cex = 0.8)


rgx = c(200,length(id_rain_sel2)-200)
plot( RainEB$Medernach[id_rain_sel2], type = "h", col = "royalblue", lwd =2,
      ylim = c(7,-0.5), xlab = "", ylab = "", axes = F, xlim = rgx)
axis(4, at = c(0,2,4,6),col.ticks = "royalblue", col ="royalblue")
axis(1, at = seq(from = rgx[1], to = rgx[2], by = 12*24),
     labels =rep("",length(seq(from = rgx[1], to = rgx[2], by = 12*24))))
mtext(side = 1,text = "day", line =0.05, adj = 1)
segments(x0 = c(1:length(id_rain_sel2))[c(1,id_date_lim_sel2)], 
         x1 = c(1:length(id_rain_sel2))[c(1,id_date_lim_sel2)],
         y0 = -10, y1 = 50, col = "gray", lty = 2)
i = 0
for(evt_i in (which(sel_2)+4)){
  i =i +1
  xx = rep(c(c(1:length(id_rain_sel2))[c(1,id_date_lim_sel2)][ c(i,i+1)]),each =2)
  polygon(x =xx , y = c(0,-1,-1,0), col =col_evts[evt_i], border =NA)
}
rgY = c(0.0, 1.3*max(as.vector(STATS_EVTS[sel_2 , c(1,2,4), "QspePeak"]), na.rm =T))*3600/1000
idx = !is.na(TAB_runoff[id_Qall_sel2, paste("Qmed_",stations[1], sep ="")])
par(new = T)
plot(c(1:length(id_Qall_sel2))[idx], TAB_runoff[ id_Qall_sel2, paste("Qmed_",stations[1], sep ="")][idx]/areas[1]*3600/1000,
     type = "l", col = coleurs[1], xlim = rgx, ylim = rgY, xlab = "", ylab = "",
     axes = F )
axis(2)
mtext(side = 4, line = 2.1, "Rainfall [mm/5min]", col ="royalblue")
mtext(side = 2, line = 2.1, bquote("Discharge"~"[mm"*".h"^{-1}*"]"))
box()
for(i in c(2,4)){
  idx = !is.na(TAB_runoff[id_Qall_sel2, paste("Qmed_",stations[i], sep ="")])
  lines( c(1:length(id_Qall_sel2))[idx],TAB_runoff[id_Qall_sel2, paste("Qmed_",stations[i], sep ="")][idx]/areas[i]*3600/1000,
         type = "l", col = coleurs[i])
  
}
text(x = c(1:length(id_rain_sel2))[id_date_lim_sel2]-120,
     y = 0.8, EVTs[which(sel_2)+4],  srt = 90, col = "gray10", cex = 0.8)

# legend(x =1950, y = 0.7, col = coleurs[c(1,2,4)], stations[c(1,2,4)], lty = 1,
#        bty = "n", cex = 0.7, seg.len=0.7)

dev.off()



################################################################################################################
RainEVTs = read.table( paste(adout, "RainEB_evts2022j.csv",sep =""), header = T,sep =";",
                     stringsAsFactors = F)
RainEVTs$date = as.POSIXct(RainEVTs$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")


RainEVTsHour = aggregate(RainEVTs[, -1], by = list(date = format(RainEVTs$date, "%Y%m%d%H")),
                         FUN = sum)



####################################################
library("RColorBrewer")
Rel_HumDay = aggregate(RelHum[,-1], by = list(date = format(RelHum$date, "%Y%m%d")),
                       FUN = mean,na.rm =T)
Rel_HumDay$date = as.POSIXct(Rel_HumDay$date, format = "%Y%m%d", tz = "UTC")
Rel_HumDay$angle = (as.numeric(format(Rel_HumDay$date,"%j")) )*360/366
Rel_HumDay$meanBOT = apply(Rel_HumDay[, c(2,4,6,8)],1,mean)
rgHuM = range(Rel_HumDay$meanBOT,na.rm =T)
seqHum = seq(from = rgHuM[1], to = 97.5, length.out = 11)
ii = as.integer((Rel_HumDay$meanBOT-rgHuM[1])/diff(seqHum)[1])+1
Rel_HumDay$coleur =  brewer.pal(11,"RdYlBu")[ii]

STATS_EVTS[1:4 ,"Heffingen",] = NA

png(paste(ad_img, "/Global_temps_de_reponseQ50th22j.png", sep=""),
    width= 5, height=5, units="in", res=400, pointsize =12)
par(mfrow = c(1,1), mar = c(3,4,1,4))
evt_date = rep(as.POSIXct(EVTs,format = "%Y_%m_%d", tz = "UTC"), 3)
POM = data.frame(date = evt_date)
POM$angle = (as.numeric(format(evt_date,"%j")) )*360/366
POM$intensite = as.vector(STATS_EVTS[,  c(1,2,4),"deltaQ50th"])
POM$coul = rep(coleurs[ c(1,2,4)], each = nrow(STATS_EVTS[, ,"Vrain" ]))
POM$cex = 0.7 + (as.vector(STATS_EVTS[,  c(1,2,4),"Vrain" ]) - range(STATS_EVTS[,c(1,2,4) ,"Vrain" ], na.rm =T)[1] )/
  (range(STATS_EVTS[,c(1,2,4),"Vrain" ], na.rm =T)[2]-range(STATS_EVTS[, c(1,2,4),"Vrain" ], na.rm =T)[1])*(2.5-0.7)

par(mfrow = c(1,1))
polar.plot(c(rep(37,75),rep(39,75)),c(seq(from = 0, to =360, length.out= 75),
                                      rev(seq(from = 0, to =360, length.out = 75))),
           main="",lwd=0.1,poly.col="gray10",rp.type ="p",
           start=90, clockwise = T, xlim = c(0,30),
           label.pos = c(0,31,59,90,120,151,181,212,243,273,304,334)*360/366,
           labels = c("jan", "fev", "mar", "apr","mai", "jun", "jul","aug","sep",
                      "oct","nov","dec"),radial.lim=c(0,30), 
           radial.labels =seq(from = 0, to =30, by = 5))

polar.plot(POM$intensite,POM$angle,main="",lwd=5,point.col=POM$coul,rp.type ="s",
           start=90, clockwise = T, xlim = c(0,30),point.symbols =16,cex = POM$cex,
           label.pos = c(0,31,59,90,120,151,181,212,243,273,304,334)*360/366,
           labels = c("jan", "fev", "mar", "apr","mai", "jun", "jul","aug","sep",
                      "oct","nov","dec"),radial.lim=c(0,30), 
           radial.labels =seq(from = 0, to =30, by = 5), add= T)

POM$cex = 0.7 + (as.vector(STATS_EVTS[,  c(1,2,4),"I15min" ]) - range(STATS_EVTS[,c(1,2,4) ,"I15min"  ], na.rm =T)[1] )/
  (range(STATS_EVTS[,c(1,2,4),"I15min"  ], na.rm =T)[2]-range(STATS_EVTS[, c(1,2,4),"I15min" ], na.rm =T)[1])*(2.5-0.7)
polar.plot(POM$intensite,POM$angle,main="",lwd=5,point.col="gray30",rp.type ="s",
           start=90, clockwise = T, xlim = c(0,35),point.symbols =3,cex = POM$cex,
           label.pos = c(0,31,59,90,120,151,181,212,243,273,304,334)*360/366,
           labels = c("jan", "fev", "mar", "apr","mai", "jun", "jul","aug","sep",
                      "oct","nov","dec"),radial.lim=c(0,35), 
           radial.labels =seq(from = 0, to =35, by = 5), add= T)


Rel_HumDay = aggregate(RelHum[,-1], by = list(date = format(RelHum$date, "2020%m%d")),
                       FUN = mean,na.rm =T)
Rel_HumDay$date = as.POSIXct(Rel_HumDay$date, format = "%Y%m%d", tz = "UTC")
Rel_HumDay$angle = (as.numeric(format(Rel_HumDay$date,"%j")) )*360/366
Rel_HumDay$meanBOT = apply(Rel_HumDay[, c(2,4,6,8)],1,mean)
rgHuM = range(Rel_HumDay$meanBOT,na.rm =T)
seqHum = seq(from = rgHuM[1], to = 97.5, length.out = 11)
ii = as.integer((Rel_HumDay$meanBOT-rgHuM[1])/diff(seqHum)[1])+1
Rel_HumDay$coleur =  brewer.pal(11,"RdYlBu")[ii]


idx = !is.na(Rel_HumDay$coleur)
for(i in 1:sum(idx)){
  tmp = Rel_HumDay[idx,][i,]
  polar.plot(c(rep(37,2),rep(39,2)),c(c(tmp$angle[1], tmp$angle[1]+360/366),
                                        rev(c(tmp$angle[1], tmp$angle[1]+360/366))),
             main="",lwd=0.1,poly.col=tmp$coleur[1],rp.type ="p",
             start=90, clockwise = T, xlim = c(0,35),
             label.pos = c(0,31,59,90,120,151,181,212,243,273,304,334)*360/366,
             labels = c("jan", "fev", "mar", "apr","mai", "jun", "jul","aug","sep",
                        "oct","nov","dec"),radial.lim=c(0,35), 
             radial.labels =seq(from = 0, to =35, by = 5), add= T)
  
}
dev.off()

################# color scale
### 
#### temps de reponse obsrvé
delta_t = as.numeric(as.vector(STATS_EVTS[,  c(1,2,4),"Tpeak" ])) - as.numeric(as.vector(STATS_EVTS[,  c(1,2,4),"Train50th" ]))
delta_t = delta_t / 3600
POM = data.frame(date = evt_date)
POM$angle = as.numeric(format(evt_date,"%j"))*360/366
POM$intensite = delta_t
POM$coul = rep(colours[ c(3,1,5)], each = nrow(STATS_EVTS[, ,"Vrain" ]))
POM$cex = 0.7 + (as.vector(STATS_EVTS[, c(1,2,4) ,"Vrain" ]) - range(STATS_EVTS[, c(1,2,4) ,"Vrain" ], na.rm =T)[1] )/
  (range(STATS_EVTS[, c(1,2,4) ,"Vrain" ], na.rm =T)[2]-range(STATS_EVTS[,  c(1,2,4),"Vrain" ], na.rm =T)[1])*(2.5-0.7)

polar.plot(POM$intensite,POM$angle,main="",lwd=5,point.col=POM$coul,rp.type ="s",
           start=90, clockwise = T, xlim = c(0,35),point.symbols =16,cex = POM$cex,
           label.pos = c(0,31,59,90,120,151,181,212,243,273,304,334)*360/366,
           labels = c("jan", "fev", "mar", "apr","mai", "jun", "jul","aug","sep",
                      "oct","nov","dec"),radial.lim=c(0,35), 
           radial.labels =seq(from = 0, to =35, by = 5))



png(paste(ad_img, "/KEYCOLrunoff_all.png", sep=""),
    width= 2.1, height=1.3, units="in", res=400, pointsize = 8)
par(mfrow =c(1,1), mar=c(0,0,0,0), oma =c(0,0,0,0))
plot(c(-10,-10), xlim =c(0,1),ylim= c(-.5,0.5) , col = 1 , pch = 16,
     axes =F, xlab ='', ylab ='')

#the labels of the legend
colleg =  c(brewer.pal(11,"RdYlBu"), "navyblue")
seqy = seq(from = 0, to = 1, length.out = 13)
for(i in 1:length(colleg)){
  
  polygon(y = c(0,0.15,0.15,0),
          x =c(seqy[i], seqy[i], seqy[i+1], seqy[i+1]),
          border = NA, col = colleg[i])
  
}
text("50 %",x =0 , y =0.2 , adj = 0 , cex = 0.85)
text("100 %",x =1 , y =0.2, adj = 1 , cex = 0.85)
text("Soil saturation (-50cm)",x =0.5 , y =0.2, adj = 0.5 , cex = 0.85, font = 2)
text(bquote(R^2 == 3, 2),x =0.5 , y =0.5, adj = 0.5 , cex = 0.85, font = 2)
dev.off()
### 
png(paste(ad_img, "/KEYCOLcercle.png", sep=""),
    width= 1.3, height=1.3, units="in", res=400, pointsize = 8)
par(mfrow =c(1,1), mar=c(0,0,0,0), oma =c(0,0,0,0))
plot(c(-10,-10), xlim =c(0,1),ylim= c(-.5,0.5) , col = 1 , pch = 16,
     axes =F, xlab ='', ylab ='')

#the labels of the legend
colleg =  c(brewer.pal(11,"RdYlBu"), "navyblue")
seqy = seq(from = 0, to = 1, length.out = 13)
for(i in 1:length(colleg)){
  
  polygon(y = c(0,0.15,0.15,0),
          x =c(seqy[i], seqy[i], seqy[i+1], seqy[i+1]),
          border = NA, col = colleg[i])
  
}
text("69 %",x =0 , y =0.2 , adj = 0 , cex = 0.6)
text("100 %",x =1 , y =0.2, adj = 1 , cex = 0.6)
text("Soil saturation (-50cm)",x =0.5 , y =0.2, adj = 0.5 , cex = 0.6)

dev.off()

png(paste(ad_img, "/KEYcercle.png", sep=""),
    width= 2.3, height=2.3, units="in", res=400, pointsize = 8)
par(mfrow =c(1,1), mar=c(0,0,0,0), oma =c(0,0,0,0))
plot(c(1,1), col ="white", axes =F, xlab ="", ylab ="")
legend("center", c(stations[c(1,2,4)],"rainfall amount", "rainfall intensity"),
       col = c(coleurs[c(1,2,4)], "black", "black"), 
       pch =c(16,16,16,1,3), cex =2, bty ="n")
dev.off()
################# temps de transfert 


png(paste(ad_img, "/Events3_Periods3_Qspe.png", sep=""),
    width= 6.4, height=2.0, units="in", res=400, pointsize =12)
par(mfrow = c(1,3), mar = c(4,3.5,2,3.5))
for(evt_i in c(5,10,15)){
  
  idxP = RainEB$date <= DATE_EVTS$FIN_Q[evt_i]  &  RainEB$date >= DATE_EVTS$DEB_P[evt_i]
  idxQ = RainEB$date <= DATE_EVTS$FIN_Q[evt_i]  &  RainEB$date >= DATE_EVTS$DEB_Q[evt_i]+3600
  rgx = c(DATE_EVTS$DEB_Q[evt_i]+3600,DATE_EVTS$FIN_Q[evt_i])
  
  plot(RainEB$date[idxP], RainEB$Reisdorf[idxP]*12, type = "h", col = "royalblue", lwd =2,
       xlim = rgx, ylim = c(30,0), xlab = "", ylab = "", axes = F)
  axis(4, col.ticks = "royalblue", col ="royalblue")
  axis.POSIXct(1, at = seq(from = rgx[1], to = rgx[2], length.out = 4),
               labels = format(seq(from = rgx[1], to = rgx[2], length.out = 4),
                               format = "%d/%m %Hh"))
  mtext(side = 4, line = 2, "Rainfall [mm/5min)]", cex = 0.75)
  
  idxT = TAB_runoff$date <= DATE_EVTS$FIN_Q[evt_i]   &  TAB_runoff$date >= DATE_EVTS$DEB_Q[evt_i]
  rgY = c(0.0, 1.5*max(apply(TAB_runoff[idxT, paste("Qmed_",stations[c(1,2,4)], sep ="")],2,max, na.rm=T)/areas[c(1,2,4)], na.rm =T))
  idx = !is.na(TAB_runoff[, paste("Qmed_",stations[1], sep ="")])
  
  par(new = T)
  plot(TAB_runoff$date[idx & idxT], TAB_runoff[idx & idxT, paste("Qmed_",stations[1], sep ="")]/areas[c(1)],
       type = "l", col = coleurs[1], xlim = rgx, ylim = rgY, xlab = "", ylab = "",
       axes = F)
  axis(2)
  mtext(side = 2, line = 2., "Discharge [m3/km2/s]", cex=0.75)
  box()
  for(i in c(2,4)){
    idx = !is.na(TAB_runoff[, paste("Qmed_",stations[i], sep ="")])
    lines(TAB_runoff$date[idx & idxT], TAB_runoff[idx & idxT, paste("Qmed_",stations[i], sep ="")]/areas[i],
          type = "l", col = coleurs[i])
    
  }
  title(main = EVTs[evt_i])
  
  
  
}
legend("right", col = c("orangered", "pink3" , "olivedrab3"),
       stations[c(1,2,4)], bty = "n", lty = 1)
dev.off()


coleurs[3] = "cyan"


png(paste(ad_img, "/Events4_Periods3_Q21.png", sep=""),
    width= 6.4, height=3.85, units="in", res=400, pointsize =12)
par(mfrow = c(2,2), mar = c(2.8,3.5,1.7,3.5))
e = 1
for(evt_i in c(5,20,10,14)){
  
  idxP = RainEB$date <= DATE_EVTS$FIN_Q[evt_i]  &  RainEB$date >= DATE_EVTS$DEB_P[evt_i]
  idxQ = RainEB$date <= DATE_EVTS$FIN_Q[evt_i]  &  RainEB$date >= DATE_EVTS$DEB_Q[evt_i]
  rgx = c(DATE_EVTS$DEB_Q[evt_i],DATE_EVTS$FIN_Q[evt_i])
  
  plot(RainEB$date[idxP], RainEB$Reisdorf[idxP]*12, type = "h", col = "royalblue", lwd =2,
       xlim = rgx, ylim = c(30,0), xlab = "", ylab = "", axes = F)
  axis(4, col.ticks = "royalblue", col ="royalblue")
  axis.POSIXct(1, at = seq(from = rgx[1], to = rgx[2], length.out = 4),
               labels = format(seq(from = rgx[1], to = rgx[2], length.out = 4),
                               format = "%d/%m %Hh"))
  mtext(side = 4, line = 2, "Rainfall [mm/h)]", cex = 0.75)
  
  idxT = TAB_runoff$date <= DATE_EVTS$FIN_Q[evt_i]   &  TAB_runoff$date >= DATE_EVTS$DEB_Q[evt_i]+3600
  rgY = c(0.0, 1.5*max(apply(TAB_runoff[idxT, paste("Qmed_",stations[c(1,2,4)], sep ="")],2,max, na.rm=T), na.rm =T))
  idx = !is.na(TAB_runoff[, paste("Qmed_",stations[1], sep ="")])
  
  par(new = T)
  plot(TAB_runoff$date[idx & idxT], TAB_runoff[idx & idxT, paste("Qmed_",stations[1], sep ="")],
       type = "l", col = coleurs[1], xlim = rgx, ylim = rgY, xlab = "", ylab = "",
       axes = F)
  axis(2)
  mtext(side = 2, line = 2., "Runoff [m3/s]", cex=0.75)
  box()
  for(i in c(2,3,4)){
    idx = !is.na(TAB_runoff[, paste("Qmed_",stations[i], sep ="")])
    lines(TAB_runoff$date[idx & idxT], TAB_runoff[idx & idxT, paste("Qmed_",stations[i], sep ="")],
          type = "l", col = coleurs[i])
    
     
  }
  if(evt_i ==5){
    text(x =rgx[1] - 3.2*3600, y = 0.72*rgY[2], cex =0.7, pos=4,
         bquote("H"[i]^{"20cm"} == .(round(STATS_EVTS[evt_i,4,"Hum_initTop"],1)) ~ "%"))
    
  }
  if(evt_i !=5){
    text(x =rgx[2] + 3*3600, y = 0.8*rgY[2], cex =0.7, pos=2,
         bquote("H"[i]^{"20cm"} == .(round(STATS_EVTS[evt_i,4,"Hum_initTop"],1)) ~ "%"))
    
  }
  
         
  title(main =paste(letters[e], ") " ,EVTs[evt_i],sep =""), cex.main =0.9)
  e = e+1
  
  if(evt_i ==5){
    legend(x = rgx[2], y = 0.82, col = coleurs[c(1,2,3,4,6)],
           stations[c(1,2,3,4,6)], bty = "n", lty = 1, cex=0.8, lwd =1.5, xjust = 1)
    i =6
    idx = !is.na(TAB_runoff[, paste("Qmed_",stations[i], sep ="")])
    lines(TAB_runoff$date[idx & idxT], TAB_runoff[idx & idxT, paste("Qmed_",stations[i], sep ="")],
          type = "l", col = coleurs[i])
    
  }
}

dev.off()
####################################################

