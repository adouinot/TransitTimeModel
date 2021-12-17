
rm(list =ls())
library(pracma)
library(plotrix)
# =======================================================
# load dischareh time series
ad = "/media/audrey/8858-1E31/PRO/2019/DATABASE/HYDRO/data/HQ_made_in_home/"
ad_img = paste("/media/audrey/8858-1E31/PRO/2019/DATABASE/HYDRO/IMG/" )

fileo = list.files(ad, pattern = "HQserie")
fileo = fileo[-4]
stations = unlist(strsplit(fileo, ".csv"))
stations = unlist(strsplit(stations, "_"))[c(2,4,6,8,10,12 )]
aires =  c(48.687, 92.981,31.14,69.41,79.04, 100.6)

TAB_discharge = read.table(paste(ad, fileo[1], sep =""), header =T,
                 stringsAsFactors = F, sep= ",")
names(TAB_discharge)= c(names(TAB_discharge)[1], paste(names(TAB_discharge)[-1], "_", stations[1], sep =""))
TAB_discharge$date = as.POSIXct(TAB_discharge$date, format = "%Y-%m-%d  %H:%M:%S", tz = "UTC")

for(i in 2:length(fileo)){
  
  tmp =  read.table(paste(ad, fileo[i], sep =""), header =T,
                    stringsAsFactors = F, sep= ",")
  names(tmp)= c(names(tmp)[1], paste(names(tmp)[-1], "_", stations[i], sep =""))
  tmp$date = as.POSIXct(tmp$date, format = "%Y-%m-%d  %H:%M:%S", tz = "UTC")
  
  TAB_discharge = merge(TAB_discharge, tmp, by = "date", all.x = T, all.y = T)
}

rm(tmp)

# =======================================================

# =======================================================
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
# =======================================================

Raintmp = merge( RainEB,Raintmp, by = "date", all.x =T, all.y =T)
for(sta_c in c("atlinster",
               "bakesmillen" ,  "schoos", "schwanterhaff")){
  
  idx = is.na(Raintmp[,paste(sta_c, ".x", sep ="")]) & !is.na(Raintmp[,paste(sta_c, ".y", sep ="")]) 
  Raintmp[idx,paste(sta_c, ".x", sep ="")] = Raintmp[idx,paste(sta_c, ".y", sep ="")]
}

RainEB = Raintmp[, 1:5]
names(RainEB) = c("date", "atlinster",
                  "bakesmillen" ,  "schoos", "schwanterhaff")

# =======================================================
# load RAINFALL time series
dir =  "/media/audrey/8858-1E31/PRO/2019/test_dispositifs/EBgather/"

RelHum = read.csv(paste(dir, "RelHum_raw21j.csv", sep =""), header = T, sep = ",", 
                  na.strings = NA, stringsAsFactors = F)
RelHum$date = as.POSIXct(RelHum$date, format = "%Y-%m-%d %H:%M:%S", tz ="UTC")
HumEB = RelHum
# =======================================================
# load RAINFALL thiessen area
dir =  "/media/audrey/8858-1E31/PRO/2019/GIS_data/"
ThiessenArea = read.csv(paste(dir, "EBraingauges_LIST_thiessenAire.csv", sep =""), 
                        header = T, sep = ",", 
                  na.strings = NA, stringsAsFactors = F)
for(i in 1:nrow(ThiessenArea)){
  ThiessenArea[i, -c(1,2)] = ThiessenArea[i, -c(1,2)]/ThiessenArea[i, 2]
}
# =======================================================

# =======================================================
# retourner le nombre d evenements et leur periode
cum = apply(RainEB[ , -1],1, mean, na.rm =T) 
idx = which(cum > 0 & !is.na(cum))

# mm evt tant qu'il n'y a pas de trou de plus de 5h = 5 *12 pdt
Nevt = rep(1,length(idx))
for(i in 2:length(idx)){
  if(diff(idx)[i-1]>12*5){
    Nevt[i:(length(Nevt))]=Nevt[i]+1
  }
}
EVTs = list()
vEV = 0
for(evt_i in 1:range(Nevt)[2]){
  
  date_evt = range(RainEB$date[idx][Nevt == evt_i])
  date_evt = date_evt + c(-3*3600,3*3600)
  if( sum(cum[idx][Nevt == evt_i], na.rm =T) > 10){
    
    #print(sum(cum[idx][Nevt == evt_i], na.rm =T))
    vEV = vEV + 1
    rgx = RainEB$date <= date_evt[2] & RainEB$date >= date_evt[1] 
    EVTs[[format(date_evt[1] , "%Y_%m_%d")]] = RainEB[rgx ,]
    
  }
    
}
# retourner le nombre d evenements et leur periode
# =======================================================

# =======================================================
# calcul des pluies moyennes sur chaque BV
pluviosN = names(ThiessenArea)[-c(1,2)]
for(evt_i in 1:length(EVTs)){
  
  for(i in 1:length(stations)){
    
    EVTs[[evt_i]][, stations[i]] = 0
    isnonaP = apply(is.na(EVTs[[evt_i]][, pluviosN]),2,sum)<10
    for(p in which(isnonaP)){
      
      EVTs[[evt_i]][, stations[i]] = EVTs[[evt_i]][, stations[i]] + 
        EVTs[[evt_i]][, pluviosN[p]]*ThiessenArea[ThiessenArea$BV == stations[i],pluviosN[p] ]/ sum(ThiessenArea[ThiessenArea$BV == stations[i],pluviosN[isnonaP] ])
        
    }
    
    
  }
}

# calcul des pluies moyennes
# =======================================================

# =======================================================
# plot des 16 evts
plot_evti = function(EVT_l, evt_i, tab_discharge, stations, colors){
  
  rgx = range(EVT_l[[evt_i]]$date)
  rgx[2] = rgx[2] + 24*3600
  rgx[1] = rgx[1] - 6*3600
  
  cum = apply(EVT_l[[evt_i]][ , -1],1, mean, na.rm =T) 
  plot(EVT_l[[evt_i]]$date, cum*12, type = "h", col = "royalblue", lwd =2,
       xlim = rgx, ylim = c(30,0), xlab = "", ylab = "", axes = F)
  axis(4, col.ticks = "royalblue", col ="royalblue")
  axis.POSIXct(1, at = seq(from = rgx[1], to = rgx[2], length.out = 4),
               labels = format(seq(from = rgx[1], to = rgx[2], length.out = 4),
                               format = "%d/%m %Hh"))
  par(new = T)
  idxT = tab_discharge$date <= rgx[2]  &  tab_discharge$date >= rgx[1]
  rgY = c(0.05/31, 1.5*max(as.vector(tab_discharge[idxT, paste("Qmed_",stations, sep ="")]), na.rm =T))
  idx = !is.na(tab_discharge[, paste("Qmed_",stations[1], sep ="")])
  plot(tab_discharge$date[idx & idxT], tab_discharge[idx & idxT, paste("Qmed_",stations[1], sep ="")],
       type = "l", col = colors[1], xlim = rgx, ylim = rgY, xlab = "", ylab = "",
       axes = F)
  axis(2)
  mtext(side = 2, line = 2.1, "specific discharge")
  box()
  for(i in 2:length(stations)){
    idx = !is.na(tab_discharge[, paste("Qmed_",stations[i], sep ="")])
    lines(tab_discharge$date[idx & idxT], tab_discharge[idx & idxT, paste("Qmed_",stations[i], sep ="")],
         type = "l", col = colors[i])
    
  }
  title(main = names(EVT_l)[evt_i])
  
  
}

colours = c("pink", "darkcyan","orangered", "navyblue","olivedrab3",   "black")

par(mfrow = c(2,2), mar = c(3,4,1,4))
for(i in 1:length(EVTs)){
  
  plot_evti(EVTs, i,TAB_discharge, stations, colours)
  
}

aires =  c(48.687,92.981, 31.14, 69.41,79.04,100.6)
TAB_SPEdischarge = TAB_discharge
for(i in 1:length(stations)){
  
  TAB_SPEdischarge[ , paste("Qmed_", stations[i], sep ="")] = TAB_discharge[ , paste("Qmed_", stations[i], sep ="")] /aires[i]
}



par(mfrow = c(2,2), mar = c(3,4,1,4))
for(i in 1:length(EVTs)){
  
  # png(paste(ad_img, "/evt_",EVTs[i],".png", sep=""),
  #     width= 5.4, height=4.5, units="in", res=400, pointsize = 8)
  plot_evti(EVTs, i,TAB_SPEdischarge, stations[c(1,2,3,4,5,6)], colours[c(1,2,3,4,5,6)])
  #dev.off()
  # if(i == 4){
  #   
  #   legend("right", col = c("olivedrab3", "pink", "orangered"),
  #          c("Medernach", "Heffingen", "Koedange"), lty =1 )
  #   
  # }
}


# =======================================================
# calcul statistiques des evts
## sur les pluies d'abord: Tpluie50th, Vpluie / BV
STATS_EVTS = array(dim = c(length(EVTs), length(stations), 13),
                   dimnames = list(names(EVTs), stations,
                                   c("DureeP", "DureeQ", "Vrain", "VrainBis", "Vruiss", "QspePeak",
                                    "Train50th", "Train50thBis", "Truiss50th", "Tpeak", "CR", "Hum_initTop","Hum_initBot" )))
DATES_EVTS = data.frame(EVTs =names(EVTs))
rgx = range(EVTs[[1]]$date)
DATES_EVTS$DEB_P = rgx[1]
DATES_EVTS$FIN_P = rgx[1]
DATES_EVTS$DEB_Q = rgx[1]
DATES_EVTS$FIN_Q = rgx[1]

discharge_evts = list()
for(evt_i in names(EVTs)){
  
  for(sta_i in 1:length(stations)){
    
    tmp =EVTs[[evt_i]][, stations[sta_i]]
    tmp[is.na(tmp)] =0
    STATS_EVTS[ evt_i, stations[sta_i], "Vrain"] = max(cumsum(tmp))
    cumucumu = cumsum(tmp)/max(cumsum(tmp))
    d50th  = EVTs[[ evt_i]][mean(which(abs(cumucumu-0.5) == min(abs(cumucumu-0.5)))), "date"]
    STATS_EVTS[ evt_i, stations[sta_i], "Train50th"] = d50th
  }
}
# sur les debits ensuite: Tpeak, Truiss50th, Vruissele, CR / BV
for(evt_i in names(EVTs)){
  
  rgx = range(EVTs[[evt_i]]$date)
  duree = (as.numeric(rgx[2]) - as.numeric(rgx[1]))/3600
  STATS_EVTS[ evt_i, , "DureeP"] = duree
  print(rgx)
  
  DATES_EVTS$DEB_P[DATES_EVTS$EVTs == evt_i] = rgx[1] 
  DATES_EVTS$FIN_P[DATES_EVTS$EVTs == evt_i] = rgx[2] 
  
  rgx[1] = rgx[1] 
  rgx_old = rgx[2]
  if(evt_i != "2019_06_05"){
    
    idxT = TAB_discharge$date <= rgx[2]  &  TAB_discharge$date >= rgx[1] & !is.na(TAB_discharge$Qmed_Koedange)
    oo = which(TAB_discharge$date >  (rgx[2]+6*3600) & TAB_discharge$date <= (rgx[2] +120*3600))
    idx = !is.na(TAB_discharge$Qmed_Koedange[oo])
    
    ee = -diff(TAB_discharge$Qmed_Koedange[oo][idx])/TAB_discharge$Qmed_Koedange[idxT][1]
    bb = TAB_discharge$Qmed_Koedange[oo][idx] < 1.3*TAB_discharge$Qmed_Koedange[idxT][1]
    
    if(sum(bb)==0){
      bb = TAB_discharge$Qmed_Koedange[oo][idx] < 2*TAB_discharge$Qmed_Koedange[idxT][1]
      if(sum(bb)==0){
        bb[length(bb)] =T
      }
    }
    #rgx[2] = TAB_discharge$date[oo][idx][min(which(ee > 0 & ee < 0.001))]
    rgx[2] = TAB_discharge$date[oo][idx][min(which(bb))]
    
  }
  if(evt_i =="2019_06_05"){
    rgx[2] = rgx[2] + 24*3600
  }
  if(evt_i =="2019_09_22"){
    rgx[2] = rgx[2] + 6*3600
  }
  if(evt_i =="2019_11_02"){
    rgx[2] = rgx[2] + 6*3600
  }
  if(evt_i =="2019_11_26"){
    rgx[2] = rgx[2] + 6*3600
  }
  if(evt_i =="2019_08_06"){
    rgx[2] = rgx[2] + 6*3600
  }
  if(evt_i =="2020_01_26"){
    rgx[2] = rgx_old + 18*3600
  }
  if(evt_i =="2019_12_20"){
    rgx[2] = rgx[2] - 48*3600
  }
  if(evt_i =="2020_01_31"){
    rgx[2] = rgx_old + 25*3600
  }
  if(evt_i =="2020_02_09"){
    rgx[2] = rgx[2] - 24*3600
  }
  if(evt_i =="2020_02_13"){
    rgx[2] = rgx[2] + 12*3600
  }
  if(evt_i =="2020_02_29"){
    rgx[2] = rgx[2] + 6*3600
  }
  duree = (as.numeric(rgx[2]) - as.numeric(rgx_old))/3600
  STATS_EVTS[ evt_i, , "DureeQ"] = duree
  
  DATES_EVTS$DEB_Q[DATES_EVTS$EVTs == evt_i] = rgx[1] 
  DATES_EVTS$FIN_Q[DATES_EVTS$EVTs == evt_i] = rgx[2] 
  
  tmp =EVTs[[evt_i]][, stations[sta_i]]
  tmp[is.na(tmp)] =0
  
  idxT = TAB_discharge$date <= rgx[2]  &  TAB_discharge$date >= rgx[1]
  discharge_evts[[evt_i]] = TAB_discharge[ idxT, c("date", paste("Qmed_", stations, sep = ""))]
  
  for(sta_i in 1:length(stations)){
    
    idxT = TAB_discharge$date <= rgx[2]  &  TAB_discharge$date >= rgx[1]
    idx = !is.na(TAB_discharge[, paste("Qmed_",stations[sta_i], sep ="")])
    tmp_tab =TAB_discharge[idx & idxT, ]
    
    
    if(nrow(tmp_tab)>0){
     
      cumRain = vector()
      for(i in 1:nrow(tmp_tab)){
        
        cumRain[i] = sum(tmp[EVTs[[evt_i]]$date < tmp_tab$date[i]])
        
      }
    
    #time of Qpeak
    peaktime = tmp_tab$date[which(tmp_tab[, paste("Qmed_",stations[sta_i], sep ="")] == max(tmp_tab[, paste("Qmed_",stations[sta_i], sep ="")]))]
    STATS_EVTS[evt_i,  stations[sta_i], "Tpeak"] = peaktime[length(peaktime)]
    
    #time of Q50th
    cumusumu = cumsum(tmp_tab[, paste("Qmed_",stations[sta_i], sep ="")]*c(5*60,diff(as.numeric(tmp_tab$date)))) 
    cumusumu = cumusumu - (as.numeric(tmp_tab$date)-as.numeric(tmp_tab$date[1]))*min(tmp_tab[, paste("Qmed_",stations[sta_i], sep ="")])
    
    #cumusumuRain = cumRain*1000*aires[sta_i] -  cumusumu
    idx = !is.na(discharge_evts[[evt_i]][, paste("Qmed_",stations[sta_i], sep ="")])
    discharge_evts[[evt_i]][idx, paste("Qmed_",stations[sta_i], sep ="")] = cumusumu
    
    STATS_EVTS[ evt_i, stations[sta_i],"Vruiss"] = max(cumusumu)
    cumusumu = cumusumu/max(cumusumu)
    d50th  = tmp_tab[mean(which(abs(cumusumu-0.5) == min(abs(cumusumu-0.5)))), "date"]
    STATS_EVTS[evt_i,  stations[sta_i], "Truiss50th"] =d50th
    
    }
    
    idxT = HumEB$date <= (rgx[1] + 5*3600)  &  HumEB$date >= rgx[1]
    al = grep("SWCtop", names(HumEB))
    STATS_EVTS[evt_i,  stations[sta_i], "Hum_initTop"] = mean(as.numeric(unlist(HumEB[idxT, al])), na.rm =T)
    al = grep("SWCbot", names(HumEB))
    STATS_EVTS[evt_i,  stations[sta_i], "Hum_initBot"] = mean(as.numeric(unlist(HumEB[idxT, al])), na.rm =T)
    
  }
}

###
plot_evti = function(EVT_l, evt_i, tab_discharge, stations, colors){
  
  rgx = range(EVT_l[[evt_i]]$date)
  rgx[2] = DATES_EVTS$FIN_Q[evt_i]
  rgx[1] = DATES_EVTS$DEB_Q[evt_i] 
  
  cum = apply(EVT_l[[evt_i]][ , -1],1, mean, na.rm =T) 
  plot(EVT_l[[evt_i]]$date, cum*12, type = "h", col = "royalblue", lwd =2,
       xlim = rgx, ylim = c(30,0), xlab = "", ylab = "", axes = F)
  axis(4, col.ticks = "royalblue", col ="royalblue")
  axis.POSIXct(1, at = seq(from = rgx[1], to = rgx[2], length.out = 4),
               labels = format(seq(from = rgx[1], to = rgx[2], length.out = 4),
                               format = "%d/%m %Hh"))
  par(new = T)
  idxT = tab_discharge$date <= rgx[2]  &  tab_discharge$date >= rgx[1]
  rgY = c(0.05/31, 1.5*max(as.vector(tab_discharge[idxT, paste("Qmed_",stations, sep ="")]), na.rm =T))
  idx = !is.na(tab_discharge[, paste("Qmed_",stations[1], sep ="")])
  plot(tab_discharge$date[idx & idxT], tab_discharge[idx & idxT, paste("Qmed_",stations[1], sep ="")],
       type = "l", col = colors[1], xlim = rgx, ylim = rgY, xlab = "", ylab = "",
       axes = F)
  axis(2)
  mtext(side = 2, line = 2.1, "specific discharge")
  box()
  for(i in 2:length(stations)){
    idx = !is.na(tab_discharge[, paste("Qmed_",stations[i], sep ="")])
    lines(tab_discharge$date[idx & idxT], tab_discharge[idx & idxT, paste("Qmed_",stations[i], sep ="")],
          type = "l", col = colors[i])
    
  }
  title(main = names(EVT_l)[evt_i])
  
  
}
par(mfrow = c(2,2), mar = c(3,4,1,4))
for(i in 1:length(EVTs)){
  
  plot_evti(EVTs, i,TAB_discharge, stations[c(1,2,3,4,5,6)], colours[c(1,2,3,4,5,6)])
  
}

#########################################################"""
adout = "/media/audrey/8858-1E31/PRO/2019/test_dispositifs/EBgather/"
write.table(DATES_EVTS, paste(adout, "range_evts2022j.csv",sep =""), row.names = F,sep =";")
###########################################################"

EVTs_bis = list()
# recalcul Vrain2 et Train2
for(evt_i in names(EVTs)){
  
  rgx = c(DATES_EVTS$DEB_Q[DATES_EVTS$EVTs == evt_i], DATES_EVTS$FIN_Q[DATES_EVTS$EVTs == evt_i])
  idx = RainEB$date <= rgx[2] & RainEB$date >= rgx[1] 
  EVTs_bis[[evt_i]] = RainEB[idx,]
  
  for(i in 1:length(stations)){
    
    EVTs_bis[[evt_i]][, stations[i]] = 0
    isnonaP = apply(is.na(EVTs_bis[[evt_i]][, pluviosN]),2,sum)<10
    for(p in which(isnonaP)){
      
      EVTs_bis[[evt_i]][, stations[i]] = EVTs_bis[[evt_i]][, stations[i]] + 
        EVTs_bis[[evt_i]][, pluviosN[p]]*ThiessenArea[ThiessenArea$BV == stations[i],pluviosN[p] ]/ sum(ThiessenArea[ThiessenArea$BV == stations[i],pluviosN[isnonaP] ])
      
    }

  }
  for(sta_i in 1:length(stations)){
    
    tmp =EVTs_bis[[evt_i]][, stations[sta_i]]
    tmp[is.na(tmp)] =0
    STATS_EVTS[ evt_i, stations[sta_i], "VrainBis"] = max(cumsum(tmp))
    cumucumu = cumsum(tmp)/max(cumsum(tmp))
    d50th  = EVTs_bis[[ evt_i]][mean(which(abs(cumucumu-0.5) == min(abs(cumucumu-0.5)))), "date"]
    STATS_EVTS[ evt_i, stations[sta_i], "Train50thBis"] = d50th
  }
}

STATS_EVTS[STATS_EVTS[,  "Koedange" ,"VrainBis"]==0,  "Koedange" ,"VrainBis"] = STATS_EVTS[STATS_EVTS[,  "Koedange" ,"VrainBis"]==0,  "Heffingen" ,"VrainBis"]
for(sta_i in 1:length(stations)){
  STATS_EVTS[, stations[sta_i], "Vruiss"] =  STATS_EVTS[, stations[sta_i],"Vruiss"]/aires[sta_i]/1000
  STATS_EVTS[,  stations[sta_i],"CR"] =100*  STATS_EVTS[, stations[sta_i] ,"Vruiss"]/STATS_EVTS[,  stations[sta_i] ,"VrainBis"]
}
# =======================================================


# =======================================================
# calcul statistiques des evts
## volume du premier pic ---
for(evt_i in 1:length(EVTs)){
  par(mfrow = c(3,2), mar = c(4,4,1,4))
  for(sta_i in 1:length(stations)){
    
    rgx = range(EVTs[[evt_i]]$date)
    idx = !is.na(discharge_evts[[evt_i]][, paste("Qmed_", stations[sta_i], sep = "")])
    if(sum(idx)>0){
      plot(discharge_evts[[evt_i]]$date[idx], ylim = c(0,15),
         discharge_evts[[evt_i]][idx,paste("Qmed_", stations[sta_i], sep = "")]/aires[sta_i]/1000, type = "l" ,
         xlab = "", ylab = "",axes = F, xlim = rgx)
    axis(2)
    par(new = T)
    idxT = TAB_discharge$date <= rgx[2]  &  TAB_discharge$date >= rgx[1]
    rgY = c(0.05/31, 1.*max(as.vector(TAB_discharge[idxT, paste("Qmed_",stations, sep ="")]), na.rm =T))
    idx = !is.na(TAB_discharge[, paste("Qmed_",stations[sta_i], sep ="")])
    plot(TAB_discharge$date[idx & idxT], TAB_discharge[idx & idxT, paste("Qmed_",stations[sta_i], sep ="")],
         type = "l", col = colours[sta_i],  ylim = rgY, xlab = "", ylab = "",
         axes = F, xlim = rgx)
    box()
    idx = !is.na(EVTs[[evt_i]][, stations[sta_i]])
    if(sum(idx)>0){
      par(new = T)
      plot(EVTs[[evt_i]]$date[idx], ylim = c(0,30),
           cumsum(EVTs[[evt_i]][idx,stations[sta_i]]), type = "l" ,
           xlab = "", ylab = "",axes = F, col = "blue", xlim = rgx)
      axis(4)
      
    }
    
    }
  }
}




par(mfrow = c(1,1), mar = c(4,4,1,1))
plot(as.vector(STATS_EVTS[, ,"Vrain" ]),as.vector(STATS_EVTS[, ,"Vruiss" ]),
     col = rep(colours, each = nrow(STATS_EVTS[, ,"Vrain" ])), 
     pch = 15,xlab = "Rain", ylab = "Ruissellement")

plot(as.vector(STATS_EVTS[, ,"Vrain" ]),as.vector(STATS_EVTS[, ,"CR" ]),
     col = rep(colours, each = nrow(STATS_EVTS[, ,"Vrain" ])), 
     pch = 15,  xlab = "Rain", ylab = "CR")

png(paste(ad_img, "/exemple_summer_winter_CR.png", sep=""),
    width= 6.4, height=3.0, units="in", res=400, pointsize =12)
evt_date = rep(as.POSIXct(names(EVTs),format = "%Y_%m_%d", tz = "UTC"), length(stations))
par(mfrow = c(1,2), mar = c(4,4,2,2))
plot(evt_date,as.vector(STATS_EVTS[, ,"CR" ]),
     col = rep(colours, each = nrow(STATS_EVTS[, ,"Vrain" ])), 
     pch = 15,  xlab = "", ylab = "", ylim = c(0,40))
mtext(side = 1, "date", line = 2.1)
mtext(side = 2, "Runoff coefficient [%]", line = 2.1)

plot(as.vector(STATS_EVTS[, ,"Hum_initBot" ])*100,as.vector(STATS_EVTS[, ,"CR" ]),
     col = rep(colours, each = nrow(STATS_EVTS[, ,"Vrain" ])), ylim = c(0,40), 
     pch = 15, xlab = "", ylab = "")
mtext(side = 1, "Soil Moisture before the event[%]", line = 2.1)
mtext(side = 2, "Runoff coefficient [%]", line = 2.1)

dev.off()

plot(as.vector(STATS_EVTS[, ,"Hum_initBot" ]),as.vector(STATS_EVTS[, ,"CR" ]),
     col = rep(colours, each = nrow(STATS_EVTS[, ,"Vrain" ])), 
     pch = 15, 
     xlab = "Hum_initBot", ylab = "CR")


delta_t = as.numeric(as.vector(STATS_EVTS[, ,"Truiss50th" ])) - as.numeric(as.vector(STATS_EVTS[, ,"Train50thBis" ]))
delta_t = delta_t / 3600
plot(as.vector(STATS_EVTS[, ,"Hum_initBot" ]), delta_t,
     col = rep(colours, each = nrow(STATS_EVTS[, ,"Vrain" ])), 
     pch = 15, 
     xlab = "Rain", ylab = "delta Runoff 50th")

plot(as.vector(STATS_EVTS[, ,"Duree" ]), delta_t,
     col = rep(colours, each = nrow(STATS_EVTS[, ,"Vrain" ])), 
     pch = 15, 
     xlab = "Rain", ylab = "delta Runoof 50th")

delta_t = as.numeric(as.vector(STATS_EVTS[, ,"Tpeak" ])) - as.numeric(as.vector(STATS_EVTS[, ,"Train50th" ]))
delta_t = delta_t / 3600
plot(evt_date, delta_t,
     col = rep(colours, each = nrow(STATS_EVTS[, ,"Vrain" ])), 
     pch = 15, 
     xlab = "Rain", ylab = "delta Tpeak")

# =======================================================
par(mfrow = c(1,1), mar = c(4,4,1,1))
plot(as.vector(STATS_EVTS[, ,"Vrain" ]),as.vector(STATS_EVTS[, ,"Vruiss" ]),
     col = rep(colours, each = nrow(STATS_EVTS[, ,"Vrain" ])), 
     pch = 15,xlab = "Rain", ylab = "Ruissellement")

#@"##################################################"

##
png(paste(ad_img, "/Global_temps_de_reponseHYDROGRAMME2j.png", sep=""),
    width= 5, height=5, units="in", res=400, pointsize =12)
par(mfrow = c(1,1), mar = c(3,4,1,4))
evt_date = rep(as.POSIXct(EVTs,format = "%Y_%m_%d", tz = "UTC"), 3)
delta_t = as.numeric(as.vector(STATS_EVTS[, c(1,3,5),"Truiss50th" ])) - as.numeric(as.vector(STATS_EVTS[,  c(1,3,5),"Train50th" ]))
delta_t = delta_t / 3600
POM = data.frame(date = evt_date)
POM$angle = (as.numeric(format(evt_date,"%j")) )*360/366
POM$intensite = delta_t
POM$coul = rep(colours[ c(1,3,5)], each = nrow(STATS_EVTS[, ,"Vrain" ]))
POM$cex = 0.7 + (as.vector(STATS_EVTS[,  c(1,2,4),"Vrain" ]) - range(STATS_EVTS[, c(1,3,5) ,"Vrain" ], na.rm =T)[1] )/
  (range(STATS_EVTS[, c(1,3,5),"Vrain" ], na.rm =T)[2]-range(STATS_EVTS[,  c(1,3,5),"Vrain" ], na.rm =T)[1])*(2.5-0.7)

par(mfrow = c(1,1))
polar.plot(POM$intensite,POM$angle,main="",lwd=5,point.col=POM$coul,rp.type ="s",
           start=90, clockwise = T, xlim = c(-5,35),point.symbols =16,cex = POM$cex,
           label.pos = c(0,31,59,90,120,151,181,212,243,273,304,334)*360/366,
           labels = c("jan", "fev", "mar", "apr","mai", "jun", "jul","aug","sep",
                      "oct","nov","dec"),radial.lim=c(0,30), 
           radial.labels =seq(from = 0, to =30, by = 5))
dev.off()

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


# reset ev
delta_t = STATS_EVTS[c(4,5,12), ,"Truiss50th" ] - STATS_EVTS[c(4,5,12), ,"Train50th" ]
delta_t = delta_t / 3600



##### plot again avec TAB DEBUT FIN
plot_evti = function(evt_i, tab_rain, tab_discharge, tab_date , stations, colors){
  
  rgx =c(tab_date$DEB_Q[tab_date$EVTs == evt_i], tab_date$FIN_Q[tab_date$EVTs == evt_i])
  
  idxR = tab_rain$date <= rgx[2] & tab_rain$date >= rgx[1]
  
  cum = apply(tab_rain[idxR,-1],1, mean, na.rm =T) 
  plot(tab_rain$date[idxR], cum*12, type = "h", col = "royalblue", lwd =2,
       xlim = rgx, ylim = c(30,0), xlab = "", ylab = "", axes = F)
  box()
  axis(4, col.ticks = "royalblue", col ="royalblue")
  axis.POSIXct(1, at = seq(from = rgx[1], to = rgx[2], length.out = 4),
               labels = format(seq(from = rgx[1], to = rgx[2], length.out = 4),
                               format = "%d/%m %Hh"))
  par(new = T)
  idxQ = tab_discharge$date <= rgx[2] & tab_discharge$date >= rgx[1]
  rgY = c(0.05/31, 1.5*max(as.vector(tab_discharge[idxQ, paste("Qmed_",stations, sep ="")]), na.rm =T))
  idx = !is.na(tab_discharge[, paste("Qmed_",stations[1], sep ="")])
  plot(tab_discharge$date[idx & idxQ], tab_discharge[idx & idxQ, paste("Qmed_",stations[1], sep ="")],
       type = "l", col = colors[1], xlim = rgx, ylim = rgY, xlab = "", ylab = "",
       axes = F)
  axis(2)
  mtext(side = 2, line = 2.1, "specific discharge")
  mtext(side = 4, line = 2.1, "rainfall [mm/h]", col = "royalblue")
  for(i in 2:length(stations)){
    idx = !is.na(tab_discharge[, paste("Qmed_",stations[i], sep ="")])
    lines(tab_discharge$date[idx & idxQ], tab_discharge[idx & idxQ, paste("Qmed_",stations[i], sep ="")],
          type = "l", col = colors[i])
    
  }
  title(main = evt_i)
  
  
}

colours = c("pink", "darkcyan","orangered", "navyblue","olivedrab3",   "black")

par(mfrow = c(2,2), mar = c(3,4,1,4))
for(evt_i in names(EVTs)){
  
  plot_evti(evt_i, RainEB,TAB_SPEdischarge,DATES_EVTS, stations, colours)
  
}

png(paste(ad_img, "/exemple_summer_winter_HYDROGRAMME2.png", sep=""),
    width= 6.8, height=5, units="in", res=400, pointsize =12)
par(mfrow = c(2,2), mar = c(3,4,1,4))
for(i in c(4,5,11,12)){
  
  plot_evti(names(EVTs)[i], RainEB,TAB_SPEdischarge,DATES_EVTS, stations[c(1,3,5)], colours[c(1,3,5)])
  if(i == 4){
    
    legend("topright", col = c("olivedrab3", "pink", "orangered"),
           c("Medernach", "Heffingen", "Koedange"), lty =1 )
    
  }
}

dev.off()



##### sortie obs Qobs
ad = "/media/audrey/8858-1E31/PRO/2019/test_dispositifs/EBgather/"
DATE_EVTS  = read.table(paste(ad, "range_evts2j.csv",sep =""), header = T,
                        stringsAsFactors = F, sep =";")
EVTs = DATE_EVTS$EVTs
for(i in 2:5){
  DATE_EVTS[,i] = as.POSIXct(DATE_EVTS[,i], tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
}
DATE_EVTS$FIN_Q = DATE_EVTS$FIN_Q + 24*3600

DATE_EVTS[12,]
Qobs_fromTable = function(adout, tab_discharge, tab_date,  station_i, evt_i, coordonnees){
  
  rgx =c(tab_date$DEB_Q[tab_date$EVTs == evt_i], tab_date$FIN_Q[tab_date$EVTs == evt_i])
  idxQ = tab_discharge$date <= rgx[2] & tab_discharge$date >= rgx[1]
  idx = !is.na(tab_discharge[, paste("Qmed_",station_i, sep ="")])
  table = tab_discharge[idxQ & idx, c("date",  paste("Qmed_",station_i, sep =""))]
  
  ado = paste(adout, "/Qobs_", station_i,"_", evt_i, ".txt", sep="")
  fileConn <- file(ado)
  
  writeLines(c(as.character(1), paste(as.character(table$date[1])," - ", as.character(table$date[length(table$date)])),
               as.character(1),
               "StationID               X            Y Type(1:discharge,2:water level)",
               paste(station_i, "        ", as.character(coordonnees[1]), as.character(coordonnees[2]), "1"), 
               "time shift: 00:00", "Date                Q(m3.s-1)"), fileConn)
  close(fileConn)
  
  write.table(table, ado , col.names = F, append =T,  row.names=F, sep=" ")
  
}


coordonnees = array(dim = c( 2,length(stations)), c(84375.00,92400.00,
                                                     85175.00,102000.00,
                                                     83175.00,89000.00,
                                                     83675.00,95200.00,
                                                     83575.00,98600.00,
                                                     87075.00,103600.00),
                    dimnames= list(NULL, stations))

adout = "/home/adouinot/BVs/Ernz_Blanche/Sources/QOBS/"

for(evt_i in EVTs[c(8:24)]){
  
  for(sta_i in stations[-c(4,6)]){
    
    Qobs_fromTable(adout, TAB_discharge, DATE_EVTS,  sta_i, evt_i, coordonnees[ , sta_i])
      
  }
}


##### sortie mrr pluviometres
Mrr_fromPluvios = function(adout, tab_rain, tab_date,  evt_i, coordonneesP){
  
  rgx =c(tab_date$DEB_Q[tab_date$EVTs == evt_i], tab_date$FIN_Q[tab_date$EVTs == evt_i])
  print(rgx)
  idxP = tab_rain$date <= rgx[2] & tab_rain$date >= rgx[1]
  #idx = !is.na(tab_discharge[, paste("Qmed_",station_i, sep ="")])
  table = tab_rain[idxP, ]
  table[,-1] = table[,-1]*10 
  
  ado = paste(adout, "/Ppluvios_", evt_i, ".txt", sep="")
  fileConn <- file(ado)
  
  writeLines(c("#-------------  EN TETE ----------------",
               "Data type: PLUVIO", "Pas de temps: 00:05:00",
               "#------------- Coordonnees -------------",
               as.character(ncol(tab_rain)-1),
               paste(as.character(coordonneesP[1,1]),as.character(coordonneesP[2,1]), names(tab_rain)[2]),
               paste(as.character(coordonneesP[1,2]),as.character(coordonneesP[2,2]), names(tab_rain)[3]),
               paste(as.character(coordonneesP[1,3]),as.character(coordonneesP[2,3]), names(tab_rain)[4]),
               paste(as.character(coordonneesP[1,4]),as.character(coordonneesP[2,4]), names(tab_rain)[5])), fileConn)
  close(fileConn)
  write.table(table, ado , col.names = F, append =T,  row.names=F, sep=" ")
  
}

Pluvios_names = names(RainEB)[-1]
coordonneesP = array(dim = c( 2,length(Pluvios_names)), c(82724,86347,
                                                    83615,100264,
                                                    79934,91330,
                                                    84599,96309),
                    dimnames= list(NULL, Pluvios_names))

adout = "/home/douinot/BVs/Ernz_Blanche/Sources/PLUIES/"

for(evt_i in names(EVTs)[c(22,23,24)]){
  
 Mrr_fromPluvios(adout, RainEB, DATES_EVTS,  evt_i, coordonneesP)
}

adout = "/media/adouinot/8858-1E31/PRO/2019/test_dispositifs/EBgather/"
write.table(DATES_EVTS, paste(adout, "range_evts.csv",sep =""), row.names = F,sep =";")
