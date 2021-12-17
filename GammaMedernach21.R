
rm(list =ls())
library("pracma", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")
library(plotrix)
library("RSAGA", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")

################################################################################################################

ad = "/media/audrey/8858-1E31/PRO/2019/test_dispositifs/EBgather/"
DATE_EVTS  = read.table(paste(ad, "range_evts2022bis.csv",sep =""), header = T,
                        stringsAsFactors = F, sep =";")
EVTs = DATE_EVTS$EVTs

adout = "/media/audrey/8858-1E31/PRO/2019/test_dispositifs/EBgather/"
load(file = paste(adout, "stats_evts2022.rda", sep = ""))

for(i in 2:5){
  DATE_EVTS[,i] = as.POSIXct(DATE_EVTS[,i], tz = "UTC", format = "%Y-%m-%d %H:%M:%S")
}
areas =  c(31.14,48.687, 69.41,79.04,92.981, 100.6)


adout = "/media/audrey/8858-1E31/PRO/2019/test_dispositifs/EBgather/"
RainEVTs = read.table( paste(adout, "RainEB_evts2022.csv",sep =""), header = T,sep =";",
                       stringsAsFactors = F)
RainEVTs$date = as.POSIXct(RainEVTs$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")

TAB_runoff =  read.table(paste(adout, "RunoffEB_evts2022.csv",sep =""),  header = T,sep =";",
                         stringsAsFactors = F)
TAB_runoff$date = as.POSIXct(TAB_runoff$date, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")


################################################################################################################

################################################################################################################
############## calcul de Pluie troncon
# RainEVTs$Medernach_troncon = 0
###### ok already done

##############  correct avec CR et l'aire
rapport = 1- STATS_EVTS[, c("Heffingen"), "Vruiss"]*48.67/
  STATS_EVTS[, c("Medernach"), "Vruiss"]/79.03894 

for(evt_i in 1:length(EVTs)){
  
  id_rain = which(RainEVTs$date <= DATE_EVTS$FIN_Q[evt_i] & RainEVTs$date >= DATE_EVTS$DEB_P[evt_i] )
  
  Vrain = sum(RainEVTs$Medernach[id_rain],na.rm =T)*(79.03894-48.67)
  VruisseleM = STATS_EVTS[evt_i, c("Medernach"), "Vruiss"]*79.03894 
  VruisseleH = STATS_EVTS[evt_i, c("Heffingen"), "Vruiss"]*48.67
  
  alpha = (VruisseleM - VruisseleH)/Vrain
  RainEVTs$Medernach[id_rain] = RainEVTs$Medernach[id_rain] *alpha *(areas[4] - areas[2])
  print(alpha)
}

####################################################
####################################################
# gumbel function
gamma_law = function(x,alpha,theta){
  
  d = x
  idx = x >0
  d[!idx] = 0
  d[idx] = dgamma(d[idx],alpha, scale = theta, log = FALSE)
  
  return(d)
}

gumbel_law = function(x,mu,beta){
  
  d = x
  idx = x >0
  d[!idx] = 0
  z = exp(-(d[idx]-mu)/beta)
  d[idx] = exp(-z)*z/beta
  
  
  d[idx] = d[idx]/(1-exp(-exp(mu/beta)))
  return(d)
}


apply_GammaTransfert = function(date, pluie, debit, alpha_v, theta_v, muQ_v, betaQ_v){
  
  DT = c(as.numeric(date) - min(as.numeric(date)))/3600
  L = length(date)
  print(L)
  
  MAT_IT = matrix(-1, nrow = L, ncol = L)
  for(t in 1:L){
    MAT_IT[t:L,t] = DT[1:length(t:L)]
  }
  diag(MAT_IT)= 150/3600
  
  
  MAT_Pvol = matrix(NA, nrow =L, ncol = L)
  for(i in 1:L){
    MAT_Pvol[i,] = pluie 
  }
  
  MAT_Qvol = matrix(NA, nrow =L, ncol = L)
  for(i in 1:L){
    MAT_Qvol[i,] = debit 
  }
  
  MAT_VOUT = matrix(NA, nrow = L, ncol = length(betaQ_v))
  MAT_QOUT = matrix(NA, nrow = L, ncol = length(betaQ_v))
  
  for(mk in 1:length(betaQ_v)){
    
    P_IT = gamma_law(MAT_IT,alpha_v[mk],theta_v[mk])
    Q_IT = gumbel_law(MAT_IT,muQ_v[mk],betaQ_v[mk])
    PVC = MAT_Pvol * P_IT
    QVC = MAT_Qvol * Q_IT
    
    POUT_sout = apply(PVC ,1, sum)
    QOUT_sout = apply(QVC ,1, sum)
    MAT_VOUT[,mk] =  POUT_sout
    MAT_QOUT[,mk] =  QOUT_sout
    
  }
  
  return(list(MAT_VOUT,MAT_QOUT) )
}

apply_GammaStation = function(TABLE, nameP, nameQ, EVTs_sel,table_range,  alpha_v, theta_v, muQ_v, betaQ_v){
  
  MAT_POUT_TOT = matrix(NA, nrow = 1, ncol = length(betaQ_v))
  MAT_QOUT_TOT = matrix(NA, nrow = 1, ncol = length(betaQ_v))
  
  DATA_RUNS = data.frame(date = NA, Pluie = NA, Debit = NA)
  
  for(evt_c in EVTs_sel){
    
    evt_i = which(EVTs== evt_c)
    print(evt_c)
    id_rain = which(TABLE$date <= table_range$FIN_Q[evt_i] & TABLE$date >= table_range$DEB_P[evt_i] )
    
    pluie = TABLE[id_rain, nameP] 
    if(sum(is.na(pluie))>10){
      "becarefull too much na in rainfall"
    }
    pluie[is.na(pluie)] = 0
    debit = TABLE[id_rain, nameQ]
    if(sum(is.na(debit))>10){
      "becarefull too much na in rainfall"
    }
    debit[is.na(debit)] = mean(debit, na.rm=T)
    date = as.numeric(TABLE[id_rain,"date"]) - min(as.numeric( TABLE[id_rain,"date"]))
    
    date_runs_tmp = data.frame(date = TABLE[id_rain,"date"], Pluie = pluie, Debit = debit)
    DATA_RUNS = rbind(DATA_RUNS, date_runs_tmp)
    
    OUT_tmp = apply_GammaTransfert(date, pluie, debit,alpha_v, theta_v, muQ_v, betaQ_v)
    
    MAT_POUT_TOT = rbind(MAT_POUT_TOT, OUT_tmp[[1]])
    MAT_QOUT_TOT = rbind(MAT_QOUT_TOT, OUT_tmp[[2]])
    
  }
  return(list(DATA_RUNS[-1,], MAT_POUT_TOT[-1,], MAT_QOUT_TOT[-1,]))
}

extract_RUNOFF = function(tab_date_source, tab_runoff , station_c, area_c  ){
  
  datess = as.POSIXct(as.numeric(tab_date_source$date), tz = "UTC", origin = "01-01-1970", format = "%d-%m-%Y")
  
  tab_tmp = tab_runoff[!is.na(paste("Qmed_",station_c ,sep ="")), ] 
  tab_tmp$datesec  = as.integer(c(as.numeric(tab_tmp$date)+240)/300)*300
  tab_tmp2  = aggregate(tab_tmp, by = list(datesec = tab_tmp$datesec), FUN = "mean", na.rm =T)
  tab_tmp2$date = as.POSIXct(as.numeric(tab_tmp2$datesec),
                             tz = "UTC", origin = "01-01-1970", format = "%d-%m-%Y")
  
  idxr  = tab_tmp2$date %in% datess
  idxd   =   datess %in% tab_tmp2$date
  #tab_date_source$date = datess
  
   
  tab_date_source$Qout = NA
  tab_date_source$Qout[idxd] = tab_tmp2[idxr, paste("Qmed_",station_c ,sep ="")] 
  tab_date_source$Qout[idxd] = tab_date_source$Qout[idxd] *5*60 / area_c / 1000  
  
  return(tab_date_source)
}

station = "Medernach"
EVTs_k = DATE_EVTS$EVTs[5:nrow(DATE_EVTS)]
alphaPV = runif(2000,0.1,16)
thetaPV = runif(2000,0.1,10)
betaQV = runif(2000,0.05,5)
muQV = runif(2000,0.1,4.5)

PARAMS = data.frame(alphaPV = alphaPV, thetaPV =thetaPV, muQV = muQV,betaQV = betaQV)
Qheffingen = extract_RUNOFF(RainEVTs, TAB_runoff, "Heffingen", 1 )
names(Qheffingen)[12] = "Qheffingen"

#complete blanck
toreplace = which(is.nan(Qheffingen$Qheffingen))[which(is.nan(Qheffingen$Qheffingen))>1867]
valuee = rep(NA, length(toreplace))
iv = vector()
io = 1
while(io < (length(toreplace))){
  
  iv = c(iv, io)
  bg = toreplace[io]-1 
  while(toreplace[io+1]== toreplace[io]+1){
    
    io = io+1
    iv = c(iv, io)
  }
  bd = toreplace[io]+1
  
  valuee[iv] = Qheffingen$Qheffingen[bg]+ (Qheffingen$Qheffingen[bd]-Qheffingen$Qheffingen[bg])*(toreplace[iv]-bg)/(bd-bg)
  io = io+1
  iv = vector()
}
Qheffingen$Qheffingen[toreplace]= valuee
#complete blanck

Medernach_Essai = apply_GammaStation(Qheffingen , "Medernach","Qheffingen",
                                      EVTs_k, DATE_EVTS, alphaPV, thetaPV, muQV, betaQV)


Qmedernach = extract_RUNOFF(Medernach_Essai[[1]], TAB_runoff, "Medernach", 1/12 )
#complete blanck
toreplace = which(is.nan(Qmedernach$Qout))
valuee = rep(NA, length(toreplace))
iv = vector()
io = 1
while(io < (length(toreplace)-1)){
  
  iv = c(iv, io)
  bg = toreplace[io]-1 
  while(toreplace[io+1]== toreplace[io]+1){
    
    io = io+1
    iv = c(iv, io)
  }
  bd = toreplace[io]+1
  
  valuee[iv] = Qmedernach$Qout[bg]+ (Qmedernach$Qout[bd]-Qmedernach$Qout[bg])*(toreplace[iv]-bg)/(bd-bg)
  io = io+1
  iv = vector()
}
Qmedernach$Qout[toreplace]= valuee
#complete blanck

Pout =Medernach_Essai[[2]]
Qout =Medernach_Essai[[3]]
PQout = Medernach_Essai[[2]] + Medernach_Essai[[3]]
####


SCORES = array(dim=c(length(EVTs_k), length(alphaPV),8),
               dimnames = list(EVTs_k, NULL, c("RMSE", "corrr", "varr", "MAE",
                                               "Nash","Res", "qth_match", "maxQ")))
for(e_i  in 1:length(EVTs_k)){
  
  evt_i = which(EVTs== EVTs_k[e_i])
  print(EVTs_k[e_i])
  id_rain = which(Qmedernach$date <= DATE_EVTS$FIN_Q[evt_i] & Qmedernach$date >= DATE_EVTS$DEB_P[evt_i] )
  
  qth_obs = as.numeric(quantile(Qmedernach$Qout[id_rain], c(0.75,0.97), na.rm =T))
  maxQobs = max(Qmedernach$Qout[id_rain], na.rm =T)
  print(qth_obs)
  for(i in 1:length(alphaPV)){
    
    l = !is.na(Qmedernach$Qout[id_rain])
    SCORES[e_i, i, "Res"]= sqrt(mean((PQout[id_rain, i]-Qmedernach$Qout[id_rain])*(PQout[id_rain, i]-Qmedernach$Qout[id_rain]),na.rm=T))
    SCORES[e_i, i, "RMSE"]= sqrt(mean((PQout[id_rain, i]-Qmedernach$Qout[id_rain])*(PQout[id_rain, i]-Qmedernach$Qout[id_rain]),na.rm=T))/mean(Qmedernach$Qout[id_rain],na.rm=T)
    SCORES[e_i, i, "corrr"] =  abs(1-cor(PQout[id_rain, i][ l],Qmedernach$Qout[id_rain][ l]))
    SCORES[e_i, i, "varr"] =  abs(1 - var(PQout[ id_rain, i][l])*mean(Qmedernach$Qout[id_rain], na.rm =T)/var(Qmedernach$Qout[id_rain], na.rm =T)/mean(PQout[id_rain, i][l]))
    SCORES[e_i, i, "MAE"] =  abs(1 - mean(PQout[ id_rain, i][l])/mean(Qmedernach$Qout[id_rain], na.rm =T))
    SCORES[e_i, i, "Nash"] = mean((PQout[id_rain, i]-Qmedernach$Qout[id_rain])*(PQout[id_rain, i]-Qmedernach$Qout[id_rain]),na.rm=T)/
      mean((Qmedernach$Qout[id_rain]-mean(Qmedernach$Qout[id_rain],na.rm=T))*(Qmedernach$Qout[id_rain]-mean(Qmedernach$Qout[id_rain],na.rm=T)), na.rm=T)
    SCORES[e_i, i, "qth_match"] = sum(abs((as.numeric(quantile(PQout[id_rain, i], c(0.75,0.97), na.rm =T))-qth_obs)/qth_obs))
    SCORES[e_i, i, "maxQ"] = abs((as.numeric(max(PQout[id_rain, i],  na.rm =T))-maxQobs)/maxQobs)
  }
  
}

plot(as.data.frame(cbind(SCORES[6,,],PARAMS )))

### calibration par evt
# 100 meilleures var + cor  PLUIE
nb = 50
CALIB_EVTS = data.frame(EVTS = EVTs_k, MU_min = NA, MU_max = NA,
                        MU_med = NA, BETA_min  = NA, BETA_max  = NA,
                        BETA_med  = NA, ESP_min = NA, ESP_max = NA,
                        ESP_med = NA, STD_min = NA, STD_med = NA, STD_max = NA,
                        RMSE_med = NA, corvar_med = NA)
for(e_i  in 1:length(EVTs_k)){
  
  limit  = sort(SCORES[e_i, , "corrr"]+SCORES[e_i, , "varr"]+SCORES[e_i, , "MAE"])[nb]
  idx = which((SCORES[e_i, , "corrr"]+SCORES[e_i, , "varr"]+SCORES[e_i, , "MAE"]) <limit)
  limit  = sort(SCORES[e_i, , "RMSE"])[nb]
  idx = which((SCORES[e_i, , "RMSE"])<limit)
  #limit  = sort(SCORES[e_i, , "qth_match"])[nb]
  #idx = which((SCORES[e_i, , "qth_match"])<limit)
  
  CALIB_EVTS[e_i, c("MU_min", "MU_max")] = range(alphaPV[idx])
  CALIB_EVTS[e_i, "MU_med"] = quantile(alphaPV[idx],0.5)
  CALIB_EVTS[e_i, c("BETA_min", "BETA_max")] = range(thetaPV[idx])
  CALIB_EVTS[e_i, "BETA_med"] = quantile(thetaPV[idx],0.5)
  CALIB_EVTS[e_i, "ESP_med"] = quantile(alphaPV[idx] *thetaPV[idx],0.5)
  CALIB_EVTS[e_i, c("ESP_min","ESP_max")] = range(alphaPV[idx] * thetaPV[idx])
  CALIB_EVTS[e_i, "STD_med"] = quantile(sqrt(alphaPV[idx] * thetaPV[idx]*thetaPV[idx]),0.5)
  CALIB_EVTS[e_i, c("STD_min", "STD_max")] = range(sqrt(alphaPV[idx] * thetaPV[idx]*thetaPV[idx]))
  CALIB_EVTS[e_i, "RMSE_med"] = quantile(SCORES[e_i,idx , "RMSE"],0.5)
  CALIB_EVTS[e_i, "corvar_med"] = quantile(SCORES[e_i,idx , "corrr"]+
                                             SCORES[e_i,idx , "varr"]+
                                             SCORES[e_i,idx , "MAE"],0.5)
  print(EVTs_k[e_i])
}


### plot calib  
par(mfrow = c(2,2), mar = c(2,2,2,2))
plot(c(1:nrow(CALIB_EVTS)), CALIB_EVTS$MU_med, pch = 15, 
     ylim = c(min(CALIB_EVTS$MU_min),max(CALIB_EVTS$MU_max)))
segments(x0 = 1:nrow(CALIB_EVTS), x1 = 1:nrow(CALIB_EVTS),
         y0 = CALIB_EVTS$MU_min, y1 = CALIB_EVTS$MU_max)

plot(c(1:nrow(CALIB_EVTS)), CALIB_EVTS$BETA_med, pch = 15, 
     ylim = c(min(CALIB_EVTS$BETA_min),max(CALIB_EVTS$BETA_max)))
segments(x0 = 1:nrow(CALIB_EVTS), x1 = 1:nrow(CALIB_EVTS),
         y0 = CALIB_EVTS$BETA_min, y1 = CALIB_EVTS$BETA_max)

plot(c(1:nrow(CALIB_EVTS)), CALIB_EVTSP$ESP2_med, pch = 15,
     ylim = c(min(CALIB_EVTSP$ESP2_min),max(CALIB_EVTSP$ESP2_max)))
segments(x0 = 1:nrow(CALIB_EVTS), x1 = 1:nrow(CALIB_EVTS),
         y0 = CALIB_EVTSP$ESP2_min, y1 = CALIB_EVTSP$ESP2_max)

plot(c(1:nrow(CALIB_EVTS)), CALIB_EVTS$STD_med, pch = 15,
     ylim = c(min(CALIB_EVTS$STD_min),max(CALIB_EVTS$STD_max)))
segments(x0 = 1:nrow(CALIB_EVTS), x1 = 1:nrow(CALIB_EVTS),
         y0 = CALIB_EVTS$STD_min, y1 = CALIB_EVTS$STD_max)

CALIB_EVTSP = CALIB_EVTS


##### new t50th
#load("/media/douinot/8858-1E31/PRO/2019/test_dispositifs/EBgather/GumbelMedernach.rda")
T50th = matrix(nrow = length(EVTs_k), ncol = nb)
for(e_i in 1:length(EVTs_k)){
  
  limit  = sort(SCORES[e_i, , "RMSE"])[nb]
  idx = which((SCORES[e_i, , "RMSE"])<=limit)
  
  X = c(0:1000)/20
  
  for(n_i in 1:length(idx)){
    
    mui = alphaPV[idx][n_i]
    betai = thetaPV[idx][n_i]
    Y = gamma_law(X, mui,betai)
    ttmp = X[min(which(cumsum(Y)/20>0.5))]
    T50th[e_i, n_i] = ttmp
  }
  
}


CALIB_EVTSP$ESP2_min = apply(T50th,1,min)
CALIB_EVTSP$ESP2_max = apply(T50th,1,max)
CALIB_EVTSP$ESP2_med = apply(T50th,1,quantile,0.5)
##################################################

### calibration par evt
# 100 meilleures var + cor  ***** DEBIT
nb = 50
CALIB_EVTS = data.frame(EVTS = EVTs_k, MU_min = NA, MU_max = NA,
                        MU_med = NA, BETA_min  = NA, BETA_max  = NA,
                        BETA_med  = NA, ESP_min = NA, ESP_max = NA,
                        ESP_med = NA, STD_min = NA, STD_med = NA, STD_max = NA,
                        RMSE_med = NA, corvar_med = NA)
for(e_i  in 1:length(EVTs_k)){
  
  limit  = sort(SCORES[e_i, , "corrr"]+SCORES[e_i, , "varr"]+SCORES[e_i, , "MAE"])[nb]
  idx = which((SCORES[e_i, , "corrr"]+SCORES[e_i, , "varr"]+SCORES[e_i, , "MAE"]) <limit)
  limit  = sort(SCORES[e_i, , "RMSE"])[nb]
  idx = which((SCORES[e_i, , "RMSE"])<limit)
  # limit  = sort(SCORES[e_i, , "qth_match"])[nb]
  # idx = which((SCORES[e_i, , "qth_match"])<limit & SCORES[e_i, , "maxQ"]<0.3)
  # 
  CALIB_EVTS[e_i, c("MU_min", "MU_max")] = range(muQV[idx])
  CALIB_EVTS[e_i, "MU_med"] = quantile(muQV[idx],0.5)
  CALIB_EVTS[e_i, c("BETA_min", "BETA_max")] = range(betaQV[idx])
  CALIB_EVTS[e_i, "BETA_med"] = quantile(betaQV[idx],0.5)
  CALIB_EVTS[e_i, "ESP_med"] = quantile(muQV[idx] + 0.577*betaQV[idx],0.5)
  CALIB_EVTS[e_i, c("ESP_min","ESP_max")] = range(muQV[idx] + 0.577*betaQV[idx])
  CALIB_EVTS[e_i, "STD_med"] = quantile(pi/sqrt(6)*betaQV[idx],0.5)
  CALIB_EVTS[e_i, c("STD_min", "STD_max")] = range(pi/sqrt(6)*betaQV[idx])
  CALIB_EVTS[e_i, "RMSE_med"] = quantile(SCORES[e_i,idx , "RMSE"],0.5)
  CALIB_EVTS[e_i, "corvar_med"] = quantile(SCORES[e_i,idx , "corrr"]+
                                             SCORES[e_i,idx , "varr"]+
                                             SCORES[e_i,idx , "MAE"],0.5)
  print(EVTs_k[e_i])
}


### plot calib  
plot(c(1:nrow(CALIB_EVTS)), CALIB_EVTS$MU_med, pch = 15, 
     ylim = c(min(CALIB_EVTS$MU_min),max(CALIB_EVTS$MU_max)))
segments(x0 = 1:nrow(CALIB_EVTS), x1 = 1:nrow(CALIB_EVTS),
         y0 = CALIB_EVTS$MU_min, y1 = CALIB_EVTS$MU_max)

plot(c(1:nrow(CALIB_EVTS)), CALIB_EVTS$BETA_med, pch = 15, 
     ylim = c(min(CALIB_EVTS$BETA_min),max(CALIB_EVTS$BETA_max)))
segments(x0 = 1:nrow(CALIB_EVTS), x1 = 1:nrow(CALIB_EVTS),
         y0 = CALIB_EVTS$BETA_min, y1 = CALIB_EVTS$BETA_max)

plot(c(1:nrow(CALIB_EVTS)), CALIB_EVTS$ESP_med, pch = 15,
     ylim = c(min(CALIB_EVTS$ESP_min),max(CALIB_EVTS$ESP_max)))
segments(x0 = 1:nrow(CALIB_EVTS), x1 = 1:nrow(CALIB_EVTS),
         y0 = CALIB_EVTS$ESP_min, y1 = CALIB_EVTS$ESP_max)

plot(c(1:nrow(CALIB_EVTS)), CALIB_EVTS$STD_med, pch = 15,
     ylim = c(min(CALIB_EVTS$STD_min),max(CALIB_EVTS$STD_max)))
segments(x0 = 1:nrow(CALIB_EVTS), x1 = 1:nrow(CALIB_EVTS),
         y0 = CALIB_EVTS$STD_min, y1 = CALIB_EVTS$STD_max)



rgHuM = range(STATS_EVTS[ , "Medernach", "Hum_initBot"], na.rm =T)
seqHum = seq(from = rgHuM[1], to =092, length.out = 11)
ii = as.integer((STATS_EVTS[ , "Medernach", "Hum_initBot"]-rgHuM[1])/diff(seqHum)[1])+1
couleur = rep("", length(EVTs))
couleur[ii<12 & !is.na(ii)] = brewer.pal(11,"RdYlBu")[ii[ii<12 & !is.na(ii)]]
couleur[ii>=12] = "navyblue"
couleur[is.na(ii)]= 1
ad_img = paste("/media/audrey/8858-1E31/PRO/2019/DATABASE/HYDRO/IMG/" )

png(paste(ad_img, "/transitTimeGammaMedernach22.png", sep=""),
    width= 6, height=2.7, units="in", res=400, pointsize = 8)
par(mfrow = c(1,2),  mar= c(1.5,6,1,0.5))
plot(c(rep(NA,4), CALIB_EVTSP$ESP2_med),c(1:length(EVTs)), pch =0,cex=2.05,col = "black",
     xlim = c(min(CALIB_EVTSP$ESP2_min),max(CALIB_EVTSP$ESP2_max)),
     ylim = c(length(EVTs),1), axes =F, xlab = "", ylab = "")
box()
axis(2, at  = c(1:length(EVTs)), labels = EVTs, cex = 0.75, las =1)
axis(1, at = seq(from = 2, to = 18, by = 2), labels = rep("", 9))
mtext(text =  as.character(seq(from = 2, to = 18, by = 2)), 1,
      at = seq(from = 2, to =18, by = 2),line = 0.5, adj = c(0.5,0.5),
      cex = 0.9)
segments(y0 =c(1:length(EVTs)), y1 =c(1:length(EVTs)),
         x0 = c(rep(NA,4),CALIB_EVTSP$ESP2_min), x1 = c(rep(NA,4),CALIB_EVTSP$ESP2_max))
points(c(rep(NA,4), CALIB_EVTSP$ESP2_med),c(1:length(EVTs)), pch = 15,cex=2,col = couleur)
# points(STATS_EVTS[-1 ,"Koedange" , "deltaQ50th"],c(2:length(EVTs)), 
#        pch = 16, col ="orangered")
title(main = "Median transit time [h] - P, Medernach",cex.main = 0.85)


polygon(x = c(1.8,18,18,1.8),
        y =c(0.9,0.9,4.3,4.3),
        border = NA, col = "gray90" )
text("Initial soil condition",x =2.5 , y =1.6 , adj = 0 , cex = 0.85)
colleg =  c(brewer.pal(11,"RdYlBu"), "navyblue")
seqy = seq(from = 4.1, to = 2.3, length.out = 13)
for(i in 1:length(colleg)){
  polygon(x = c(2.5,3.4,3.4,2.5),
          y =c(seqy[i], seqy[i], seqy[i+1], seqy[i+1]),
          border = NA, col = colleg[i])
}
text("100 % sat.",x = 3.9 , y =4.1-0.4 , adj = 0 , cex = 0.8)
text("44 % sat.",x =3.9 , y =2.5, adj = 0 , cex = 0.8)


rgHuM = range(STATS_EVTS[ , "Medernach", "QspePeak"], na.rm =T)
seqHum = exp(seq(from = log(rgHuM[1]-0.00001), to = log(rgHuM[2]), length.out = 9))
ii = rep(NA, length(EVTs))
for(evt_i in 1:length(EVTs)){
  if(!is.na(STATS_EVTS[evt_i , "Medernach", "QspePeak"])){
    ii[evt_i] = max(which(seqHum <= STATS_EVTS[evt_i , "Medernach", "QspePeak"]))
  }
}
couleur = rep("", length(EVTs))
couleur[ii<10 & !is.na(ii)] = brewer.pal(9,"YlOrRd")[ii[ii<10 & !is.na(ii)]]
couleur[ii>=10] = "navyblue"
couleur[is.na(ii)]= 1

plot(c(rep(NA,4), CALIB_EVTS$ESP_med),c(1:length(EVTs)), pch =0,cex=2.05,col = "black",
     xlim = c(min(CALIB_EVTS$ESP_min),max(CALIB_EVTS$ESP_max)),
     ylim = c(length(EVTs),1), axes =F, xlab = "", ylab = "")
box()
axis(2, at  = c(1:length(EVTs)), labels = EVTs, cex = 0.75, las =1)
axis(1, at = seq(from =2, to = 18, by = 2), labels = rep("", 9))
mtext(text =  as.character(seq(from = 2, to = 18, by = 2)), 1,
      at = seq(from = 2, to =18, by = 2),line = 0.5, adj = c(0.5,0.5),
      cex = 0.9)
segments(y0 =c(1:length(EVTs)), y1 =c(1:length(EVTs)),
         x0 = c(rep(NA,4),CALIB_EVTS$ESP_min), x1 = c(rep(NA,4),CALIB_EVTS$ESP_max))
points(c(rep(NA,4), CALIB_EVTS$ESP_med),c(1:length(EVTs)), pch = 15,cex=2,col = couleur)
# points(STATS_EVTS[-1 ,"Koedange" , "deltaQ50th"],c(2:length(EVTs)), 
#        pch = 16, col ="orangered")
title(main = "Median transit time [h] - Q, Medernach",cex.main = 0.85)

# legend = carre de polygone

polygon(x = c(0.8,7,7,0.8),
        y =c(0.9,0.9,4.3,4.3),
        border = NA, col = "gray90" )
text("Maximum discharge",x =1.0 , y =1.6 , adj = 0 , cex = 0.85)
colleg =  brewer.pal(9,"YlOrRd")
seqy = seq(from = 4.1, to = 2.3, length.out = 10)
for(i in 1:length(colleg)){
  polygon(x = c(1.1,1.7,1.7,1.1),
          y =c(seqy[i], seqy[i], seqy[i+1], seqy[i+1]),
          border = NA, col = colleg[i])
}
text("3.0 L/km2/s",x = 1.95 , y =4.1-0.4 , adj = 0 , cex = 0.8)
text("201 L/km2/s",x =1.95 , y =2.5, adj = 0 , cex = 0.8)

dev.off()



### plot calib HYETO
par(mfrow = c(2,2))
for(e_i  in 1:length(EVTs_k)){
  
  png(paste(ad_img, "/GammaMedernach_",EVTs[e_i+4], "1.png", sep=""),
      width= 6.4,height=3, units="in", res=400, pointsize = 8)
  par(mfrow = c(1,1))
  
  # limit  = sort(abs(SCORES[e_i, , "corrr"])+abs(SCORES[e_i, , "varr"]))[nb]
  # idx = which(abs(SCORES[e_i, , "corrr"])+abs(SCORES[e_i, , "varr"]) <limit)
  # limit  = sort(abs(SCORES[e_i, , "corrr"]))[nb]
  # idx = which(abs(SCORES[e_i, , "corrr"])<limit)
  id1 = SCORES[e_i, , "qth_match"]<0.2
  if(sum(id1)==0){
    id1 = SCORES[e_i, , "qth_match"]<0.3
  }
  limit  = sort(SCORES[e_i, id1, "RMSE"])[min(nb,sum(id1))]
  idx = which((SCORES[e_i, , "RMSE"])<limit & SCORES[e_i, , "qth_match"]<0.2)
  if(length(idx)==0){
    idx = which((SCORES[e_i, , "RMSE"])<limit & SCORES[e_i, , "qth_match"]<0.3)
    
  }
   limit  = sort(abs(SCORES[e_i, , "RMSE"]))[nb]
   idx = which(abs(SCORES[e_i, , "RMSE"])<limit)
  # 
  coloo  = colorRampPalette(c("gray10", "gray90"))(50)
  coloo1  = colorRampPalette(c("chartreuse1", "green4"))(50)
  
  evt_c = EVTs_k[e_i]
  evt_i = which(DATE_EVTS$EVTs == evt_c) 
  print(evt_c)
  print(evt_i)
  id_rain = which(Qmedernach$date <= DATE_EVTS$FIN_Q[evt_i] & Qmedernach$date >= DATE_EVTS$DEB_P[evt_i] )
  id_rain2 = which(Qheffingen$date <= DATE_EVTS$FIN_Q[evt_i] & Qheffingen$date >= DATE_EVTS$DEB_P[evt_i] )
  plot(1:length(id_rain), Qmedernach$date[id_rain], type = "l", 
       ylim = c(0, 1.3*max(Qmedernach$Qout[id_rain], na.rm =T))) 
  title(main = evt_c)
  for(i in 1:length(idx)){
    
    lines(1:length(id_rain),Pout[id_rain,idx][,i], col =coloo[i] )
    lines(1:length(id_rain),PQout[id_rain,idx][,i], col =coloo1[i] )
  }
  lines(1:length(id_rain), Qmedernach$Qout[id_rain], lwd = 2, col = "red3")
  lines(1:length(id_rain2), Qheffingen$Qheffingen[id_rain2]*12, lwd = 2, col = "pink4")
  par(new = T)
  plot(1:length(id_rain), Medernach_Essai[[1]]$Pluie[id_rain]*12/31, type = "h", 
       ylim = c(5,0), axes = F, col = "royalblue", xlab ="", ylab="") 
 dev.off()
}


############################
save.image("/media/audrey/8858-1E31/PRO/2019/test_dispositifs/EBgather/GammaMedernach21.rda")

write.table(CALIB_EVTS, paste(adout, "Gamma_parameterQMedernach21.csv", sep =""),row.names = F,
            sep =";")
write.table(CALIB_EVTSP, paste(adout, "Gamma_parameterPMedernach21.csv", sep =""),row.names = F,
            sep =";")

#########################################
# save project R

plot(data.frame(SCORES[18, , ],betaPV = betaPV ,muPV= muPV))


plot(data.frame(RMSE = RMSE,
                cor = corrr, var = varrr, beta = betaV, mu = muV ))
idx = RMSE <0.2 & corrr < 0.2
sum(idx)
plot(1:nrow(Qkoedange), Qkoedange$Qout, type = "l") 
for(i in 1:sum(idx)){
  
  lines(KOedange_Essai[[2]][,idx][,i], col = i)
}

par(mfrow = c(2,2))
for(evt_c in EVTs_k){
  
  evt_i = which(EVTs== evt_c)
  print(evt_c)
  id_rain = which(Qkoedange$Date <= DATE_EVTS$FIN_Q[evt_i] & Qkoedange$Date >= DATE_EVTS$DEB_P[evt_i] )
  
  plot(1:length(id_rain), Qkoedange$Qout[id_rain], type = "l") 
  title(main = evt_c)
  for(i in 1:sum(idx)){
    
    lines(KOedange_Essai[[2]][id_rain,idx][,i], col = i)
  }
}

# RMSE = vector()
# corrr =  vector()
# varrr =  vector()
# for(i in 1:length(betaV)){
#   RMSE[i]= mean((KOedange_Essai[[2]][, i]-Qkoedange$Qout)*(KOedange_Essai[[2]][, i]-Qkoedange$Qout),na.rm=T)
#   corrr[i] =  (1-cor(KOedange_Essai[[2]][!is.na(Qkoedange$Qout), i],Qkoedange$Qout[!is.na(Qkoedange$Qout)]))^2
#   varrr[i] =  (1 - var(KOedange_Essai[[2]][!is.na(Qkoedange$Qout), i])*mean(Qkoedange$Qout, na.rm =T)/var(Qkoedange$Qout, na.rm =T)/mean(KOedange_Essai[[2]][!is.na(Qkoedange$Qout), i]))^2
# 
# }


####

X = c(0:1000)/20
Y = gumbel_law(X, 0.1,2)
plot(X, Y, type ="l")

