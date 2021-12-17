
rm(list =ls())
library(pracma)
library(plotrix)
library("RSAGA", lib.loc="~/R/x86_64-pc-linux-gnu-library/3.4")

################################################################################################################

ad = "/media/audrey/8858-1E31/PRO/2019/test_dispositifs/EBgather/"
DATE_EVTS  = read.table(paste(ad, "range_evts2022bis.csv",sep =""), header = T,
                        stringsAsFactors = F, sep =";")
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
adout = "/media/audrey/8858-1E31/PRO/2019/test_dispositifs/EBgather/"
load(file = paste(adout, "stats_evts2022.rda", sep = ""))

EVTs = DATE_EVTS$EVTs

# correct avec CR
for(evt_i in 1:length(EVTs)){
  
  id_rain = which(RainEVTs$date <= DATE_EVTS$FIN_Q[evt_i] & RainEVTs$date >= DATE_EVTS$DEB_P[evt_i] )
  RainEVTs$Koedange[id_rain] = RainEVTs$Koedange[id_rain] * STATS_EVTS[evt_i, "Koedange","CR"]/100
}

#######################"

####################################################
####################################################
# gumbel function
gumbel_law = function(x,mu,beta){
  
  d = x
  idx = x >0
  d[!idx] = 0
  z = exp(-(d[idx]-mu)/beta)
  d[idx] = exp(-z)*z/beta
  d[idx] = d[idx]/(1-exp(-exp(mu/beta)))
  return(d)
}


gamma_law = function(x,alpha,theta){
  
  d = x
  idx = x >0
  d[!idx] = 0
  d[idx] = dgamma(d[idx],alpha, scale = theta, log = FALSE)
  
  return(d)
}

apply_GammaTransfert = function(date, pluie, mu_v, beta_v){
  
  DT = c(as.numeric(date) - min(as.numeric(date)))/3600
  L = length(date)
  
  MAT_IT = matrix(-1, nrow = L, ncol = L)
  for(t in 1:L){
    MAT_IT[t:L,t] = DT[1:length(t:L)]
  }
  diag(MAT_IT)= 150/3600
  
  MAT_Pvol = matrix(NA, nrow =L, ncol = L)
  for(i in 1:L){
    MAT_Pvol[i,] = pluie 
  }
  
  MAT_OUT = matrix(NA, nrow = L, ncol = length(beta_v))
  
  for(mk in 1:length(beta_v)){
    
    #GAMMA_IT = dgamma(MAT_IT, alpha[mk], scale =beta[mk])
    GAMMA_IT = gamma_law(MAT_IT,mu_v[mk],beta_v[mk])
    VC = MAT_Pvol * GAMMA_IT 
    OUT_sout = apply(VC ,1, sum)
    MAT_OUT[,mk] =  OUT_sout
    
  }
  
  return(MAT_OUT)
}

apply_GammaStation = function(TABLE, station_c, EVTs_sel,table_range, mu_v, beta_v){
  
  MAT_OUT_TOT = matrix(NA, nrow = 1, ncol = length(beta_v))
  
  DATA_RUNS = data.frame(Date = NA, Pluie = NA)
  
  for(evt_c in EVTs_sel){
    
    evt_i = which(table_range$EVTs== evt_c)
    print(evt_c)
    id_rain = which(TABLE$date <= table_range$FIN_Q[evt_i] & TABLE$date >= table_range$DEB_P[evt_i] )
    
    pluie = TABLE[id_rain,station_c] 
    if(sum(is.na(pluie))>10){
      "becarefull too much na in rainfall"
    }
    pluie[is.na(pluie)] = 0
    
    date = as.numeric(TABLE[id_rain,"date"]) - min(as.numeric( TABLE[id_rain,"date"]))
    
    date_runs_tmp = data.frame(Date = TABLE[id_rain,"date"], Pluie = pluie)
    DATA_RUNS = rbind(DATA_RUNS, date_runs_tmp)
    
    OUT_tmp = apply_GammaTransfert(date, pluie, mu_v, beta_v)
    
    MAT_OUT_TOT = rbind(MAT_OUT_TOT, OUT_tmp)
    
  }
  return(list(DATA_RUNS[-1,], MAT_OUT_TOT[-1,]))
}

extract_RUNOFF = function(tab_data_run, tab_runoff , station_c, area_c  ){
  
  datess = as.POSIXct(as.numeric(tab_data_run$Date), tz = "UTC", origin = "01-01-1970", format = "%d-%m-%Y")
  
  tab_tmp = tab_runoff[!is.na(paste("Qmed_",station_c ,sep ="")), ] 
  idxr  = tab_tmp$date %in% datess
  idxd   =   datess %in% tab_tmp$date
  tab_data_run$Date = datess
  
  
  tab_data_run$Qout = NA
  tab_data_run$Qout[idxd] = tab_tmp[idxr, paste("Qmed_",station_c ,sep ="")] 
  tab_data_run$Qout[idxd] = tab_data_run$Qout[idxd] *5*60*12 / area_c / 1000  
  
  return(tab_data_run)
}

station = "Koedange"
EVTs_k = DATE_EVTS$EVTs[1:nrow(DATE_EVTS)]
thetaV = runif(1000,0.1,15)
alphaV = runif(1000,1,18)

KOedange_Essai = apply_GammaStation(RainEVTs, station, EVTs_k,DATE_EVTS, alphaV, thetaV)
Qkoedange = extract_RUNOFF(KOedange_Essai[[1]], TAB_runoff,  station, areas[1] )

####

SCORES = array(dim=c(length(EVTs_k), length(alphaV),6),
               dimnames = list(EVTs_k, NULL, c("RMSE", "corrr", "varr", "MAE", "Nash", "Res")))
for(e_i  in 1:length(EVTs_k)){
  
  evt_i = which(EVTs== EVTs_k[e_i])
  print(EVTs_k[e_i])
  id_rain = which(Qkoedange$Date <= DATE_EVTS$FIN_Q[evt_i] & Qkoedange$Date >= DATE_EVTS$DEB_P[evt_i] )
  
  for(i in 1:length(alphaV)){
    
    l = !is.na(Qkoedange$Qout[id_rain])
    SCORES[e_i, i, "Res"]= sqrt(mean((KOedange_Essai[[2]][id_rain, i]-Qkoedange$Qout[id_rain])*(KOedange_Essai[[2]][id_rain, i]-Qkoedange$Qout[id_rain]),na.rm=T))
    SCORES[e_i, i, "RMSE"]= sqrt(mean((KOedange_Essai[[2]][id_rain, i]-Qkoedange$Qout[id_rain])*(KOedange_Essai[[2]][id_rain, i]-Qkoedange$Qout[id_rain]),na.rm=T))/mean(Qkoedange$Qout[id_rain],na.rm=T)
    SCORES[e_i, i, "corrr"] =  abs(1-cor(KOedange_Essai[[2]][id_rain, i][ l],Qkoedange$Qout[id_rain][ l]))
    SCORES[e_i, i, "varr"] =  abs(1 - var(KOedange_Essai[[2]][ id_rain, i][l])*mean(Qkoedange$Qout[id_rain], na.rm =T)/var(Qkoedange$Qout[id_rain], na.rm =T)/mean(KOedange_Essai[[2]][id_rain, i][l]))
    SCORES[e_i, i, "MAE"] =  abs(1 - mean(KOedange_Essai[[2]][ id_rain, i][l])/mean(Qkoedange$Qout[id_rain], na.rm =T))
    SCORES[e_i, i, "Nash"] = mean((KOedange_Essai[[2]][id_rain, i]-Qkoedange$Qout[id_rain])*(KOedange_Essai[[2]][id_rain, i]-Qkoedange$Qout[id_rain]),na.rm=T)/
      mean((Qkoedange$Qout[id_rain]-mean(Qkoedange$Qout[id_rain],na.rm=T))*(Qkoedange$Qout[id_rain]-mean(Qkoedange$Qout[id_rain],na.rm=T)), na.rm =T)
  }
  
}


### calibration par evt
# 100 meilleures var + cor 
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
  idx = which((SCORES[e_i, , "RMSE"])<=limit)
  
  CALIB_EVTS[e_i, c("MU_min", "MU_max")] = range(alphaV[idx])
  CALIB_EVTS[e_i, "MU_med"] = quantile(alphaV[idx],0.5)
  CALIB_EVTS[e_i, c("BETA_min", "BETA_max")] = range(thetaV[idx])
  CALIB_EVTS[e_i, "BETA_med"] = quantile(thetaV[idx],0.5)
  CALIB_EVTS[e_i, "ESP_med"] = quantile(alphaV[idx] *thetaV[idx],0.5)
  CALIB_EVTS[e_i, c("ESP_min","ESP_max")] = range(alphaV[idx] * thetaV[idx])
  CALIB_EVTS[e_i, "STD_med"] = quantile(sqrt(alphaV[idx] * thetaV[idx]*thetaV[idx]),0.5)
  CALIB_EVTS[e_i, c("STD_min", "STD_max")] = range(sqrt(alphaV[idx] * thetaV[idx]*thetaV[idx]))
  CALIB_EVTS[e_i, "RMSE_med"] = quantile(SCORES[e_i,idx , "RMSE"],0.5)
  CALIB_EVTS[e_i, "corvar_med"] = quantile(SCORES[e_i,idx , "corrr"]+
                                             SCORES[e_i,idx , "varr"]+
                                             SCORES[e_i,idx , "MAE"],0.5)
  print(EVTs_k[e_i])
}

#### calcul other T50th
T50th = matrix(nrow = length(EVTs_k), ncol = nb)
for(e_i in 1:length(EVTs_k)){
  
  limit  = sort(SCORES[e_i, , "RMSE"])[nb]
  idx = which((SCORES[e_i, , "RMSE"])<=limit)
  
  X = c(0:500)/10
  
  for(n_i in 1:length(idx)){
    
    mui = alphaV[idx][n_i]
    betai = thetaV[idx][n_i]
    Y = gamma_law(X, mui,betai)
    ttmp = X[min(which(cumsum(Y)/10>0.5))]
    T50th[e_i, n_i] = ttmp
  }
  
}


CALIB_EVTS$ESP2_min = apply(T50th,1,min)
CALIB_EVTS$ESP2_max = apply(T50th,1,max)
CALIB_EVTS$ESP2_med = apply(T50th,1,quantile,0.5)

### plot calib 
par(mfrow = c(2,2), mar = c(2,2 ,2,2))
plot(c(1:nrow(CALIB_EVTS)), CALIB_EVTS$MU_med, pch = 15, 
     ylim = c(min(CALIB_EVTS$MU_min),max(CALIB_EVTS$MU_max)))
segments(x0 = 1:nrow(CALIB_EVTS), x1 = 1:nrow(CALIB_EVTS),
         y0 = CALIB_EVTS$MU_min, y1 = CALIB_EVTS$MU_max)
points(c(1:nrow(CALIB_EVTS)), STATS_EVTS[-1 ,"Koedange" , "deltaQpeak"],
       pch = 16, col ="orangered")

plot(c(1:nrow(CALIB_EVTS)), CALIB_EVTS$BETA_med, pch = 15, 
     ylim = c(min(CALIB_EVTS$BETA_min),max(CALIB_EVTS$BETA_max)))
segments(x0 = 1:nrow(CALIB_EVTS), x1 = 1:nrow(CALIB_EVTS),
         y0 = CALIB_EVTS$BETA_min, y1 = CALIB_EVTS$BETA_max)

plot(c(1:nrow(CALIB_EVTS)), CALIB_EVTS$ESP2_med, pch = 15,
     ylim = c(min(CALIB_EVTS$ESP2_min),max(CALIB_EVTS$ESP2_max)))
segments(x0 = 1:nrow(CALIB_EVTS), x1 = 1:nrow(CALIB_EVTS),
         y0 = CALIB_EVTS$ESP2_min, y1 = CALIB_EVTS$ESP2_max)
points(c(1:nrow(CALIB_EVTS)), STATS_EVTS[-1 ,"Koedange" , "deltaQ50th"],
       pch = 16, col ="orangered")

plot(c(1:nrow(CALIB_EVTS)), CALIB_EVTS$STD_med, pch = 15,
     ylim = c(min(CALIB_EVTS$STD_min),max(CALIB_EVTS$STD_max)))
segments(x0 = 1:nrow(CALIB_EVTS), x1 = 1:nrow(CALIB_EVTS),
         y0 = CALIB_EVTS$STD_min, y1 = CALIB_EVTS$STD_max)


# figure paper
rgHuM = range(STATS_EVTS[ , "Koedange", "Hum_initTop"], na.rm =T)
seqHum = seq(from = rgHuM[1], to = 97, length.out = 11)
ii = as.integer((STATS_EVTS[ , "Koedange", "Hum_initTop"]-rgHuM[1])/diff(seqHum)[1])+1

couleur = rep("", length(EVTs))
couleur[ii<12] = brewer.pal(11,"RdYlBu")[ii[ii<12 & !is.na(ii)]]
couleur[ii>=12] = "navyblue"
ad_img = paste("/media/audrey/8858-1E31/PRO/2019/test_dispositifs/EBgather/IMG/" )

png(paste(ad_img, "/transitTimeGammaKoedange21.png", sep=""),
    width= 3, height=2.7, units="in", res=400, pointsize = 8)
par(mfrow = c(1,1),  mar= c(1.5,6,1,0.5))
plot(c( CALIB_EVTS$ESP2_med),c(1:length(EVTs)), pch =0,cex=2.05,col = "black",
     xlim = c(min(CALIB_EVTS$ESP2_min),max(CALIB_EVTS$ESP2_max)),
     ylim = c(length(EVTs),1), axes =F, xlab = "", ylab = "")
box()
axis(2, at  = c(1:length(EVTs)), labels = EVTs, cex = 0.75, las =1)
axis(1, at = seq(from = 6, to =18, by = 2), labels = rep("", 7))
mtext(text =  as.character(seq(from = 6, to = 18, by = 2)), 1,
     at = seq(from = 6, to = 18, by = 2),line = 0.5, adj = c(0.5,0.5),
      cex = 0.9)
segments(y0 =c(1:length(EVTs)), y1 =c(1:length(EVTs)),
         x0 = c(NA,CALIB_EVTS$ESP2_min), x1 = c(NA,CALIB_EVTS$ESP2_max))
points(c( CALIB_EVTS$ESP2_med),c(1:length(EVTs)), pch = 15,cex=2,col = couleur)
# points(STATS_EVTS[-1 ,"Koedange" , "deltaQ50th"],c(2:length(EVTs)), 
#        pch = 16, col ="orangered")
title(main = "Median transit time [h] - Koedange",cex.main = 0.85)

# legend = carre de polygone

polygon(x = c(4.5,9.,9.,4.5),
        y =c(1,1,6, 6),
        border = NA, col = "gray90" )
text("Initial ",x =6.0 , y =1.5 , adj = 0 , cex = 0.8)
text("soil condition",x =5.0 , y =2.15 , adj = 0 , cex = 0.8)
colleg =  c(brewer.pal(11,"RdYlBu"), "navyblue")
seqy = seq(from = 5.8, to = 5.8-3, length.out = 13)
for(i in 1:length(colleg)){
  
  polygon(x = c(4.8,5.5,5.5,4.8),
          y =c(seqy[i], seqy[i], seqy[i+1], seqy[i+1]),
          border = NA, col = colleg[i])
}
text("100 % sat.",x =5.8 , y =2.8+0.3 , adj = 0 , cex = 0.8)
text("67 % sat.",x =5.8 , y =5.8-0.3, adj = 0 , cex = 0.8)

dev.off()

####  perfect calcul of T50th

X = c(0:200)/5
Y = gumbel_law(X, 2,5)
t50th = 2+0.577*5
cumsum(Y)/5


########
png(paste(ad_img, "/KEYCOLtransitTimeKoedange.png", sep=""),
    width= 0.3, height=1.5, units="in", res=400, pointsize = 8)
par(mfrow =c(1,1), mar=c(0,0,0,0), oma =c(0,0,0,0))
plot(c(-10,-10), xlim =c(0,1),ylim= c(0,1) , col = 1 , pch = 16,
     axes =F, xlab ='', ylab ='')
#the labels of the legend

m  =  c(round(seqHum*100 ,digits=0),97,100)
m = c(seq(from = 45,to = 95, by = 5),97)
col.labels <- m
color.legend( xl =0 , yb = 0, xr = 0.3, yt = 1 , # the coordinates
              legend = col.labels , gradient="y", cex=0.95,
              rect.col=c(brewer.pal(11,"RdYlBu"), "navyblue"), align="rb" )
dev.off()

### plot calib HYETO
par(mfrow = c(2,2), mar = c(4,2,2,2))
for(e_i  in 1:length(EVTs_k)){
  
  png(paste(ad_img, "/tGumbelKoedange_", EVTs_k[e_i],"21.png", sep=""),
       width= 4, height=4.5, units="in", res=400, pointsize = 8)
  limit  = sort(SCORES[e_i, , "corrr"]+SCORES[e_i, , "varr"]+SCORES[e_i, , "MAE"])[nb]
  idx = which((SCORES[e_i, , "corrr"]+SCORES[e_i, , "varr"]+SCORES[e_i, , "MAE"]) <limit)
  limit  = sort(SCORES[e_i, , "RMSE"])[nb]
  idx = which((SCORES[e_i, , "RMSE"])<limit)
  
  coloo  = colorRampPalette(c("gray10", "gray90"))(50)
  evt_c = EVTs_k[e_i]
  evt_i = which(DATE_EVTS$EVTs == evt_c) 
  print(evt_c)
  print(evt_i)
  id_rain = which(Qkoedange$Date <= DATE_EVTS$FIN_Q[evt_i] & Qkoedange$Date >= DATE_EVTS$DEB_P[evt_i] )
  plot(1:length(id_rain), Qkoedange$Qout[id_rain], type = "l", 
       ylim = c(0, 1.3*max(Qkoedange$Qout[id_rain], na.rm =T))) 
  title(main = evt_c)
  for(i in 1:length(idx)){
    
    lines(1:length(id_rain),KOedange_Essai[[2]][id_rain,idx][,i], col =coloo[i] )
  }
  lines(1:length(id_rain), Qkoedange$Qout[id_rain], lwd = 2, col = "red3")
  par(new = T)
  plot(1:length(id_rain), Qkoedange$Pluie[id_rain]*12*2, type = "h", 
       ylim = c(10,0), axes = F, col = "royalblue", xlab ="", ylab="") 
  
  dev.off()
}



### plot calib gumbel
e_i = 1
Thour = c(1 : 30)
limit  = sort(SCORES[e_i, , "RMSE"])[1]
idx = which((SCORES[e_i, , "RMSE"])<=limit)

coloo  = colorRampPalette(c("gray10", "gray90"))(50)
plot(Thour, gumbel_law(Thour,muV[idx],betaV[idx]), type = "l", ylim = c(0,0.25))
for(e_i  in 2:length(EVTs_k)){
  
  limit  = sort(SCORES[e_i, , "RMSE"])[1]
  idx = which((SCORES[e_i, , "RMSE"])<=limit)
  
  coloo  = colorRampPalette(c("gray10", "gray90"))(50)
  lines(Thour, gumbel_law(Thour,muV[idx],betaV[idx]), type = "l", col =e_i)
  
}




############################
save.image("/media/audrey/8858-1E31/PRO/2019/test_dispositifs/EBgather/GumbelKoedange21.rda")

write.table(CALIB_EVTS, paste(adout, "Gumbel_parameter_Koedange21.csv", sep =""),row.names = F,
            sep =";")

CALIB_EVTS = read.table(paste(adout, "Gumbel_parameter_Koedange21.csv", sep =""),header = T,
           sep =";", stringsAsFactors = F)

##############################################
# essai 2 gumbel funciton pour vet de octobre novembre

apply_DoubleGumbelStation = function(TABLE, station_c, EVTs_sel,table_range, muMARLS_v, betaMARLS_v,
                                     muGRES_v, betaGRES_v, alpha_v){
  
  MAT_OUT_TOT = matrix(NA, nrow = 1, ncol = length(betaMARLS_v))
  
  DATA_RUNS = data.frame(Date = NA, Pluie = NA)
  
  for(evt_c in EVTs_sel){
    
    evt_i = which(table_range$EVTs== evt_c)
    print(evt_c)
    id_rain = which(TABLE$date <= table_range$FIN_Q[evt_i] & TABLE$date >= table_range$DEB_P[evt_i] )
    
    pluie = TABLE[id_rain,station_c] 
    if(sum(is.na(pluie))>10){
      "becarefull too much na in rainfall"
    }
    pluie[is.na(pluie)] = 0
    
    date = as.numeric(TABLE[id_rain,"date"]) - min(as.numeric( TABLE[id_rain,"date"]))
    
    date_runs_tmp = data.frame(Date = TABLE[id_rain,"date"], Pluie = pluie)
    DATA_RUNS = rbind(DATA_RUNS, date_runs_tmp)
    
    OUT_tmp = apply_DoubleGumbelTransfert(date, pluie, muMARLS_v, betaMARLS_v,
                                          muGRES_v, betaGRES_v, alpha_v)
    
    MAT_OUT_TOT = rbind(MAT_OUT_TOT, OUT_tmp)
    
  }
  return(list(DATA_RUNS[-1,], MAT_OUT_TOT[-1,]))
}

apply_DoubleGumbelTransfert = function(date, pluie, muMARLS_v, betaMARLS_v,
                                       muGRES_v, betaGRES_v, alpha_v){
  
  DT = c(as.numeric(date) - min(as.numeric(date)))/3600
  L = length(date)
  
  MAT_IT = matrix(-1, nrow = L, ncol = L)
  for(t in 1:L){
    MAT_IT[t:L,t] = DT[1:length(t:L)]
  }
  diag(MAT_IT)= 150/3600
  
  MAT_Pvol = matrix(NA, nrow =L, ncol = L)
  for(i in 1:L){
    MAT_Pvol[i,] = pluie 
  }
  
  MAT_OUT = matrix(NA, nrow = L, ncol = length(betaMARLS_v))
  
  for(mk in 1:length(betaMARLS_v)){
    
    #GAMMA_IT = dgamma(MAT_IT, alpha[mk], scale =beta[mk])
    GAMMAmarls_IT = gumbel_law(MAT_IT,muMARLS_v[mk],betaMARLS_v[mk])
    GAMMAgres_IT = gumbel_law(MAT_IT,muGRES_v[mk],betaGRES_v[mk])
    VC = alpha_v[mk]* MAT_Pvol *  GAMMAmarls_IT + (1- alpha_v[mk])* MAT_Pvol *  GAMMAgres_IT 
    OUT_sout = apply(VC ,1, sum)
    MAT_OUT[,mk] =  OUT_sout
    
  }
  
  return(MAT_OUT)
}


station = "Koedange"
EVTs_k = DATE_EVTS$EVTs[7:10]
betaMARLSV = runif(1000,1.,10)
muMARLSV = runif(1000,2.6,13)
betaGRESV = vector()
muGRESV = vector()
for(i in 1:length(muMARLSV )){
  betaGRESV[i] = runif(1, betaMARLSV[i],25)
  muGRESV[i] = runif(1, muMARLSV[i], 30)
  
  
}
alphaV = runif(1000,0.4,0.8)

PARAMs = data.frame(muMARLS = muMARLSV, betaMARLS = betaMARLSV,
                    muGRES = muGRESV, betaGRES = betaGRESV, alpha = alphaV)
KOedange_Double = apply_DoubleGumbelStation(RainEVTs, station, EVTs_k,DATE_EVTS, muMARLSV, betaMARLSV,
                                     muGRESV, betaGRESV, alphaV)
QKoedange_Double = extract_RUNOFF(KOedange_Double[[1]], TAB_runoff,  station, areas[1] )

SCORESdouble = array(dim=c(length(EVTs_k), length(betaGRESV),4),
                           dimnames = list(EVTs_k, NULL, c("RMSE", "corrr", "varr", "MAE")))
for(e_i  in 1:length(EVTs_k)){
  
  evt_i = which(EVTs== EVTs_k[e_i])
  print(EVTs_k[e_i])
  id_rain = which(QKoedange_Double$Date <= DATE_EVTS$FIN_Q[evt_i] & QKoedange_Double$Date >= DATE_EVTS$DEB_P[evt_i] )
  
  for(i in 1:length(betaGRESV)){
    
    l = !is.na(QKoedange_Double$Qout[id_rain])
    SCORESdouble[e_i, i, "RMSE"]= mean((KOedange_Double[[2]][id_rain, i]-QKoedange_Double$Qout[id_rain])*(KOedange_Double[[2]][id_rain, i]-QKoedange_Double$Qout[id_rain]),na.rm=T)
    SCORESdouble[e_i, i, "corrr"] =  abs(1-cor(KOedange_Double[[2]][id_rain, i][ l],QKoedange_Double$Qout[id_rain][ l]))
    SCORESdouble[e_i, i, "varr"] =  abs(1 - var(KOedange_Double[[2]][ id_rain, i][l])*mean(QKoedange_Double$Qout[id_rain], na.rm =T)/var(QKoedange_Double$Qout[id_rain], na.rm =T)/mean(KOedange_Double[[2]][id_rain, i][l]))
    SCORESdouble[e_i, i, "MAE"] =  abs(1 - mean(KOedange_Double[[2]][ id_rain, i][l])/mean(QKoedange_Double$Qout[id_rain], na.rm =T))
    
  }
  
}



### calibration par evt
# 100 meilleures var + cor 
nb = 30
CALIB_EVTS_double = data.frame(EVTS = EVTs_k, MUmarls_min = NA, MUmarls_max = NA,
                        MUmarls_med = NA, BETAmarls_min  = NA, BETAmarls_max  = NA,
                        BETAmarls_med  = NA, ESPmarls_min = NA, ESPmarls_max = NA,
                        ESPmarls_med = NA, STDmarls_min = NA, STDmarls_med = NA, STDmarls_max = NA,
                        MUgres_min = NA, MUgres_max = NA,
                        MUgres_med = NA, BETAgres_min  = NA, BETAgres_max  = NA,
                        BETAgres_med  = NA, ESPgres_min = NA, ESPgres_max = NA,
                        ESPgres_med = NA, STDgres_min = NA, STDgres_med = NA, STDgres_max = NA,
                        ALPHA_min  = NA, ALPHA_max  = NA, ALPHA_med  = NA,
                        RMSE_med = NA, corvar_med = NA)

for(e_i  in 1:length(EVTs_k)){
  
  limit  = sort(SCORESdouble[e_i, , "corrr"]+SCORESdouble[e_i, , "varr"]+SCORESdouble[e_i, , "MAE"])[nb]
  idx = which((SCORESdouble[e_i, , "corrr"]+SCORESdouble[e_i, , "varr"]+SCORESdouble[e_i, , "MAE"]) <limit)
  limit  = sort(SCORESdouble[e_i, , "RMSE"])[nb]
  idx = which((SCORESdouble[e_i, , "RMSE"])<limit)
  
  #plot(PARAMs[idx,])
  CALIB_EVTS_double[e_i, c("MUgres_min", "MUgres_max")] = range(muGRESV[idx])
  CALIB_EVTS_double[e_i, "MUgres_med"] = quantile(muGRESV[idx],0.5)
  CALIB_EVTS_double[e_i, c("BETAgres_min", "BETAgres_max")] = range(betaGRESV[idx])
  CALIB_EVTS_double[e_i, "BETAgres_med"] = quantile(betaGRESV[idx],0.5)
  CALIB_EVTS_double[e_i, "ESPgres_med"] = quantile(muGRESV[idx] + 0.577*betaGRESV[idx],0.5)
  CALIB_EVTS_double[e_i, c("ESPgres_min","ESPgres_max")] = range(muGRESV[idx] + 0.577*betaGRESV[idx])
  CALIB_EVTS_double[e_i, "STDgres_med"] = quantile(pi/sqrt(6)*betaGRESV[idx],0.5)
  CALIB_EVTS_double[e_i, c("STDgres_min", "STDgres_max")] = range(pi/sqrt(6)*betaGRESV[idx])
  
  CALIB_EVTS_double[e_i, c("MUmarls_min", "MUmarls_max")] = range(muMARLSV[idx])
  CALIB_EVTS_double[e_i, "MUmarls_med"] = quantile(muMARLSV[idx],0.5)
  CALIB_EVTS_double[e_i, c("BETAmarls_min", "BETAmarls_max")] = range(betaMARLSV[idx])
  CALIB_EVTS_double[e_i, "BETAmarls_med"] = quantile(betaMARLSV[idx],0.5)
  CALIB_EVTS_double[e_i, "ESPmarls_med"] = quantile(muMARLSV[idx] + 0.577*betaMARLSV[idx],0.5)
  CALIB_EVTS_double[e_i, c("ESPmarls_min","ESPmarls_max")] = range(muMARLSV[idx] + 0.577*betaMARLSV[idx])
  CALIB_EVTS_double[e_i, "STDmarls_med"] = quantile(pi/sqrt(6)*betaMARLSV[idx],0.5)
  CALIB_EVTS_double[e_i, c("STDmarls_min", "STDmarls_max")] = range(pi/sqrt(6)*betaMARLSV[idx])
  
  CALIB_EVTS_double[e_i, "ALPHA_med"] = quantile(alphaV[idx],0.5)
  CALIB_EVTS_double[e_i, c("ALPHA_min", "ALPHA_max")] = range(alphaV[idx])
  
  CALIB_EVTS_double[e_i, "RMSE_med"] = quantile(SCORESdouble[e_i,idx , "RMSE"],0.5)
  CALIB_EVTS_double[e_i, "corvar_med"] = quantile(SCORESdouble[e_i,idx , "corrr"]+
                                             SCORESdouble[e_i,idx , "varr"]+
                                             SCORESdouble[e_i,idx , "MAE"],0.5)
  print(EVTs_k[e_i])
}




write.table(CALIB_EVTS_double, paste(adout, "Koedange_double_Gumbel_parameter.csv", sep =""),row.names = F,
            sep =";")

