rm(list =ls())
library(pracma)
library(plotrix)
library(RSAGA)
###

ad = "/media/audrey/8858-1E31/PRO/2019/test_dispositifs/EBgather/"
DATE_EVTS  = read.table(paste(ad, "range_evts2.csv",sep =""), header = T,
                        stringsAsFactors = F, sep =";")
EVTs = DATE_EVTS$EVTs

adout = "/media/audrey/8858-1E31/PRO/2019/test_dispositifs/EBgather/IMG/"

## info
stations = c("Koedange","Heffingen", "Larochette", "Medernach", "Hessemillen", "Reisdorf")
id_station = c(16,15,14,13,12,3)
coleurs = rev(c("black", "darkcyan", "olivedrab3","navyblue", "pink3", "orangered"))
areas =  c(31.14,48.687, 69.41,79.04,92.981, 100.6)
distance_brut = list()


load("/media/audrey/8858-1E31/PRO/2019/test_dispositifs/EBgather/GammaMedernach21.rda")
SCORES_M = SCORES
Params_M = data.frame(alphaPV = alphaPV, betaQV = betaQV, thetaPV = thetaPV, muQV = muQV )

load("/media/audrey/8858-1E31/PRO/2019/test_dispositifs/EBgather/GumbelKoedange21.rda")
SCORES_K = SCORES
Params_K = data.frame(alphaPV = alphaV, thetaPV = thetaV)

EVTs_k = DATE_EVTS$EVTs[1:nrow(DATE_EVTS)]
nb = 50

load("/media/audrey/8858-1E31/PRO/2019/test_dispositifs/EBgather/stats_evts2022.rda")

##########################################
##########################################
quantile_score = array(dim = c(2,length(EVTs), 3, 4),
                       dimnames = list(c("Koedange", "Medernach"),
                                       EVTs,c("RMSE", "NASH", "MAE"), c("q05", "q50", "q95", "nb")))

for(e_i in 1:length(EVTs)){
  
  nb = 50
  limit  = sort(SCORES_K[e_i, , "RMSE"])[nb]
  idx = which((SCORES_K[e_i, , "RMSE"])<=limit)
  quantile_score[1,e_i,1,1:3] = quantile(SCORES_K[e_i,idx, "RMSE"], c(0.05,0.5,0.95))
  if(quantile_score[1,e_i,1,3]-quantile_score[1,e_i,1,1]<0.1){
    quantile_score[1,e_i,1,4] = 50
    quantile_score[1,e_i,1,1:3] =quantile(SCORES_K[e_i,idx, "RMSE"], c(0.05,0.5,0.95))
  }
  if(diff(quantile(SCORES_K[e_i,idx, "RMSE"], c(0.05,0.95)))>0.1){
    idx = which((SCORES_K[e_i, , "RMSE"])<=(min(SCORES_K[e_i, , "RMSE"]) + 0.1))
    quantile_score[1,e_i,1,1:3] = quantile(SCORES_K[e_i,idx, "RMSE"], c(0.05,0.5,0.95))
    quantile_score[1,e_i,1,4] = length(idx)
  }
  quantile_score[1,e_i,2,1:3] = quantile(SCORES_K[e_i,idx, "Nash"], c(0.05,0.5,0.95))
  quantile_score[1,e_i,3,1:3] = quantile(SCORES_K[e_i,idx, "Res"], c(0.05,0.5,0.95))
  
  nb = 50
  if(e_i > 4){
     limit  = sort(SCORES_M[e_i-4, , "RMSE"])[nb]
     idx = which((SCORES_M[e_i-4, , "RMSE"])<=limit)
     quantile_score[2,e_i,1,1:3] = quantile(SCORES_M[e_i-4,idx, "RMSE"], c(0.05,0.5,0.95))
  if(diff(quantile(SCORES_M[e_i-4,idx, "RMSE"], c(0.05,0.95)))<0.1){
    quantile_score[2,e_i,1,1:3] = quantile(SCORES_M[e_i-4,idx, "RMSE"], c(0.05,0.5,0.95))
    quantile_score[2,e_i,1,4] = 50
    } 
  if(diff(quantile(SCORES_M[e_i-4,idx, "RMSE"], c(0.05,0.95)))>0.1){
    
    idx = which((SCORES_M[e_i-4, , "RMSE"])<=(quantile_score[2,e_i,1,1] + 0.1))
    if(length(idx) == 0){
      nb = 10
      limit  = sort(SCORES_M[e_i-4, , "RMSE"])[nb]
      idx = which((SCORES_M[e_i-4, , "RMSE"])<=limit)
      
    }
    quantile_score[2,e_i,1,1:3] = quantile(SCORES_M[e_i-4,idx, "RMSE"], c(0.05,0.5,0.95))
    quantile_score[2,e_i,1,4] = length(idx)
    
  }
    quantile_score[2,e_i,2,1:3] =quantile(SCORES_M[e_i-4,idx, "Nash"], c(0.05,0.5,0.95))
    quantile_score[2,e_i,3,1:3] =quantile(SCORES_M[e_i-4,idx, "Res"], c(0.05,0.5,0.95))
    
  }
  
}



##########################################
##########################################
# calculate again the scores but the mean rather the q80th
EVTs_k = DATE_EVTS$EVTs[1:nrow(DATE_EVTS)]
EVTs_M = DATE_EVTS$EVTs[5:nrow(DATE_EVTS)]

SCORES_finaux = array(dim=c(length(EVTs),2, 50,5),
                      dimnames = list(EVTs,c("K", "M"),
                                      NULL,
                                      c("Res","RMSE", "RMSE_Qmax","Nash", "PDF")))

for(e_i  in 1:length(EVTs)){
  
  factor =  5*60*12 / 31.14 / 1000  
  if(EVTs[e_i] %in% EVTs_k){
    
    id_rain = which(Qkoedange$Date <= DATE_EVTS$FIN_Q[e_i] & Qkoedange$Date >= DATE_EVTS$DEB_P[e_i] )
    maxQobs = max(Qkoedange$Qout[id_rain], na.rm =T)
    
    nb = quantile_score[1,e_i,1, 4 ]
    evt_ki =  which(EVTs_k == EVTs[e_i])
    limit  = sort(SCORES_K[evt_ki, , "RMSE"])[nb]
    idx = which((SCORES_K[evt_ki, , "RMSE"])<limit)
    
    for(f in 1:length(idx)){
      
      i = idx[f]
      l = !is.na(Qkoedange$Qout[id_rain])
      SCORES_finaux[e_i,1, f, "Res"]= sqrt(mean((KOedange_Essai[[2]][id_rain, i]-Qkoedange$Qout[id_rain])*(KOedange_Essai[[2]][id_rain, i]-Qkoedange$Qout[id_rain]),na.rm=T))/factor
      SCORES_finaux[e_i,1, f, "RMSE"]= sqrt(mean((KOedange_Essai[[2]][id_rain, i]-Qkoedange$Qout[id_rain])*(KOedange_Essai[[2]][id_rain, i]-Qkoedange$Qout[id_rain]),na.rm=T))/mean(Qkoedange$Qout[id_rain],na.rm=T)
      SCORES_finaux[e_i,1, f, "RMSE_Qmax"]= sqrt(mean((KOedange_Essai[[2]][id_rain, i]-Qkoedange$Qout[id_rain])*(KOedange_Essai[[2]][id_rain, i]-Qkoedange$Qout[id_rain]),na.rm=T))/maxQobs
      SCORES_finaux[e_i,1, f, "PDF"] =   sqrt(mean((sort(KOedange_Essai[[2]][id_rain, i])-sort(Qkoedange$Qout[id_rain]))*(sort(KOedange_Essai[[2]][id_rain, i])-sort(Qkoedange$Qout[id_rain])),na.rm=T))/mean(Qkoedange$Qout[id_rain],na.rm=T)
      SCORES_finaux[e_i,1, f, "Nash"] = mean((KOedange_Essai[[2]][id_rain, i]-Qkoedange$Qout[id_rain])*(KOedange_Essai[[2]][id_rain, i]-Qkoedange$Qout[id_rain]),na.rm=T)/
        mean((Qkoedange$Qout[id_rain]-mean(Qkoedange$Qout[id_rain],na.rm=T))*(Qkoedange$Qout[id_rain]-mean(Qkoedange$Qout[id_rain],na.rm=T)), na.rm =T)
    }
  }
  
  factor =  5*60 *12 / 1000    
  
  if(EVTs[e_i] %in% EVTs_M){
    
    id_rain = which(Qmedernach$date <= DATE_EVTS$FIN_Q[e_i] & Qmedernach$date >= DATE_EVTS$DEB_P[e_i] )
    qth_obs = as.numeric(quantile(Qmedernach$Qout[id_rain], c(0.75,0.97), na.rm =T))
    maxQobs = max(Qmedernach$Qout[id_rain], na.rm =T)
    print(qth_obs)
    
    nb = quantile_score[2,e_i, 1,4 ]
    evt_ki =  which(EVTs_M == EVTs[e_i])
    limit  = sort(SCORES_M[evt_ki, , "RMSE"])[nb]
    idx = which((SCORES_M[evt_ki, , "RMSE"])<limit)
    
    for(i in 1:length(idx)){
      
      i = idx[f]
      l = !is.na(Qmedernach$Qout[id_rain])
      SCORES_finaux[e_i,2, f, "Res"]= sqrt(mean((PQout[id_rain, i]-Qmedernach$Qout[id_rain])*(PQout[id_rain, i]-Qmedernach$Qout[id_rain]),na.rm=T))/factor
      SCORES_finaux[e_i,2, f, "RMSE"] =  sqrt(mean((PQout[id_rain, i]-Qmedernach$Qout[id_rain])*(PQout[id_rain, i]-Qmedernach$Qout[id_rain]),na.rm=T))/mean(Qmedernach$Qout[id_rain],na.rm=T)
      SCORES_finaux[e_i,2, f, "RMSE_Qmax"] =  sqrt(mean((PQout[id_rain, i]-Qmedernach$Qout[id_rain])*(PQout[id_rain, i]-Qmedernach$Qout[id_rain]),na.rm=T))/maxQobs
      SCORES_finaux[e_i,2, f, "Nash"] = mean((PQout[id_rain, i]-Qmedernach$Qout[id_rain])*(PQout[id_rain, i]-Qmedernach$Qout[id_rain]),na.rm=T)/
        mean((Qmedernach$Qout[id_rain]-mean(Qmedernach$Qout[id_rain],na.rm=T))*(Qmedernach$Qout[id_rain]-mean(Qmedernach$Qout[id_rain],na.rm=T)), na.rm=T)
      SCORES_finaux[e_i,2, f, "PDF"] = sqrt(mean((sort(PQout[id_rain, i])-sort(Qmedernach$Qout[id_rain]))*(sort(PQout[id_rain, i])-sort(Qmedernach$Qout[id_rain])),na.rm=T))/mean(Qmedernach$Qout[id_rain],na.rm=T)
    }
  }
  
}


A = apply(SCORES_finaux, c(1,2,4), quantile, c(0.05,0.5,0.95), na.rm =T)

png(paste(ad_img, "/ErrorsGamma21.png", sep=""),
    width= 3, height=2.7, units="in", res=400, pointsize = 8)

par(mfrow = c(1,1),  mar= c(1.5,6,1,0.5))
plot(quantile_score[1, , 2],1:length(EVTs), pch =0,cex=2.05,col = "black",
     xlim = range(as.vector(quantile_score[ , , -4]), na.rm =T),lwd = 2,
     ylim = c(length(EVTs),1), axes =F, xlab = "", ylab = "")
box()
axis(2, at  = c(1:length(EVTs)), labels = EVTs, cex = 0.75, las =1)
axis(1, at = seq(from = 0, to = 0.6, by = 0.1), labels = rep("", 7))
mtext(text =  as.character(seq(from = 0.1, to = 0.6, by = 0.1)*100), 1,
      at = seq(from = 0.1, to = 0.6, by = 0.1),line = 0.5, adj = c(0.5,0.5),
      cex = 0.9)
segments(y0 =c(1:length(EVTs)), y1 =c(1:length(EVTs)),lwd = 1.5,
         x0 = quantile_score[1, , 1], x1 = quantile_score[1, , 3], col = "orangered4")
points(quantile_score[1, , 2],1:length(EVTs), pch = 15,cex=2,col = "orangered")


points(quantile_score[2, , 2],1:length(EVTs), pch = 0,cex=2.05,col = "black")
segments(y0 =c(1:length(EVTs)), y1 =c(1:length(EVTs)),lwd = 1.5,
         x0 = quantile_score[2, , 1], x1 = quantile_score[2, , 3], col  = "olivedrab4")
points(quantile_score[2, , 2],1:length(EVTs), pch = 15,cex=2,col = "olivedrab3")

title(main = "Modelling Error [%]",cex.main = 0.85)
dev.off()




##########################################
##########################################
# calculate again the scores but the mean rather the q80th
# EVTs_k = DATE_EVTS$EVTs[2:nrow(DATE_EVTS)]
# 
# SCORES_finaux = array(dim=c(length(EVTs), 50,3),
#                dimnames = list(EVTs, NULL, c("RMSE_V", "RMSE_P","Nash")))
# for(e_i  in 1:length(EVTs_k)){
#   
#   if(EVTs[e_i] %in% EVTs_k){
#    
#     id_rain = which(Qmedernach$date <= DATE_EVTS$FIN_Q[e_i] & Qmedernach$date >= DATE_EVTS$DEB_P[e_i] )
#     qth_obs = as.numeric(quantile(Qmedernach$Qout[id_rain], c(0.75,0.97), na.rm =T))
#     maxQobs = max(Qmedernach$Qout[id_rain], na.rm =T)
#     print(qth_obs)
#     
#     nb = quantile_score[1,e_i, 4 ]
#     evt_ki =  which(EVTs_k == EVTs[e_i])
#     limit  = sort(SCORES_K[evt_ki, , "RMSE"])[nb]
#     idx = which((SCORES_K[evt_ki, , "RMSE"])<limit)
#     
#     for(i in 1:nb){
#       
#       l = !is.na(Qmedernach$Qout[id_rain])
#       SCORES[e_i, i, "RMSE"]= sqrt(mean((PQout[id_rain, i]-Qmedernach$Qout[id_rain])*(PQout[id_rain, i]-Qmedernach$Qout[id_rain]),na.rm=T))/mean(Qmedernach$Qout[id_rain],na.rm=T)
#       SCORES[e_i, i, "MAE"] =  abs(1 - mean(PQout[ id_rain, i][l])/mean(Qmedernach$Qout[id_rain], na.rm =T))
#       SCORES[e_i, i, "Nash"] = mean((PQout[id_rain, i]-Qmedernach$Qout[id_rain])*(PQout[id_rain, i]-Qmedernach$Qout[id_rain]),na.rm=T)/
#         mean((Qmedernach$Qout[id_rain]-mean(Qmedernach$Qout[id_rain],na.rm=T))*(Qmedernach$Qout[id_rain]-mean(Qmedernach$Qout[id_rain],na.rm=T)), na.rm=T)
#       SCORES[e_i, i, "qth_match"] = sum(abs((as.numeric(quantile(PQout[id_rain, i], c(0.75,0.97), na.rm =T))-qth_obs)/qth_obs))
#       SCORES[e_i, i, "maxQ"] = abs((as.numeric(max(PQout[id_rain, i],  na.rm =T))-maxQobs)/maxQobs)
#     }
#   }
#      
# 
#   
# }
# 

ad_img = "/media/audrey/8858-1E31/PRO/2019/test_dispositifs/EBgather/IMG/"

##########################################
##########################################
SCORES = SCORES_M
for(e_i  in 1:length(EVTs_M)){
  
  png(paste(ad_img, "/GammaMedernach_",EVTs[e_i+4], "21.png", sep=""),
      width= 3,height=3, units="in", res=400, pointsize = 8)
  par(mfrow = c(1,1), mar = c(4,4,2,4))
  
  nb = quantile_score[2,e_i+4,1, 4 ]
  limit  = sort(SCORES[e_i, , "RMSE"])[nb]
  idx = which((SCORES[e_i, , "RMSE"])<limit)
  
  coloo  = colorRampPalette(c("gray10", "gray90"))(50)
  coloo1  = colorRampPalette(c("chartreuse1", "green4"))(50)
  
  evt_c = EVTs_M[e_i]
  evt_i = which(DATE_EVTS$EVTs == evt_c) 
  print(evt_c)
  print(evt_i)
  id_rain = which(Qmedernach$date <= DATE_EVTS$FIN_Q[evt_i] & Qmedernach$date >= DATE_EVTS$DEB_P[evt_i] )
  id_rain2 = which(Qheffingen$date <= DATE_EVTS$FIN_Q[evt_i] & Qheffingen$date >= DATE_EVTS$DEB_P[evt_i] )
  id_rain3 = which(RainEVTs$date <= DATE_EVTS$FIN_Q[evt_i] & RainEVTs$date >= DATE_EVTS$DEB_P[evt_i] )
  plot(1:length(id_rain), Qmedernach$date[id_rain], type = "l", 
       ylim = c(0, 1.3*max(Qmedernach$Qout[id_rain]/5/60* 1000/12, na.rm =T)), 
       xlab = "", ylab ="", axes =F)
  axis(1, at = seq(from = 1, to = length(id_rain), by = 24*2),
       labels = seq(from = 1, by = 1, length.out = length(seq(from = 1, to = length(id_rain), by = 24*2))))
  axis(2)
  box()
  mtext(side =1, "day", adj = 1, line=2.1)
  mtext(side =2, "discharge [m3/s]", adj = 0, line=2.1)
  mtext(side =4, "runoff [mm/h]", adj = 1, line=2.1, col = "royalblue")
  title(main = evt_c)
  for(i in 1:length(idx)){
    
    lines(1:length(id_rain),Pout[id_rain,idx][,i]/5/60* 1000/12, col =coloo[i] )
    lines(1:length(id_rain),PQout[id_rain,idx][,i]/5/60* 1000/12, col =coloo1[i] )
  }
  lines(1:length(id_rain), Qmedernach$Qout[id_rain]/5/60* 1000/12  , lwd = 1., col = "red3")
  lines(1:length(id_rain2), Qheffingen$Qheffingen[id_rain2]/5/60*1000, lwd = 1., col = "pink3")
  par(new = T)
  plot(1:length(id_rain3), RainEVTs$Medernach[id_rain3]*12, type = "h", 
       ylim = c(max(RainEVTs$Medernach[id_rain3]*12, na.rm=T)*1.8,0), axes = F, col = "royalblue", xlab ="", ylab="", lwd = 1.) 
  axis(4, col.axis = "royalblue", col.ticks = "royalblue", cex.axis =1.2)
  par(new = T)
  plot(1:length(id_rain), Medernach_Essai[[1]]$Pluie[id_rain]*12/30.35, type = "h", 
       ylim = c(max(Medernach_Essai[[1]]$Pluie[id_rain]*12/30.35/STATS_EVTS[e_i+4 ,"Medernach" , "CR"]*100, na.rm=T)*1.8,0),
       axes = F, col = "darkcyan", xlab ="", ylab="", lwd = 1.) 
  dev.off()
}



png(paste(ad_img, "/GumbelMedernachMixteMauvais21.png", sep=""),
    width= 6.4,height=2.5, units="in", res=400, pointsize = 8)
a = 0 
par(mfrow = c(1,3), mar = c(3.5,4,2,4))
for(e_i  in c(1,10,12)){
  
  a= a+1
  
  nb = quantile_score[1,e_i+4, 1,4 ]
  limit  = sort(SCORES[e_i, , "RMSE"])[nb]
  idx = which((SCORES[e_i, , "RMSE"])<limit)
  
  coloo  = colorRampPalette(c("gray10", "gray90"))(50)
  coloo1  = colorRampPalette(c("chartreuse1", "green4"))(50)
  
  evt_c = EVTs_k[e_i]
  evt_i = which(DATE_EVTS$EVTs == evt_c) 
  print(evt_c)
  print(evt_i)
  id_rain = which(Qmedernach$date <= DATE_EVTS$FIN_Q[evt_i] & Qmedernach$date >= DATE_EVTS$DEB_P[evt_i] )
  id_rain2 = which(Qheffingen$date <= DATE_EVTS$FIN_Q[evt_i] & Qheffingen$date >= DATE_EVTS$DEB_P[evt_i] )
  id_rain3 = which(RainEVTs$date <= DATE_EVTS$FIN_Q[evt_i] & RainEVTs$date >= DATE_EVTS$DEB_P[evt_i] )
  plot(1:length(id_rain), Qmedernach$date[id_rain], type = "l", 
       ylim = c(0, 1.3*max(Qmedernach$Qout[id_rain]/5/60* 1000/12, na.rm =T)), 
       xlab = "", ylab ="", axes =F)
  axis(1, at = seq(from = 1, to = length(id_rain), by = 12*24),
       labels = seq(from = 1, by = 1, 
                    length.out = length(seq(from = 1, to = length(id_rain), by = 24*12))),
       cex.axis = 1.2)
  axis(2, cex.axis = 1.2)
  box()
  mtext(side =1, "day", adj = 1, line=1.5)
  mtext(side =2,expression(paste("Discharge [m"^3,".s"^{-1},"]", sep = "")), adj = 0, line=2.1)
  mtext(side =4,expression(paste("Rainfall [mm.h"^{-1},"]", sep = "")) , adj = 1, line=2.1, col = "royalblue")
  title(main = paste(letters[a], ') ',evt_c, sep = ""), cex.main =1.2)
  for(i in 1:length(idx)){
    
    lines(1:length(id_rain),Pout[id_rain,idx][,i]/5/60* 1000/12, col =coloo[i] )
    lines(1:length(id_rain),PQout[id_rain,idx][,i]/5/60* 1000/12, col =coloo1[i] )
  }
  lines(1:length(id_rain), Qmedernach$Qout[id_rain]/5/60* 1000/12  , lwd = 1., col = "red3")
  lines(1:length(id_rain2), Qheffingen$Qheffingen[id_rain2]/5/60*1000, lwd = 1., col = "pink3")
  par(new = T)
  plot(1:length(id_rain3), RainEVTs$Medernach[id_rain3]*12, type = "h", 
       ylim = c(35,0), axes = F, col = "royalblue", xlab ="", ylab="", lwd = 1.) 
  axis(4, col.axis = "royalblue", col.ticks = "royalblue", cex.axis =1.2)
  par(new = T)
  plot(1:length(id_rain), Medernach_Essai[[1]]$Pluie[id_rain]*12/30.35, type = "h", 
       ylim = c(35,0),
       axes = F, col = "darkcyan", xlab ="", ylab="", lwd = 1.) 
  if(a == 1){
    legend("topright", col = c("green", "red", "pink3"),c("Models (50)", "Obs. (Medernach)", "Obs. (Heffingen)"),
           cex =1.1, bty = "0", lty =1)
  }
}
dev.off()

SCORES = SCORES_M

png(paste(ad_img, "/GammaMedernachMixte4.png", sep=""),
    width= 6.4,height=2.5, units="in", res=400, pointsize = 10)
a = 0 
par(mfrow = c(1,3), mar = c(4,4,2,4))
for(e_i  in c(8,15,17)){
  
  a= a+1
  
  nb = quantile_score[1,e_i+4,1, 4 ]
  if(nb <= 5){
    nb = 10
  }
  limit  = sort(SCORES[e_i, , "RMSE"])[nb]
  idx = which((SCORES[e_i, , "RMSE"])<limit)
  
  coloo  = colorRampPalette(c("gray10", "gray90"))(50)
  coloo1  = colorRampPalette(c("chartreuse1", "green4"))(50)
  
  evt_c = EVTs_M[e_i]
  evt_i = which(DATE_EVTS$EVTs == evt_c) 
  print(evt_c)
  print(evt_i)
  id_rain = which(Qmedernach$date <= DATE_EVTS$FIN_Q[evt_i] & Qmedernach$date >= DATE_EVTS$DEB_P[evt_i] )
  id_rain2 = which(Qheffingen$date <= DATE_EVTS$FIN_Q[evt_i] & Qheffingen$date >= DATE_EVTS$DEB_P[evt_i] )
  id_rain3 = which(RainEVTs$date <= DATE_EVTS$FIN_Q[evt_i] & RainEVTs$date >= DATE_EVTS$DEB_P[evt_i] )
  plot(1:length(id_rain), Qmedernach$date[id_rain], type = "l", 
       ylim = c(0, 1.3*max(Qmedernach$Qout[id_rain]/5/60* 1000/12, na.rm =T)), 
       xlab = "", ylab ="", axes =F)
  axis(1, at = seq(from = 1, to = length(id_rain), by = 12*24),
       labels = seq(from = 1, by = 1, 
                    length.out = length(seq(from = 1, to = length(id_rain), by = 24*12))),
       cex.axis = 1.2)
  axis(2, cex.axis = 1.2)
  box()
  mtext(side =1, "day", adj = 1, line=1.8)
  mtext(side =2,expression(paste("Discharge [m"^3,".s"^{-1},"]", sep = "")), adj = 0, line=2.1)
  mtext(side =4,expression(paste("Net rainfall [mm.h"^{-1},"]", sep = "")) , adj = 1, line=2.6, col = "royalblue")
  #mtext(side =2, "discharge [m3/s]", adj = 0, line=2.1)
  #mtext(side =4, "rainfall[mm/h]", adj = 1, line=2.1, col = "royalblue")
  title(main = paste(letters[a], ') ',evt_c, sep = ""), cex.main =1.2)
  for(i in 1:length(idx)){
    
    lines(1:length(id_rain),Pout[id_rain,idx][,i]/5/60* 1000/12, col =coloo[i] )
    lines(1:length(id_rain),PQout[id_rain,idx][,i]/5/60* 1000/12, col =coloo1[i] )
  }
  lines(1:length(id_rain), Qmedernach$Qout[id_rain]/5/60* 1000/12  , lwd = 1., col = "red3")
  lines(1:length(id_rain2), Qheffingen$Qheffingen[id_rain2]/5/60*1000, lwd = 1., col = "pink3")
  par(new = T)
  plot(1:length(id_rain), Medernach_Essai[[1]]$Pluie[id_rain]*12/30.35, type = "h", 
       axes = F, col = "royalblue", xlab ="", ylab="", lwd = 1.,
       ylim = c(max(Medernach_Essai[[1]]$Pluie[id_rain]*12/30.35, na.rm=T)*1.8,0)) 
  axis(4, col.axis = "royalblue", col.ticks = "royalblue", cex.axis =1.2)
  # if(a == 3){
  #   legend("topright", col = c("orangered", "black"),c("Models (50)", "Observations"),
  #          cex =1.1, bty = "0", lty =1)
  # }
  if(a == 3){
    legend(x =100, y = 1.14, col = c( "grey","chartreuse", "red3", "pink3"),
           c("Gamma model (Ga)", "Ga + hydraulic model", "Medernach obs.", "Heffingen obs."),
           cex =0.8, bty = "n", lty =1)
  }
}
dev.off()


EVTs_k = DATE_EVTS$EVTs[1:nrow(DATE_EVTS)]
SCORES = SCORES_K
for(e_i  in 1:length(EVTs_k)){
  
  png(paste(ad_img, "/GammaKoedange_",EVTs[e_i], "21.png", sep=""),
      width= 3,height=3, units="in", res=400, pointsize = 8)
  par(mfrow = c(1,1), mar = c(4,4,2,4))
  
  nb = quantile_score[1,e_i,1, 4 ]
  if(nb <= 10){
    nb = 10
  }
  limit  = sort(SCORES[e_i, , "RMSE"])[nb]
  idx = which((SCORES[e_i, , "RMSE"])<limit)
  
  coloo  = colorRampPalette(c("gray10", "gray90"))(50)
  coloo1  = colorRampPalette(c("orangered1", "orangered4"))(50)
  
  evt_c = EVTs_k[e_i]
  evt_i = which(DATE_EVTS$EVTs == evt_c) 
  print(evt_c)
  print(evt_i)
  id_rain = which(Qkoedange$Date <= DATE_EVTS$FIN_Q[evt_i] & Qkoedange$Date >= DATE_EVTS$DEB_P[evt_i] )
  plot(1:length(id_rain), Qkoedange$Date[id_rain], type = "l", 
       ylim = c(0, 1.3*max(Qkoedange$Qout[id_rain]/5/60*31.14* 1000/12, na.rm =T)), 
       xlab = "", ylab ="", axes =F)
  axis(1, at = seq(from = 1, to = length(id_rain), by = 12*24),
       labels = seq(from = 1, by = 1, 
                    length.out = length(seq(from = 1, to = length(id_rain), by = 24*12))))
  axis(2)
  box()
  mtext(side =1, "day", adj = 1, line=2.1)
  mtext(side =2, "discharge [m3/s]", adj = 0, line=2.1)
  mtext(side =4, "runoff [mm/h]", adj = 1, line=2.1, col = "royalblue")
  title(main = evt_c)
  for(i in 1:length(idx)){
    
    lines(1:length(id_rain),KOedange_Essai[[2]][id_rain,idx][,i]/5/60*31.14* 1000/12, 
          col =coloo1[i] )

  }
  lines(1:length(id_rain), Qkoedange$Qout[id_rain]/5/60*1000*31.14/12, lwd = 1., col = "black")
  par(new = T)
  plot(1:length(id_rain), KOedange_Essai[[1]]$Pluie[id_rain]*12, type = "h", 
       ylim = c(max(KOedange_Essai[[1]]$Pluie[id_rain]*12, na.rm=T)*1.8,0), axes = F, col = "royalblue", xlab ="", ylab="", lwd = 1.) 
  axis(4, col.axis = "royalblue", col.ticks = "royalblue")
  dev.off()
}



png(paste(ad_img, "/GammaKoedangeMixte21.png", sep=""),
    width= 6.4,height=2.5, units="in", res=400, pointsize = 10)
a = 0 
par(mfrow = c(1,3), mar = c(4,4,2,4))
for(e_i  in c(12,19,23)){
  
  a= a+1
  
  nb = quantile_score[1,e_i,1, 4 ]
  if(nb <= 10){
    nb = 10
  }
  limit  = sort(SCORES[e_i, , "RMSE"])[nb]
  idx = which((SCORES[e_i, , "RMSE"])<limit)
  
  coloo  = colorRampPalette(c("gray10", "gray90"))(50)
  coloo1  = colorRampPalette(c("orangered1", "orangered4"))(50)
  
  evt_c = EVTs_k[e_i]
  evt_i = which(DATE_EVTS$EVTs == evt_c) 
  print(evt_c)
  print(evt_i)
  id_rain = which(Qkoedange$Date <= DATE_EVTS$FIN_Q[evt_i] & Qkoedange$Date >= DATE_EVTS$DEB_P[evt_i] )
  plot(1:length(id_rain), Qkoedange$Date[id_rain], type = "l", 
       ylim = c(0, 1.3*max(Qkoedange$Qout[id_rain]/5/60*31.14* 1000/12, na.rm =T)), 
       xlab = "", ylab ="", axes =F)
  axis(1, at = seq(from = 1, to = length(id_rain), by = 12*24),
       labels = seq(from = 1, by = 1, 
                    length.out = length(seq(from = 1, to = length(id_rain), by = 24*12))),
       cex.axis = 1.2)
  axis(2, cex.axis = 1.2)
  box()
  mtext(side =1, "day", adj = 1, line=1.8)
  mtext(side =2,expression(paste("Discharge [m"^3,".s"^{-1},"]", sep = "")), adj = 0, line=2.1)
  mtext(side =4,expression(paste("Net rainfall [mm.h"^{-1},"]", sep = "")) , adj = 1, line=2.6, col = "royalblue")
  title(main = paste(letters[a], ') ',evt_c, sep = ""), cex.main =1.2)
  for(i in 1:length(idx)){
    
    lines(1:length(id_rain),KOedange_Essai[[2]][id_rain,idx][,i]/5/60*31.14* 1000/12, 
          col =coloo1[i] )
    
  }
  lines(1:length(id_rain), Qkoedange$Qout[id_rain]/5/60*1000*31.14/12, lwd = 1., col = "black")
  par(new = T)
  plot(1:length(id_rain), KOedange_Essai[[1]]$Pluie[id_rain]*12, type = "h", 
       ylim = c(max(KOedange_Essai[[1]]$Pluie[id_rain]*12, na.rm=T)*1.8,0), axes = F, col = "royalblue", xlab ="", ylab="", lwd = 1.) 
  axis(4, col.axis = "royalblue", col.ticks = "royalblue", cex.axis =1.2)
  if(a == 3){
    legend("topright", col = c("orangered", "black"),c("Sim.", "Obs."),
           cex =1.1, bty = "0", lty =1)
  }
}
dev.off()


##########################################################################################""
###### TTD PROPERTIES  ###################################################################


X = c(0:1000)/20
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
averageHour = function(x){
  mean(sort(x, decreasing = T)[1:20])
}
timePeak = function(x){
  mean(which(x==max(x))/20)
  
}
VolBeforePeak = function(x){
  
  id_pk = as.integer(mean(which(x==max(x))))
  if(!is.na(id_pk)){
    return(sum(x[1:id_pk])/20) 
  }
  if(is.na(id_pk)){
    return(NA) 
  }
  
}

# complete matrix 50
nb = 50
TransitTime_K = array(dim = c(nb,length(X),length(EVTs)))
TransitTime_M = array(dim = c(nb,length(X),length(EVTs)))
for(e_i in 1:length(EVTs)){
  
  if(EVTs[e_i] %in% EVTs_k){
    
    nb = quantile_score[1,e_i,1,4]
    limit  = sort(SCORES_K[e_i, , "RMSE"])[nb]
    idx = which((SCORES_K[e_i, , "RMSE"])<=limit)
    for(i in 1:length(idx)){
      TransitTime_K[i, , e_i] = gamma_law(X, Params_K$alphaPV[idx][i], Params_K$thetaPV[idx][i])
    }
    
  }
  
  if(EVTs[e_i] %in% EVTs_M){
  
    nb = quantile_score[2,e_i,1,4]
    limit  = sort(SCORES_M[e_i-4, , "RMSE"])[nb]
    idx = which((SCORES_M[e_i-4, , "RMSE"])<=limit)
    for(i in 1:length(idx)){
      TransitTime_M[i, , e_i] = gamma_law(X, Params_M$alphaPV[idx][i], Params_M$thetaPV[idx][i])
    }
  }
}


png(paste(ad_img, "/explicationTTD.png", sep=""),
    width= 3.9, height=2.6, units="in", res=400, pointsize = 8)
par(mfrow = c(1,1), mar = c(4,4,2,4))
plot(X,TransitTime_M[ 1, ,12], type = "l", xlab = "", ylab = "", 
     ylim = c(0,0.12),axes = F, xlim = range(X[1:600]),lwd = 2,
     col = "gray30")

segments(x0 = X[101], x1 = X[101], y0 = -5, y1 =5,
       lwd = 1, col = "gray20" , length = 0.1, lty = 2)

axis(1)
mtext(side = 1, "hour", adj =1, line = 2.3)
mtext(side = 2, "hydrological response [-]", col = "gray20",adj =0.5, line = 2.3)
mtext(side = 4, "cumulative response [-]", adj =0.5, line = 2.1,
      col = "red3")
mtext(side = 3, "Rainfall dirac", adj =0, line = 0.0,
      col = "royalblue")
axis(2, at = c(0,0.05,0.10))
par(new = T)
plot(c(-X[100:1],c(-5:0)/20,X),c( rep(0,100), rep(1,11), rep(0,996)),
     type = "h", xlab = "", ylab = "",
     xlim =  range(X[1:600]), ylim = c(2,0), axes = F, col = "royalblue")

arrows(x0 = 0, x1 = X[101], y0 = 0.25, y1 =0.25,
       lty = 1,lwd = 1.5, col = "gray20" , length = 0.07)
arrows(x1 = 0, x0 = X[101], y0 = 0.25, y1 =0.25,
       lty = 1,lwd = 1.5, col = "gray20" , length = 0.07)


segments(x0 = 0, x1 = 0, y0 = -5, y1 = 5, lty = 2, "navyblue")
par(new = T)
plot(X,cumsum(TransitTime_M[ 1, ,12])/20, type = "l", 
     xlab = "", ylab = "", lwd = 2,
     ylim = c(0,1.2),axes = F, xlim = range(X[1:600]), col = "red3")
segments(x0 = X[136], x1 = X[136], y0 = -5, y1 =0.5,
         col = "red3",lty = 2)

arrows(x0 = 0, x1 = X[136], y0 = 0.15, y1 =0.15,
       lty = 1,lwd = 2, length = 0.05, col = "red3")
arrows(x1 = 0, x0 = X[136], y0 = 0.15, y1 =0.15,
       lty = 1,lwd = 2, length = 0.05, col = "red3")

axis(4, at = c(0,0.5,1.0), col = "red3", col.axis = "red3")
box()
dev.off()

CALIB_EVTS_PM = data.frame(EVTS = EVTs_M, MU_min = NA, MU_max = NA,
                        MU_med = NA, BETA_min  = NA, BETA_max  = NA,
                        BETA_med  = NA, ESP_min = NA, ESP_max = NA,
                        ESP_med = NA, STD_min = NA, STD_med = NA, STD_max = NA,
                        RMSE_med = NA, corvar_med = NA, VolBefPk_med = NA, VolBefPk_q95 = NA,
                        VolBefPk_q05 = NA)
for(e_i  in 1:length(EVTs_M)){
  
  limit  = sort(SCORES_M[e_i, , "RMSE"])[quantile_score[2, e_i+4,1,"nb"]]
  idx = which((SCORES_M[e_i, , "RMSE"])<limit)
  
  CALIB_EVTS_PM[e_i, c("MU_min", "MU_max")] = range(alphaPV[idx])
  CALIB_EVTS_PM[e_i, "MU_med"] = quantile(alphaPV[idx],0.5)
  CALIB_EVTS_PM[e_i, c("BETA_min", "BETA_max")] = range(thetaPV[idx])
  CALIB_EVTS_PM[e_i, "BETA_med"] = quantile(thetaPV[idx],0.5)
  CALIB_EVTS_PM[e_i, "ESP_med"] = quantile(alphaPV[idx] *thetaPV[idx],0.5)
  CALIB_EVTS_PM[e_i, c("ESP_min","ESP_max")] = range(alphaPV[idx] * thetaPV[idx])
  CALIB_EVTS_PM[e_i, "STD_med"] = quantile(sqrt(alphaPV[idx] * thetaPV[idx]*thetaPV[idx]),0.5)
  CALIB_EVTS_PM[e_i, c("STD_min", "STD_max")] = range(sqrt(alphaPV[idx] * thetaPV[idx]*thetaPV[idx]))
  
  print(EVTs_M[e_i])
}

nb = 50
T50th = matrix(nrow = length(EVTs_M), ncol = nb)
for(e_i in 1:length(EVTs_M)){
  
  limit  = sort(SCORES_M[e_i, , "RMSE"])[quantile_score[2, e_i+4,1,"nb"]]
  idx = which((SCORES_M[e_i, , "RMSE"])<=limit)
  for(n_i in 1:length(idx)){
    mui = Params_M$alphaPV[idx][n_i]
    betai = Params_M$thetaPV[idx][n_i]
    Y = gamma_law(X, mui,betai)
    ttmp = X[min(which(cumsum(Y)/20>0.5))]
    T50th[e_i, n_i] = ttmp
  }
  
}


CALIB_EVTS_PM$ESP2_min = apply(T50th,1,min, na.rm =T)
CALIB_EVTS_PM$ESP2_max = apply(T50th,1,max, na.rm =T)
CALIB_EVTS_PM$TTD50 = apply(T50th,1,quantile,0.5, na.rm =T)


CALIB_EVTS_PK = data.frame(EVTS = EVTs_k, MU_min = NA, MU_max = NA,
                           MU_med = NA, BETA_min  = NA, BETA_max  = NA,
                           BETA_med  = NA, ESP_min = NA, ESP_max = NA,
                           ESP_med = NA, STD_min = NA, STD_med = NA, STD_max = NA,
                           RMSE_med = NA, corvar_med = NA)
for(e_i  in 1:length(EVTs_k)){
  
  limit  = sort(SCORES_K[e_i, , "RMSE"])[quantile_score[1, e_i,1,"nb"]]
  idx = which((SCORES_K[e_i, , "RMSE"])<limit)
  
  CALIB_EVTS_PK[e_i, c("MU_min", "MU_max")] = range(Params_K$alphaPV[idx])
  CALIB_EVTS_PK[e_i, "MU_med"] = quantile(Params_K$alphaPV[idx],0.5)
  CALIB_EVTS_PK[e_i, c("BETA_min", "BETA_max")] = range(Params_K$thetaPV[idx])
  CALIB_EVTS_PK[e_i, "BETA_med"] = quantile(Params_K$thetaPV[idx],0.5)
  CALIB_EVTS_PK[e_i, "ESP_med"] = quantile(Params_K$alphaPV[idx] *Params_K$thetaPV[idx],0.5)
  CALIB_EVTS_PK[e_i, c("ESP_min","ESP_max")] = range(Params_K$alphaPV[idx] * Params_K$thetaPV[idx])
  CALIB_EVTS_PK[e_i, "STD_med"] = quantile(sqrt(Params_K$alphaPV[idx] * Params_K$thetaPV[idx]*Params_K$thetaPV[idx]),0.5)
  CALIB_EVTS_PK[e_i, c("STD_min", "STD_max")] = range(sqrt(Params_K$alphaPV[idx] * Params_K$thetaPV[idx]*Params_K$thetaPV[idx]))
  
  print(EVTs_k[e_i])
}

nb = 50
T50th = matrix(nrow = length(EVTs_k), ncol = nb)
for(e_i in 1:length(EVTs_k)){
  
  limit  = sort(SCORES_K[e_i, , "RMSE"])[quantile_score[1, e_i,1,"nb"]]
  idx = which((SCORES_K[e_i, , "RMSE"])<=limit)
  
  for(n_i in 1:length(idx)){
    
    mui = Params_K$alphaPV[idx][n_i]
    betai = Params_K$thetaPV[idx][n_i]
    Y = gamma_law(X, mui,betai)
    ttmp = X[min(which(cumsum(Y)/20>0.5))]
    T50th[e_i, n_i] = ttmp
  }
  
}


CALIB_EVTS_PK$ESP2_min = apply(T50th,1,min, na.rm =T)
CALIB_EVTS_PK$ESP2_max = apply(T50th,1,max, na.rm =T)
CALIB_EVTS_PK$TTD50 = apply(T50th,1,quantile,0.5, na.rm =T)

CALIB_EVTS_PK$VOL1H = NA
CALIB_EVTS_PM$VOL1H = NA
CALIB_EVTS_PK$VOL1H = apply(apply(TransitTime_K,c(1,3),averageHour),2,quantile,0.5, na.rm =T)
CALIB_EVTS_PM$VOL1H = apply(apply(TransitTime_M,c(1,3),averageHour),2,quantile,0.5, na.rm =T)[-c(1:4)]

CALIB_EVTS_PK$VOL1H_q05 = NA
CALIB_EVTS_PM$VOL1H_q05 = NA
CALIB_EVTS_PK$VOL1H_q05= apply(apply(TransitTime_K,c(1,3),averageHour),2,quantile,0.05, na.rm =T)
CALIB_EVTS_PM$VOL1H_q05 = apply(apply(TransitTime_M,c(1,3),averageHour),2,quantile,0.05, na.rm =T)[-c(1:4)]

CALIB_EVTS_PK$VOL1H_q95 = NA
CALIB_EVTS_PM$VOL1H_q95 = NA
CALIB_EVTS_PK$VOL1H_q95 = apply(apply(TransitTime_K,c(1,3),averageHour),2,quantile,0.95, na.rm =T)
CALIB_EVTS_PM$VOL1H_q95 = apply(apply(TransitTime_M,c(1,3),averageHour),2,quantile,0.95, na.rm =T)[-c(1:4)]

CALIB_EVTS_PK$TTDpk = NA
CALIB_EVTS_PM$TTDpk = NA
CALIB_EVTS_PK$TTDpk= apply(apply(TransitTime_K,c(1,3),timePeak),2,quantile,0.5, na.rm =T)
CALIB_EVTS_PM$TTDpk = apply(apply(TransitTime_M,c(1,3),timePeak),2,quantile,0.5, na.rm =T)[-c(1:4)]

CALIB_EVTS_PK$TTDpk_q05 = NA
CALIB_EVTS_PM$TTDpk_q05 = NA
CALIB_EVTS_PK$TTDpk_q05= apply(apply(TransitTime_K,c(1,3),timePeak),2,quantile,0.05, na.rm =T)
CALIB_EVTS_PM$TTDpk_q05 = apply(apply(TransitTime_M,c(1,3),timePeak),2,quantile,0.05, na.rm =T)[-c(1:4)]

CALIB_EVTS_PK$TTDpk_q95 = NA
CALIB_EVTS_PM$TTDpk_q95 = NA
CALIB_EVTS_PK$TTDpk_q95 = apply(apply(TransitTime_K,c(1,3),timePeak),2,quantile,0.95, na.rm =T)
CALIB_EVTS_PM$TTDpk_q95 = apply(apply(TransitTime_M,c(1,3),timePeak),2,quantile,0.95, na.rm =T)[-c(1:4)]

CALIB_EVTS_PM$VolBefPk_q95 = apply(apply(TransitTime_M,c(1,3),VolBeforePeak),2,quantile,0.95, na.rm =T)[-c(1:4)]
CALIB_EVTS_PM$VolBefPk_q05 = apply(apply(TransitTime_M,c(1,3),VolBeforePeak),2,quantile,0.05, na.rm =T)[-c(1:4)]
CALIB_EVTS_PM$VolBefPk_med = apply(apply(TransitTime_M,c(1,3),VolBeforePeak),2,quantile,0.5, na.rm =T)[-c(1:4)]


load(file = paste(adout, "stats_evts2022.rda", sep = ""))
coleurs = rev(c("black", "darkcyan", "olivedrab3","navyblue", "pink3", "orangered"))

#ad_img = "/media/adouinot/8858-1E31/PRO/2019/DATABASE/HYDRO/IMG/"



png(paste(ad_img, "/concentrationGammaKoedangeMedernach521b.png", sep=""),
    width= 3.5, height=4, units="in", res=400, pointsize = 9)
par(mfrow = c(1,1),  mar= c(1.5,0.5,0,0.5))
plot(c( CALIB_EVTS_PK$VOL1H),c(1:length(EVTs))+0.1, pch =0,cex=2.05,col = "black",
     xlim = c(-0.1,max(CALIB_EVTS_PM$VOL1H_q95, na.rm =T)),
     ylim = c(length(EVTs),1), axes =F, xlab = "", ylab = "")
axis(1, at = seq(from = 0, to = 0.3, by = 0.05), labels = rep("", 7))

for(e_i in c(1:as.integer(length(EVTs)/2))*2){
  
  polygon(x = c(-10,25.,25,-10),
          y =c(e_i-0.5,e_i-0.5,e_i+0.5,e_i+0.5),
          border = NA, col = "gray90" )
  
}
mtext(text = EVTs, 2,
      at = c(1:length(EVTs)),line = -1, adj = c(0),
      cex = 0.9, las = 1)
mtext(text =  as.character(seq(from = 0, to = 0.3, by = 0.05)*100), 1,
      at = seq(from = 0, to = 0.3, by = 0.05),line = 0.5, adj = c(0.5,0.5),
      cex = 0.9)
points(c(CALIB_EVTS_PK$VOL1H),
       c(1:length(EVTs))+0.1, pch = 15,cex=2,col = "orangered")
points(c( CALIB_EVTS_PK$VOL1H),
       c(1:length(EVTs))+0.1, pch = 0,cex=2.05,col = "black")

points(c(rep(NA,4), CALIB_EVTS_PM$VOL1H),c(1:length(EVTs))-0.1, 
       pch = 15,cex=2,col ="olivedrab3")
points(c(NA,NA, NA, NA, CALIB_EVTS_PM$VOL1H),c(1:length(EVTs))-0.1, pch =0,cex=2.05,
       col = "black")

segments(y0 =c(1:length(EVTs))+0.1, y1 =c(1:length(EVTs))+0.1,
         x0 = c(CALIB_EVTS_PK$VOL1H_q05), 
         x1 = c(CALIB_EVTS_PK$VOL1H_q95),  col = "orangered4")
segments(y0 =c(1:length(EVTs))-0.1, y1 =c(1:length(EVTs))-0.1,
         x0 = c(rep(NA,4),CALIB_EVTS_PM$VOL1H_q05), 
         x1 = c(rep(NA,4),CALIB_EVTS_PM$VOL1H_q95),  col = "olivedrab4")
title(main = "                  ",cex.main = 0.95)


legend("right", c("KOE", "HM"), fill = c("orangered", "olivedrab3")       )

dev.off()

orderK = sort.int( format(as.POSIXct(DATE_EVTS$EVTs, format = "%Y_%m_%d", tz ="UTC"), "%j"), index.return = T)$ix
orderM = orderK[-c(1:4)] - 4

png(paste(ad_img, "/concentrationGammaKoedangeMedernachOrdered21.png", sep=""),
    width= 3.5, height=3.5, units="in", res=400, pointsize = 9)
par(mfrow = c(1,1),  mar= c(1.5,0.5,0,0.5))
plot(c( CALIB_EVTS_PK$VOL1H)[orderK],c(1:length(EVTs))+0.1, pch =0,cex=2.05,col = "black",
     xlim = c(-0.1,0.33),
     ylim = c(length(EVTs),1), axes =F, xlab = "", ylab = "")
axis(1, at = seq(from = 0, to = 0.3, by = 0.05), labels = rep("", 7))

for(e_i in c(1:as.integer(length(EVTs)/2))*2){
  
  polygon(x = c(-10,25.,25,-10),
          y =c(e_i-0.5,e_i-0.5,e_i+0.5,e_i+0.5),
          border = NA, col = "gray90" )
  
}
mtext(text = EVTs[orderK], 2,
      at = c(1:length(EVTs)),line = -1, adj = c(0),
      cex = 0.9, las = 1)
mtext(text =  as.character(seq(from = 0, to = 0.3, by = 0.05)*100), 1,
      at = seq(from = 0, to = 0.3, by = 0.05),line = 0.5, adj = c(0.5,0.5),
      cex = 0.9)
points(c(CALIB_EVTS_PK$VOL1H)[orderK],
       c(1:length(EVTs))+0.1, pch = 15,cex=2,col = "orangered")
points(c( CALIB_EVTS_PK$VOL1H)[orderK],
       c(1:length(EVTs))+0.1, pch = 0,cex=2.05,col = "black")

points(c(rep(NA,4),CALIB_EVTS_PM$VOL1H)[orderK],c(1:length(EVTs))-0.1, 
       pch = 15,cex=2,col ="olivedrab3")
points(c(rep(NA,4), CALIB_EVTS_PM$VOL1H)[orderK],c(1:length(EVTs))-0.1, pch =0,cex=2.05,
       col = "black")

segments(y0 =c(1:length(EVTs))+0.1, y1 =c(1:length(EVTs))+0.1,
         x0 = c(CALIB_EVTS_PK$VOL1H_q05)[orderK], 
         x1 = c(CALIB_EVTS_PK$VOL1H_q95)[orderK],  col = "orangered4")
segments(y0 =c(1:length(EVTs))-0.1, y1 =c(1:length(EVTs))-0.1,
         x0 = c(rep(NA,4),CALIB_EVTS_PM$VOL1H_q05)[orderK], 
         x1 = c(rep(NA,4),CALIB_EVTS_PM$VOL1H_q95)[orderK],  col = "olivedrab4")
title(main = "                  ",cex.main = 0.95)


legend("bottomright", c("KOE", "HM"), fill = c("orangered", "olivedrab3")       )

dev.off()



png(paste(ad_img, "/LagGammaKoedangeMedernach521b.png", sep=""),
    width= 3.5, height=4, units="in", res=400, pointsize = 9)
par(mfrow = c(1,1),  mar= c(1.5,0.5,0,0.))
plot(c( CALIB_EVTS_PK$TTDpk),c(1:length(EVTs))+0.1, pch =0,cex=2.05,col = "black",
     xlim = c(-4,max(CALIB_EVTS_PK$TTDpk_q95, na.rm =T)),
     ylim = c(length(EVTs),1), axes =F, xlab = "", ylab = "")
axis(1, at = seq(from = 0, to = 16, by = 2), labels = rep("", 9))

for(e_i in c(1:as.integer(length(EVTs)/2))*2){
  
  polygon(x = c(-10,25.,25,-10),
          y =c(e_i-0.5,e_i-0.5,e_i+0.5,e_i+0.5),
          border = NA, col = "gray90" )
  
}
mtext(text = EVTs, 2,
      at = c(1:length(EVTs)),line = -1, adj = c(0),
      cex = 0.7, las = 1)
mtext(text =  as.character(seq(from = 0, to = 16, by = 2)), 1,
      at = seq(from = 0, to = 16, by = 2),line = 0.5, adj = c(0.5,0.5),
      cex = 0.9)
points(c( CALIB_EVTS_PK$TTDpk),
       c(1:length(EVTs))+0.1, pch = 15,cex=2,col = "orangered")
points(c( CALIB_EVTS_PK$TTDpk),
       c(1:length(EVTs))+0.1, pch = 0,cex=2.05,col = "black")

points(c(rep(NA,4), CALIB_EVTS_PM$TTDpk),c(1:length(EVTs))-0.1, 
       pch = 15,cex=2,col ="olivedrab3")
points(c(NA,NA, NA, NA, CALIB_EVTS_PM$TTDpk),c(1:length(EVTs))-0.1, pch =0,cex=2.05,
       col = "black")

segments(y0 =c(1:length(EVTs))+0.1, y1 =c(1:length(EVTs))+0.1,
         x0 = c(CALIB_EVTS_PK$TTDpk_q05), 
         x1 = c(CALIB_EVTS_PK$TTDpk_q95),  col = "orangered4")
segments(y0 =c(1:length(EVTs))-0.1, y1 =c(1:length(EVTs))-0.1,
         x0 = c(rep(NA,4),CALIB_EVTS_PM$TTDpk_q05), 
         x1 = c(rep(NA,4),CALIB_EVTS_PM$TTDpk_q95),  col = "olivedrab4")
title(main = "                         ",cex.main = 0.95)

dev.off()


png(paste(ad_img, "/LagGammaKoedangeMedernachOrder21.png", sep=""),
    width= 3.5, height=3.5, units="in", res=400, pointsize = 9)
par(mfrow = c(1,1),  mar= c(1.5,0.5,0,0.))
plot(c( CALIB_EVTS_PK$TTDpk[orderK]),c(1:length(EVTs))+0.1, pch =0,cex=2.05,col = "black",
     xlim = c(-4,max(CALIB_EVTS_PK$TTDpk_q95, na.rm =T)),
     ylim = c(length(EVTs),1), axes =F, xlab = "", ylab = "")
axis(1, at = seq(from = 0, to = 16, by = 2), labels = rep("", 9))

for(e_i in c(1:as.integer(length(EVTs)/2))*2){
  
  polygon(x = c(-10,25.,25,-10),
          y =c(e_i-0.5,e_i-0.5,e_i+0.5,e_i+0.5),
          border = NA, col = "gray90" )
  
}
mtext(text = EVTs[orderK], 2,
      at = 1:length(EVTs),line = -1, adj = c(0),
      cex = 0.7, las = 1)
mtext(text =  as.character(seq(from = 0, to = 16, by = 2)), 1,
      at = seq(from = 0, to = 16, by = 2),line = 0.5, adj = c(0.5,0.5),
      cex = 0.9)
points(c( CALIB_EVTS_PK$TTDpk[orderK]),
       c(1:length(EVTs))+0.1, pch = 15,cex=2,col = "orangered")
points(c( CALIB_EVTS_PK$TTDpk[orderK]),
       c(1:length(EVTs))+0.1, pch = 0,cex=2.05,col = "black")

points( c(rep(NA, 4), CALIB_EVTS_PM$TTDpk)[orderK],c(1:length(EVTs))-0.1, 
       pch = 15,cex=2,col ="olivedrab3")
points( c(rep(NA, 4), CALIB_EVTS_PM$TTDpk)[orderK],c(1:length(EVTs))-0.1, pch =0,cex=2.05,
       col = "black")

segments(y0 =c(1:length(EVTs))+0.1, y1 =c(1:length(EVTs))+0.1,
         x0 = c(CALIB_EVTS_PK$TTDpk_q05)[orderK], 
         x1 = c(CALIB_EVTS_PK$TTDpk_q95)[orderK],  col = "orangered4")
segments(y0 =c(1:length(EVTs))-0.1, y1 =c(1:length(EVTs))-0.1,
         x0 =c(rep(NA, 4), CALIB_EVTS_PM$TTDpk_q05)[orderK], 
         x1 = c(rep(NA, 4),CALIB_EVTS_PM$TTDpk_q95)[orderK],  col = "olivedrab4")
title(main = "                         ",cex.main = 0.95)

dev.off()

#######################################################


png(paste(ad_img, "/transitTimeGammaKoedangeMedernach421b.png", sep=""),
    width= 3.8, height=4., units="in", res=400, pointsize = 9)
par(mfrow = c(1,1),  mar= c(1.5,0.5,0,0.0))
plot(c( CALIB_EVTS_PK$TTD50),c(1:length(EVTs))+0.1, pch =0,cex=2.05,col = "black",
     xlim = c(-6,max(CALIB_EVTS_PM$ESP2_max)),
     ylim = c(length(EVTs),1), axes =F, xlab = "", ylab = "")

for(e_i in c(1:as.integer(length(EVTs)/2))*2){
  
  polygon(x = c(-10,25.,25,-10),
          y =c(e_i-0.5,e_i-0.5,e_i+0.5,e_i+0.5),
          border = NA, col = "gray90" )
  
}

par(new = T)
plot(c( CALIB_EVTS_PK$TTD50),c(1:length(EVTs))+0.1, pch =0,cex=2.05,col = "black",
     xlim = c(-6,max(CALIB_EVTS_PM$ESP2_max)),
     ylim = c(length(EVTs),1), axes =F, xlab = "", ylab = "")
#box()
#axis(2, at  = c(1:length(EVTs)), labels = EVTs, cex = 0.75, las =1)
axis(1, at = seq(from = 0, to = 20, by = 2), labels = rep("", 11))
mtext(text = EVTs, 2,
      at = c(1:length(EVTs)),line = -1, adj = c(0),
      cex = 0.9, las = 1)
mtext(text =  as.character(seq(from = 0, to = 20, by = 2)), 1,
      at = seq(from = 0, to =20, by = 2),line = 0.5, adj = c(0.5,0.5),
      cex = 0.9)

segments(y0 =c(1:length(EVTs))+0.1, y1 =c(1:length(EVTs))+0.1,
         x0 = c(CALIB_EVTS_PK$ESP2_min), 
         x1 = c(CALIB_EVTS_PK$ESP2_max),  col = "orangered4")
points(c( CALIB_EVTS_PK$TTD50),c(1:length(EVTs))+0.1, pch = 15,cex=2,col = "orangered")
# points(STATS_EVTS[-1 ,"Koedange" , "deltaQ50th"],c(2:length(EVTs)), 
#        pch = 16, col ="orangered")
title(main = "                          ",cex.main = 0.95)


#box()
segments(y0 =c(1:length(EVTs))-0.1, y1 =c(1:length(EVTs)-0.1),
         x0 = c(rep(NA,4),CALIB_EVTS_PM$ESP2_min),
         x1 = c(rep(NA,4),CALIB_EVTS_PM$ESP2_max), col = "olivedrab4")
points(c(rep(NA,4), CALIB_EVTS_PM$TTD50),c(1:length(EVTs))-0.1, 
       pch = 15,cex=2,col ="olivedrab3")
points(c(rep(NA,4), CALIB_EVTS_PM$TTD50),c(1:length(EVTs))-0.1, pch =0,cex=2.05,col = "black",
       xlim = c(min(CALIB_EVTS_PM$ESP2_min),max(CALIB_EVTS_PM$ESP2_max)),
       ylim = c(length(EVTs),1))
# points(STATS_EVTS[-1 ,"Koedange" , "deltaQ50th"],c(2:length(EVTs)), 
#        pch = 16, col ="orangered")

dev.off()



png(paste(ad_img, "/transitTimeGammaKoedangeMedernachOrder21.png", sep=""),
    width= 3.8, height=3.5, units="in", res=400, pointsize = 9)
par(mfrow = c(1,1),  mar= c(1.5,0.5,0,0.0))
plot(c( CALIB_EVTS_PK$TTD50)[orderK],c(1:length(EVTs))+0.1, pch =0,cex=2.05,col = "black",
     xlim = c(-6,max(CALIB_EVTS_PM$ESP2_max)),
     ylim = c(length(EVTs),1), axes =F, xlab = "", ylab = "")

for(e_i in c(1:as.integer(length(EVTs)/2))*2){
  
  polygon(x = c(-10,25.,25,-10),
          y =c(e_i-0.5,e_i-0.5,e_i+0.5,e_i+0.5),
          border = NA, col = "gray90" )
  
}

par(new = T)
plot(c( CALIB_EVTS_PK$TTD50)[orderK],c(1:length(EVTs))+0.1, pch =0,cex=2.05,col = "black",
     xlim = c(-6,max(CALIB_EVTS_PM$ESP2_max)),
     ylim = c(length(EVTs),1), axes =F, xlab = "", ylab = "")
#box()
#axis(2, at  = c(1:length(EVTs)), labels = EVTs, cex = 0.75, las =1)
axis(1, at = seq(from = 0, to = 20, by = 2), labels = rep("", 11))
mtext(text = EVTs[orderK], 2,
      at = c(1:length(EVTs)),line = -1, adj = c(0),
      cex = 0.9, las = 1)
mtext(text =  as.character(seq(from = 0, to = 20, by = 2)), 1,
      at = seq(from = 0, to =20, by = 2),line = 0.5, adj = c(0.5,0.5),
      cex = 0.9)

segments(y0 =c(1:length(EVTs))+0.1, y1 =c(1:length(EVTs))+0.1,
         x0 = c(CALIB_EVTS_PK$ESP2_min[orderK]), 
         x1 = c(CALIB_EVTS_PK$ESP2_max[orderK]),  col = "orangered4")
points(c( CALIB_EVTS_PK$TTD50)[orderK],c(1:length(EVTs))+0.1, pch = 15,
       cex=2,col = "orangered")
points(c( CALIB_EVTS_PK$TTD50)[orderK],c(1:length(EVTs))+0.1, pch = 0,
       cex=2,col = "black")
# points(STATS_EVTS[-1 ,"Koedange" , "deltaQ50th"],c(2:length(EVTs)), 
#        pch = 16, col ="orangered")
title(main = "                          ",cex.main = 0.95)


#box()
segments(y0 =c(1:length(EVTs))-0.1, y1 =c(1:length(EVTs)-0.1),
         x0 = c(rep(NA,4),CALIB_EVTS_PM$ESP2_min)[orderK],
         x1 = c(rep(NA,4),CALIB_EVTS_PM$ESP2_max)[orderK], col = "olivedrab4")
points(c(rep(NA,4), CALIB_EVTS_PM$TTD50)[orderK],c(1:length(EVTs))-0.1, 
       pch = 15,cex=2,col ="olivedrab3")
points(c(rep(NA,4), CALIB_EVTS_PM$TTD50)[orderK],c(1:length(EVTs))-0.1, pch =0,cex=2.05,col = "black")
# points(STATS_EVTS[-1 ,"Koedange" , "deltaQ50th"],c(2:length(EVTs)), 
#        pch = 16, col ="orangered")

dev.off()

evts_i = c(1:23)
orderK = sort.int( format(as.POSIXct(DATE_EVTS$EVTs[1:23], format = "%Y_%m_%d", tz ="UTC"), "%j"), index.return = T)$ix
orderM = orderK[-c(1:4)] - 4


png(paste(ad_img, "/transitTimeGammaKoedangeMedernachOrder21sans21.png", sep=""),
    width= 3.8, height=3.5, units="in", res=400, pointsize = 9)
par(mfrow = c(1,1),  mar= c(1.5,0.5,0,0.0))
plot(c( CALIB_EVTS_PK$TTD50)[orderK],c(evts_i)+0.1, pch =0,cex=2.05,col = "black",
     xlim = c(-6,max(CALIB_EVTS_PM$ESP2_max)),
     ylim = c(length(evts_i ),1), axes =F, xlab = "", ylab = "")

for(e_i in c(1:as.integer(length(evts_i )/2))*2){
  
  polygon(x = c(-10,25.,25,-10),
          y =c(e_i-0.5,e_i-0.5,e_i+0.5,e_i+0.5),
          border = NA, col = "gray90" )
  
}

par(new = T)
plot(c( CALIB_EVTS_PK$TTD50)[orderK],c(evts_i)+0.1, pch =0,cex=2.05,col = "black",
     xlim = c(-6,max(CALIB_EVTS_PM$ESP2_max)),
     ylim = c(length(evts_i ),1), axes =F, xlab = "", ylab = "")
#box()
#axis(2, at  = c(1:length(EVTs)), labels = EVTs, cex = 0.75, las =1)
axis(1, at = seq(from = 0, to = 20, by = 2), labels = rep("", 11))
mtext(text = EVTs[orderK], 2,
      at = c(evts_i),line = -1, adj = c(0),
      cex = 0.9, las = 1)
mtext(text =  as.character(seq(from = 0, to = 20, by = 2)), 1,
      at = seq(from = 0, to =20, by = 2),line = 0.5, adj = c(0.5,0.5),
      cex = 0.9)

segments(y0 =c(evts_i)+0.1, y1 =c(evts_i)+0.1,
         x0 = c(CALIB_EVTS_PK$ESP2_min[orderK]), 
         x1 = c(CALIB_EVTS_PK$ESP2_max[orderK]),  col = "orangered4")
points(c( CALIB_EVTS_PK$TTD50)[orderK],c(evts_i)+0.1, pch = 15,
       cex=2,col = "orangered")
points(c( CALIB_EVTS_PK$TTD50)[orderK],c(evts_i)+0.1, pch = 0,
       cex=2,col = "black")
# points(STATS_EVTS[-1 ,"Koedange" , "deltaQ50th"],c(2:length(EVTs)), 
#        pch = 16, col ="orangered")
title(main = "                          ",cex.main = 0.95)


#box()
segments(y0 =c(evts_i)-0.1, y1 =c(evts_i-0.1),
         x0 = c(rep(NA,4),CALIB_EVTS_PM$ESP2_min)[orderK],
         x1 = c(rep(NA,4),CALIB_EVTS_PM$ESP2_max)[orderK], col = "olivedrab4")
points(c(rep(NA,4), CALIB_EVTS_PM$TTD50)[orderK],c(evts_i)-0.1, 
       pch = 15,cex=2,col ="olivedrab3")
points(c(rep(NA,4), CALIB_EVTS_PM$TTD50)[orderK],c(evts_i)-0.1, pch =0,cex=2.05,col = "black")
# points(STATS_EVTS[-1 ,"Koedange" , "deltaQ50th"],c(2:length(EVTs)), 
#        pch = 16, col ="orangered")

dev.off()

##########################################################################################
##########################################################################################
##########################################################################################


names(CALIB_EVTS_PK)
dimnames(STATS_EVTS)
library(corrplot)
library(Hmisc)


dimnames(STATS_EVTS)[[3]] = c("Rduration", "Rcumul" ,  "Intensite" ,  "I15min","I1h"  ,
                              "Imoy50","Imoy75" , "Imean" ,"Vruiss" , "QspePeak" ,"Train50th" ,
                              "Truiss50th" , "Tpeak","RC", "deltaQpeak", "deltaQ50th",  "SWC20" ,
                              "SWC50", "Qbase", "dq5min")



png(paste(ad_img, "/correlationGammaKoedangeMedernach21b.png", sep=""),
    width= 6.4, height=3.0, units="in", res=1000, pointsize = 7)
par(mfrow = c(1,2), mar = c(0,0,0,0))
A = rcorr(as.matrix(cbind(cbind(STATS_EVTS[ , 1,],data.frame(LAI =VEGE))[ -23, c(1,2,4,5,8,17,18,19, 21,14)], 
                          CALIB_EVTS_PK[-23, c("TTD50","TTDpk","VOL1H")])),type = "pearson")
corrplot(A$r , p.mat = A$P,diag =F,
         type="upper",  tl.col="black", tl.srt=45)

A = rcorr(as.matrix(cbind(cbind(STATS_EVTS[ , 4,],data.frame(LAI =VEGE))[-c(1:4) , c(1,2,4,5,8,17,18,19,21,14)], 
                          CALIB_EVTS_PM[ ,  c("TTD50","TTDpk","VOL1H")])), type = "pearson")
corrplot(A$r , p.mat = A$P,diag =F, sig.level = 0.02,
         type="upper",  tl.col="black", tl.srt=45)

dev.off()


png(paste(ad_img, "/kendallGammaKoedangeMedernach321.png", sep=""),
    width= 6.4, height=3.0, units="in", res=1000, pointsize = 7)
par(mfrow = c(1,2), mar = c(0,0,0,0))

MAT = cbind(cbind(STATS_EVTS[ , 1,],data.frame(LAI =VEGE))[-23, c(1,2,4,5,8,17,18,19, 21,14)], 
            CALIB_EVTS_PK[-23, c("TTD50","TTDpk","VOL1H")])
A = cor(cbind(cbind(STATS_EVTS[ , 1,],data.frame(LAI =VEGE))[ -23, c(1,2,4,5,8,17,18,19, 21,14)], 
              CALIB_EVTS_PK[-23, c("TTD50","TTDpk","VOL1H")]), method = "kendall")
MATP = A
for(i in 1:ncol(MAT)){
  for(j in 1:ncol(MAT)){
    
    MATP[i,j]= cor.test(MAT[,i],MAT[,j], method = "kendall", exact = F)$p.value
  }
}
bg_colors <- MATP> 0.04
bg_colors[bg_colors == T] <- "white"
bg_colors[bg_colors == F] <- "red3"
diag(bg_colors) <- "white"
bg_colors_upper <- bg_colors[upper.tri(bg_colors, diag=F)]

corrplot(A , p.mat = MATP, sig.level =c(0.001,0.01,0.02),insig ="label_sig",
         type="upper",  tl.col=c(rep("midnightblue",5),rep( "darkgreen",4), rep("black",3)),
         tl.srt=45, pch.cex=0.9,pch.col = "yellow",
         diag =F,bg=bg_colors_upper)


MAT =cbind(cbind(STATS_EVTS[ , 4,],data.frame(LAI =VEGE))[-c(1:4) , c(1,2,4,5,8,17,18,19,21,14)], 
           CALIB_EVTS_PM[ ,  c("TTD50","TTDpk","VOL1H")])

A = cor(cbind(cbind(STATS_EVTS[ , 4,],data.frame(LAI =VEGE))[-c(1:4) , c(1,2,4,5,8,17,18,19,21,14)], 
              CALIB_EVTS_PM[ ,  c("TTD50","TTDpk","VOL1H")]), method = "kendall")
MATP = A
for(i in 1:ncol(MAT)){
  for(j in 1:ncol(MAT)){
    
    MATP[i,j]= cor.test(MAT[,i],MAT[,j], method = "kendall", exact = F)$p.value
  }
}
bg_colors <- MATP> 0.02
bg_colors[bg_colors == T] <- "white"
bg_colors[bg_colors == F] <- "red3"
diag(bg_colors) <- "white"
bg_colors_upper <- bg_colors[upper.tri(bg_colors, diag=F)]

corrplot(A , p.mat = MATP, sig.level =c(0.001,0.01,0.02),insig ="label_sig",
         type="upper",   tl.col=c(rep("midnightblue",5),rep( "darkgreen",4), rep("black",3))
         ,tl.srt=45,pch.cex=0.9,pch.col = "yellow",
         diag =F,bg=bg_colors_upper)


dev.off()


png(paste(ad_img, "/spearmanGammaKoedangeMedernach21.png", sep=""),
    width= 6.4, height=3.0, units="in", res=1000, pointsize = 7)
par(mfrow = c(1,2), mar = c(0,0,0,0))
A = rcorr(as.matrix(cbind(cbind(STATS_EVTS[ , 1,],data.frame(LAI =VEGE))[ , c(1,2,4,5,8,17,18,19, 21,14)], 
              CALIB_EVTS_PK[, c("TTD50","TTDpk","VOL1H")])),type = "spearman")
bg_colors <-A$P > 0.05
bg_colors[bg_colors == T] <- "white"
bg_colors[bg_colors == F] <- "red3"
diag(bg_colors) <- "white"
bg_colors_upper <- bg_colors[upper.tri(bg_colors, diag=F)]

corrplot(A$r , p.mat = A$P, sig.level =c(0.001,0.01,0.05),insig ="label_sig",
         type="upper",   tl.col=c(rep("midnightblue",5),rep( "darkgreen",4), rep("black",3)),
         tl.srt=45,pch.cex=0.9,pch.col = "yellow",
         diag =F,bg=bg_colors_upper)

A = rcorr(as.matrix(cbind(cbind(STATS_EVTS[ , 4,],data.frame(LAI =VEGE))[-c(1:4) , c(1,2,4,5,8,17,18,19,21,14)], 
              CALIB_EVTS_PM[ ,  c("TTD50","TTDpk","VOL1H")])), type = "spearman")
bg_colors <-A$P > 0.02
bg_colors[bg_colors == T] <- "white"
bg_colors[bg_colors == F] <- "red3"
diag(bg_colors) <- "white"
bg_colors_upper <- bg_colors[upper.tri(bg_colors, diag=F)]

corrplot(A$r , p.mat = A$P, sig.level =c(0.001,0.01,0.02),insig ="label_sig",
         type="upper",  tl.col=c(rep("midnightblue",5),rep( "darkgreen",4), rep("black",3)),
         tl.srt=45, pch.cex=0.9,pch.col = "yellow",
         diag =F,bg=bg_colors_upper)

dev.off()



png(paste(ad_img, "/HoeffDGammaKoedangeMedernach321.png", sep=""),
    width= 6.4, height=3.0, units="in", res=1000, pointsize = 7)
par(mfrow = c(1,2), mar = c(0,0,0,0))
A = hoeffd(as.matrix(cbind(cbind(STATS_EVTS[ , 1,],data.frame(LAI =VEGE))[ , c(1,2,4,5,8,17,18,19, 21,14)], 
                           CALIB_EVTS_PK[, c("TTD50","TTDpk","VOL1H")])))

A$D[A$D<=0]=0
A$D[A$D==1]=0

bg_colors <-A$P > 0.02
bg_colors[bg_colors == T] <- "white"
bg_colors[bg_colors == F] <- "red3"
diag(bg_colors) <- "white"
bg_colors_upper <- bg_colors[upper.tri(bg_colors, diag=F)]


corrplot(A$D, p.mat = A$P,  cl.lim = c(0,0.66), sig.level =c(0.001,0.01,0.02),insig ="label_sig",
         type="upper",   tl.col=c(rep("midnightblue",5),rep( "darkgreen",4), rep("black",3)),
         tl.srt=45, is.corr = F,pch.cex=0.9,pch.col = "yellow",
         diag =F,bg=bg_colors_upper)


A = hoeffd(as.matrix(cbind(cbind(STATS_EVTS[ , 4,],data.frame(LAI =VEGE))[-c(1:4) , c(1,2,4,5,8,17,18,19,21,14)], 
              CALIB_EVTS_PM[ ,  c("TTD50","TTDpk","VOL1H")])))

A$D[A$D<=0]=0
A$D[A$D==1]=0
bg_colors <-A$P > 0.02
bg_colors[bg_colors == T] <- "white"
bg_colors[bg_colors == F] <- "red3"
diag(bg_colors) <- "white"
bg_colors_upper <- bg_colors[upper.tri(bg_colors, diag=F)]

corrplot(A$D, p.mat = A$P,  cl.lim = c(0,0.65), sig.level =c(0.001,0.01,0.02),insig ="label_sig",
         type="upper",   tl.col=c(rep("midnightblue",5),rep( "darkgreen",4), rep("black",3)),
         tl.srt=45, is.corr = F,pch.cex=0.9,pch.col = "yellow",
         diag =F,bg=bg_colors_upper)
dev.off()



