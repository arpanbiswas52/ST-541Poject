#```{r}
# Quantifying storage

Storage_realizations <- function(Inflow1, Inflow2, Storage_initial, Inflow_initial, Outflow_initial, Outflows, delta_t, t, n){
  GCLInflowsdata_samples <- Inflow1
  LWGInflowsdata_samples <- Inflow2
  V_in <- Storage_initial
  I_in <- Inflow_initial
  Q_in <- Outflow_initial
  Q_GCL <- Outflows[1, ]
  Q_LWG <- Outflows[2, ]
  Q_MCN <- Outflows[3, ]
  MCNInflows <- Outflows[1, ] + Outflows[2, ]
  Storage_realizations_GCL <- matrix(0L, nrow =n, ncol = t)
  Storage_realizations_LWG <- matrix(0L, nrow =n, ncol = t)
  Storage_realizations_MCN <- matrix(0L, nrow =1, ncol = t)
  #1st timestep
  Storage_realizations_GCL[, 1] <- ((I_in[1] + GCLInflowsdata_samples[, 1])*0.5) + ((Q_in[1] + Q_GCL[1])*0.5)*delta_t + V_in[1]
  
  Storage_realizations_LWG[, 1] <- ((I_in[2] + LWGInflowsdata_samples[, 1])*0.5 + (Q_in[2] + Q_LWG[1])*0.5)*delta_t + V_in[2]
  
  Storage_realizations_MCN[, 1] <- ((I_in[3] + MCNInflows[1])*0.5 + (Q_in[3] + Q_MCN[1])*0.5)*delta_t + V_in[3]
  
  #Rest of the timesteps
  
  for (i in 2:t){
    Storage_realizations_GCL[, i] <- ((GCLInflowsdata_samples[, i-1] + GCLInflowsdata_samples[, i])*0.5 + (Q_GCL[i-1] + Q_GCL[i])*0.5)*delta_t +Storage_realizations_GCL[, i-1]
    
    Storage_realizations_LWG[, i] <- ((LWGInflowsdata_samples[, i-1] + LWGInflowsdata_samples[, i])*0.5 + (Q_LWG[i-1] + Q_LWG[i])*0.5)*delta_t +Storage_realizations_LWG[, i-1]
    
    Storage_realizations_MCN[i] <- ((MCNInflows[i-1] + MCNInflows[i])*0.5 + (Q_MCN[i-1] + Q_MCN[i])*0.5)*delta_t +Storage_realizations_MCN[i-1]
  }
  
  Storage_realizations <- rbind(Storage_realizations_GCL, Storage_realizations_LWG, Storage_realizations_MCN)
  
}

#```
#```{r}
#Storage_largerealizations <- Storage_realizations(GCLInflowsdata_samples, LWGInflowsdata_samples, Current_Storage, Current_Inflows, Current_Outflows, Outflows, delta_t = 1, t=14, ntimes)

#```



#```{r}
#Quantifying Forebay Elevation

Forebay_realizations <- function(Storage_largerealizations, Fb_coeff, t, n){
  Storage_realizations_GCL <- Storage_largerealizations[1:n,]
  Storage_realizations_LWG <- Storage_largerealizations[(n+1):(2*n),]
  Storage_realizations_MCN <- Storage_largerealizations[(2*n)+1,]
  Forebay_realizations_GCL <- matrix(0L, nrow =n, ncol = t)
  Forebay_realizations_LWG <- matrix(0L, nrow =n, ncol = t)
  Forebay_realizations_MCN <- matrix(0L, nrow =1, ncol = t)
  
  Forebay_realizations_GCL <- Fb_coeff[1,1]* Storage_realizations_GCL^2 + Fb_coeff[1,2]* Storage_realizations_GCL + Fb_coeff[1,3]
  
  Forebay_realizations_LWG <- Fb_coeff[2,1]* Storage_realizations_LWG^2 + Fb_coeff[2,2]* Storage_realizations_LWG + Fb_coeff[2,3]
  
  Forebay_realizations_MCN <- Fb_coeff[3,1]* Storage_realizations_MCN^2 + Fb_coeff[3,2]* Storage_realizations_MCN + Fb_coeff[3,3]
  
  Forebay_realizations <- rbind(Forebay_realizations_GCL, Forebay_realizations_LWG, Forebay_realizations_MCN)
}


#```

#```{r}
#Forebay_largerealizations <- Forebay_realizations(Storage_largerealizations, Fb_coeff, t=14, ntimes)

#```


#```{r}

#Quantifying Tailwater

Tailwater_realizations <- function(Forebay_largerealizations, Outflow_initial, Forebay_initial, Tailwater_initial, Tw_coeff, Outflows, t, n){
  
  Q_GCL <- Outflows[1, ]
  Q_LWG <- Outflows[2, ]
  Q_MCN <- Outflows[3, ]
  Q_in <- Outflow_initial
  Fb_in <- Forebay_initial
  Tw_in <- Tailwater_initial
  Tailwater_realizations_GCL <- matrix(0L, nrow =n, ncol = t)
  Tailwater_realizations_LWG <- matrix(0L, nrow =n, ncol = t)
  Tailwater_realizations_MCN <- matrix(0L, nrow =1, ncol = t)
  
  #1st timestep
  Tailwater_realizations_GCL[, 1] <- Tw_coeff[1,1] + Tw_coeff[2,1]*Q_GCL[1] +Tw_coeff[3,1]*Fb_in[3]
  
  Tailwater_realizations_LWG[, 1] <- Tw_coeff[1,2] + Tw_coeff[2,2]*Q_LWG[1] +Tw_coeff[3,2]*Fb_in[3]
  
  Tailwater_realizations_MCN[, 1] <- Tw_coeff[1,3] + Tw_coeff[2,3]*Tw_in[3] +Tw_coeff[3,3]*(Q_MCN[1] - Q_in[3])
  
  
  #Rest of the timestep
  
  for (i in 2:t){
    Tailwater_realizations_GCL[, i] <- Tw_coeff[1,1] + Tw_coeff[2,1]*Q_GCL[i] + Tw_coeff[3,1]*Forebay_largerealizations[(2*n)+1, i-1]
    
    Tailwater_realizations_LWG[, i] <- Tw_coeff[1,2] + Tw_coeff[2,2]*Q_LWG[i] + Tw_coeff[3,2]*Forebay_largerealizations[(2*n)+1, i-1]
    
    Tailwater_realizations_MCN[, i] <- Tw_coeff[1,3] + Tw_coeff[2,3]*Tailwater_realizations_MCN[, i-1] +Tw_coeff[3,3]*(Q_MCN[i] - Q_MCN[i-1])
  }
  
  Tailwater_realizations <- rbind(Tailwater_realizations_GCL, Tailwater_realizations_LWG, Tailwater_realizations_MCN)
}
#```

#```{r}
#Tailwater_largerealizations <- Tailwater_realizations(Forebay_largerealizations, Current_Outflows, Current_Forebay, Current_Tailwater, Tw_coeff, Outflows, t=14, ntimes)
#```

#```{r}
#Quantifying Head

Head_realizations <- function(Forebay_largerealizations, Tailwater_largerealizations){
  
  Head_realizations <- Forebay_largerealizations - Tailwater_largerealizations
}

#```

#```{r}
#Head_largerealizations <- Head_realizations(Forebay_largerealizations, Tailwater_largerealizations)

#```

#```{r}
#Quantifying Energy

Energy_realizations <- function(Head_largerealizations, Outflows, efficieny, n){
  e <- efficieny
  Q_GCL <- Outflows[1, ] %>% rep(n) %>% matrix(nrow = n, byrow = TRUE)
  Q_LWG <- Outflows[2, ] %>% rep(n) %>% matrix(nrow = n, byrow = TRUE)
  Q_MCN <- Outflows[3, ]
  Q_out <- rbind(Q_GCL, Q_LWG, Q_MCN) 
  g <- 9.81
  Energy_realizations <- e*Head_largerealizations*0.3048*Q_out*28.31684*g*10^-3
}

#```
#```{r}
#Energy_largerealizations <- Energy_realizations(Head_largerealizations, Outflows, efficieny= 0.75, ntimes)

#```
