
# Quantifying storage

Storage_realizations <- function(Inflow1, Inflow2, Storage_initial, Inflow_initial, Outflow_initial, Outflows, delta_t, t, ntimes){
  n<-ntimes
  #browser()
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
  #browser()
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
  #browser()
  Storage_realizations <- rbind(Storage_realizations_GCL, Storage_realizations_LWG, Storage_realizations_MCN)
  
}


#Quantifying Forebay Elevation

Forebay_realizations <- function(Storage_largerealizations, Fb_coeff, t, ntimes){
  n<- ntimes
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




#Quantifying Tailwater

Tailwater_realizations <- function(Forebay_largerealizations, Outflow_initial, Forebay_initial, Tailwater_initial, Tw_coeff, Outflows, t, ntimes){
n<- ntimes
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

#Quantifying Head

Head_realizations <- function(Forebay_largerealizations, Tailwater_largerealizations){
  
  Head_realizations <- Forebay_largerealizations - Tailwater_largerealizations
}


#Quantifying Energy

Energy_realizations <- function(Head_largerealizations, Outflows, efficieny, ntimes){
  e <- efficieny
  n<- ntimes
  Q_GCL <- Outflows[1, ] %>% rep(n) %>% matrix(nrow = n, byrow = TRUE)
  Q_LWG <- Outflows[2, ] %>% rep(n) %>% matrix(nrow = n, byrow = TRUE)
  Q_MCN <- Outflows[3, ]
  Q_out <- rbind(Q_GCL, Q_LWG, Q_MCN) 
  g <- 9.81
  Energy_realizations <- e*Head_largerealizations*0.3048*Q_out*28.31684*g*10^-3
}

#Quantifying mean and standard deviation for the QoI

mean_sd_sim_antithetic <- function(samples, Storage_initial, Inflow_initial, Outflow_initial,  Forebay_initial, Tailwater_initial, Outflows, Fb_coeff, Tw_coeff, delta_t, efficieny, t, r, n_samples){
  #Antithetic approach
  GCLInflowsdata_samples<- samples_antithetic$GCLInflowsdata_samples
  LWGInflowsdata_samples<- samples_antithetic$LWGInflowsdata_samples
  
  Storage_largerealizations <- Storage_realizations(GCLInflowsdata_samples, LWGInflowsdata_samples, Storage_initial, Inflow_initial, Outflow_initial, Outflows, delta_t, t, n_samples)
  
  #browser()
  Forebay_largerealizations <- Forebay_realizations(Storage_largerealizations, Fb_coeff, t, n_samples)
  
  Tailwater_largerealizations <- Tailwater_realizations(Forebay_largerealizations, Outflow_initial,  Forebay_initial, Tailwater_initial, Tw_coeff, Outflows, t, n_samples)
  
  Head_largerealizations <- Head_realizations(Forebay_largerealizations, Tailwater_largerealizations)
  
  Energy_largerealizations <- Energy_realizations(Head_largerealizations, Outflows, efficieny, n_samples)
  n <- n_samples
  
  Storage_mean <- matrix(0L, nrow =r, ncol = t)
  #Storage_mean2 <- matrix(0L, nrow =r, ncol = t)
  Storage_sd <- matrix(0L, nrow =r, ncol = t)
  Forebay_mean <- matrix(0L, nrow =r, ncol = t)
  #Forebay_mean2 <- matrix(0L, nrow =r, ncol = t)
  Forebay_sd <- matrix(0L, nrow =r, ncol = t)
  Energy_mean <- matrix(0L, nrow =r, ncol = t)
  #Energy_mean2 <- matrix(0L, nrow =r, ncol = t)
  Energy_sd <- matrix(0L, nrow =r, ncol = t)
  
  pair_average <- function(x1, x2){
    1/2 * (x1 + x2)
  }
  
  pair_averages_Storage_GCL <- pair_average(Storage_largerealizations[1:(n/2), ], Storage_largerealizations[((n/2)+1):n, ])
  pair_averages_Storage_LWG <- pair_average(Storage_largerealizations[(n+1):((n/2)+n), ], Storage_largerealizations[((n/2)+n+1) :(2*n), ])
  
  
  Storage_mean[1, ] <- colMeans(pair_averages_Storage_GCL)
  Storage_mean[2, ] <- colMeans(pair_averages_Storage_LWG)
  Storage_mean[3, ] <- (Storage_largerealizations[(2*n)+1, ])
  #Storage_mean2[1, ] <- colMeans(pair_averages_Storage_GCL^2)
  #Storage_mean2[2, ] <- colMeans(pair_averages_Storage_LWG^2)
  #Storage_mean2[3, ] <- (Storage_largerealizations[(2*n)+1, ]^2)
  
  #Storage_sd <- sqrt(Storage_mean2 - (Storage_mean^2))
  
  
  
  pair_averages_Forebay_GCL <- pair_average(Forebay_largerealizations[1:(n/2), ], Forebay_largerealizations[((n/2)+1):n, ])
  pair_averages_Forebay_LWG <- pair_average(Forebay_largerealizations[(n+1):((n/2)+n), ], Forebay_largerealizations[((n/2)+n+1) :(2*n), ])
  
  Forebay_mean[1, ] <- colMeans(pair_averages_Forebay_GCL)
  Forebay_mean[2, ] <- colMeans(pair_averages_Forebay_LWG)
  Forebay_mean[3, ] <- (Forebay_largerealizations[(2*n)+1, ])
  #Forebay_mean2[1, ] <- colMeans(pair_averages_Forebay_GCL^2)
  #Forebay_mean2[2, ] <- colMeans(pair_averages_Forebay_LWG^2)
  #Forebay_mean2[3, ] <- (Forebay_largerealizations[(2*n)+1, ]^2)
  
  # Forebay_sd <- sqrt(Forebay_mean2 - (Forebay_mean^2))
  
  pair_averages_Energy_GCL <- pair_average(Energy_largerealizations[1:(n/2), ], Forebay_largerealizations[((n/2)+1):n, ])
  pair_averages_Energy_LWG <- pair_average(Energy_largerealizations[(n+1):((n/2)+n), ], Forebay_largerealizations[((n/2)+n+1):(2*n), ])
  
  Energy_mean[1, ] <- colMeans(pair_averages_Energy_GCL)
  Energy_mean[2, ] <- colMeans(pair_averages_Energy_LWG)
  Energy_mean[3, ] <- (Energy_largerealizations[(2*n)+1, ])
  # Energy_mean2[1, ] <- colMeans(pair_averages_Energy_GCL^2)
  #Energy_mean2[2, ] <- colMeans(pair_averages_Energy_LWG^2)
  #Energy_mean2[3, ] <- (Energy_largerealizations[(2*n)+1, ]^2)
  
  # Energy_sd <- sqrt(Energy_mean2 - (Energy_mean^2))
  
  for (i in 1:t){
    Storage_sd[1,i ] <- sd(pair_averages_Storage_GCL[ ,i])
    Storage_sd[2,i ] <- sd(pair_averages_Storage_LWG[ ,i])
    
    Forebay_sd[1,i ] <- sd(pair_averages_Forebay_GCL[ ,i])
    Forebay_sd[2,i ] <- sd(pair_averages_Forebay_LWG[ ,i])
    
    Energy_sd[1,i ] <- sd(pair_averages_Energy_GCL[ ,i])
    Energy_sd[2,i ] <- sd(pair_averages_Energy_LWG[ ,i])
  } 
  Storage_sd[3, ] <- 0*c(1:t)
  Forebay_sd[3, ] <- 0*c(1:t)
  Energy_sd[3, ] <- 0*c(1:t)
  mean_sim <- rbind(Storage_mean, Forebay_mean, Energy_mean)
  sd_sim <- rbind(Storage_sd, Forebay_sd, Energy_sd)
  
  mean_sd_sim_antithetic <- list("Means" = mean_sim, "Std dev" = sd_sim, "Storage realizations" = Storage_largerealizations, "Forebay realizations" = Forebay_largerealizations, "Tailwater realizations" = Tailwater_largerealizations, "Head realizations" = Head_largerealizations, "Energy_realizations" = Energy_largerealizations)
}

#Quantifying mean and standard deviation for the QoI
mean_sd_sim <- function(samples, Storage_initial, Inflow_initial, Outflow_initial,  Forebay_initial, Tailwater_initial, Outflows, Fb_coeff, Tw_coeff, delta_t, efficieny, t, r, n_samples){
  #MC approach
  GCLInflowsdata_samples<- samples$GCLInflowsdata_samples
  LWGInflowsdata_samples<- samples$LWGInflowsdata_samples
  #browser()
  Storage_largerealizations <- Storage_realizations(GCLInflowsdata_samples, LWGInflowsdata_samples, Storage_initial, Inflow_initial, Outflow_initial, Outflows, delta_t, t, n_samples)
  
  #browser()
  Forebay_largerealizations <- Forebay_realizations(Storage_largerealizations, Fb_coeff, t, n_samples)
  
  Tailwater_largerealizations <- Tailwater_realizations(Forebay_largerealizations, Outflow_initial,  Forebay_initial, Tailwater_initial, Tw_coeff, Outflows, t, n_samples)
  
  Head_largerealizations <- Head_realizations(Forebay_largerealizations, Tailwater_largerealizations)
  
  Energy_largerealizations <- Energy_realizations(Head_largerealizations, Outflows, efficieny, n_samples)
  n <- n_samples
  
  Storage_mean <- matrix(0L, nrow =r, ncol = t)
  Storage_mean2 <- matrix(0L, nrow =r, ncol = t)
  Forebay_mean <- matrix(0L, nrow =r, ncol = t)
  Forebay_mean2 <- matrix(0L, nrow =r, ncol = t)
  Energy_mean <- matrix(0L, nrow =r, ncol = t)
  Energy_mean2 <- matrix(0L, nrow =r, ncol = t)
  
  Storage_mean[1, ] <- colMeans(Storage_largerealizations[1:n, ])
  Storage_mean[2, ] <- colMeans(Storage_largerealizations[(n+1):(2*n), ])
  Storage_mean[3, ] <- (Storage_largerealizations[(2*n)+1, ])
  Storage_mean2[1, ] <- colMeans(Storage_largerealizations[1:n, ]^2)
  Storage_mean2[2, ] <- colMeans(Storage_largerealizations[(n+1):(2*n), ]^2)
  Storage_mean2[3, ] <- (Storage_largerealizations[(2*n)+1, ]^2)
  
  Storage_sd <- sqrt(Storage_mean2 - (Storage_mean^2))
  
  Forebay_mean[1, ] <- colMeans(Forebay_largerealizations[1:n, ])
  Forebay_mean[2, ] <- colMeans(Forebay_largerealizations[(n+1):(2*n), ])
  Forebay_mean[3, ] <- (Forebay_largerealizations[(2*n)+1, ])
  Forebay_mean2[1, ] <- colMeans(Forebay_largerealizations[1:n, ]^2)
  Forebay_mean2[2, ] <- colMeans(Forebay_largerealizations[(n+1):(2*n), ]^2)
  Forebay_mean2[3, ] <- (Forebay_largerealizations[(2*n)+1, ]^2)
  
  Forebay_sd <- sqrt(Forebay_mean2 - (Forebay_mean^2))
  
  Energy_mean[1, ] <- colMeans(Energy_largerealizations[1:n, ])
  Energy_mean[2, ] <- colMeans(Energy_largerealizations[(n+1):(2*n), ])
  Energy_mean[3, ] <- (Energy_largerealizations[(2*n)+1, ])
  Energy_mean2[1, ] <- colMeans(Energy_largerealizations[1:n, ]^2)
  Energy_mean2[2, ] <- colMeans(Energy_largerealizations[(n+1):(2*n), ]^2)
  Energy_mean2[3, ] <- (Energy_largerealizations[(2*n)+1, ]^2)
  
  Energy_sd <- sqrt(Energy_mean2 - (Energy_mean^2))
  
  mean_sim <- rbind(Storage_mean, Forebay_mean, Energy_mean)
  sd_sim <- rbind(Storage_sd, Forebay_sd, Energy_sd)
  
  mean_sd_sim <- list("Means" = mean_sim, "Std dev" = sd_sim, "Storage realizations" = Storage_largerealizations, "Forebay realizations" = Forebay_largerealizations, "Tailwater realizations" = Tailwater_largerealizations, "Head realizations" = Head_largerealizations, "Energy_realizations" = Energy_largerealizations)
}

