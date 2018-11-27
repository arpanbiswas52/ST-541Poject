get_samples_antithetic <- function(t, ntimes){
  GCLInflowsdata_Mean <- rowMeans(GCLInflowsdata[,2:28])
  LWGInflowsdata_Mean <- rowMeans(LWGInflowsdata[,2:28])
  Pricedata_Mean <- colMeans((Pricedata[,2:15]))
  GCLInflowsdata_Mean2 <- rowMeans(GCLInflowsdata[,2:28]^2)
  LWGInflowsdata_Mean2 <- rowMeans(LWGInflowsdata[,2:28]^2)
  Pricedata_Mean2 <- colMeans((Pricedata[,2:15])^2)
  GCLInflowsdata_sd<- sqrt(GCLInflowsdata_Mean2 - GCLInflowsdata_Mean^2)
  LWGInflowsdata_sd<- sqrt(LWGInflowsdata_Mean2 - LWGInflowsdata_Mean^2)
  Pricedata_sd <- sqrt(Pricedata_Mean2 - Pricedata_Mean^2)
  b_Price <- Pricedata_Mean + sqrt(3)*Pricedata_sd
  a_Price <- 2*Pricedata_Mean - b_Price
  
  
  GCLInflows_Mean<- rep(GCLInflowsdata_Mean, ntimes/2)
  GCLInflows_sd<- rep(GCLInflowsdata_sd, ntimes/2)
  LWGInflows_Mean<- rep(LWGInflowsdata_Mean, ntimes/2)
  LWGInflows_sd<- rep(LWGInflowsdata_sd, ntimes/2)
  max_Price<- rep((b_Price), (ntimes^2)/2)
  min_Price <- rep((a_Price), (ntimes^2)/2)
  
  set.seed(1)
  GCLInflowsdata_first_in_pair <- rnorm((ntimes/2)*t, GCLInflows_Mean, GCLInflows_sd) 
  GCLInflowsdata_second_in_pair <- (2*GCLInflows_Mean)- GCLInflowsdata_first_in_pair
  
  GCLInflowsdata_first_in_pair <- matrix(GCLInflowsdata_first_in_pair, nrow = ntimes/2, byrow = TRUE)
  GCLInflowsdata_second_in_pair<- matrix(GCLInflowsdata_second_in_pair, nrow = ntimes/2, byrow = TRUE)
    
  LWGInflowsdata_first_in_pair <- rnorm((ntimes/2)*t, LWGInflows_Mean, LWGInflows_sd) 
  LWGInflowsdata_second_in_pair <- (2*LWGInflows_Mean)- LWGInflowsdata_first_in_pair
  
  LWGInflowsdata_first_in_pair <- matrix(LWGInflowsdata_first_in_pair, nrow = ntimes/2, byrow = TRUE)
  LWGInflowsdata_second_in_pair<- matrix(LWGInflowsdata_second_in_pair, nrow = ntimes/2, byrow = TRUE)
  
  Price_first_in_pair <- runif(((ntimes^2)/2)*t, min_Price, max_Price) 
  Price_second_in_pair <- min_Price + max_Price - Price_first_in_pair
  
  Price_first_in_pair <- matrix(Price_first_in_pair, nrow = ((ntimes^2)/2), byrow = TRUE)
  Price_second_in_pair <- matrix(Price_second_in_pair, nrow = ((ntimes^2)/2), byrow = TRUE)
  
  GCLInflowsdata_samples <- rbind(GCLInflowsdata_first_in_pair, GCLInflowsdata_second_in_pair)
  LWGInflowsdata_samples <- rbind(LWGInflowsdata_first_in_pair, LWGInflowsdata_second_in_pair)
  Price_samples <- rbind(Price_first_in_pair, Price_second_in_pair)
  
  get_samples <- list("GCLInflowsdata_samples" = GCLInflowsdata_samples, "LWGInflowsdata_samples" = LWGInflowsdata_samples,
                      "Price_samples" = Price_samples)
}


get_samples_impsampling <- function(t, ntimes){
  GCLInflowsdata_Mean <- rowMeans(GCLInflowsdata[,2:28])
  LWGInflowsdata_Mean <- rowMeans(LWGInflowsdata[,2:28])
  Pricedata_Mean <- colMeans((Pricedata[,2:15]))
  GCLInflowsdata_Mean2 <- rowMeans(GCLInflowsdata[,2:28]^2)
  LWGInflowsdata_Mean2 <- rowMeans(LWGInflowsdata[,2:28]^2)
  Pricedata_Mean2 <- colMeans((Pricedata[,2:15])^2)
  GCLInflowsdata_sd<- sqrt(GCLInflowsdata_Mean2 - GCLInflowsdata_Mean^2)
  LWGInflowsdata_sd<- sqrt(LWGInflowsdata_Mean2 - LWGInflowsdata_Mean^2)
  Pricedata_sd <- sqrt(Pricedata_Mean2 - Pricedata_Mean^2)
  b_Price <- Pricedata_Mean + sqrt(3)*Pricedata_sd
  a_Price <- 2*Pricedata_Mean - b_Price
  
  
  GCLInflows_Mean<- rep(GCLInflowsdata_Mean, ntimes)
  GCLInflows_sd<- rep(GCLInflowsdata_sd, ntimes)
  LWGInflows_Mean<- rep(LWGInflowsdata_Mean, ntimes)
  LWGInflows_sd<- rep(LWGInflowsdata_sd, ntimes)
  max_Price<- rep((b_Price), ntimes^2)
  min_Price <- rep((a_Price), ntimes^2)
  
  GCLInflowsdata_samples <- rnorm(ntimes*t, GCLInflows_Mean, GCLInflows_sd) 
  weights_GCLInflowsdata_samples <- dnorm(GCLInflowsdata_samples, mean = GCLInflows_Mean, sd = 1)/
    dnorm(GCLInflowsdata_samples,GCLInflows_Mean,GCLInflows_sd)%>% matrix(nrow = ntimes, byrow = TRUE)
  matrix(GCLInflowsdata_samples, nrow = ntimes, byrow = TRUE)
  
  LWGInflowsdata_samples <- rnorm(ntimes*t, LWGInflows_Mean, LWGInflows_sd) 
  weights_LWGInflowsdata_samples <- dnorm(LWGInflowsdata_samples, mean =LWGInflowsdata_Mean, sd = 1)/
    dnorm(LWGInflowsdata_samples,LWGInflowsdata_Mean,LWGInflows_sd)%>% matrix(nrow = ntimes, byrow = TRUE)
  matrix(LWGInflowsdata_samples, nrow = ntimes, byrow = TRUE)
  
  Price_samples <- runif((ntimes^2)*t, min_Price, max_Price)
  weights_Price_samples <- dunif(Price_samples, 0, 1)/
    runif(Price_samples,min_Price,max_Price)%>% matrix(nrow = ntimes, byrow = TRUE)
  matrix(Price_samples, nrow = ntimes, byrow = TRUE)
  
  get_samples <- list("GCLInflowsdata_samples" = GCLInflowsdata_samples, "LWGInflowsdata_samples" = LWGInflowsdata_samples,
                      "Price_samples" = Price_samples, "weights_GCLInflowsdata_samples" = weights_GCLInflowsdata_samples,
                      "weights_LWGInflowsdata_samples" = weights_LWGInflowsdata_samples, "weights_Price_samples" = weights_Price_samples)
}





#samples_antithetic <- get_samples_antithetic(t,ntimes)

#mean_sd_simulations_antithetic <- mean_sd_sim_antithetic(samples_antithetic, Current_Storage, Current_Inflows, Current_Outflows, Current_Forebay, Current_Tailwater, Outflows, Fb_coeff, Tw_coeff, delta_t=1, efficieny=0.75, t=14, r=3, 100)




#Quantifying mean and standard deviation for the QoI
##Antithetic approach
mean_sd_sim_antithetic <- function(samples, Storage_initial, Inflow_initial, Outflow_initial,  Forebay_initial, Tailwater_initial, Outflows, Fb_coeff, Tw_coeff, delta_t, efficieny, t, r, n_samples){
  GCLInflowsdata_samples<- samples_antithetic$GCLInflowsdata_samples
  LWGInflowsdata_samples<- samples_antithetic$LWGInflowsdata_samples
  
  Storage_largerealizations <- Storage_realizations(GCLInflowsdata_samples, LWGInflowsdata_samples, Current_Storage, Current_Inflows, Current_Outflows, Outflows, delta_t, t, n_samples)
  
  #browser()
  Forebay_largerealizations <- Forebay_realizations(Storage_largerealizations, Fb_coeff, t, n_samples)
  
  Tailwater_largerealizations <- Tailwater_realizations(Forebay_largerealizations, Current_Outflows, Current_Forebay, Current_Tailwater, Tw_coeff, Outflows, t, n_samples)
  
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