#' Quantifying the Total Revenue
#'
#' Quantifying the Total Revenue for \code{t} days optimization period
#' 
#'
#' Quantifies the total revenue generated for \code{t} days considering the uncertainities in inflows and Prices of electricity.
#' This function is required to evaluate the objective function in the model. For example, if \code{ntimes} = 100 and 
#' \code{t} = 14, due to 2 sources of inflows uncertaitinity from Grand Coulee and Lower Granite reservoirs, the total 
#' number of simulations will be \code{ntimes^2} = 10000  
#'
#' @param mean_sd_simulations A large list of simulations along with the mean and standard deviation
#' for Storage, Forebay, Energy etc. For example, in the simulation matrix of Energy (Energy_realizations) inside the list, each rows
#' represent a single realization, \code{ntimes} and each columns represent the time steps,\code{t} in days  
#' @param Price A large matrix of \code{ntimes^2} simulations of Price for \code{t} = 14 days. Each rows
#' represent a single realization and each columns represent the time steps,\code{t} in days   
#' @param t An integer indicating the number of timesteps. If we consider daily timestep for 14 day optimization
#' period, \code{t} = 14. 
#' @param ntimes An integer indicating the number of simulations/realizations.
#'
#' @return A vector of length = 2 with the mean and standard deviation of the total revenue for 
#' \code{t} days optimization period like \code{c(Total_Revenue_Mean, Total_Revenue_sd)}  
#' @export
#'
#' @examples
#' mean_sd_simulations <- mean_sd_simulations
#' Price <- Price_samples
#' t <- 14
#' ntimes <-100
#' Total_Revenue_cal(mean_sd_simulations, Price, t, ntimes)
Total_Revenue_cal <- function(mean_sd_simulations, Price, t, ntimes){
  
  n<-ntimes
  Energy_realizations_GCL <- mean_sd_simulations$`Energy_realizations`[1:n,] %>% t(.) %>% rep(n) %>% matrix(ncol=t, byrow= TRUE)
  
  Energy_realizations_LWG<- do.call(cbind, replicate(n,mean_sd_simulations$`Energy_realizations`[(n+1):(2*n),], simplify=FALSE)) %>% t(.) %>% matrix(ncol=t, byrow = TRUE)
  
  
  Energy_realizations_MCN <- do.call(rbind, replicate(n*n,mean_sd_simulations$`Energy_realizations`[(2*n +1),], simplify=FALSE))
  
  Total_Energy_realizations <- 24*(Energy_realizations_GCL+Energy_realizations_LWG+Energy_realizations_MCN)
  
  Total_Revenue_Realizations <- rowSums(Price*Total_Energy_realizations)
  
  Total_Revenue_Mean <- mean(Total_Revenue_Realizations)
  Total_Revenue_Mean2 <- mean(Total_Revenue_Realizations^2)
  Total_Revenue_sd <- sqrt(Total_Revenue_Mean2 - (Total_Revenue_Mean^2))
  Total_Revenue_stat = c(Total_Revenue_Mean, Total_Revenue_sd)
}
