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
#' represent a single realization, \code{2*ntimes+1} and each columns represent the time steps,\code{t} in days.
#' The first \code{ntimes} rows are the simulations for GrandCoulee, next \code{ntimes} rows are the simulations for Lower Granite
#' and the last row of the matrix is the deterministic Energy generation data for McNaire. McNaire Inflows doesnot have any uncertainity
#' since it is the total outflows released from GCL and LWG which are deterministic. In future, we can introduce other source
#' of uncertainity in the inflows of McNaire.
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

#' Evaluation of Robust Objective Function
#'
#' Calculating the robust objective function value for \code{t} days optimization period
#' 
#'
#' Quantifies the robust objective considering the uncertainities in inflows and Prices of electricity.
#' To calculate the robust objective, the function calls two other functions:Firstly, \code{mean_sd_sim()} or \code{mean_sd_sim_antithetic()}
#' based on the user choice to go for MC or Antithetic Variable approach to generate simulations of energy.
#' After that, another function \code{Total_Revenue_cal()} is called which provides mean and standard deviation of the total revenue for 
#' \code{t} days optimization period. 
#'    
#'
#' @param X The is the vector of outflows data (decision variables) generated by SQP algorithm. It is a vector 
#' of length \code{t}*number of reservoirs. For the case of \code{t} =14 and 3 reservoir system, the length of 
#' \code{X} = 14*3 = 42 
#' @param samples A large list of \code{ntimes} random normal samples of Inflows and \code{ntimes^2} random uniform samples of Price for \code{t} = 14 days.
#' For example, in the samples of Inflows to Grand Coulle reservoir (GCLInflowsdata_samples)
#' inside the list, each rows represent a single realization, \code{ntimes} and each columns represent the time steps,\code{t} in days  
#' @param initial_cond A large list for all the initial conditions required 
#' for the functions \code{mean_sd_sim()}, \code{mean_sd_sim_antithetic()} and \code{Total_Revenue_cal()} 
#' which are called inside this function 
#' 
#'
#' @return An integer value of Robust objective function
#' @export
#'
#' @examples
#' X <- Outflows
#' samples <- samples
#' initial_cond <- initial_cond
#' obj_Func_eval(X, samples, initial_cond)

obj_Func_eval <- function(X, samples, initial_cond){
  
  #browser()
  Price <- samples$Price_samples
  Current_Storage <- initial_cond$Current_Storage
  Current_Inflows <- initial_cond$Current_Inflows
  Current_Outflows <- initial_cond$Current_Outflows
  Current_Forebay <- initial_cond$Current_Forebay
  Current_Tailwater <- initial_cond$Current_Tailwater
  Fb_coeff<- initial_cond$Fb_coeff
  Tw_coeff<- initial_cond$Tw_coeff
  t<- initial_cond$t
  r<- initial_cond$r
  ntimes <- initial_cond$ntimes
  methodSampling <- initial_cond$methodSampling
  Outflows<- X %>% matrix(ncol = t, byrow = TRUE)
  #browser()
  if (methodSampling == 1){
    mean_sd_simulations <- mean_sd_sim_antithetic(samples, Current_Storage, Current_Inflows, Current_Outflows, Current_Forebay, 
                                                  Current_Tailwater, Outflows, Fb_coeff, Tw_coeff, delta_t=1, 
                                                  efficieny=0.75, t, r, ntimes)
  }else if (methodSampling == 2) {
    mean_sd_simulations <- mean_sd_sim(samples, Current_Storage, Current_Inflows, Current_Outflows, Current_Forebay, 
                                       Current_Tailwater, Outflows, Fb_coeff, Tw_coeff, delta_t=1, 
                                       efficieny=0.75, t, r, ntimes)
  } else {
    print("Invalid Option: Choose methodSampling between 1 or 2")
  }
  #browser()
  
  
  Total_Revenue_stat <- Total_Revenue_cal(mean_sd_simulations, Price, t, ntimes)
  
  #Calculating Robust objective for maximizing the objective function
  
  Risk_coeff <- 10^6
  
  Robust_obj <- Total_Revenue_stat[1] - ((Total_Revenue_stat[2]^2)/(2*Risk_coeff))
  
  obj_Func_eval <- -Robust_obj
  #browser()
}

