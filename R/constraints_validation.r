
#' Validating Storage Constraints
#'
#' Validates Probabilistic Storage Constraints with Reliability \code{rel_index} = 0.9
#' 
#' Validates Probabilistic Storage Constraints with Reliability \code{rel_index} = 0.9
#'
#' @param mean_sd_simulations A large list of simulations along with the mean and standard deviation
#' for Storage, Forebay, Energy etc. For example, in the simulation matrix of Energy (Energy_realizations) inside the list, each rows
#' represent a single realization, \code{2*ntimes+1} and each columns represent the time steps,\code{t} in days.
#' The first \code{ntimes} rows are the simulations for GrandCoulee, next \code{ntimes} rows are the simulations for Lower Granite
#' and the last row of the matrix is the deterministic Energy generation data for McNaire. McNaire Inflows doesnot have any uncertainity
#' since it is the total outflows released from GCL and LWG which are deterministic. In future, we can introduce other source
#' of uncertainity in the inflows of McNaire.  
#' @param Storage_min A vector of length 3 with minimum storage constraints of Grand Coulee, Lower Granite and McNaire
#' for all \code{t} days. This is provided in the model as initial condition of the optimization.
#' @param Storage_max A vector of length 3 with maximum storage constraints of Grand Coulee, Lower Granite and McNaire
#' for all \code{t} days. This is provided in the model as initial condition of the optimization.
#' @param rel_index Reliability index which is set as 0.9
#' @param r An integer indicating the number of reservoirs. In this case study, \code{r}=3
#' @param t An integer indicating the number of timesteps. If we consider daily timestep for 14 day optimization
#' period, \code{t} = 14.
#'
#' @return A matrix of constraints validated values where positive number is the satisfaction of the constraints by
#' @export
#'
#' @examples
#' samples <- samples
#' Storage_initial <- Current_Storage
#' Inflow_initial <- Current_Inflows
#' Outflow_initial <- Current_Outflows
#' Forebay_initial <- Current_Forebay
#' Tailwater_initial <- Current_Tailwater
#' Outflows <- Outflows_matrixform
#' Fb_coeff <- Fb_coeff
#' Tw_coeff <- Tw_coeff
#' delta_t <- 1
#' efficieny <- 0.75
#' t <- 14
#' r<- 3
#' n_samples <-500
#' mean_sd_sim(samples, Storage_initial, Inflow_initial, Outflow_initial,  Forebay_initial, Tailwater_initial, Outflows, Fb_coeff, Tw_coeff, delta_t, efficieny, t, r, n_samples)

Storage_constraints <- function(mean_sd_simulations, Storage_min, Storage_max, rel_index, r, t){
  V_min <- rep(Storage_min, t) %>% matrix(ncol = t) 
  V_max <- rep(Storage_max, t) %>% matrix(ncol = t)
  Storage_mean <- mean_sd_simulations$`Means`[1:r,]
  Storage_sd <- mean_sd_simulations$`Std dev`[1:r,]
  V_rel_high <- qnorm(rel_index, Storage_mean, Storage_sd)
  V_rel_low <- qnorm((1-rel_index), Storage_mean, Storage_sd)
  
  MinStorage_val <- V_min - V_rel_low
  MaxStorage_val <- - V_max + V_rel_high
  
  Storage_constraints <- cbind(-MinStorage_val, -MaxStorage_val)
} 


#Validating Forebay Constraints

Forebay_constraints <- function(mean_sd_simulations, Forebay_min, Forebay_max, rel_index, r, t){
  Fb_min <- rep(Forebay_min, t) %>% matrix(ncol = t) 
  Fb_max <- rep(Forebay_max, t) %>% matrix(ncol = t)
  Forebay_mean <- mean_sd_simulations$`Means`[(r+1):(2*r),]
  Forebay_sd <- mean_sd_simulations$`Std dev`[(r+1):(2*r),]
  Fb_rel_high <- qnorm(rel_index, Forebay_mean, Forebay_sd)
  Fb_rel_low <- qnorm((1-rel_index), Forebay_mean, Forebay_sd)
  
  MinForebay_val <- Fb_min - Fb_rel_low
  MaxForebay_val <- - Fb_max + Fb_rel_high
  
  Forebay_constraints <- cbind(-MinForebay_val, -MaxForebay_val)
} 


#Validating Energy Constraints

  Energy_constraints <- function(mean_sd_simulations, Energy_min, Energy_max, rel_index, r, t){
  E_min <- rep(Energy_min, t) %>% matrix(ncol = t) 
  E_max <- rep(Energy_max, t) %>% matrix(ncol = t)
  E_mean <- mean_sd_simulations$`Means`[((2*r)+1):(3*r),]
  E_sd <- mean_sd_simulations$`Std dev`[((2*r)+1):(3*r),]
  E_rel_high <- qnorm(rel_index, E_mean, E_sd)
  E_rel_low <- qnorm((1-rel_index), E_mean, E_sd)
  
  MinForebay_val <- E_min - E_rel_low
  MaxForebay_val <- - E_max + E_rel_high
  
  Energy_constraints <- cbind(-MinForebay_val, -MaxForebay_val)
} 


# Validating Forebay elevation at end of optimization period, t=14
# This constraint is only for Grand Coulee reservoir

ForebayEndofPeriod_constraints <- function(mean_sd_simulations, Fb_target, rel_index, r, t){
  Fbtarget_lb <- 0.5*Fb_target 
  Fbtarget_ub <- 0.5*Fb_target
  Forebay_mean <- mean_sd_simulations$`Means`[(r+1),t] %>% t(.)
  Forebay_sd <- mean_sd_simulations$`Std dev`[(r+1),t] %>% t(.)
  Fbend_rel_high <- qnorm(rel_index, Forebay_mean, Forebay_sd)
  Fbend_rel_low <- qnorm((1-rel_index), Forebay_mean, Forebay_sd)
  
  MinForebayEOP_val <- Fb_target - Fbtarget_lb - Fbend_rel_low
  MaxForebayEOP_val <- - Fb_target - Fbtarget_ub  + Fbend_rel_high
  
  ForebayEndofPeriod_constraints <- cbind(-MinForebayEOP_val, -MaxForebayEOP_val)
  ForebayEndofPeriod_constraints <- rbind(ForebayEndofPeriod_constraints, c(1,1),c(1,1))
} 

#This function calls all the constraints validation function from R and return a list of data where if value >= 0 means the constraint is satisfied and if value <0 means constraint is violated

Constraints_validation <- function(X){
  GCLInflowsdata <- read_csv("data/GCLInflowsdata.csv")
  LWGInflowsdata <- read_csv("data/LWGInflowsdata.csv")
  Pricedata <- read_csv("data/Pricedata.csv")
  #Setting initial conditions
  t <- 14 # number of daily timesteps for 14 days
  r <- 3 # number of reservoirs = 3 
  Outflows<- X %>% matrix(ncol = t, byrow = TRUE)
  #browser()
  rel_index <- 0.9
  ntimes<- 100
  Current_Inflows <- c(81.45, 11.04, 131.27) 
  Current_Outflows <- c(55.72, 13.54, 127.03)
  Current_Storage <- c(2260, 47.8, 79.1)
  Current_Forebay <- c(1280.9, 735.93, 339.14)
  Current_Tailwater <- c(957.46, 636.41, 265.03)
  Outflows_lb <- c(1.13, 8.2, 7)
  Outflows_ub <- c(294.04, 150, 214.2) 
  Fb_coeff <- matrix(c(-3.63e-6, 0.0406, 1208, -3.6467e-4, 0.2689, 724, 0, 0.0571, 334.5), nrow = 3, ncol = 3, byrow = TRUE)
  Tw_coeff <- matrix(c(447.97, 122.81, 18.60, 0.0910, 0.0210, 0.0202, 0.5286, 0.8060, 0.9234), nrow = 3, ncol = 3, byrow = TRUE)
  Storage_min <- c(0, 0, 0) 
  Storage_max <- c(2614.3, 56.2, 105)
  Forebay_min <- c(1208, 733, 335)
  Forebay_max <- c(1290, 738, 340)
  Energy_min <- c(0, 0, 250)
  Energy_max <- c(6735, 930, 1120)
  Fb_target <- 1281 
  
  #set.seed(1)
  samples <- get_samples(t, ntimes,GCLInflowsdata,LWGInflowsdata,Pricedata)
  #samples_antithetic <- get_samples_antithetic(t, ntimes,GCLInflowsdata,LWGInflowsdata,Pricedata)
  #samples <- samples_antithetic
  
  
  #Current_Storage <- initial_cond$Current_Storage
  #Current_Inflows <- initial_cond$Current_Inflows
  #Current_Outflows <- initial_cond$Current_Outflows
  #Current_Forebay <- initial_cond$Current_Forebay
  #Current_Tailwater <- initial_cond$Current_Tailwater
  #Fb_coeff<- initial_cond$Fb_coeff
  #Tw_coeff<- initial_cond$Tw_coeff
  #t<- initial_cond$t
  #r<- initial_cond$r
  #ntimes <- initial_cond$ntimes
  #rel_index <- initial_cond$rel_index
  #Storage_min <- initial_cond$Storage_min
  #Storage_max <- initial_cond$Storage_max
  #Forebay_min <- initial_cond$Forebay_min
  #Forebay_max <- initial_cond$Forebay_max
  #Energy_min <- initial_cond$Energy_min
  #Energy_max <- initial_cond$Energy_max
  #Fb_target <- initial_cond$Fb_target
  
  
  mean_sd_simulations <- mean_sd_sim(samples, Current_Storage, Current_Inflows, Current_Outflows, Current_Forebay, 
                                     Current_Tailwater, Outflows, Fb_coeff, Tw_coeff, delta_t=1, 
                                     efficieny=0.75, t, r, ntimes)
  #mean_sd_simulations <- mean_sd_sim_antithetic(samples, Current_Storage, Current_Inflows, Current_Outflows, Current_Forebay, 
  #                                              Current_Tailwater, Outflows, Fb_coeff, Tw_coeff, delta_t=1, 
  #                                              efficieny=0.75, t, r, ntimes)
  
  Storage_constraints_validation <- Storage_constraints(mean_sd_simulations, Storage_min, Storage_max, rel_index, r, t) 
  
  Forebay_constraints_validation <- Forebay_constraints(mean_sd_simulations, Forebay_min, Forebay_max, rel_index, r, t)
  
  Energy_constraints_validation <- Energy_constraints(mean_sd_simulations, Energy_min, Energy_max, rel_index, r, t) 
  
  ForebayEndofPeriod_constraints_validation <- ForebayEndofPeriod_constraints(mean_sd_simulations, Fb_target, rel_index, r, t)
  
  Constraints_validation_matrix <- cbind(Storage_constraints_validation, 
                                  Forebay_constraints_validation, 
                                  Energy_constraints_validation, 
                                  ForebayEndofPeriod_constraints_validation)
  Constraints_validation <- as.vector(Constraints_validation_matrix)
 #browser()
}
