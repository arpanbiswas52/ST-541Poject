#' Generating random samples
#'
#' Generating random samples using Antithetic Variable approach
#' 
#'
#' Generating \code{ntimes} normal random samples of inflows of Grand Coulee and Lower Granite for each day of 
#' of the optimization period, based on the means and standard deviations of 27 predicted realizations 
#' of historic inflows data. Also generates \code{ntimes^2} uniform random samples of Price for each day of the
#' optimization period, based on the means and standard deviations of 5 predicted realizations. 
#'
#' @param t An integer indicating the number of timesteps. If we consider daily timestep for 14 day optimization
#' period, \code{t} = 14. 
#' @param ntimes An integer indicating the number of simulations/realizations.
#' @param GCLInflowsdata A matrix of inputs of predicted Inflows of GCL where each row (m) is  
#' a day of optimization period \code{t} and each column (n) represent a single realization. The first column constains
#' the characters mentioning the days of the inflows.
#' @param LWGInflowsdata A matrix of inputs of predicted Inflows of LWG where each row (m) is  
#' a day of optimization period \code{t} and each column (n) represent a single realization. The first column constains
#' the characters mentioning the days of the inflows.
#' @param Pricedata A matrix of inputs of predicted Price where each row (m) represent a single realization  
#' and each column (n) is a day of optimization period \code{t}. The first column constains
#' the characters mentioning the realization number.
#' 
#'
#' @return A large list of \code{ntimes} random normal samples of Inflows and \code{ntimes^2} random uniform samples of Price for \code{t} = 14 days.
#' For example, in the samples of Inflows to Grand Coulle reservoir (GCLInflowsdata_samples)
#' inside the list, each rows represent a single realization, \code{ntimes} and each columns represent the time steps,\code{t} in days  
#' @export
#'
#' @examples
#' t <- 14
#' ntimes <-100
#' GCLInflowsdata <- GCLInflowsdata
#' LWGInflowsdata <- LWGInflowsdata
#' Pricedata<-Pricedata
#' get_samples_antithetic(t, ntimes,GCLInflowsdata,LWGInflowsdata,Pricedata)
get_samples_antithetic <- function(t, ntimes,GCLInflowsdata,LWGInflowsdata,Pricedata){
  #Antithetic Variable approach
  GCLInflowsdata_Mean <- rowMeans(GCLInflowsdata[,2:length(GCLInflowsdata)])
  LWGInflowsdata_Mean <- rowMeans(LWGInflowsdata[,2:length(LWGInflowsdata)])
  Pricedata_Mean <- colMeans((Pricedata[,2:length(Pricedata)]))
  GCLInflowsdata_Mean2 <- rowMeans(GCLInflowsdata[,2:length(GCLInflowsdata)]^2)
  LWGInflowsdata_Mean2 <- rowMeans(LWGInflowsdata[,2:length(LWGInflowsdata)]^2)
  Pricedata_Mean2 <- colMeans((Pricedata[,2:length(Pricedata)])^2)
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

#' Generating random samples
#'
#' Generating random samples using Monte Carlo approach
#' 
#'
#' Generating \code{ntimes} normal random samples of inflows of Grand Coulee and Lower Granite for each day of 
#' of the optimization period, based on the means and standard deviations of 27 predicted realizations 
#' of historic inflows data. Also generates \code{ntimes^2} uniform random samples of Price for each day of the
#' optimization period, based on the means and standard deviations of 5 predicted realizations. 
#'
#' @param t An integer indicating the number of timesteps. If we consider daily timestep for 14 day optimization
#' period, \code{t} = 14. 
#' @param ntimes An integer indicating the number of simulations/realizations.
#' @param GCLInflowsdata A matrix of inputs of predicted Inflows of GCL where each row (m) is  
#' a day of optimization period \code{t} and each column (n) represent a single realization. The first column constains
#' the characters mentioning the days of the inflows.
#' @param LWGInflowsdata A matrix of inputs of predicted Inflows of LWG where each row (m) is  
#' a day of optimization period \code{t} and each column (n) represent a single realization. The first column constains
#' the characters mentioning the days of the inflows.
#' @param Pricedata A matrix of inputs of predicted Price where each row (m) represent a single realization  
#' and each column (n) is a day of optimization period \code{t}. The first column constains
#' the characters mentioning the realization number.
#' 
#'
#' @return A large list of \code{ntimes} random normal samples of Inflows and \code{ntimes^2} random uniform samples of Price for \code{t} = 14 days.
#' For example, in the samples of Inflows to Grand Coulle reservoir (GCLInflowsdata_samples)
#' inside the list, each rows represent a single realization, \code{ntimes} and each columns represent the time steps,\code{t} in days  
#' @export
#'
#' @examples
#' t <- 14
#' ntimes <-500
#' GCLInflowsdata <- GCLInflowsdata
#' LWGInflowsdata <- LWGInflowsdata
#' Pricedata<-Pricedata
#' get_samples(t, ntimes,GCLInflowsdata,LWGInflowsdata,Pricedata)
 
get_samples <- function(t, ntimes,GCLInflowsdata,LWGInflowsdata,Pricedata){
  # MC approach
  GCLInflowsdata_Mean <- rowMeans(GCLInflowsdata[,2:length(GCLInflowsdata)])
  LWGInflowsdata_Mean <- rowMeans(LWGInflowsdata[,2:length(LWGInflowsdata)])
  Pricedata_Mean <- colMeans((Pricedata[,2:length(Pricedata)]))
  GCLInflowsdata_Mean2 <- rowMeans(GCLInflowsdata[,2:length(GCLInflowsdata)]^2)
  LWGInflowsdata_Mean2 <- rowMeans(LWGInflowsdata[,2:length(LWGInflowsdata)]^2)
  Pricedata_Mean2 <- colMeans((Pricedata[,2:length(Pricedata)])^2)
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
  set.seed(1)
  GCLInflowsdata_samples <- rnorm(ntimes*t, GCLInflows_Mean, GCLInflows_sd) %>% matrix(nrow = ntimes, byrow = TRUE)
  
  LWGInflowsdata_samples <- rnorm(ntimes*t, LWGInflows_Mean, LWGInflows_sd) %>% matrix(nrow = ntimes, byrow = TRUE)
  
  Price_samples <- runif((ntimes^2)*t, min_Price, max_Price) %>% matrix(nrow = (ntimes^2), byrow = TRUE)
  get_samples <- list("GCLInflowsdata_samples" = GCLInflowsdata_samples, "LWGInflowsdata_samples" = LWGInflowsdata_samples,
                      "Price_samples" = Price_samples)
}
#```


