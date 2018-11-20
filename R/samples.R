#```{r}

# Sampling Inflows data realizations 
get_samples <- function(t, ntimes){
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
  GCLInflowsdata_samples <- rnorm(ntimes*t, GCLInflows_Mean, GCLInflows_sd) %>% matrix(nrow = ntimes, byrow = TRUE)
  
  LWGInflowsdata_samples <- rnorm(ntimes*t, LWGInflows_Mean, LWGInflows_sd) %>% matrix(nrow = ntimes, byrow = TRUE)
  
  Price_samples <- runif((ntimes^2)*t, min_Price, max_Price) %>% matrix(nrow = (ntimes^2), byrow = TRUE)
  get_samples <- list("GCLInflowsdata_samples" = GCLInflowsdata_samples, "LWGInflowsdata_samples" = LWGInflowsdata_samples,
                      "Price_samples" = Price_samples)
}
#```