#```{r}
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
#```

#```{r}
#Total_Revenue_stat <- Total_Revenue_cal(mean_sd_simulations, Price_samples, t=14, ntimes)
#```