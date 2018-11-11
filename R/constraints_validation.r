#```{r}
#Validating Storage Constraints

Storage_constraints <- function(mean_sd_simulations, Storage_min, Storage_max, rel_index, t){
  V_min <- rep(Storage_min, t) %>% matrix(ncol = t) 
  V_max <- rep(Storage_max, t) %>% matrix(ncol = t)
  Storage_mean <- mean_sd_simulations$`Means`[1:3,]
  Storage_sd <- mean_sd_simulations$`Std dev`[1:3,]
  V_rel_high <- qnorm(rel_index, Storage_mean, Storage_sd)
  V_rel_low <- qnorm((1-rel_index), Storage_mean, Storage_sd)
  
  MinStorage_val <- V_min - V_rel_low
  MaxStorage_val <- - V_max + V_rel_high
  
  Storage_constraints <- cbind(MinStorage_val, MaxStorage_val)
} 

#```

#```{r}
#Storage_constraints_validation <- Storage_constraints(mean_sd_simulations, Storage_min, Storage_max, rel_index =0.9, t = 14) 
#```

#```{r}
#Validating Forebay Constraints

Forebay_constraints <- function(mean_sd_simulations, Forebay_min, Forebay_max, rel_index, t){
  Fb_min <- rep(Forebay_min, t) %>% matrix(ncol = t) 
  Fb_max <- rep(Forebay_max, t) %>% matrix(ncol = t)
  Forebay_mean <- mean_sd_simulations$`Means`[4:6,]
  Forebay_sd <- mean_sd_simulations$`Std dev`[4:6,]
  Fb_rel_high <- qnorm(rel_index, Forebay_mean, Forebay_sd)
  Fb_rel_low <- qnorm((1-rel_index), Forebay_mean, Forebay_sd)
  
  MinForebay_val <- Fb_min - Fb_rel_low
  MaxForebay_val <- - Fb_max + Fb_rel_high
  
  Forebay_constraints <- cbind(MinForebay_val, MaxForebay_val)
} 

#```

#```{r}
#Forebay_constraints_validation <- Forebay_constraints(mean_sd_simulations, Forebay_min, Forebay_max, rel_index =0.9, t = 14) 
#```

#```{r}
#Validating Energy Constraints

  Energy_constraints <- function(mean_sd_simulations, Energy_min, Energy_max, rel_index, t){
  E_min <- rep(Energy_min, t) %>% matrix(ncol = t) 
  E_max <- rep(Energy_max, t) %>% matrix(ncol = t)
  E_mean <- mean_sd_simulations$`Means`[7:9,]
  E_sd <- mean_sd_simulations$`Std dev`[7:9,]
  E_rel_high <- qnorm(rel_index, E_mean, E_sd)
  E_rel_low <- qnorm((1-rel_index), E_mean, E_sd)
  
  MinForebay_val <- E_min - E_rel_low
  MaxForebay_val <- - E_max + E_rel_high
  
  Energy_constraints <- cbind(MinForebay_val, MaxForebay_val)
} 

#```

#```{r}
#Energy_constraints_validation <- Energy_constraints(mean_sd_simulations, Energy_min, Energy_max, rel_index =0.9, t = 14) 
#```
#```{r}
# Validating Forebay elevation at end of optimization period, t=14
# This constraint is only for Grand Coulee reservoir

ForebayEndofPeriod_constraints <- function(mean_sd_simulations, Fb_target, rel_index, t){
  Fbtarget_lb <- 0.5*Fb_target 
  Fbtarget_ub <- 0.5*Fb_target
  Forebay_mean <- mean_sd_simulations$`Means`[4,t] %>% t(.)
  Forebay_sd <- mean_sd_simulations$`Std dev`[4,t] %>% t(.)
  Fbend_rel_high <- qnorm(rel_index, Forebay_mean, Forebay_sd)
  Fbend_rel_low <- qnorm((1-rel_index), Forebay_mean, Forebay_sd)
  
  MinForebayEOP_val <- Fb_target - Fbtarget_lb - Fbend_rel_low
  MaxForebayEOP_val <- - Fb_target - Fbtarget_ub  + Fbend_rel_high
  
  ForebayEndofPeriod_constraints <- cbind(MinForebayEOP_val, MaxForebayEOP_val)
} 

#```

#```{r}
#ForebayEndofPeriod_constraints_validation <- ForebayEndofPeriod_constraints(mean_sd_simulations, Fb_target, rel_index =0.9, t = 14) 
#```