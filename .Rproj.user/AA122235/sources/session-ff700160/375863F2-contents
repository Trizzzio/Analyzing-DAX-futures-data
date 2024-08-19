library(tidyverse)
library(tidyquant)
library(lubridate)
library(kableExtra)

# Save plots to working directory
save_plots <- T

# Plot plots?
plot_plots <- T

#Load data 

FFIndustry = read.csv("./10_Industry_Portfolios.csv", skip=11) %>% 
  rename(date=X) %>%
  mutate(date=ymd(parse_date_time(date,"%Y%m")))%>%
  transmute(date=ymd(date),
            NoDur=as.numeric(NoDur),
            Durbl=as.numeric(Durbl),
            Manuf=as.numeric(Manuf),
            Enrgy=as.numeric(Enrgy),
            HiTec=as.numeric(HiTec),
            Telcm=as.numeric(Telcm),
            Shops=as.numeric(Shops),
            Hlth=as.numeric(Hlth),
            Utils=as.numeric(Utils),
            Other=as.numeric(Other)) 

#Compute sample average mean and variance covariance matrix of return. which portfolio has highes sharpe-ratio

returns = FFIndustry %>% select(-date) 

sigma_true <- returns %>%
  cov(use = "pairwise.complete.obs") # Compute return sample covariance matrix

mu_true <- returns %>%
  colMeans() %>%
  as.matrix()

ann_mu=12*mu_true

ann_sigma=12*sigma_true
##### Creating Table of mean and variance for Latex
as.data.frame(apply(sigma_true,c(1,2),function(x) round(x,2))) %>% kable(align = 'c', format = 'latex') %>% 
  kable_styling(full_width = FALSE,latex_options = "striped")
#####

# Sharpe ratio (assuming r_f = 0 and not using annualized measures).
SR <- mu_true/sqrt(diag(sigma_true))
print(paste0("The stock with the largest SR was: ",round(SR[which(SR==max(SR))][1],2)," of ",attributes(SR[which(SR==max(SR))])$names,"."))

