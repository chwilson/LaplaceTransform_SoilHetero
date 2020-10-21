library(SoilR)



totalC_t0 <- 7.7;         # not included in data, so hard code here
t0 <- 0;
N_t <- 25;                # calculated by inspection
ts <- eCO2[1:25,2];
eCO2mean <- eCO2[1:25,3];   
eCO2sd <- eCO2[1:25,4];   

library(ggplot2,quietly=TRUE);
df <- data.frame(list(ts=ts,eCO2mean=eCO2mean,eCO2sd=eCO2sd));
interval_95pct <- aes(ymin = eCO2mean + 1.96 * eCO2sd, 
                      ymax = eCO2mean - 1.96 * eCO2sd);
plot_data <- 
  ggplot(df, aes(x=ts, y=eCO2mean)) +
  geom_point() + 
  geom_errorbar(interval_95pct, width=0, colour="blue") +
  scale_x_continuous(name="time (days)") +
  scale_y_continuous(name="evolved CO2 (mgC g-1 soil)") +
  ggtitle("Evolved CO2 Measurements (with 95% intervals)");
plot(plot_data);


# Subsetting cleaner part of data - shouldn't matter: 
plot(seq(1,N_t-3,1),totalC_t0-eCO2mean[1:22])

# Installing Stan 
install.packages("rstan", dependencies = TRUE)

### Troubleshooting on Windows machine: 
file.rename("~/.R/Makevars.win", "~/.R/Makevars.win.bak")
M <- file.path(Sys.getenv("HOME"), ".R", ifelse(.Platform$OS.type == "windows", "Makevars.win", "Makevars"))
file.edit(M)



# Loading Stan 
library(rstan)
options(mc.cores = parallel::detectCores())
rstan_options(auto_write = TRUE)


stan_dat <- list(N = N_t, y = (totalC_t0-eCO2mean)/(totalC_t0),
                 time = ts)

Wilson1 <- stan(file = "WilsonSOC_Bayes.stan", data = stan_dat, chains = 4, iter = 500)
print(Wilson1)

Wilson2 <- stan(fit = Wilson1, data= stan_dat, chains = 5, iter = 5000)
print(Wilson2)

pairs(Wilson2)

## Plotting fitted model 
wfunc <- function(x,mu=mu,phi=phi){
  return(((mu*((1/phi)+1))/(mu*((1/phi)+1) + x))^((1/phi)+2))
}

plot_fit <- data.frame(time = ts, y = (totalC_t0-eCO2mean)/(totalC_t0))
ggplot() + 
  geom_point(data = plot_fit, aes(x=time,y=y)) + 
  stat_function(data = data.frame(x=plot_fit$time),fun=wfunc, args = list(mu=15.6,phi=0.06)) +
  stat_function(data = data.frame(x=plot_fit$time),fun=function(x){exp(-(1/15.6)*x)},color="red") 
  
# So, in these data, we are approaching the first order or one pool model. This implies that heterogeneity 
# within a single pool is not sufficient to describe these data. Instead, we would want two coarser classes. 
# An example of hypothetico-deductive inference...

sqrt(0.25)
