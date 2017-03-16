source("02 Exploratory Analysis.R")
#Stan model normal for females---------------------
year_id <- ind$year.id
state_id <- ind$state.id
imr <- (ind$Female)
percap <- ind$percap
N <- length(imr)

fit1 <- stan("normal.stan",
             data = list("year_id", "state_id", "N", "imr","percap"),
             iter = 2000, chains = 4)

print(fit1, pars = c("mu_alpha_s", "tau_alpha_s", 
                     "mu_alpha_t", "tau_alpha_t",
                     "beta", "alpha_t"))

#extract quantiles---------------------
ind$y.upper <- NA
ind$y.lower <- NA
for(i in 1:N){
  ind$y.lower[i] <- quantile(extract(fit1)$y_pred[,i], 0.25)
  ind$y.upper[i] <- quantile(extract(fit1)$y_pred[,i], 0.75)
}

#plot predicted values by state for females---------------------
#blue is predicted green is actual
ind$pred <- (colMeans(extract(fit1)$y_pred))

pred <- list()
for(i in 1:30){
  pred[[i]] <- ggplot(subset(ind,ind$state.id == i), 
                      aes(Year, pred)) + 
    public + #geom_line()  +
    geom_line(aes(Year, Female), colour = jbpal$green) +
    geom_line(aes(Year,y.lower), linetype = 'dashed', alpha = 0.3) +
    geom_line(aes(Year,y.upper), linetype = 'dashed', alpha = 0.3) +
    scale_x_continuous(breaks = c(2005, 2012)) +
    scale_y_continuous(breaks = c(0, 80), limits = c(0,80 ) ) +
    
    geom_text(x = 2012, y = round(max(ind[ind$state.id == i,]$Female)), label = round(min(ind[ind$state.id == i,]$Female)), size = 2) +
    geom_text(x = 2005, y = round(min(ind[ind$state.id == i,]$Female)), label = round(max(ind[ind$state.id == i,]$Female)), size = 2) +
    
    labs(x = 'Year', y = 'IMR', title = unique(ind$State[ind$state.id == i])) +
    theme(axis.text.x = element_text(size = 5),
          axis.text.y = element_text(size = 5),
          plot.title = element_text(size = 9),
          axis.title.x = element_text(size = 7),
          axis.title.y = element_text(size = 7),
          panel.grid.major = element_blank())
}
ggsave("predfem.pdf", plot = grid.arrange(grobs = pred, nrow = 6, ncol = 5), width = 17, height = 8.5)

#Posterior Predictive Check---------------------
ggplot(ind, aes(pred, Female)) + public + geom_abline(slope = 1, intercept = 0) + geom_errorbarh(data = ind, aes(y = Female, x = pred, xmin = y.lower, xmax = y.upper)) + geom_point() + ggtitle("Posterior Predictive Check on Females") + xlim(c(0,80)) + ylim(c(0,80))
ggsave("postpredfem.pdf", width = 17, height = 8.5)


#Male pred model---------------------
year_id <- ind$year.id
state_id <- ind$state.id
imr <- (ind$Male)
percap <- ind$percap
N <- length(imr)

fitm <- stan("normal.stan",
             data = list("year_id", "state_id", "N", "imr","percap"),
             iter = 2000, chains = 4)

print(fitm, pars = c("mu_alpha_s", "tau_alpha_s", 
                     "mu_alpha_t", "tau_alpha_t",
                     "beta", "alpha_t"))

#extract quantiles---------------------
ind$y.upperm <- NA
ind$y.lowerm <- NA
for(i in 1:N){
  ind$y.lowerm[i] <- quantile(extract(fitm)$y_pred[,i], 0.25)
  ind$y.upperm[i] <- quantile(extract(fitm)$y_pred[,i], 0.75)
}

#plot predicted values by state for males
#blue is predicted green is actual
ind$predm <- (colMeans(extract(fitm)$y_pred))
predm <- list()
for(i in 1:30){
  predm[[i]] <- ggplot(subset(ind,ind$state.id == i), 
                       aes(Year, predm)) + 
    public + #geom_line()  +
    geom_line(aes(Year, Male), colour = jbpal$green) +
    geom_line(aes(Year,y.lowerm), linetype = 'dashed', alpha = 0.3) +
    geom_line(aes(Year,y.upperm), linetype = 'dashed', alpha = 0.3) +
    scale_x_continuous(breaks = c(2005, 2012)) +
    scale_y_continuous(breaks = c(0, 80), limits = c(0,80 ) ) +
    
    geom_text(x = 2012, y = round(max(ind[ind$state.id == i,]$Male)), label = round(min(ind[ind$state.id == i,]$Male)), size = 2) +
    geom_text(x = 2005, y = round(min(ind[ind$state.id == i,]$Male)), label = round(max(ind[ind$state.id == i,]$Male)), size = 2) +
    
    labs(x = 'Year', y = 'IMR', title = unique(ind$State[ind$state.id == i])) +
    theme(axis.text.x = element_text(size = 5),
          axis.text.y = element_text(size = 5),
          plot.title = element_text(size = 9),
          axis.title.x = element_text(size = 7),
          axis.title.y = element_text(size = 7),
          panel.grid.major = element_blank())
}
ggsave("predmal.pdf", plot = grid.arrange(grobs = predm, nrow = 6, ncol = 5), width = 17, height = 8.5)


#Posterior Predictive Check Male---------------------
ggplot(ind, aes(predm, Male)) + public + geom_abline(slope = 1, intercept = 0) + geom_errorbarh(data = ind, aes(y = Male, x = predm, xmin = y.lowerm, xmax = y.upperm)) + geom_point() + ggtitle("Posterior Predictive Check on Males") + xlim(c(0,80)) + ylim(c(0,80))
ggsave("postpredfem.pdf", width = 17, height = 8.5)
