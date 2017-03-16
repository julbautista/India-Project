source("03 Standard Model.R")

#Run the same model on per cap exp lagged---------------------
year_id <- ind1$year.id
state_id <- ind1$state.id
imr <- log(ind1$Female)
percap <- log(ind1$lag.exp)
N <- length(imr)

fit2 <- stan("normal.stan",
             data = list("year_id", "state_id", "N", "imr","percap"),
             iter = 2000, chains = 4)

print(fit2, pars = c("mu_alpha_s", "tau_alpha_s", 
                     "mu_alpha_t", "tau_alpha_t",
                     "beta", "alpha_t"))

ind1$y.upper2 <- NA
ind1$y.lower2 <- NA
for(i in 1:N){
  ind1$y.lower2[i] <- exp(quantile(extract(fit2)$y_pred[,i], 0.25))
  ind1$y.upper2[i] <- exp(quantile(extract(fit2)$y_pred[,i], 0.75))
}

#plot predicted values by state---------------------
#blue is predicted green is actual
ind1$pred2 <- exp(colMeans(extract(fit2)$y_pred))

pred2 <- list()
for(i in 1:30){
  pred2[[i]] <- ggplot(subset(ind1,ind1$state.id == i), 
                       aes(Year, pred2)) + 
    public + #geom_line()  +
    geom_line(aes(Year, Female), colour = jbpal$green) +
    geom_line(aes(Year,y.lower2), linetype = 'dashed', alpha = 0.3) +
    geom_line(aes(Year,y.upper2), linetype = 'dashed', alpha = 0.3) +
    scale_x_continuous(breaks = c(2006, 2012)) +
    scale_y_continuous(breaks = c(0, 90), limits = c(0,90) ) +
    labs(x = 'Year', y = 'IMR', title = unique(ind1$State[ind1$state.id == i])) +
    
    geom_text(x = 2012, y = round(max(ind1[ind1$state.id == i,]$Female)), label = round(min(ind1[ind1$state.id == i,]$Female)), size = 2) +
    geom_text(x = 2006, y = round(min(ind1[ind1$state.id == i,]$Female)), label = round(max(ind1[ind1$state.id == i,]$Female)), size = 2) +
    
    theme(axis.text.x = element_text(size = 5),
          axis.text.y = element_text(size = 5),
          plot.title = element_text(size = 9),
          axis.title.x = element_text(size = 7),
          axis.title.y = element_text(size = 7),
          panel.grid.major = element_blank())
}
ggsave("lagpredfem.pdf", plot = grid.arrange(grobs = pred2, nrow = 6, ncol = 5), width = 17, height = 8.5)


#Posterior Predictive Check---------------------
ggplot(ind1, aes(pred2, Female)) + public + geom_abline(slope = 1, intercept = 0) + geom_errorbarh(data = ind1, aes(y = Female, x = pred2, xmin = y.lower2, xmax = y.upper2)) + geom_point() + ggtitle("Posterior Predictive Check on Females") + xlim(c(0,90)) + ylim(c(0,90)) + xlab("Pred")
ggsave("lagpostpredfem.pdf", width = 17, height = 8.5)



#Run the same model on per cap exp lagged for boys---------------------
year_id <- ind1$year.id
state_id <- ind1$state.id
imr <- log(ind1$Male)
percap <- log(ind1$lag.exp)
N <- length(imr)

fit3 <- stan("normal.stan",
             data = list("year_id", "state_id", "N", "imr","percap"),
             iter = 2000, chains = 4)

print(fit3, pars = c("mu_alpha_s", "tau_alpha_s", 
                     "mu_alpha_t", "tau_alpha_t",
                     "beta", "alpha_t"))

ind1$y.upper3 <- NA
ind1$y.lower3 <- NA
for(i in 1:N){
  ind1$y.lower3[i] <- exp(quantile(extract(fit3)$y_pred[,i], 0.25))
  ind1$y.upper3[i] <- exp(quantile(extract(fit3)$y_pred[,i], 0.75))
}

#plot predicted values by state---------------------
#blue is predicted green is actual
ind1$pred3 <- exp(colMeans(extract(fit3)$y_pred))

pred3 <- list()
for(i in 1:30){
  pred3[[i]] <- ggplot(subset(ind1,ind1$state.id == i), 
                       aes(Year, pred3)) + 
    public + #geom_line()  +
    geom_line(aes(Year, Male), colour = jbpal$green) +
    geom_line(aes(Year,y.lower3), linetype = 'dashed', alpha = 0.3) +
    geom_line(aes(Year,y.upper3), linetype = 'dashed', alpha = 0.3) +
    scale_x_continuous(breaks = c(2006, 2012)) +
    scale_y_continuous(breaks = c(0, 85 ), limits = c(0,85 ) ) +
    geom_text(x = 2012, y = round(max(ind1[ind1$state.id == i,]$Male)), label = round(min(ind1[ind1$state.id == i,]$Male)), size = 2) +
    geom_text(x = 2006, y = round(min(ind1[ind1$state.id == i,]$Male)), label = round(max(ind1[ind1$state.id == i,]$Male)), size = 2) +
    labs(x = 'Year', y = 'IMR', title = unique(ind1$State[ind1$state.id == i])) +
    theme(axis.text.x = element_text(size = 5),
          axis.text.y = element_text(size = 5),
          plot.title = element_text(size = 9),
          axis.title.x = element_text(size = 7),
          axis.title.y = element_text(size = 7),
          panel.grid.major = element_blank())
}
ggsave("lagpredmal.pdf", plot = grid.arrange(grobs = pred3, nrow = 6, ncol = 5), width = 17, height = 8.5)


#Posterior Predictive Check---------------------
ggplot(ind1, aes(pred3, Male)) + public + geom_abline(slope = 1, intercept = 0) + geom_errorbarh(data = ind1, aes(y = Male, x = pred3, xmin = y.lower3, xmax = y.upper3)) + geom_point() + ggtitle("Posterior Predictive Check on Males") + xlim(c(0,85)) + ylim(c(0,85)) + xlab("Pred")
ggsave("lagpostpredmal.pdf", width = 17, height = 8.5)

