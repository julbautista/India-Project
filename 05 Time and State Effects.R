source("04 Lagged Models.R")
#Look at distribution of effects by state in lagged log model---------------------
#create beta data_frame
beta_male <- data.frame(extract(fitm)$beta)
beta_female <- data.frame(extract(fit1)$beta)

dense <- list()
for(i in 1:30){
  dense[[i]] <- ggplot(beta_male, 
                       aes_(x = beta_male[,i] )) + 
    public + geom_density()  + xlim(-0.5,0.5) + ylim(0,9.5) +
    #geom_density(data = beta_female, aes_(x = beta_female[,i]), colour = jbpal$green) +
    geom_density(data = beta_female, aes_(x = beta_female[,i]), colour = jbpal$red) +
    labs(x = 'IMR', title = unique(ind1$State[ind1$state.id == i])) +
    geom_vline(xintercept = 0, alpha = 0.4, linetype = 2) +
    theme(axis.text.x = element_text(size = 5),
          axis.text.y = element_blank(),
          plot.title = element_text(size = 9),
          axis.title.x = element_text(size = 7),
          axis.title.y = element_blank(),
          axis.line.y = element_blank(),
          panel.grid.major = element_blank())
}

ggsave("standardbetas.pdf", plot = grid.arrange(grobs = dense, nrow = 6, ncol = 5), width = 17, height = 8.5)

#Look at distribution of effects by state in lagged log model---------------------
#create beta data_frame
beta_male <- data.frame(extract(fit3)$beta)
beta_female <- data.frame(extract(fit2)$beta)

dense <- list()
for(i in 1:30){
  dense[[i]] <- ggplot(beta_male, 
                       aes_(x = beta_male[,i] )) + 
    public + geom_density()  + xlim(-0.5,0.5) + ylim(0,9.5) +
    geom_density(data = beta_female, aes_(x = beta_female[,i]), colour = jbpal$green) +
    labs(x = 'IMR', title = unique(ind1$State[ind1$state.id == i])) +
    geom_vline(xintercept = 0, alpha = 0.4, linetype = 2) +
    theme(axis.text.x = element_text(size = 5),
          axis.text.y = element_blank(),
          plot.title = element_text(size = 9),
          axis.title.x = element_text(size = 7),
          axis.title.y = element_blank(),
          axis.line.y = element_blank(),
          panel.grid.major = element_blank())
}

ggsave("betas.pdf", plot = grid.arrange(grobs = dense, nrow = 6, ncol = 5), width = 17, height = 8.5)


#plotting alpha for standard females
alpha.per <- colMeans(extract(fit1)$alpha_t[,1:8])
alpha.per.upper <- NULL
alpha.per.lower <- NULL
for(i in 1:8){
  alpha.per.upper[i] <- quantile(extract(fit1)$alpha_t[,i], 0.25)
  alpha.per.lower[i] <- quantile(extract(fit1)$alpha_t[,i], 0.75)
}

alphas <- data.frame(year = c(2005:2012), alpha.per, alpha.per.upper, alpha.per.lower)

ggplot(alphas, aes(year, alpha.per))  + ylim(c(0,35)) + public +
  geom_errorbar(ymin = alpha.per.lower, ymax = alpha.per.upper, colour = jbpal$green, alpha = 1, width = 0.1) + scale_x_continuous(breaks = c(2005:2012)) + theme(panel.grid.major = element_blank()) + ggtitle("Time Intercepts") + labs(x = "Year", y = "Intercept") + geom_point(size = 3)
ggsave("alphas.pdf", width = 17, height = 8.5)


#plotting alpha for male and female
alpha.female <- exp(colMeans(extract(fit2)$alpha_t[,1:8]))
alpha.male <- exp(colMeans(extract(fit3)$alpha_t[,1:8]))
alpha.female.upper <- NULL
alpha.female.lower <- NULL
alpha.male.upper <- NULL
alpha.male.lower <- NULL
for(i in 1:8){
  alpha.female.upper[i] <- exp(quantile(extract(fit2)$alpha_t[,i], 0.25))
  alpha.female.lower[i] <- exp(quantile(extract(fit2)$alpha_t[,i], 0.75))
  alpha.male.upper[i] <- exp(quantile(extract(fit3)$alpha_t[,i], 0.25))
  alpha.male.lower[i] <- exp(quantile(extract(fit3)$alpha_t[,i], 0.75))
}


#plot time intercepts
# par(mfrow = c(1, 1))
# plot(c(2005:2012), alpha.per,
#      xlim = c(2005, 2012), 
#      ylim = c(0, 40),
#      pch = 16,
#      cex = 0.6,
#      xlab = "Years",
#      ylab = "Time Intercept")
# abline(0, 1)
# arrows(x0 = c(2005:2012),
#        y0 = alpha.per,
#        x1 = c(2005:2012),
#        y1 = alpha.per.upper,
#        length = 0,
#        col = "darkgrey")
# arrows(x0 = c(2005:2012),
#        y0 = alpha.per,
#        x1 = c(2005:2012),
#        y1 = alpha.per.lower,
#        length = 0,
#        col = "darkgrey")
# points(c(2005:2012), alpha.per,
#        pch = 16, 
#        cex = 0.6)



#plot betas
# par(mfrow = c(1, 2))
# plot(c(2005:2012), alpha.male,
#      xlim = c(2005, 2012), 
#      ylim = c(-20, 300),
#      pch = 16,
#      cex = 0.6,
#      main = "Male Time Intercept",
#      xlab = "Years",
#      ylab = "Time Intercept")
# abline(0, 1)
# arrows(x0 = c(2005:2012),
#        y0 = alpha.male,
#        x1 = c(2005:2012),
#        y1 = alpha.male.upper,
#        length = 0,
#        col = "darkgrey")
# arrows(x0 = c(2005:2012),
#        y0 = alpha.male,
#        x1 = c(2005:2012),
#        y1 = alpha.male.lower,
#        length = 0,
#        col = "darkgrey")
# points(c(2005:2012), alpha.male,
#        pch = 16, 
#        cex = 0.6)
# 
# plot(c(2005:2012), alpha.female,
#      xlim = c(2005, 2012), 
#      ylim = c(-20, 300),
#      pch = 16,
#      cex = 0.6,
#      main = "Female Time Intercept",
#      xlab = "Years",
#      ylab = "Time Intercept")
# abline(0, 1)
# arrows(x0 = c(2005:2012),
#        y0 = alpha.female,
#        x1 = c(2005:2012),
#        y1 = alpha.female.upper,
#        length = 0,
#        col = "darkgrey")
# arrows(x0 = c(2005:2012),
#        y0 = alpha.female,
#        x1 = c(2005:2012),
#        y1 = alpha.female.lower,
#        length = 0,
#        col = "darkgrey")
# points(c(2005:2012), alpha.female,
#        pch = 16, 
#        cex = 0.6)

