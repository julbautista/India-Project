source("https://raw.githubusercontent.com/julbautista/Startup/master/julian_startup.R")
setwd("C:/Users/Julian Bautista/Documents/Portfolio/India Project")

df <- read.csv("https://raw.githubusercontent.com/julbautista/India-Project/master/India_Data.csv", header = T, skip = 2)

#Drop Andaman and Nicobar & Dadra NH & Daman and Diu & Lakshadweep & Chandigarh and 2003
df <- df[df$Year != 2003,]
df <- df[df$State != "Andaman & Nicobar Islands"
         & df$State != "Dadra & Nagar Haveli"
         & df$State != "Daman & Diu" 
         & df$State != "Lakshadweep"
         & df$State != "Chandigarh",]

df$State <- as.character(df$State)

df$State[df$State == "Odisha"] <- "Orissa"
df$State[df$State == "Chhatisgarh"] <- "Chhattisgarh"

#Read in population data
pop <- read.csv("https://raw.githubusercontent.com/julbautista/India-Project/master/population.csv", header = TRUE)

pop <- pop[pop$State != "Andaman and Nicobar Islands"
           & pop$State != "Dadra and Nagar Haveli"
           & pop$State != "Daman and Diu" 
           & pop$State != "Lakshadweep"
           & pop$State != "Chandigarh",]

#Merge population to dataframe
ind <- merge(df, pop)

#Function for projecting population up or down
pop.func <- function(year, pop2011){
  pop <- NULL
  pop <- ifelse(year > 2011,
                pop2011*(1.0184^(year - 2011)),
                ifelse(year < 2011, 
                       pop2011*((1/1.0184)^(2011 - year)), 
                       pop2011))
  return(pop)
}

#Add column of projected population numbers
ind$population <- pop.func(ind$Year, ind$X2011pop)

#Remove old population numbers
ind <- ind[,-8]
rm(df,pop)

#Put all currency in Ruppee 
ind$exp.rup <- NULL
for(i in 1:length(ind[,1])){
  ind$exp.rup[i] <- ifelse(ind$Units[i] == "Lakh",
                           ind$Health.Exp[i]*100000,
                           ind$Health.Exp[i]*1000000)
}

#Get spending per capita
ind$percap <- ind$exp.rup/ind$population

#Create ratio of female to male IMR
ind$fm.imr <- ind$Female/ind$Male

#Regress IMR diff on health exp
summary(lm(fm.imr ~ percap, data = ind))

#Sort by year
ind <- ind[order(ind$Year),]

#Create state id
ind <- within(ind, state.id <- match(State, unique(State)))

#Create Year id
ind <- within(ind, year.id <- match(Year, unique(Year)))

#Plot per cap health exp by state over time
#non uniform y limit 
healthexp <- list()
for(i in 1:30){
  plotter <- ggplot(subset(ind,ind$state.id == i), aes(Year, percap)) + jbplot + geom_line() + 
    labs(x = 'Year', y = 'Health Expenditure', title = unique(ind$State[ind$state.id == i])) + 
    scale_x_continuous(breaks = c(2005, 2012)) +
    scale_y_continuous(breaks = c(0, round(max(ind[ind$state.id == i,]$percap)))) +
    theme(axis.text.x = element_text(size = 5),
          axis.text.y = element_text(size = 5),
          plot.title = element_text(size = 9),
          axis.title.x = element_text(size = 7),
          axis.title.y = element_text(size = 7),
          panel.grid.major = element_blank())
  healthexp[[i]] <- plotter
}
multiplot(plotlist = healthexp, cols = 6)

#uniform y limits healthexp
healthexp2 <- list()
for(i in 1:30){
  plotter <- ggplot(subset(ind,ind$state.id == i), aes(Year, percap)) + jbplot + geom_line() + 
    scale_x_continuous(breaks = c(2005, 2012)) +
    scale_y_continuous(breaks = c(0, round(min(ind[ind$state.id == i,]$percap)), 3000 ), limits = c(0,3000) ) +
    labs(x = 'Year', y = 'Health Expenditure', title = unique(ind$State[ind$state.id == i])) +
    theme(axis.text.x = element_text(size = 5),
          axis.text.y = element_text(size = 5),
          plot.title = element_text(size = 9),
          axis.title.x = element_text(size = 7),
          axis.title.y = element_text(size = 7),
          panel.grid.major = element_blank())
  healthexp2[[i]] <- plotter
}
multiplot(plotlist = healthexp2, cols = 6)


#Plot female and male IMR by state over time
genderIMR <- list()
for(i in 1:30){
  plotter <- ggplot(subset(ind,ind$state.id == i), aes(Year, Female)) + jbplot + 
    geom_line(colour = jbpal$red) + 
    geom_line(aes(Year, Male), colour = jbpal$green) + 
    scale_x_continuous(breaks = c(2005, 2012)) +
    scale_y_continuous(breaks = c(0, round(max(ind[ind$state.id == i,]$Female))), limits = c(0, 85 ) +
    labs(x = 'Year', y = 'IMR', title = unique(ind$State[ind$state.id == i])) +
    theme(axis.text.x = element_text(size = 5),
          axis.text.y = element_text(size = 5),
          plot.title = element_text(size = 9),
          axis.title.x = element_text(size = 7),
          axis.title.y = element_text(size = 7),
          panel.grid.major = element_blank())
  genderIMR[[i]] <- plotter
}
multiplot(plotlist = genderIMR, cols = 6)


#Plot IMR by state over time
IMR <- list()
for(i in 1:30){
  plotter <- ggplot(subset(ind,ind$state.id == i), aes(Year, Person)) + jbplot + geom_line() + 
    scale_x_continuous(breaks = c(2005, 2012)) +
    scale_y_continuous(breaks = c(0, round(min(ind[ind$state.id == i,]$Person)), round(max(ind[ind$state.id == i,]$Person)), 80 ), limits = c(0,80) ) +
    labs(x = 'Year', y = 'IMR', title = unique(ind$State[ind$state.id == i])) +
    theme(axis.text.x = element_text(size = 5),
          axis.text.y = element_text(size = 5),
          plot.title = element_text(size = 9),
          axis.title.x = element_text(size = 7),
          axis.title.y = element_text(size = 7),
          panel.grid.major = element_blank())
  IMR[[i]] <- plotter
}
multiplot(plotlist = IMR, cols = 6)

#Plot IMR RATIO by state over time
ratioIMR <- list()
for(i in 1:30){
  plotter <- ggplot(subset(ind,ind$state.id == i), aes(Year, fm.imr)) + jbplot + geom_line() + 
    scale_x_continuous(breaks = c(2005, 2012)) +
    scale_y_continuous(breaks = c(0, round(min(ind[ind$state.id == i,]$fm.imr)), round(max(ind[ind$state.id == i,]$Person)), 2.7 ), limits = c(0,2.7) ) +
    labs(x = 'Year', y = 'IMR Ratio', title = unique(ind$State[ind$state.id == i])) +
    theme(axis.text.x = element_text(size = 5),
          axis.text.y = element_text(size = 5),
          plot.title = element_text(size = 9),
          axis.title.x = element_text(size = 7),
          axis.title.y = element_text(size = 7),
          panel.grid.major = element_blank())
  ratioIMR[[i]] <- plotter
}
multiplot(plotlist = ratioIMR, cols = 6)


#Plot IMR in 2012 by State
ind2 <- ind
ind2$State <- factor(ind2$State)
ind2$State <- factor(ind2$State, levels = ind2$State[order(ind2$Person[ind2$Year == 2012])])

ggplot(ind2[ind2$Year == 2012| ind2$Year == 2005,], aes(Person, State, fill = factor(Year))) +
  jbplot  + geom_dotplot(binaxis = "y", dotsize = 0.5) + 
  labs(x = 'IMR', y = "") +
  jbcol + jbfill +
  theme(legend.title = element_blank(), panel.grid.major.x = element_blank(), 
        axis.line = element_blank()) 

# par(mfrow = c(1, 1),
#     mar = c(3, 9, 1, 2),
#     las = 1,
#     cex = 0.7)
# plot(ind$Person[ind$Year == 2012],
#      c(1:30),
#      yaxt = 'n',
#      ylab = " ",
#      xlim = c(0, 80),
#      pch = 16,
#      lwd = 0.5,
#      xlab = "Infant Mortality Rate",
#      main = NULL)
# axis(2, at = c(1:30), 
#      labels = c(ind$State[ind$Year == 2012]),
#      cex = 0.8)
# abline(h = c(1:30), col = "grey")
# points(ind$Person[ind$Year == 2012],
#        c(1:30),
#        pch = 16,
#        cex = 0.8)
# text(50, 30, "2012")
# text(80, 30, "2005",
#      col = "blue")
# 
# #Add blue points for 2005
# points(ind$Person[ind$Year == 2005],
#        c(1:30),
#        pch = 16,
#        cex = 1,
#        col = "blue")

#Stan model normal for females
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

#extract quantiles
ind$y.upper <- NULL
ind$y.lower <- NULL
for(i in 1:240){
  ind$y.lower[i] <- quantile(extract(fit1)$y_pred[,i], 0.25)
  ind$y.upper[i] <- quantile(extract(fit1)$y_pred[,i], 0.75)
}

#plot predicted values by state for females
#blue is predicted green is actual
ind$pred <- (colMeans(extract(fit1)$y_pred))

pred <- list()
for(i in 1:30){
  plotter <- ggplot(subset(ind,ind$state.id == i), 
                    aes(Year, pred)) + 
    jbplot + geom_line()  +
    geom_line(aes(Year, Female), colour = jbpal$green) +
    geom_line(aes(Year,y.lower), linetype = 'dashed', alpha = 0.3) +
    geom_line(aes(Year,y.upper), linetype = 'dashed', alpha = 0.3) +
    scale_x_continuous(breaks = c(2005, 2012)) +
    scale_y_continuous(breaks = c(0, round(min(ind[ind$state.id == i,]$pred)), round(max(ind[ind$state.id == i,]$pred)), 80 ), limits = c(0,80 ) ) +
    labs(x = 'Year', y = 'IMR', title = unique(ind$State[ind$state.id == i])) +
    theme(axis.text.x = element_text(size = 5),
          axis.text.y = element_text(size = 5),
          plot.title = element_text(size = 9),
          axis.title.x = element_text(size = 7),
          axis.title.y = element_text(size = 7),
          panel.grid.major = element_blank())
  pred[[i]] <- plotter
}
multiplot(plotlist = pred, cols = 6)


#Posterior Predictive Check
ggplot(ind, aes(pred, Female)) + jbplot + geom_abline(slope = 1, intercept = 0) + geom_errorbarh(data = ind, aes(y = Female, x = pred, xmin = y.lower, xmax = y.upper)) + geom_point() 


library(plyr)
#create lagged per cap variable
ind <- ddply(ind, .(state.id), 
             transform, 
             lag.exp = c(NA, percap[-length(percap)]))

#Get rid of 2005
ind1 <- ind[ind$Year != 2005,]

#Run the same model on per cap exp lagged
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

ind1$y.upper2 <- NULL
ind1$y.lower2 <- NULL
for(i in 1:N){
  ind1$y.lower2[i] <- exp(quantile(extract(fit2)$y_pred[,i], 0.25))
  ind1$y.upper2[i] <- exp(quantile(extract(fit2)$y_pred[,i], 0.75))
}

#plot predicted values by state 
#blue is predicted green is actual
ind1$pred2 <- exp(colMeans(extract(fit2)$y_pred))

pred2 <- list()
for(i in 1:6){
  plotter <- ggplot(subset(ind1,ind1$state.id == i), 
                    aes(Year, pred2)) + 
    jbplot + geom_line()  +
    geom_line(aes(Year, Female), colour = jbpal$green) +
    geom_line(aes(Year,y.lower2), linetype = 'dashed', alpha = 0.3) +
    geom_line(aes(Year,y.upper2), linetype = 'dashed', alpha = 0.3) +
    scale_x_continuous(breaks = c(2006, 2012)) +
    scale_y_continuous(breaks = c(0, round(min(ind1[ind1$state.id == i,]$pred2)), round(max(ind1[ind1$state.id == i,]$pred2)), 90), limits = c(0,90) ) +
    labs(x = 'Year', y = 'IMR', title = unique(ind1$State[ind1$state.id == i])) +
    theme(axis.text.x = element_text(size = 5),
          axis.text.y = element_text(size = 5),
          plot.title = element_text(size = 9),
          axis.title.x = element_text(size = 7),
          axis.title.y = element_text(size = 7),
          panel.grid.major = element_blank())
  pred2[[i]] <- plotter
}
multiplot(plotlist = pred2, cols = 6)


#Posterior Predictive Check
ggplot(ind1, aes(pred2, Female)) + jbplot + geom_abline(slope = 1, intercept = 0) + geom_errorbarh(data = ind1, aes(y = Female, x = pred2, xmin = y.lower2, xmax = y.upper2)) + geom_point() 



#Run the same model on per cap exp lagged for boys
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

ind1$y.upper3 <- NULL
ind1$y.lower3 <- NULL
for(i in 1:N){
  ind1$y.lower3[i] <- exp(quantile(extract(fit3)$y_pred[,i], 0.25))
  ind1$y.upper3[i] <- exp(quantile(extract(fit3)$y_pred[,i], 0.75))
}

#plot predicted values by state
#blue is predicted green is actual
ind1$pred3 <- exp(colMeans(extract(fit3)$y_pred))

pred3 <- list()
for(i in 1:30){
  plotter <- ggplot(subset(ind1,ind1$state.id == i), 
                    aes(Year, pred3)) + 
    jbplot + geom_line()  +
    geom_line(aes(Year, Male), colour = jbpal$green) +
    geom_line(aes(Year,y.lower3), linetype = 'dashed', alpha = 0.3) +
    geom_line(aes(Year,y.upper3), linetype = 'dashed', alpha = 0.3) +
    scale_x_continuous(breaks = c(2006, 2012)) +
    scale_y_continuous(breaks = c(0, round(min(ind1[ind1$state.id == i,]$pred3)), round(max(ind1[ind1$state.id == i,]$pred3)), 85 ), limits = c(0,85 ) ) +
    labs(x = 'Year', y = 'IMR', title = unique(ind1$State[ind1$state.id == i])) +
    theme(axis.text.x = element_text(size = 5),
          axis.text.y = element_text(size = 5),
          plot.title = element_text(size = 9),
          axis.title.x = element_text(size = 7),
          axis.title.y = element_text(size = 7),
          panel.grid.major = element_blank())
  pred3[[i]] <- plotter
}
multiplot(plotlist = pred3, cols = 6)


#Posterior Predictive Check
ggplot(ind1, aes(pred3, Male)) + jbplot + geom_abline(slope = 1, intercept = 0) + geom_errorbarh(data = ind1, aes(y = Male, x = pred3, xmin = y.lower3, xmax = y.upper3)) + geom_point() 


#Look at distribution of effects by state in lagged log model

#create beta data_frame
beta_male <- data.frame(extract(fit3)$beta)
beta_female <- data.frame(extract(fit2)$beta)

dense <- list()
for(i in 1:30){
  plotter <- ggplot(beta_male, 
                    aes(beta_male[,i] )) + 
    jbplot + geom_density()  + xlim(-0.5,0.5) + ylim(0,9.5) +
    geom_density(data = beta_female, aes(beta_female[,i]), colour = jbpal$green) +
    labs(x = 'IMR', title = unique(ind1$State[ind1$state.id == i])) +
    theme(axis.text.x = element_text(size = 5),
          axis.text.y = element_blank(),
          plot.title = element_text(size = 9),
          axis.title.x = element_text(size = 7),
          axis.title.y = element_blank(),
          axis.line.y = element_blank(),
          panel.grid.major = element_blank())
  dense[[i]] <- plotter
}
multiplot(plotlist = dense, cols = 6)

par(mfrow = c(5, 6), 
    mar = c(2, 1, 1, 1),
    mgp = c(2, 1, 0))
for(i in 1:30){
  plot(density(extract(fit3)$beta[,i]),
       yaxt = 'n',
       xaxt = 'n',
       xlim = c(-.5, .5),
       ylim = c(0, 9.5),
       main = paste(unique(ind1$State[ind1$state.id == i])), 
       cex.main = 0.9,
       col = "blue")
  abline(v = 0,
         col = "darkgrey",
         lty = 2,
         lwd = 2)
  axis(1, at = c(-.5, 0, .5),
       cex.axis = 0.8)
  #And compare to the ladies
  lines(density(extract(fit2)$beta[,i]),
        col = "red")
}

#Just look at the Mus

#plotting alpha for persons
alpha.per <- colMeans(extract(fit1)$alpha_t[,1:8])
alpha.per.upper <- NULL
alpha.per.lower <- NULL
for(i in 1:8){
  alpha.per.upper[i] <- quantile(extract(fit1)$alpha_t[,i], 0.25)
  alpha.per.lower[i] <- quantile(extract(fit1)$alpha_t[,i], 0.75)
}

par(mfrow = c(1, 1))
plot(c(2005:2012), alpha.per,
     xlim = c(2005, 2012), 
     ylim = c(0, 40),
     pch = 16,
     cex = 0.6,
     xlab = "Years",
     ylab = "Time Intercept")
abline(0, 1)
arrows(x0 = c(2005:2012),
       y0 = alpha.per,
       x1 = c(2005:2012),
       y1 = alpha.per.upper,
       length = 0,
       col = "darkgrey")
arrows(x0 = c(2005:2012),
       y0 = alpha.per,
       x1 = c(2005:2012),
       y1 = alpha.per.lower,
       length = 0,
       col = "darkgrey")
points(c(2005:2012), alpha.per,
       pch = 16, 
       cex = 0.6)


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



par(mfrow = c(1, 2))
plot(c(2005:2012), alpha.male,
     xlim = c(2005, 2012), 
     ylim = c(-20, 300),
     pch = 16,
     cex = 0.6,
     main = "Male Time Intercept",
     xlab = "Years",
     ylab = "Time Intercept")
abline(0, 1)
arrows(x0 = c(2005:2012),
       y0 = alpha.male,
       x1 = c(2005:2012),
       y1 = alpha.male.upper,
       length = 0,
       col = "darkgrey")
arrows(x0 = c(2005:2012),
       y0 = alpha.male,
       x1 = c(2005:2012),
       y1 = alpha.male.lower,
       length = 0,
       col = "darkgrey")
points(c(2005:2012), alpha.male,
       pch = 16, 
       cex = 0.6)

plot(c(2005:2012), alpha.female,
     xlim = c(2005, 2012), 
     ylim = c(-20, 300),
     pch = 16,
     cex = 0.6,
     main = "Female Time Intercept",
     xlab = "Years",
     ylab = "Time Intercept")
abline(0, 1)
arrows(x0 = c(2005:2012),
       y0 = alpha.female,
       x1 = c(2005:2012),
       y1 = alpha.female.upper,
       length = 0,
       col = "darkgrey")
arrows(x0 = c(2005:2012),
       y0 = alpha.female,
       x1 = c(2005:2012),
       y1 = alpha.female.lower,
       length = 0,
       col = "darkgrey")
points(c(2005:2012), alpha.female,
       pch = 16, 
       cex = 0.6)

