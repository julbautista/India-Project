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
summary(lm(fm.imr ~ percap,
           data = ind))

#Sort by year
ind <- ind[order(ind$Year),]

#Create state id
ind <- within(ind, state.id <- match(State, unique(State)))

#Create Year id
ind <- within(ind, year.id <- match(Year, unique(Year)))

#Plot per cap health exp by state over time
par(mfrow = c(5, 6), 
    mar = c(1.5, 1.75, 1, 1),
    mgp = c(2, .5, 0),
    las = 1,
    cex = 0.6)
for(i in 1:30){
  plot(ind$Year[ind$state.id == i], 
       ind$percap[ind$state.id == i],
       type = "l",
       ylim = c(0, 3000),
       yaxt = "n", 
       xaxt = "n",
       xlab = "year",
       main = paste(unique(ind$State[ind$state.id == i])), 
       cex.main = 0.9)
  axis(1, at = c(2005, 2012), 
       lwd = 0.5, 
       cex.axis = 0.6)
  axis(2, at = c(0, 3000),
       lwd = 0.5,
       cex.axis = 0.6)
  title(ylab = "IMR", 
        line=0, cex.lab= .6)
  title(xlab = "yr", 
        line=0, cex.lab= .6)
}

#Plot male IMR by state over time
par(mfrow = c(5, 6), 
    mar = c(1.5, 1.75, 1, 1),
    mgp = c(2, .5, 0),
    las = 1,
    cex = 0.8)
for(i in 1:30){
  plot(ind$Year[ind$state.id == i], 
       ind$Male[ind$state.id == i],
       type = "l",
       ylim = c(0, max(ind$Male)),
       yaxt = "n", xaxt = "n",
       main = paste(unique(ind$State[ind$state.id == i])), 
       cex.main = 0.9)
  axis(1, at = c(2005, 2012), 
       lwd = 0.5, 
       cex.axis = 0.6)
  axis(2, at = c(0, 75),
       lwd = 0.5,
       cex.axis = 0.6)
}

#Plot female IMR by state over time
par(mfrow = c(5, 6), 
    mar = c(1.5, 1.75, 1, 1),
    mgp = c(2, .5, 0),
    las = 1,
    cex = 0.8)
for(i in 1:30){
  plot(ind$Year[ind$state.id == i], 
       ind$Female[ind$state.id == i],
       type = "l",
       ylim = c(0, max(ind$Female)),
       yaxt = "n", xaxt = "n",
       main = paste(unique(ind$State[ind$state.id == i])), 
       cex.main = 0.9)
  axis(1, at = c(2005, 2012), 
       lwd = 0.5, 
       cex.axis = 0.6)
  axis(2, at = c(0, 75),
       lwd = 0.5,
       cex.axis = 0.6)
}

#Plot female and male IMR by state over time
par(mfrow = c(5, 6), 
    mar = c(1.4, 1.1, 1, 1),
    mgp = c(2, .5, 0),
    las = 1,
    cex = 0.8)
for(i in 1:30){
  plot(ind$Year[ind$state.id == i], 
       ind$Female[ind$state.id == i],
       type = "l",
       ylim = c(0, max(ind$Female)),
       yaxt = "n", 
       xaxt = "n",
       main = paste(unique(ind$State[ind$state.id == i])), 
       cex.main = 0.9,
       col = "red")
  lines(ind$Year[ind$state.id == i], 
        ind$Male[ind$state.id == i],
        type = "l",
        col = "blue",
        lty = 1)
  axis(1, at = c(2005, 2012), 
       lwd = 0.5, 
       cex.axis = 0.6)
  axis(2, at = c(0, 80),
       lwd = 0.5,
       cex.axis = 0.6)
  title(ylab = "IMR", 
        line=0, cex.lab= .6)
  title(xlab = "yr", 
        line=0, cex.lab= .6)
}

#Plot IMR by state over time
par(mfrow = c(5, 6), 
    mar = c(1.5, 1.5, 1, 1),
    cex = 0.6,
    las = 1)
for(i in 1:30){
  plot(ind$Year[ind$state.id == i], 
       ind$Person[ind$state.id == i],
       type = "l",
       ylim = c(0, 100), 
       yaxt = "n", xaxt = "n",
       main = paste(unique(ind$State[ind$state.id == i])),
       cex.main = 0.9)
  axis(1, at = c(2005, 2012), 
       lwd = 0.5, 
       cex.axis = 0.6)
  axis(2, at = c(0, 80),
       lwd = 0.5,
       cex.axis = 0.6)
  title(ylab = "IMR", 
        line=0, cex.lab= .8)
  title(xlab = "yr", 
        line=0, cex.lab= .8)
}

#Plot IMR RATIO by state over time
par(mfrow = c(5, 6), 
    mar = c(1.5, 1.5, 1, 1),
    cex = 0.6
)
for(i in 1:30){
  plot(ind$Year[ind$state.id == i], 
       ind$fm.imr[ind$state.id == i],
       type = "l", 
       ylim = c(1.1*min(ind$fm.imr), 3),
       yaxt = "n", xaxt = "n",
       main = paste(unique(ind$State[ind$state.id == i])), 
       cex.main = 0.9)
  axis(2, 
       at = c(1, 3),
       cex.axis = 0.8)
  abline(h = 1, 
         col = "grey",
         lty = 2)
  axis(1, at = c(2005, 2012), 
       lwd = 0.5, 
       cex.axis = 0.8)
  title(ylab = "IMR Ratio", 
        line=0, cex.lab= .6)
  title(xlab = "yr", 
        line=0, cex.lab= .6)
}

ind <- ind[order(ind$Person),]

#Plot IMR in 2012 by State
par(mfrow = c(1, 1),
    mar = c(3, 9, 1, 2),
    las = 1,
    cex = 0.7)
plot(ind$Person[ind$Year == 2012],
     c(1:30),
     yaxt = 'n',
     ylab = " ",
     xlim = c(0, 80),
     pch = 16,
     lwd = 0.5,
     xlab = "Infant Mortality Rate",
     main = NULL)
axis(2, at = c(1:30), 
     labels = c(ind$State[ind$Year == 2012]),
     cex = 0.8)
abline(h = c(1:30), col = "grey")
points(ind$Person[ind$Year == 2012],
       c(1:30),
       pch = 16,
       cex = 0.8)
text(50, 30, "2012")
text(80, 30, "2005",
     col = "blue")

#Add blue points for 2005
points(ind$Person[ind$Year == 2005],
       c(1:30),
       pch = 16,
       cex = 1,
       col = "blue")

#All eight years
par(mfrow = c(2, 4),
    mar = c(3, 6, 1, 1),
    mgp = c(1.5, .5, 0),
    las = 1,
    cex = 0.8)
for(i in 1:8){
  plot(ind$Person[ind$year.id == i],
       c(1:30),
       yaxt = 'n',
       ylab = " ",
       xaxt = 'n',
       pch = 16,
       lwd = 0.5,
       xlab = "IMR",
       cex.lab = .8,
       xlim = c(0, 80),
       main = paste(unique(ind$Year[ind$year.id == i])), 
       cex.main = 0.9)
  axis(2, at = c(1:30), 
       labels = c(ind$State[ind$year.id == i]),
       cex.axis = 0.7)
  axis(1, at = c(0, 20, 40, 60, 80), 
       cex.axis = 0.8)
  abline(h = c(1:30), col = "grey")
  points(ind$Person[ind$year.id == i],
         c(1:30),
         pch = 16,
         cex = 0.8)
}

#Reorder by year
ind <- ind[order(ind$Year),]

#Stan model normal 
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

ind$y.upper <- NULL
ind$y.lower <- NULL
for(i in 1:240){
  ind$y.lower[i] <- quantile(extract(fit1)$y_pred[,i], 0.25)
  ind$y.upper[i] <- quantile(extract(fit1)$y_pred[,i], 0.75)
}

#plot predicted values by state
ind$pred <- (colMeans(extract(fit1)$y_pred))

par(mfrow = c(5, 6), 
    mar = c(1, 1, 1, 1),
    cex = 0.6
)
for(i in 1:30){
  plot(ind$Year[ind$state.id == i], 
       ind$pred[ind$state.id == i],
       type = "l", 
       ylim = c(0.75*min(ind$Female), 
                1.1*max(ind$Female)),
       yaxt = "n", 
       xaxt = "n",
       main = paste(unique(ind$State[ind$state.id == i])), 
       cex.main = 0.9)
  axis(2, at = 1,
       labels = 1)
  abline(h = 1, 
         col = "grey",
         lty = 2)
  lines(ind$Year[ind$state.id == i], 
        ind$Female[ind$state.id == i],
        type = "l",
        col = "red")
  lines(ind$Year[ind$state.id == i], 
        (ind$y.lower[ind$state.id == i]),
        type = "l",
        col = "darkgrey",
        lty = 2)
  lines(ind$Year[ind$state.id == i], 
        (ind$y.upper[ind$state.id == i]),
        type = "l",
        col = "darkgrey",
        lty = 2)
}

#Posterior Predictive Check
par(mfrow = c(1, 1), 
    mar = c(3, 3, 1, 1),
    mgp = c(2, 1, 0),
    cex = 0.8)
plot(colMeans(extract(fit1)$y_pred), imr, 
     xlim = c(0, 80), 
     ylim = c(0, 80),
     pch = 16,
     cex = 0.6,
     xlab = "Predicted IMR",
     ylab = "IMR")
abline(0, 1)
arrows(x0 = colMeans(extract(fit1)$y_pred),
       y0 = imr,
       x1 = ind$y.upper,
       y1 = imr,
       length = 0,
       col = "darkgrey")
arrows(x0 = colMeans(extract(fit1)$y_pred),
       y0 = imr,
       x1 = ind$y.lower,
       y1 = imr,
       length = 0,
       col = "darkgrey")
points(colMeans(extract(fit1)$y_pred), imr,
       pch = 16, 
       cex = 0.6)

#Look at distribution of effects by state
par(mfrow = c(5, 6), 
    mar = c(2, 1, 1, 1),
    mgp = c(2, 1, 0))
for(i in 1:30){
  hist((extract(fit1)$beta[,i]),
       yaxt = 'n',
       xaxt = 'n',
       xlim = c(-.05, .05),
       main = paste(unique(ind$State[ind$state.id == i])), 
       cex.main = 0.9,
       breaks = 10)
  abline(v = 0,
         col = "darkgrey",
         lty = 2)
  axis(1, at = c(-.05, 0, .05),
       cex.axis = 0.8)
}

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
  ind1$y.lower2[i] <- quantile(extract(fit2)$y_pred[,i], 0.25)
  ind1$y.upper2[i] <- quantile(extract(fit2)$y_pred[,i], 0.75)
}

#plot predicted values by state
ind1$pred2 <- (colMeans(extract(fit2)$y_pred))

par(mfrow = c(5, 6), 
    mar = c(1, 1, 1, 1),
    cex = 0.6
)
for(i in 1:30){
  plot(ind1$Year[ind1$state.id == i], 
       exp(ind1$pred2[ind1$state.id == i]),
       type = "l", 
       ylim = c(0.75*min(ind1$Female), 
                1.1*max(ind1$Female)),
       yaxt = "n", 
       xaxt = "n",
       main = paste(unique(ind1$State[ind1$state.id == i])), 
       cex.main = 0.9)
  axis(2, at = 1,
       labels = 1)
  abline(h = 1, 
         col = "grey",
         lty = 2)
  lines(ind1$Year[ind1$state.id == i], 
        (ind1$Female[ind1$state.id == i]),
        type = "l",
        col = "red")
  lines(ind1$Year[ind1$state.id == i], 
        exp(ind1$y.lower2[ind1$state.id == i]),
        type = "l",
        col = "darkgrey",
        lty = 2)
  lines(ind1$Year[ind1$state.id == i], 
        exp(ind1$y.upper2[ind1$state.id == i]),
        type = "l",
        col = "darkgrey",
        lty = 2)
}

#Posterior Predictive Check
par(mfrow = c(1, 1), 
    mar = c(3, 3, 1, 1),
    mgp = c(2, 1, 0),
    cex = 0.8)
plot(exp(colMeans(extract(fit2)$y_pred)), exp(imr), 
     xlim = c(0, 85), 
     ylim = c(0, 85),
     pch = 16,
     cex = 0.6,
     xlab = "Predicted IMR",
     ylab = "IMR")
abline(0, 1)
arrows(x0 = exp(colMeans(extract(fit2)$y_pred)),
       y0 = exp(imr),
       x1 = exp(ind1$y.upper2),
       y1 = exp(imr),
       length = 0,
       col = "darkgrey")
arrows(x0 = exp(colMeans(extract(fit2)$y_pred)),
       y0 = exp(imr),
       x1 = exp(ind1$y.lower2),
       y1 = exp(imr),
       length = 0,
       col = "darkgrey")
points(exp(colMeans(extract(fit2)$y_pred)), exp(imr),
       pch = 16, 
       cex = 0.6)

#Look at distribution of effects by state
par(mfrow = c(5, 6), 
    mar = c(2, 1, 1, 1),
    mgp = c(2, 1, 0))
for(i in 1:30){
  hist((extract(fit2)$beta[,i]),
       yaxt = 'n',
       xaxt = 'n',
       xlim = c(-.25, .25),
       main = paste(unique(ind1$State[ind1$state.id == i])), 
       cex.main = 0.9,
       breaks = 20)
  abline(v = 0,
         col = "darkgrey",
         lty = 2,
         lwd = 2)
  axis(1, at = c(-.25, 0, .25),
       cex.axis = 0.8)
}


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
  ind1$y.lower3[i] <- quantile(extract(fit3)$y_pred[,i], 0.25)
  ind1$y.upper3[i] <- quantile(extract(fit3)$y_pred[,i], 0.75)
}

#plot predicted values by state
ind1$pred3 <- (colMeans(extract(fit3)$y_pred))

par(mfrow = c(5, 6), 
    mar = c(1, 1, 1, 1),
    cex = 0.6
)
for(i in 1:30){
  plot(ind1$Year[ind1$state.id == i], 
       exp(ind1$pred3[ind1$state.id == i]),
       type = "l", 
       ylim = c(0.75*min(ind1$Male), 
                1.1*max(ind1$Male)),
       yaxt = "n", 
       xaxt = "n",
       main = paste(unique(ind1$State[ind1$state.id == i])), 
       cex.main = 0.9)
  axis(2, at = 1,
       labels = 1)
  abline(h = 1, 
         col = "grey",
         lty = 2)
  lines(ind1$Year[ind1$state.id == i], 
        (ind1$Male[ind1$state.id == i]),
        type = "l",
        col = "red")
  lines(ind1$Year[ind1$state.id == i], 
        exp(ind1$y.lower3[ind1$state.id == i]),
        type = "l",
        col = "darkgrey",
        lty = 2)
  lines(ind1$Year[ind1$state.id == i], 
        exp(ind1$y.upper3[ind1$state.id == i]),
        type = "l",
        col = "darkgrey",
        lty = 2)
}

#Posterior Predictive Check
par(mfrow = c(1, 1), 
    mar = c(3, 3, 1, 1),
    mgp = c(2, 1, 0),
    cex = 0.8)
plot(exp(colMeans(extract(fit3)$y_pred)), exp(imr), 
     xlim = c(0, 85), 
     ylim = c(0, 85),
     pch = 16,
     cex = 0.6,
     xlab = "Predicted IMR",
     ylab = "IMR")
abline(0, 1)
arrows(x0 = exp(colMeans(extract(fit3)$y_pred)),
       y0 = exp(imr),
       x1 = exp(ind1$y.upper3),
       y1 = exp(imr),
       length = 0,
       col = "darkgrey")
arrows(x0 = exp(colMeans(extract(fit3)$y_pred)),
       y0 = exp(imr),
       x1 = exp(ind1$y.lower3),
       y1 = exp(imr),
       length = 0,
       col = "darkgrey")
points(exp(colMeans(extract(fit3)$y_pred)), exp(imr),
       pch = 16, 
       cex = 0.6)

#Look at distribution of effects by state
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
par(mfrow = c(1, 1))
plot(density(rnorm(10^4, 
                   mean = mean(extract(fit3)$mu_beta),
                   sd = mean(extract(fit3)$tau_beta))),
     col = "blue",
     lwd = 2,
     ylim = c(0, 15),
     main = ' ',
     yaxt = 'n')
lines(density(rnorm(10^4, 
                    mean = mean(extract(fit2)$mu_beta),
                    sd = mean(extract(fit2)$tau_beta))),
      col = "red",
      lwd = 2)
abline(v = mean(extract(fit2)$mu_beta),
       col = "red",
       lty = 2)
abline(v = mean(extract(fit3)$mu_beta),
       col = "blue",
       lty = 2)

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

#time intercepts for persons
alpha.per <- colMeans(extract(fit1)$alpha_t[,1:8])
alpha.per.upper <- NULL
alpha.per.lower <- NULL
for(i in 1:8){
   alpha.per.upper[i] <- quantile(extract(fit1)$alpha_t[,i], 0.25)
   alpha.per.lower[i] <- quantile(extract(fit1)$alpha_t[,i], 0.75)
 }
par(mfrow = c(1, 1))
plot(c(2005:2012), alpha.per,
      xlim = c(0, 85), 
      ylim = c(0, 85),
      pch = 16,
      cex = 0.6,
      xlab = "Time",
      ylab = "Time Intercept")
abline(0, 1)
arrows(x0 = c(2005:2012),
        y0 = alpha.per,
        x1 = alpha.per.upper,
        y1 = c(2005:2012),
        length = 0,
        col = "darkgrey")
arrows(x0 = c(2005:2012),
        y0 = alpha.per,
        x1 = alpha.per.lower,
        y1 = c(2005:2012),
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

