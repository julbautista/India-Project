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
#plot(lm(fm.imr ~ percap, data = ind))

#Sort by year
ind <- ind[order(ind$Year),]

#Create state id
ind <- within(ind, state.id <- match(State, unique(State)))

#Create Year id
ind <- within(ind, year.id <- match(Year, unique(Year)))

#plot pooled values across time, naive look
pool <- ind %>% group_by(Year) %>% summarise(poolIMR = mean(Person), poolHexp = mean(percap))
ggplot(pool, aes(poolHexp, poolIMR, label = Year)) + public +
  geom_smooth(alpha = 0.12, method = 'lm') + 
  geom_text_repel(segment.color = jbpal$green, point.padding = unit(1.5, "lines"), size = 5) + 
  geom_point(size = 2.5) + ggtitle("Pooled IMR and Health Exp") +
  xlab("Health Expenditure") + ylab("Infant Mortality")
ggsave("naive.pdf", width = 17, height = 8.5)

rm(pool)

#Plot per cap health exp by state over time
#non uniform y limit 

# healthexp <- list()
# for(i in 1:30){
#   plotter <- ggplot(subset(ind,ind$state.id == i), aes(Year, percap)) + public + geom_line() + 
#     labs(x = 'Year', y = 'Health Exp', title = unique(ind$State[ind$state.id == i])) + 
#     scale_x_continuous(breaks = c(2005, 2012)) +
#     scale_y_continuous(breaks = c(0, round(max(ind[ind$state.id == i,]$percap)))) +
#     theme(axis.text.x = element_text(size = 5),
#           axis.text.y = element_text(size = 9),
#           plot.title = element_text(size = 13),
#           axis.title.x = element_text(size = 7),
#           axis.title.y = element_text(size = 7),
#           panel.grid.major = element_blank())
#   healthexp[[i]] <- plotter
# }
# 
# ggsave("HexpTime.pdf", plot = grid.arrange(grobs = healthexp, nrow = 6, ncol = 5), width = 17, height = 8.5)



#uniform y limits healthexp
healthexp2 <- list()
for(i in 1:30){
  plotter <- ggplot(subset(ind,ind$state.id == i), aes(Year, percap) ) + public + geom_line() + 
    geom_text(x = 2005 + 0.25, y = round(min(ind[ind$state.id == i & ind$Year == 2005,]$percap)) + 350, label = round(min(ind[ind$Year == 2005 & ind$state.id == i ,]$percap)), size = 2) +
    geom_text(x = 2012 - 0.25, y = round(max(ind[ind$state.id == i & ind$Year == 2012,]$percap)) + 350, label = round(max(ind[ind$Year == 2012 & ind$state.id == i ,]$percap)), size = 2) +
    scale_x_continuous(breaks = c(2005, 2012)) +
    scale_y_continuous(breaks = c(0, 3000 ), limits = c(0,3100) ) +
    labs(x = 'Year', y = 'Health Exp', title = unique(ind$State[ind$state.id == i])) +
    
    theme(axis.text.x = element_text(size = 7),
          axis.text.y = element_text(size = 7),
          plot.title = element_text(size = 13),
          axis.title.x = element_text(size = 9),
          axis.title.y = element_text(size = 9),
          panel.grid.major = element_blank())
  healthexp2[[i]] <- plotter
}
ggsave("HexpTime.pdf", plot = grid.arrange(grobs = healthexp2, nrow = 6, ncol = 5), width = 17, height = 8.5)

#Plot female and male IMR by state over time
genderIMR <- list()
for(i in 1:30){
  #j and gap help create labels for the largest IMR gender gaps per state
  j <- which.max(abs(ind[ind$state.id == i,]$Female - ind[ind$state.id == i,]$Male))
  gap <- ind[ind$state.id == i,][j,]
  
  plotter <- ggplot(subset(ind,ind$state.id == i), aes(Year, Female)) + public + 
    geom_line(colour = jbpal$red) + 
    geom_line(aes(Year, Male), colour = jbpal$green) + 
    scale_x_continuous(breaks = c(2005, 2012)) +
    scale_y_continuous(breaks = c(0, round(max(ind[ind$state.id == i,]$Female))), limits = c(0, 85)) +
    geom_text(x = gap$Year, y = gap$Female, label = gap$Female, size = 2, colour =jbpal$red) +
    geom_text(x = gap$Year, y = gap$Male, label = gap$Male, size = 2, colour = jbpal$green) +
    labs(x = 'Year', y = 'IMR', title = unique(ind$State[ind$state.id == i])) +
    theme(axis.text.x = element_text(size = 5),
          axis.text.y = element_text(size = 5),
          plot.title = element_text(size = 9),
          axis.title.x = element_text(size = 7),
          axis.title.y = element_text(size = 7),
          panel.grid.major = element_blank())
  genderIMR[[i]] <- plotter
}
ggsave("genderIMR.pdf", plot = grid.arrange(grobs = genderIMR, nrow = 6, ncol = 5), width = 17, height = 8.5)


#Plot IMR by state over time
IMR <- list()
for(i in 1:30){
  plotter <- ggplot(subset(ind,ind$state.id == i), aes(Year, Person)) + public + geom_line() + 
    scale_x_continuous(breaks = c(2005, 2012)) +
    scale_y_continuous(breaks = c(0,80), limits = c(0,80) ) +
    geom_text(x = 2012, y = round(max(ind[ind$state.id == i,]$Person)), label = round(min(ind[ind$state.id == i,]$Person)), size = 2) +
    geom_text(x = 2005, y = round(min(ind[ind$state.id == i,]$Person)), label = round(max(ind[ind$state.id == i,]$Person)), size = 2) +
    
    labs(x = 'Year', y = 'IMR', title = unique(ind$State[ind$state.id == i])) +
    theme(axis.text.x = element_text(size = 5),
          axis.text.y = element_text(size = 5),
          plot.title = element_text(size = 9),
          axis.title.x = element_text(size = 7),
          axis.title.y = element_text(size = 7),
          panel.grid.major = element_blank())
  IMR[[i]] <- plotter
}
ggsave("IMR.pdf", plot = grid.arrange(grobs = IMR, nrow = 6, ncol = 5), width = 17, height = 8.5)

#Plot IMR RATIO by state over time
ratioIMR <- list()
for(i in 1:30){
  plotter <- ggplot(subset(ind,ind$state.id == i), aes(Year, fm.imr)) + public + geom_line() + 
    scale_x_continuous(breaks = c(2005, 2012)) +
    scale_y_continuous(breaks = c(0, round(min(ind[ind$state.id == i,]$fm.imr)), round(max(ind[ind$state.id == i,]$Person)), 2.7 ), limits = c(0,2.7) ) +
    geom_hline(yintercept = 1, linetype = 2, alpha = 0.2) +    
    labs(x = 'Year', y = 'IMR Ratio', title = unique(ind$State[ind$state.id == i])) +
    theme(axis.text.x = element_text(size = 5),
          axis.text.y = element_text(size = 5),
          plot.title = element_text(size = 9),
          axis.title.x = element_text(size = 7),
          axis.title.y = element_text(size = 7),
          panel.grid.major = element_blank())
  ratioIMR[[i]] <- plotter
}
ggsave("IMRratio.pdf", plot = grid.arrange(grobs = ratioIMR, nrow = 6, ncol = 5), width = 17, height = 8.5)



#Plot IMR in 2012 and 2005 by State
ind2 <- ind
ind2$State <- factor(ind2$State)
ind2$State <- factor(ind2$State, levels = ind2$State[order(ind2$Person[ind2$Year == 2012])])

IMR0512 <- ggplot(ind2[ind2$Year == 2012| ind2$Year == 2005,], aes(Person, State, colour = factor(Year), fill = factor(Year))) + 
  ggtitle("Infant Mortality Rate per State, 2012 and 2005") +
  public  + 
  geom_dotplot(binaxis = "y", dotsize = 0.35) + 
  labs(x = 'IMR', y = "") +
  jbcol + jbfill +
  theme(legend.title = element_blank(), panel.grid.major.x = element_blank(), 
        axis.line = element_blank(),
        axis.text = element_text(colour = "#2f4f4f", family = "Open Sans", size = 14),
        legend.text = element_text(colour = "#2f4f4f", family = "Open Sans", size = 20)) 
ggsave("IMR0512.pdf", plot = IMR0512, width = 17, height = 8.5)

rm(ind2)


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
  pred[[i]] <- plotter
}
ggsave("predfem.pdf", plot = grid.arrange(grobs = pred, nrow = 6, ncol = 5), width = 17, height = 8.5)

#Posterior Predictive Check
ggplot(ind, aes(pred, Female)) + public + geom_abline(slope = 1, intercept = 0) + geom_errorbarh(data = ind, aes(y = Female, x = pred, xmin = y.lower, xmax = y.upper)) + geom_point() + ggtitle("Posterior Predictive Check on Females") + xlim(c(0,80)) + ylim(c(0,80))
ggsave("postpredfem.pdf", width = 17, height = 8.5)

#Male pred model
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

#extract quantiles
ind$y.upperm <- NULL
ind$y.lowerm <- NULL
for(i in 1:240){
  ind$y.lowerm[i] <- quantile(extract(fitm)$y_pred[,i], 0.25)
  ind$y.upperm[i] <- quantile(extract(fitm)$y_pred[,i], 0.75)
}

#plot predicted values by state for males
#blue is predicted green is actual
ind$predm <- (colMeans(extract(fitm)$y_pred))
predm <- list()
for(i in 1:30){
  plotter <- ggplot(subset(ind,ind$state.id == i), 
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
  predm[[i]] <- plotter
}
ggsave("predmal.pdf", plot = grid.arrange(grobs = predm, nrow = 6, ncol = 5), width = 17, height = 8.5)


#Posterior Predictive Check
ggplot(ind, aes(predm, Male)) + public + geom_abline(slope = 1, intercept = 0) + geom_errorbarh(data = ind, aes(y = Male, x = predm, xmin = y.lowerm, xmax = y.upperm)) + geom_point() + ggtitle("Posterior Predictive Check on Males") + xlim(c(0,80)) + ylim(c(0,80))
ggsave("postpredfem.pdf", width = 17, height = 8.5)


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
for(i in 1:30){
  plotter <- ggplot(subset(ind1,ind1$state.id == i), 
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
  pred2[[i]] <- plotter
}
ggsave("lagpredfem.pdf", plot = grid.arrange(grobs = pred2, nrow = 6, ncol = 5), width = 17, height = 8.5)


#Posterior Predictive Check
ggplot(ind1, aes(pred2, Female)) + public + geom_abline(slope = 1, intercept = 0) + geom_errorbarh(data = ind1, aes(y = Female, x = pred2, xmin = y.lower2, xmax = y.upper2)) + geom_point() + ggtitle("Posterior Predictive Check on Females") + xlim(c(0,90)) + ylim(c(0,90)) + xlab("Pred")
ggsave("lagpostpredfem.pdf", width = 17, height = 8.5)





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
  pred3[[i]] <- plotter
}
ggsave("lagpredmal.pdf", plot = grid.arrange(grobs = pred3, nrow = 6, ncol = 5), width = 17, height = 8.5)


#Posterior Predictive Check
ggplot(ind1, aes(pred3, Male)) + public + geom_abline(slope = 1, intercept = 0) + geom_errorbarh(data = ind1, aes(y = Male, x = pred3, xmin = y.lower3, xmax = y.upper3)) + geom_point() + ggtitle("Posterior Predictive Check on Males") + xlim(c(0,85)) + ylim(c(0,85)) + xlab("Pred")
ggsave("lagpostpredmal.pdf", width = 17, height = 8.5)


#Look at distribution of effects by state in lagged log model

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
  #dense[[i]] <- plotter
}

ggsave("betas.pdf", plot = grid.arrange(grobs = dense, nrow = 6, ncol = 5), width = 17, height = 8.5)



#Just look at the Mus

#plotting alpha for persons
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

