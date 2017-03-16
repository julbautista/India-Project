source("01 Data Munging.R")
#plot pooled values across time, naive look---------------------
pool <- ind %>% group_by(Year) %>% summarise(poolIMR = mean(Person), poolHexp = mean(percap))
ggplot(pool, aes(poolHexp, poolIMR, label = Year)) + public +
  geom_smooth(alpha = 0.12, method = 'lm') + 
  geom_text_repel(segment.color = jbpal$green, point.padding = unit(1.5, "lines"), size = 5) + 
  geom_point(size = 2.5) + ggtitle("Pooled IMR and Health Exp") +
  xlab("Health Expenditure") + ylab("Infant Mortality")
ggsave("naive.pdf", width = 17, height = 8.5)

rm(pool)

#this is an alternative plot that is not published
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
  healthexp2[[i]] <- 
    ggplot(subset(ind,ind$state.id == i), aes(Year, percap) ) + public + geom_line() + 
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
}
ggsave("HexpTime.pdf", plot = grid.arrange(grobs = healthexp2, nrow = 6, ncol = 5), width = 17, height = 8.5)


#Plot female and male IMR by state over time
genderIMR <- list()
for(i in 1:30){
  #j and gap help create labels for the largest IMR gender gaps per state
  j <- which.max(abs(ind[ind$state.id == i,]$Female - ind[ind$state.id == i,]$Male))
  gap <- ind[ind$state.id == i,][j,]
  
  genderIMR[[i]] <- ggplot(subset(ind,ind$state.id == i), aes(Year, Female)) + public + 
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
}
ggsave("genderIMR.pdf", plot = grid.arrange(grobs = genderIMR, nrow = 6, ncol = 5), width = 17, height = 8.5)

#Plot IMR by state over time
IMR <- list()
for(i in 1:30){
  IMR[[i]] <- 
    ggplot(subset(ind,ind$state.id == i), aes(Year, Person)) + public + geom_line() + 
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
}
ggsave("IMR.pdf", plot = grid.arrange(grobs = IMR, nrow = 6, ncol = 5), width = 17, height = 8.5)

#Plot IMR RATIO by state over time
ratioIMR <- list()
for(i in 1:30){
  ratioIMR[[i]] <- 
    ggplot(subset(ind,ind$state.id == i), aes(Year, fm.imr)) + public + geom_line() + 
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
