# Lab trials - anesthesia
# creating a line plot
# running analysis 
# Larval Biology project 2
# 10/19/16


rm(list=ls())

# required
library(lme4)

df <- read.csv("LabTrials_all.csv", header=TRUE)
df
head(df)

failure <- df$Sample.size-df$Success
df <- cbind(df, failure)
head(df)

# visualize data
plot(df$Success/df$Sample.size ~ df$Time + df$Concentration)
boxplot(df$Success/df$Sample.size ~ df$Time + df$Concentration)



# df$Time <- as.factor(df$Time)

# treatments (concentrations)
conc <- unique(df$Concentration)
conc
length(conc)

# time (time in relaxation)
time <- unique(df$Time)
time
length(time)

# Make each treatment its own df
df.0 <- df[df$Concentration==0, ]
df.75 <- df[df$Concentration==75, ]
df.85 <- df[df$Concentration==85, ]
df.100 <- df[df$Concentration==100, ]

# all data used
# sum(nrow(df.0), nrow(df.75), nrow(df.85), nrow(df.100))
# nrow(df)

# # calculating the mean and sse for each time
# summarize <- function (df.85) {
#   s.mean <- tapply(df.85$Success/df.85$Sample.size, df.85$Time, mean)
#   sd <- tapply(df.85$Success/df.85$Sample.size, df.85$Time, sd)
#   n <- tapply(df.85$Success/df.85$Sample.size, df.85$Time, length)
#   CI <- -qnorm(.05/2) * sd/sqrt(n)
#   min <- unique(df.85$Time)
#   results <- as.data.frame(cbind(min, s.mean, CI))
#   return(results)
# }
# 
# # summary tables
# sum.0 <- summarize(df.0)
# sum.75 <- summarize(df.75)
# sum.85 <- summarize(df.85)
# sum.100 <- summarize(df.100)
# 
# is.data.frame(sum.100)
# sum.0
# sum.75
# sum.85
# sum.100

df[df.85$Time < 75, ]      # removing time >60

sum.data <- function (df.85) {
  df.85 <- df.85[df.85$Time < 75, ]
  open <- tapply(df.85$Success, df.85$Time, sum)
  n <- tapply(df.85$Sample.size, df.85$Time, sum)
  n.fail <- tapply(df.85$failure, df.85$Time, sum)
#  CI <- -qnorm(.05/2) * sd/sqrt(n)
  openProp <- open/n
  min <- unique(df.85$Time)
  results <- as.data.frame(cbind(min, open, n, n.fail, openProp))
  return(results)
}


sum.0 <- sum.data(df.0)
sum.75 <- sum.data(df.75)
sum.85 <- sum.data(df.85)
sum.100 <- sum.data(df.100)





###############################################
#============ PLOT

# list of data frames
#dd <- list(sum.0, sum.75, sum.85, sum.100)


windows(6,5)
#graphical parameters
col <- rep('black', 4)
pch <- c(19,0,1,2)
lty <- c(3,2,4,1)
label <- c('Control', '75 g/L', '85 g/L', '100 g/L')

# base plot
plot(sum.100$min, sum.100$openProp,
     xlab='Time (min)', ylab='Cumulative proportion anesthetized',
     type='n', bty='l', xaxs='i', yaxs='i', xpd=TRUE, yaxt='n',
     xlim=c(0,61), ylim=c(0,.42), main='Lab Trial Anesthesia Success')
axis(2, at=c(0,.1,.2,.3,.4), labels=TRUE, las=2)

points(sum.0$min, sum.0$openProp, type='o', col=col[1], pch=pch[1], lty=lty[1],
       xpd=TRUE)
#error.bar(sum.0$min, sum.0$s.mean, sum.0$CI, xpd=TRUE)
points(sum.75$min, sum.75$openProp, type='o', col=col[2], pch=pch[2], lty=lty[2],
       xpd=TRUE)
#error.bar(sum.75$min, sum.75$s.mean, sum.75$CI)
points(sum.85$min, sum.85$openProp, type='o', col=col[3], pch=pch[3], lty=lty[3],
       xpd=TRUE)
#error.bar(sum.85$min, sum.85$s.mean, sum.85$CI)
points(sum.100$min, sum.100$openProp, type='o', col=col[4], pch=pch[4], lty=lty[4],
       xpd=TRUE)
#error.bar(sum.100$min, sum.100$s.mean, sum.100$CI)
legend('topleft', legend=label, pch=pch, lty=lty, bty='n')


# #adding points
# for (i in dd) {
#     points(i$min, i$s.mean, type='o', col=col, pch=17)
# }


# ==== end plot


# #====== ERROR BAR FUNCTION
# error.bar <- function(x, y, upper, lower=upper, length=0.1,...){
#   if(length(x) != length(y) | length(y) !=length(lower) | length(lower) != length(upper))
#     stop("vectors must be same length")
#   arrows(x,y+upper, x, y-lower, angle=90, code=3, length=length, ...)
# }
