#install packages

install.packages("ggimage")
install.packages("Lahman")
install.packages("ggplot2")
require(dplyr)
require(Lahman)
require(ggplot2)

# see dataframes
LahmanData

head(Teams)

#Taking subset for analysis, get rid of useless columns
#filter data for 2021 season
WinexWin <- Teams %>% select(yearID,teamID,W,L,R,RA)%>%
  filter(yearID==2021)%>%
  
  #generate predictor
  #if team scores more runs than it gives up the quotient should be greater than 0.5
  #team that scores less runs than RA quotient will be less than 0.5
  #pythagorean run expectation
  mutate(wpct=R^2/(R^2+RA^2),expwin=round(wpct*(W+L)),diff=W-expwin)

#view data, shows us actual wins vs expected wins
WinexWin

#graph W vs Expected Wins w/ regression line
ggplot(WinexWin,aes(expwin,W)) + geom_point()+ stat_smooth(method="lm")


#linear regression model to assess 
#lm = linear model
mod <- lm(W~expwin,data=WinexWin)

#call mod to get y-intercept & slope
mod

#p value < .001 indicates highly significant regression
#Multiple R-squareed = 0.8879 means 88% of the variation in 
# of games won can be explained by RA or R
summary(mod)
