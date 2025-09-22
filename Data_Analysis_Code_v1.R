rm(list=ls())
# Jan Blackburn
# Math 261A
# Project 1 R-Code

# Importing Raw Data and Cleaning Up
# Original data url: https://data.scorenetwork.org/basketball/nba_wingspans_and_performance.html

# Importing needed libraries
library(ggplot2)

# Importing and Cleaning the data file
trb_data = read.csv("nba_wingspan_data.csv")
head(trb_data)
summary(trb_data)
trb_data = na.omit(trb_data)
summary(trb_data)

# Generating a scatter plot to visually inspect the response and predictor variables
ggplot(trb_data, aes(x=wingspan_advantage, y=trb))+
  geom_point(color="blue", size=2, alpha=0.7)+
  geom_smooth(method="lm", color="red",linetype="solid",se=FALSE)+
  labs(title="Scatter Plot of Wingspan Advantage vs Total Rebounds",
       x="Wingspan Advantage (inches)",
       y="Total Rebounds")


ggplot(trb_data, aes(x=wingspan_inches, y=trb))+
  geom_point(color="blue", size=2, alpha=0.7)+
  geom_smooth(method="lm", color="red",linetype="solid",se=FALSE)+
  labs(title="Scatter Plot of Wingspan Advantage vs Total Rebounds",
       x="Wingspan (inches)",
       y="Total Rebounds")

# Running the simple regression model
model = lm(trb ~ wingspan_inches, data = trb_data)
summary(model)

# Adding the residuals and fitted data to the data frame
trb_data$residuals = resid(model)
trb_data$fitted = fitted(model)

# Residual plot: evaluate independence and constant variance of the residuals
ggplot(trb_data, aes(x=wingspan_inches, y=residuals))+
  geom_point()+
  scale_x_continuous(breaks=seq(70,120, by=10))+
  geom_hline(yintercept = 0, color="red")+
  labs(title="Residuals vs Wingspan", x="Wingspan (inches)", y="Residuals")

# Q-Q plot: Evaluate normality of the residuals
qqnorm(resid(model))
qqline(resid(model), col="red")

# Write the modified data to a new csv file
write.csv(trb_data, "trb_data_2.csv", row.names = FALSE)


