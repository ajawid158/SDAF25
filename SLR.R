##Simple Linear Regression Model


dt=read.csv("tips.csv")
names(dt)

cor(dt$total_bill, dt$tip)
plot(dt$total_bill, dt$tip)


model0=lm(dt$tip~dt$total_bill)
summary(model0)

model1=lm(dt$tip~dt$total_bill+dt$time)
summary(model1)

###employee
dt=read.csv("employee.csv")
View(dt)
names(dt)

##focus on physical characteristics
#Predict Weight using other relevant variables
#Y, LHS, Predicted variable, Dependent variable = Weight
#Height> X, predictor, RHS, independent
cor(dt$Height, dt$Weight)
plot(dt$Height, dt$Weight)

##we run the L model of Y on X
model0= lm(dt$Weight~dt$Height)
summary(model0)

names(dt)
#Spending > Y
#Best predictor > Salary  X

cor(dt$Salary, dt$Spending)

model1=lm(dt$Spending~dt$Salary)
summary(model1)

names(dt)

##Weight Model
#we used,  X:=Height,Age, Gender,Gym

# Weight = f(Height, Gender)

model2=lm(dt$Weight~dt$Height+dt$Gender)
summary(model2)

