##Simple Linear Regression Model


dt=read.csv("tips.csv")
names(dt)

cor(dt$total_bill, dt$tip)
plot(dt$total_bill, dt$tip)


model0=lm(dt$tip~dt$total_bill)
summary(model0)


plot(dt$total_bill, dt$tip)
abline(model0, col="red")
scatter.smooth(model0)


scatter.smooth(dt$total_bill, dt$tip)


