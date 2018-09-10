data = read.csv("~/Desktop/Project/forestfires.csv")
lm.all = lm(data$area ~ data$X + data$Y + data$month + data$day + data$FFMC + data$DMC + data$DC + data$ISI + data$temp + data$RH + data$wind + data$rain) 
summary(lm.all)
summary(data$month)
plot(data)

par(mfrow = c(1,2))
boxplot(data5$area ~ data5$month)
boxplot(data5$temp ~ data5$month)
boxplot(data5$temp ~ data5$month)
summary(data$area)


data15 = data.frame(data$area, data$month, data$X, data$temp, data$wind, data$rain, 6, 0, 0)
colnames(data15)<-c("area","month","X","temp", "wind", "rain", "fire_classifition", "season", "intersection")
data15[which(data15[,2]=="dec" | data15[,2]=="jan" | data15[,2]=="feb"),8] = 1
data15[which(data15[,1] == 0),7] = 1
data15[which(data15[,1] >0 & data15[,1] <= 4),7] = 2
data15[which(data15[,1] >4 & data15[,1] <= 40),7] = 3
data15[which(data15[,1] >40 & data15[,1] <= 120),7] = 4
data15[which(data15[,1] >120 & data15[,1] <= 400),7] = 5
data15$intersection = data15$season * data15$temp
View(data15)

lm.dummy5 = lm(data15$fire_classifition ~ X + temp + wind + rain + data15$season + data15$intersection, data = data15)
summary(lm.dummy5)

lm_dummy2 = lm(log(data11$area + 1) ~ X + Y + FFMC**10 + DMC + DC + ISI + temp + RH + wind + rain + (FFMC**10)*S1 + (FFMC**10)*S2 + (FFMC**10)*S3 + DMC*S1 + DMC*S2 + DMC*S3 + DC*S1 + DC*S2 + DC*S3 + ISI*S1 + ISI*S2 + ISI*S3 + temp*S1 + temp*S2 + temp*S3 + RH*S1 + RH*S2 + RH*S3 + rain*S1 + rain*S2 + rain*S3 + S1 + S2 + S3, data = data11)
summary(lm_dummy2)
step(lm_dummy2)
lm_dummy_linear = lm(log(data11$area + 1) ~ temp + X + wind + rain + S1 + temp*S1, data = data11)
summary(lm_dummy_linear)


data16 = data
data16 = cbind(data, 0, 0, 0, 2, 6)
colnames(data16)<-c("X","Y","month", "day", "FFMC", "DMC", "DC", "ISI", "temp", "RH", "wind", "rain", "area", "S1","S2","S3","season","fire_class")
data16[which(data16[,3]=="dec" | data16[,3]=="jan" | data16[,3]=="feb"),14] = 1
data16[which(data16[,3]=="mar" | data16[,3]=="apr" | data16[,3]=="may"),15] = 1
data16[which(data16[,3]=="jul" | data16[,3]=="aug" | data16[,3]=="sep"),16] = 1

data16[which(data16[,3]=="dec" | data16[,3]=="jan" | data16[,3]=="feb"),17] = 1
data16[which(data16[,3]=="mar" | data16[,3]=="apr" | data16[,3]=="may"),17] = 3
data16[which(data16[,3]=="jul" | data16[,3]=="aug" | data16[,3]=="sep"),17] = 4

data16[which(data16[,13] == 0),18] = 1
data16[which(data16[,13] >0 & data16[,13] <= 4),18] = 2
data16[which(data16[,13] >4 & data16[,13] <= 40),18] = 3
data16[which(data16[,13] >40 & data16[,13] <= 120),18] = 4
data16[which(data16[,13] >120 & data16[,13] <= 400),18] = 5

View(data16)

lm_dummy6 = lm(fire_class ~ X + Y + FFMC**10 + DMC + DC + ISI + temp + RH + wind + rain + (FFMC**10)*S1 + (FFMC**10)*S2 + (FFMC**10)*S3 + DMC*S1 + DMC*S2 + DMC*S3 + DC*S1 + DC*S2 + DC*S3 + ISI*S1 + ISI*S2 + ISI*S3 + temp*S1 + temp*S2 + temp*S3 + RH*S1 + RH*S2 + RH*S3 + rain*S1 + rain*S2 + rain*S3 + S1 + S2 + S3, data = data16)
summary(lm_dummy6)
step(lm_dummy6)

lm_dummy7 = lm(fire_class ~ X + temp + RH + S1 + temp*S1 + RH*S1, data = data16)
summary(lm_dummy7)
lm_dummy8 = lm(fire_class ~ X + temp + S1 + temp*S1 + RH*S1, data = data16)
summary(lm_dummy8)
anova(lm_dummy7, lm_dummy8)


lm_dummy9 = lm(fire_class ~  X + temp + S1  + temp*S1  + RH + RH*S1, data = data16)
anova(lm_dummy7, lm_dummy9)
summary(lm_dummy10)


coplot(data15$fire_classifition ~ temp| season, data=data15, panel = function(x,y,...){
  panel.smooth(x,y,span = 0.8,iter = 5,col.smooth = "green4",...)
  abline(lm(y ~ x), col="red")},
  rows=1,pch=19, cex=0.3)


table(data16$fire_class)
data16$fire_class = as.ordered(data16$fire_class)

ind = sample(2, nrow(data16), replace = TRUE, prob = c(0.8, 0.2))
train = data16[ind == 1, ]
test = data16[ind == 2, ]
data5$fire_class

library(MASS)
View(data5$fire_class)
model = polr(fire_class ~ X + temp + temp*S1 ,data5, Hess = TRUE)
model1 = polr(fire_class ~ X + temp + RH + wind + temp*S1 + temp*S2 + temp*S3 + RH*S1 + RH*S2 + RH*S3 + S1 + S2 + S3 ,  data5, Hess = TRUE)
summary(model1)

library(MASS)
model1 = polr(fire_class ~ X + RH + DC + DMC + FFMC + ISI + temp + wind + S1 + S2 + S3 + temp*S1 + temp*S2 + temp*S3 + wind*S1 , data5, Hess = TRUE)
summary(model1) 

step(model1)

ctable = coef(summary(model))
p = pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable = cbind(ctable, "p value" = p))

model = polr(fire_class ~ X + temp + RH + S1 + S2 + temp*S1 + temp*S2 + RH*S1 , data16, Hess = TRUE)
summary(model)

model = polr(fire_class ~ X + temp + S1 + S2 + S3 + temp*S1 + temp*S2, data5, Hess = TRUE)

ctable = coef(summary(model))
p = pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2
(ctable = cbind(ctable, "p value" = p))

pred = predict(model, train)
print(pred, digits = 3)

(tab <- table(pred, train$fire_class))
1 - sum(diag(tab))/sum(tab)

pred1 = predict(model, test)
(tab1 = table(pred1, test$fire_class))
1 - sum(diag(tab))/sum(tab)

View(data16)

X = matrix(c(rep.int(1,517),data$X, data$Y,data$FFMC, data$DMC, data$DC, data$ISI, data$temp, data$RH, data$wind, data$rain),nrow=517,ncol=11)
h_hat = X%*%(solve(t(X)%*%X))%*%t(X)
row_num = matrix(row.names(data))
hat_value = data.frame(row_num,diag(h_hat))
colnames(hat_value) = c('row_num','h_hat')
high_leverage_value = data.frame(hat_value[hat_value$h_hat >= (mean(hat_value$h_hat)*2),])
c(high_leverage_value['row_num'])
data2 = data[- c(high_leverage_value$row_num), ]
plot(data2)
par(mfrow = c(1,1))
plot(data2$area)
hist(data$area,300)
area1 = log(data$area + 1 )
hist(area1,100)
data_zero = subset(data, area == 0)
data_nzero = subset(data, area != 0)
summary(data_zero)
summary(data_nzero)
subset(data_nzero, area<1)
hist(log(data_nzero$area + 1), 100)

par(mfrow = c(3,3))
hist(log(data$area + 1), 100)
hist(data$FFMC, 100)
hist(data$DMC, 100)
hist(data$DC, 100)
hist(data$ISI, 100)
hist(data$temp, 100)
hist(data$RH, 100)
hist(data$wind, 100)
hist(data$rain, 100)

par(mfrow = c(3,3))
hist(log(data_nzero1$area + 1), 100)
hist(data_nzero1$FFMC, 100)
hist(data_nzero1$DMC, 100)
hist((data_nzero1$DC)**2, 100)
hist(data_nzero1$ISI, 100)
hist(data_nzero1$temp, 100)
hist(data_nzero1$RH, 100)
hist(data_nzero1$wind, 100)
hist(data_nzero1$rain, 100)

View(data)
OutVals = boxplot(data_nzero$FFMC)$out
max(OutVals)
data_nzero1 = subset(data_nzero, FFMC > max(OutVals))

lm.nzero = lm(log(area + 1) ~ FFMC + DMC + DC**2 + ISI + temp + RH + wind + rain, data = data_nzero1)
summary(lm.nzero)
step(lm.nzero, data = data, direction = "backward")







View(data_zero)
plot(data_zero)


library(car)
data3 = data[, -c(3,4)]
data4 = data3[- c(high_leverage_value$row_num), ]
scatterplotMatrix(data4, panel = function(x,y,...){panel.smooth(x,y,span = 0.8,iter = 5,col.smooth = "green4",...)
  abline(lm(y ~ x), col="red")})
scatterplotMatrix(data)
plot(data)

data6 = subset(data4, area<600)
plot(data6, panel = function(x,y,...){panel.smooth(x,y,span = 0.8,iter = 5,col.smooth = "green4",...)
  abline(lm(y ~ x), col="red")})
cor(data6)

lm_X_temp = lm(area ~ temp, data = data6)
summary(lm_X_temp)
plot(data6$temp, data6$area)
 
data5 = subset(data2, area<600)
plot(data5, panel = function(x,y,...){panel.smooth(x,y,span = 0.8,iter = 5,col.smooth = "green4",...)
  abline(lm(y ~ x), col="red")})
plot(data5$DMC, data5$area)
lm.all = lm(data5$area ~ data5$X + data5$Y + data5$FFMC + data5$DMC + data5$DC + data5$ISI + data5$temp + data5$RH + data5$wind + data5$rain) 
summary(lm.all)
abline(lm.all)

plot(lm.all)
step(lm.all, data = data, direction = "backward")
qqplot(lm.all, main = "QQ Plot")
pairs(data)
boxplot(area ~ month, data = data)
plot(data$X)

plot(data$area)
data1 = subset(data, area < 278)
plot(data1$area)
lm.all1 = lm(data1$area ~ data1$X + data1$Y + data1$month + data1$day + data1$FFMC + data1$DMC + data1$DC + data1$ISI + data1$temp + data1$RH + data1$wind + data1$rain) 
summary(lm.all1)


lm.all = lm(data2$area ~ data2$X + data2$Y + data2$FFMC + data2$DMC + data2$DC + data2$ISI + data2$temp + data2$RH + data2$wind + data2$rain) 
summary(lm.all)
abline(lm.all)

plot(data$FFMC, data$DMC)
abline(lm(data$DMC ~ data$FFMC))
plot(data$DMC,data$DC)
abline(lm(data$DMC ~ data$DC))


x1 <- data$area[data$X == 4]
x2 <- data$area[data$X ==5]
t.test(x1, x2, var.equal = TRUE, paired = FALSE)

x3 <- data$DMC[data$day == "mon"]
x4 <- data$DMC[data$day == "sun"]
t.test(x3, x4, var.equal = TRUE, paired = FALSE, conf.level = 0.95)

coplot(area ~ DMC + temp| month, data=data2, panel = function(x,y,...){
  panel.smooth(x,y,span = 0.8,iter = 5,col.smooth = "green4",...)
  abline(lm(y ~ x), col="red")},
  rows=1,pch=19, cex=0.3)

coplot(area ~ DMC| day, data=data1, panel = function(x,y,...){
  panel.smooth(x,y,span = 0.8,iter = 5,col.smooth = "green4",...)
  abline(lm(y ~ x), col="red")},
  rows=1,pch=19, cex=0.3)

summary(data6)
par(mfrow = c(1,2))
plot(data$temp[data$month == "jan"], data$area[data$month == "jan"], col =2, xlim = c(0, 35), ylim = c(0, 1100), xlab = "temp", ylab = "area", main = "area vs. temp, month")
points(data$temp[data$month == "feb"], data$area[data$month == "feb"], col =3)
points(data$temp[data$month == "mar"], data$area[data$month == "mar"], col =4)
points(data$temp[data$month == "apr"], data$area[data$month == "apr"], col =5)
points(data$temp[data$month == "may"], data$area[data$month == "may"], col =6)
points(data$temp[data$month == "jun"], data$area[data$month == "jun"], col =7)
legend(0, 1000, legend = c("jan", "feb", "mar", "apr", "may", "jun"), col = 2:7, pch = 1, cex = 0.8)

plot(data$temp[data$month == "jul"], data$area[data$month == "jul"], col =2, xlim = c(0, 35), ylim = c(0, 1100), xlab = "temp", ylab = "area", main = "area vs. temp, month")
points(data$temp[data$month == "aug"], data$area[data$month == "aug"], col =3)
points(data$temp[data$month == "sep"], data$area[data$month == "sep"], col =4)
points(data$temp[data$month == "oct"], data$area[data$month == "oct"], col =5)
points(data$temp[data$month == "nov"], data$area[data$month == "nov"], col =6)
points(data$temp[data$month == "dec"], data$area[data$month == "dec"], col =7)
legend(0, 1000, legend = c("jul", "aug", "sep", "oct", "nov", "dec"), col = 2:7, pch = 1, cex = 0.8)

par(mfrow = c(1,1))
plot(data$DMC[data$day == "mon"], data$area[data$day == "mon"], col =2, xlim = c(0, 300), ylim = c(0, 1100), xlab = "DMC", ylab = "area", main = "area vs. DMC, day")
points(data$DMC[data$day == "tue"], data$area[data$day == "tue"], col =3)
points(data$DMC[data$day == "wed"], data$area[data$day == "wed"], col =4)
points(data$DMC[data$day == "thu"], data$area[data$day == "thu"], col =5)
points(data$DMC[data$day == "fri"], data$area[data$day == "fri"], col =6)
points(data$DMC[data$day == "sat"], data$area[data$day == "sat"], col =7)
points(data$DMC[data$day == "sun"], data$area[data$day == "sun"], col =8)
legend(0, 1000, legend = c("mon", "tue", "wed", "thu", "fri", "sat", "sun"), col = 2:8, pch = 1, cex = 0.8)

plot(data$DMC[data$day == "mon"], data$area[data$day == "mon"], col =2, xlim = c(0, 300), ylim = c(0, 1100), xlab = "DMC", ylab = "area", main = "area vs. DMC, day")
points(data$DMC[data$day == "tue"], data$area[data$day == "tue"], col =3)
points(data$DMC[data$day == "wed"], data$area[data$day == "wed"], col =4)
points(data$DMC[data$day == "thu"], data$area[data$day == "thu"], col =5)
points(data$DMC[data$day == "fri"], data$area[data$day == "fri"], col =6)
points(data$DMC[data$day == "sat"], data$area[data$day == "sat"], col =7)
points(data$DMC[data$day == "sun"], data$area[data$day == "sun"], col =8)

data_jun = subset(data2, month == "jun")
lm_jun = lm(area ~ X + Y + FFMC + DMC + DC + ISI + temp + RH + wind + rain, data= data_jun)
summary(lm_jun)
cor(data_jun[, -c(3,4)])
plot(data_jun)
data_jun1 = subset(data_jun, area <40)
plot(data_jun1)

lm_jun1 = lm(area ~ Y + ISI + RH + wind, data= data_jun1)
lm_jun2 = lm(sqrt(area) ~ Y + ISI + RH, data = data_jun1)
lm_jun3 = lm(area ~ Y + RH, data = data_jun1)
anova(lm_jun3, lm_jun2)
summary(lm_jun2)

data7 = data.frame(data_jun1$Y, data_jun1$RH, data_jun1$ISI, data_jun1$wind, data_jun1$area)
plot(data7)
plot(data_jun1$area, data_jun1$ISI)
abline(lm(data_jun1$area ~ data_jun1$ISI, col = "red"))
panel.smooth(data_jun1$ISI, data_jun1$area, span =0.8, iter = 5, col.smooth = "green4")
plot(c(data_jun1$area, data_jun1$ISI), panel = function(x,y,...){panel.smooth(x,y,span = 0.8,iter = 5,col.smooth = "green4",...)
  abline(lm(y ~ x), col="red")})

data8 = data.frame(data2$X, data2$Y, data2$RH, data2$ISI, data2$rain, data2$area, data2$month)
data8 = data_nzero
data8 = cbind(data8, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0)
colnames(data8)<-c("X","Y","month", "day", "FFMC", "DMC", "DC", "ISI", "temp", "RH", "wind", "rain", "area", "D1","D2","D3","D4","D5","D6","D7","D8","D9","D10","D11")

data8[which(data8[,3]=="jan"),14] = 1
data8[which(data8[,3]=="feb"),15] = 1
data8[which(data8[,3]=="mar"),16] = 1
data8[which(data8[,3]=="apr"),17] = 1
data8[which(data8[,3]=="may"),18] = 1
data8[which(data8[,3]=="jun"),19] = 1
data8[which(data8[,3]=="jul"),20] = 1
data8[which(data8[,3]=="aug"),21] = 1
data8[which(data8[,3]=="sep"),22] = 1
data8[which(data8[,3]=="oct"),23] = 1
data8[which(data8[,3]=="nov"),24] = 1

View(data8)

data9 = data_nzero
data9 = cbind(data9, 0, 0, 0)
colnames(data9)<-c("X","Y","month", "day", "FFMC", "DMC", "DC", "ISI", "temp", "RH", "wind", "rain", "area", "S1","S2","S3")
data9[which(data9[,3]=="dec" | data9[,3]=="jan" | data9[,3]=="feb"),14] = 1
data9[which(data9[,3]=="mar" | data9[,3]=="apr" | data9[,3]=="may"),15] = 1
data9[which(data9[,3]=="jul" | data9[,3]=="aug" | data9[,3]=="sep"),16] = 1
View(data9)

lm_dummy2 = lm(log(data9$area + 1) ~ X + Y + temp + ISI + RH + S1 + S2 + S3 + S1*temp + S2*temp + S3* + S1*RH + S2*RH + S3*RH + S1*ISI + S2*ISI + S3*ISI, data = data9)
summary(lm_dummy2)
step(lm_dummy2)


data10 = data5
data10 = cbind(data10, 0, 0, 0, 2)
colnames(data10)<-c("X","Y","month", "day", "FFMC", "DMC", "DC", "ISI", "temp", "RH", "wind", "rain", "area", "S1","S2","S3","season")
data10[which(data10[,3]=="dec" | data10[,3]=="jan" | data10[,3]=="feb"),14] = 1
data10[which(data10[,3]=="mar" | data10[,3]=="apr" | data10[,3]=="may"),15] = 1
data10[which(data10[,3]=="jul" | data10[,3]=="aug" | data10[,3]=="sep"),16] = 1

data10[which(data10[,3]=="dec" | data10[,3]=="jan" | data10[,3]=="feb"),17] = 1
data10[which(data10[,3]=="mar" | data10[,3]=="apr" | data10[,3]=="may"),17] = 3
data10[which(data10[,3]=="jul" | data10[,3]=="aug" | data10[,3]=="sep"),17] = 4

View(data10)

lm_dummy2 = lm(log(data10$area + 1) ~ X + Y + temp + ISI + RH + S1 + S2 + S3 + S1*temp + S2*temp + S3*temp + S1*RH + S2*RH + S3*RH + S1*ISI + S2*ISI + S3*ISI, data = data10)
summary(lm_dummy2)
step(lm_dummy2)


lm_dummy3 = lm(log(data11$area + 1) ~ X + Y + FFMC**10 + DMC + DC + ISI + temp + RH + wind + rain + (FFMC**10)*S1 + (FFMC**10)*S1 + (FFMC**10)*S1 + DMC*S1 + DMC*S2 + DMC*S3 + DC*S1 + DC*S2 + DC*S3 + ISI*S1 + ISI*S2 + ISI*S3 + temp*S1 + temp*S2 + temp*S3 + RH*S1 + RH*S2 + RH*S3 + rain*S1 + rain*S2 + rain*S3 + S1 + S2 + S3, data = data11)
summary(lm_dummy3)
step(lm_dummy3)

lm_dummy4 = lm(log(data11$area + 1) ~ X + temp + wind + rain + S1 + temp*S1, data = data11)
summary(lm_dummy4)

View(data11)


coplot(log(data11$area + 1) ~ temp| season, data=data11, panel = function(x,y,...){
  panel.smooth(x,y,span = 0.8,iter = 5,col.smooth = "green4",...)
  abline(lm(y ~ x), col="red")},
  rows=1,pch=19, cex=0.3)

par(mfrow = c(2,4))
plot(data5$FFMC, data10$area)
plot(data5$DMC, data10$area)
plot(data5$DC, data10$area)
plot(data5$ISI, data10$area)
plot(data5$temp, data10$area)
plot(data5$RH, data10$area)
plot(data5$wind, data10$area)
plot(data5$rain, data10$area)

data11 = subset(data10, FFMC>20 & ISI <40 & rain < 4)
data12 = data.frame(data11$X, data11$Y, data11$FFMC, data11$DMC, data11$DC, data11$ISI, data11$temp, data11$RH, data11$wind, data11$rain, log(data11$area + 1))
View(data12)
cor(data12)
data_nzero2 = data_nzero[ ,-c(3,4)]
data13 = data.frame(data_nzero2$X, data_nzero2$Y, data_nzero2$FFMC, data_nzero2$DMC, data_nzero2$DC, data_nzero2$ISI, data_nzero2$temp, data_nzero2$RH, data_nzero2$wind, data_nzero2$rain, log(data_nzero2$area + 1))
cor(data13)

par(mfrow = c(2,4))
plot((data11$FFMC)**10, data11$area)
plot(data11$DMC, data11$area)
plot(data11$DC, data11$area)
plot(data11$ISI, data11$area)
plot(data11$temp, data11$area)
plot(data11$RH, data11$area)
plot(data11$wind, data11$area)
plot(data11$rain, data11$area)

hist((data11$FFMC)**10, 100)


coplot(log(area + 1) ~ FFMC**10| season, data=data11, panel = function(x,y,...){
  panel.smooth(x,y,span = 0.8,iter = 5,col.smooth = "green4",...)
  abline(lm(y ~ x), col="red")},
  rows=1,pch=19, cex=0.3)


lm_dummy1 = lm(log(data8$area + 1) ~ D1 + D2 + D3 + D4 + D5 + D6 + D7 + D8 + D9 + D10 + D11 + D1*RH + D2*RH + D3*RH + D4*RH + D5*RH + D6*RH + D7*RH + D8*RH + D9*RH + D10*RH + D11*RH + D1*ISI + D2*ISI + D3*ISI + D4*ISI + D5*ISI + D6*ISI + D7*ISI + D9*ISI + D10*ISI + D11*ISI, data = data8)
summary(lm_dummy1)

plot(data$DMC[data$month == "jan"], data$area[data$month == "jan"], col =3)
data$area[data$month == "jan"]

predict(model2, data5[1,])
install.packages("DAAG")
library(DAAG)
cv.lm(data = data5, form.lm = formula(fire_class ~ X + temp + wind + S1 + S3 + temp*S1 + temp*S3 + wind*S1))

insall.packages(caret)
library(caret)
library(rpart)
install.packages("caret")

train_control = trainControl(method = "cv", number = 10)

model = train(fire_class~., data = data5, trControl = train_control, method = "rpart")

predictions = predict(model, data5)

data5 = cbind(data5, predictions)

confusionMatrix = confusionMatrix(data5$predictions, data5$fire_class)

install.packages("bootstrap")
library(bootstrap)
cv<-function(fit,k=10){  
  require(bootstrap)  
  theta.fit<-function(x,y){lsfit(x,y)}  
  theta.predict<-function(fit,x){cbind(1,x)%*%fit$coef}  
  x<-fit$model[,2:ncol(fit$model)]  
  y<-fit$model[,1]  
  results<-crossval(x,y,theta.fit,theta.predict,ngroup=k)  
  r2<-cor(y,fit$fitted.values)^2  
  r2cv<-cor(y,results$cv.fit)^2  
  cat("Original R-square=",r2,"\n")  
  cat(k,"Fold Cross-Validated R-square=",r2cv,"\n")  
  cat("Change=",r2-r2cv,"\n")  
}  
cv(lm_dummy5)


k_fold_cv <- function(lmfit, k=10) {
  mydata <- lmfit$model
  outcome <- names(lmfit$model)[1]
  predictors <- names(lmfit$model)[-1]
  
  theta.fit <- function(x,y){lsfit(x,y)}
  theta.predict <- function(fit,x){cbind(1,x)%*%fit$coef} 
  X <- as.matrix(mydata[predictors])
  y <- as.matrix(mydata[outcome]) 
  
  results <- crossval(X,y,theta.fit,theta.predict,ngroup=k)
  raw_rsq <- cor(y, lmfit$fitted.values)**2 # raw R2 
  cv_rsq <- cor(y,results$cv.fit)**2 # cross-validated R2
  
  cat("Original R-square=",raw_rsq,"\n")  
  cat(k,"Fold Cross-Validated R-square=",cv_rsq,"\n")  
  cat("Change=",raw_rsq-cv_rsq,"\n") 
}
k_fold_cv(lm_dummy6)

names(lm_dummy5$model)[-1]
cor(data5$X, data5$Y)
boxplot(data5$X ~ data5$Y)
boxplot(data5$fire_class ~ data5$X)
boxplot(data5$fire_class ~ data5$Y)

install.packages("ISLR")
library(ISLR)
Hitters <- na.omit(Hitters)
dim(Hitters) # 除去Salary做为因变量，还剩下19个特征
library(leaps)
regfit.full = regsubsets(Salary~.,Hitters,nvmax = 19) #选择最大19个特征的全子集选择模型
Hitters$Salary ~.



