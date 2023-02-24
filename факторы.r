
library(readxl)
df <- read_excel("C:\\Users\\annap\\Desktop\\вуз\\3 курс\\ЭКОНОМЕТРИКА\\РГЗ\\данные\\данные.xlsx")

#графический анализ
pairs(df[,-1],main='Исходные данные',lower.panel = panel.smooth,upper.panel=NULL)

# построение коррелограммы
library(corrgram)
corrgram(df[,-1],order=TRUE, lower.panel=panel.shade,upper.panel = panel.pie)
options(digits=2)
cor(df[,-1])

#регрессионный анализ
m1<-lm(Y3~.,data=df[,-1])
summary(m1)
plot(df$Y3,m1$fitted.values,type='p',col='blue',xlab='Y3',ylab='y_calc',main='сравнение наблюдаемых и вычисленных значений')
plot(m1$residuals,type='l',xlab='номер наблюдения',ylab='ошибка',col='green',lwd=2)
#анализируем распределение ошибок
par(mfrow=c(1,2))
hist(m1$residuals,breaks=6,col='orange',main='')
boxplot(m1$residuals,horizontal = TRUE,col='green',main='')
#построение диаграммы «квантиль-квантиль»
par(mfrow=c(1,1))
qqnorm(m1$residuals,xlab='теоретические значения квантиля',ylab='наблюдаемые значения',main='')
#пошаговая регрессия
m2<-lm(Y3~.,data=df[,-1])
library(MASS)
stepAIC(m2, direction="backward")
stepAIC(m2, direction="forward")
stepAIC(m2, direction="both")
