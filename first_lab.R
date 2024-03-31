#--------1. получение выборок--------#
library (moments)
vector1 = rnorm(80, mean = 1, sd = 6)
vector2 = rnorm(60, mean = 21, sd = 5)


#--------2. составление вариационного ряда--------#
s = c(vector1, vector2)
data_ <- 	data.frame(
  sampl = s
)
data_$sampl = sort(data_$sampl)
print(data_)
write.csv(data_, "data.csv")
#построим диаграмму рассеяния
plot(data_$sampl)


#--------3. выборочные характеристики--------#
n = length(data_$sampl)
cat("n = ", length(data_$sampl))
cat("x_max = ", max(data_$sampl))
cat("x_min = ", min(data_$sampl))
cat("x_mean = ", mean(data_$sampl))
cat("median = ", median(data_$sampl))
find_mode <- function (x) {
  distinct_A <- unique(x)
  matches <- match(x, distinct_A)
  table_A <- tabulate(matches)
  max_A <- which.max(table_A)
  mode<-distinct_A[max_A]
}
cat("mode = ", find_mode(data_$sampl))
#mode = sort(unique(data_$sampl))[which.max(data_$sampl)]
#cat("mode = ", mode)
#cat("var_range = ", range(data_$sampl))
cat("_range = ", max(data_$sampl) - min(data_$sampl))
cat("var (несмещенная выборочная дисперсия) = ", var(data_$sampl))
cat("var (смещенная выборочная дисперсия) = ", var(data_$sampl)*(n-1)/n)
cat("sd (выб-ое среднее квадратическое отклонение по несмещенной дисп)= ", sd(data_$sampl))
cat("sd (выб-ое среднее квадратическое отклонение по смещенной дисп)= ", sd(data_$sampl)*sqrt((n-1)/n))
cat("среднее абсолютное отклонение = ", mad(data_$sampl))

hist(data_$sampl, col='steelblue')
cat("эксцесс эмпирического распределения = ", kurtosis(data_$sampl))
cat("асимметрия эмпирического распределения = ", skewness(data_$sampl))
cat("cv: variation coeff = ", ((sd(data_$sampl) / mean(data_$sampl)) * 100))
cat("ошибка выборки = ", sd(data_$sampl)/(sqrt(n)))


#--------4. описательная статистика--------#
summary(data_$sampl)
#install.packages("psych")
#library("psych")
psych::describe(data_$sampl)

#--------5-10. гистограммa частот--------#
#рассчитаем количество интервалов на диапазоне выборки с помощью правила Стёрджеса
n_int <- 1 + floor(log(length(data_$sampl), base = 2))
print(n_int)
#Таблица из значений и интервала, куда попало значение
interv_data <- data.frame(data_$sampl, cut(data_$sampl, breaks = n_int + 1))
breaks = seq(min(data_$sampl), max(data_$sampl), length.out=n_int + 1)

write.csv(interv_data, "interv_data.csv")

#количество значений в интервале
frequ <- table(cut(data_$sampl, b = n_int + 1))
write.csv(frequ, "frequ.csv")

#Таблица из значений и их частот
sample_freq<-data.frame(table(data_$sampl))
write.csv(sample_freq, "sample_freq.csv")
#гистограммa частот
hist(data_$sampl, breaks = breaks, col = "lightblue", labels=T, main='Частоты')


#гистограммa плотности вероятности
hist(data_$sampl, breaks = breaks, freq = FALSE, col = "lightblue",
       xlab = "х",
       ylab = "плотность вероятности",
       main = "гистограмма, совмещенная с кривой плотности")
lines(density(data_$sampl), col = "red", lwd = 2)


#--------6-10. гистограммa относительных частот--------#
#Таблица из значений и их частот(в процентах)
sample_2<-data.frame(table(data_$sampl)/length(data_$sampl))
write.csv(sample_2, "sample_2.csv")
breaks = seq(min(data_$sampl), max(data_$sampl), length.out=n_int + 1)
library(package = "lattice")
histogram(data_$sampl, breaks=breaks, labels=T, col = "lightblue")
#другой способ:
breaks = seq(min(data_$sampl), max(data_$sampl), length.out=n_int +1)
freq2 = hist(data_$sampl, breaks = breaks, plot = F)
freq2$counts = freq2$counts / sum(freq2$counts)
plot(freq2, main='Относительные частоты', col = "lightblue")
#--------7-10. гистограммa плотности относительных частот--------#
breaks = seq(min(data_$sampl), max(data_$sampl), length.out=n_int +1)
freq_dens = hist(data_$sampl, breaks = breaks, plot = F)
freq_dens$counts = freq_dens$counts / sum(freq_dens$counts)
delta = freq_dens$breaks[2] - freq_dens$breaks[1]
freq_dens$counts = freq_dens$counts / delta
plot(freq_dens, col = "lightblue")
lines(freq_dens$mids, freq_dens$counts, col = "red", lwd = 2, main='Плотности отноcительных частот')

#--------8-10. гистограммa кумулятивных частот--------#
freq_int = hist(data_$sampl, breaks = breaks, plot = F)
for (i in 2:n_int){
  freq_int$counts[i] = freq_int$counts[i] + freq_int$counts[i-1]
}
plot(freq_int, col = "lightblue")
lines(freq_int$mids, freq_int$counts, col = "red", lwd = 2, main='Кумулятивные частоты')



#--------9-10. гистограммa относительных кумулятивных частот--------#
freq_int2 = hist(data_$sampl, breaks = breaks, plot = F)
for (i in 2:n_int){
  freq_int2$counts[i] = freq_int2$counts[i] + freq_int2$counts[i-1]
}
freq_int2$counts = freq_int2$counts / sum(freq_int2$counts)
plot(freq_int2, col = "lightblue")
lines(freq_int2$mids, freq_int2$counts, col = "red", lwd = 2, main='Относительные кумулятивные частоты')



#--------11. графики частот и плотности--------#
#---11.1---#
breaks = seq(min(data_$sampl), max(data_$sampl), length.out=n_int +1)
freq2 = hist(data_$sampl, breaks = breaks, plot = F)
freq2$counts = freq2$counts / sum(freq2$counts)
plot(freq2, main='Относительные частоты')
#--------. гистограммa частот--------#
n_int = 10
print(n_int)
#Таблица из значений и интервала, куда попало значение
interv_data <- data.frame(data_$sampl, cut(data_$sampl, breaks = n_int + 1))
breaks = seq(min(data_$sampl), max(data_$sampl), length.out=n_int + 1)
#количество значений в интервале
frequ <- table(cut(data_$sampl, b = n_int + 1))
#Таблица из значений и их частот
sample_freq<-data.frame(table(data_$sampl))
#гистограммa частот
hist(data_$sampl, breaks = breaks, col = "lightblue", labels=T, main='Частоты')


#гистограммa плотности вероятности
hist(data_$sampl, breaks = breaks, freq = FALSE, col = "lightblue",
     xlab = "х",
     ylab = "плотность вероятности",
     main = "гистограмма, совмещенная с кривой плотности")
lines(density(data_$sampl), col = "red", lwd = 2)


#--------. гистограммa относительных частот--------#
#Таблица из значений и их частот(в процентах)
sample_2<-data.frame(table(data_$sampl)/length(data_$sampl))
library(package = "lattice")
breaks = seq(min(data_$sampl), max(data_$sampl), length.out=n_int +1)
freq2 = hist(data_$sampl, breaks = breaks, plot = F)
freq2$counts = freq2$counts / sum(freq2$counts)
plot(freq2, main='Относительные частоты', col = "lightblue")

#--------. гистограммa плотности относительных частот--------#
breaks = seq(min(data_$sampl), max(data_$sampl), length.out=n_int +1)
freq_dens = hist(data_$sampl, breaks = breaks, plot = F)
freq_dens$counts = freq_dens$counts / sum(freq_dens$counts)
delta = freq_dens$breaks[2] - freq_dens$breaks[1]
freq_dens$counts = freq_dens$counts / delta
plot(freq_dens, col = "lightblue")
lines(freq_dens$mids, freq_dens$counts, col = "red", lwd = 2, main='Плотности отноcительных частот')

#--------. гистограммa кумулятивных частот--------#
freq_int = hist(data_$sampl, breaks = breaks, plot = F)
for (i in 2:n_int){
  freq_int$counts[i] = freq_int$counts[i] + freq_int$counts[i-1]
}
plot(freq_int, col = "lightblue")
lines(freq_int$mids, freq_int$counts, col = "red", lwd = 2, main='Кумулятивные частоты')


#--------. гистограммa относительных кумулятивных частот--------#
freq_int2 = hist(data_$sampl, breaks = breaks, plot = F)
for (i in 2:n_int){
  freq_int2$counts[i] = freq_int2$counts[i] + freq_int2$counts[i-1]
}
freq_int2$counts = freq_int2$counts / sum(freq_int2$counts)
plot(freq_int2, col = "lightblue")
lines(freq_int2$mids, freq_int2$counts, col = "red", lwd = 2, main='Относительные кумулятивные частоты')




#--------------------------------------------
#---11.2---#
#--------. гистограммa частот--------#
#рассчитаем количество интервалов на диапазоне выборки
n_int <- 1 + floor(3.2*log(length(data_$sampl), base = 2))
print(n_int)
#Таблица из значений и интервала, куда попало значение
interv_data <- data.frame(data_$sampl, cut(data_$sampl, breaks = n_int + 1))
breaks = seq(min(data_$sampl), max(data_$sampl), length.out=n_int + 1)
#количество значений в интервале
frequ <- table(cut(data_$sampl, b = n_int + 1))
#Таблица из значений и их частот
sample_freq<-data.frame(table(data_$sampl))
#гистограммa частот
hist(data_$sampl, breaks = breaks, col = "lightblue", labels=T, main='Частоты')


#гистограммa плотности вероятности
hist(data_$sampl, breaks = breaks, freq = FALSE, col = "lightblue",
     xlab = "х",
     ylab = "плотность вероятности",
     main = "гистограмма, совмещенная с кривой плотности")
lines(density(data_$sampl), col = "red", lwd = 2)


#--------. гистограммa относительных частот--------#
#Таблица из значений и их частот(в процентах)
sample_2<-data.frame(table(data_$sampl)/length(data_$sampl))
library(package = "lattice")
breaks = seq(min(data_$sampl), max(data_$sampl), length.out=n_int +1)
freq2 = hist(data_$sampl, breaks = breaks, plot = F)
freq2$counts = freq2$counts / sum(freq2$counts)
plot(freq2, main='Относительные частоты', col = "lightblue")

#--------. гистограммa плотности относительных частот--------#
breaks = seq(min(data_$sampl), max(data_$sampl), length.out=n_int +1)
freq_dens = hist(data_$sampl, breaks = breaks, plot = F)
freq_dens$counts = freq_dens$counts / sum(freq_dens$counts)
delta = freq_dens$breaks[2] - freq_dens$breaks[1]
freq_dens$counts = freq_dens$counts / delta
plot(freq_dens, col = "lightblue")
lines(freq_dens$mids, freq_dens$counts, col = "red", lwd = 2, main='Плотности отноcительных частот')

#--------. гистограммa кумулятивных частот--------#
freq_int = hist(data_$sampl, breaks = breaks, plot = F)
for (i in 2:n_int){
  freq_int$counts[i] = freq_int$counts[i] + freq_int$counts[i-1]
}
plot(freq_int, col = "lightblue")
lines(freq_int$mids, freq_int$counts, col = "red", lwd = 2, main='Кумулятивные частоты')


#--------. гистограммa относительных кумулятивных частот--------#
freq_int2 = hist(data_$sampl, breaks = breaks, plot = F)
for (i in 2:n_int){
  freq_int2$counts[i] = freq_int2$counts[i] + freq_int2$counts[i-1]
}
freq_int2$counts = freq_int2$counts / sum(freq_int2$counts)
plot(x = freq_int2,y = freq_int2$counts, col = "lightblue")
lines(freq_int2$mids, freq_int2$counts, col = "red", lwd = 2, main='Относительные кумулятивные частоты')



########################################
############---Корреляция---############
########################################
library (moments)
#1
x_data1 = data.frame(
  s = rnorm(n = 140, 0, 1)
)

#2
x_data2 = data.frame(
  s = rnorm(n = 140, 2, 1)
)

#3 - выборочные коэффициенты корреляции

#коэффициент Пирсона x_data1, x_data2
cor(x_data1$s, x_data2$s) 
#коэффициент Пирсона x_data1, data_$sampl
cor(x_data1$s, data_$sampl) 
#коэффициент Пирсона x_data2, data_$sampl
cor(x_data2$s, data_$sampl) 
n = 140
#критическое значение t-критерия
qt_ = qt(p = 0.05, df = n - 2, lower.tail= FALSE)
qt_

sd0 = sd(data_$sampl)
sd1 = sd(x_data1$s)
sd2 = sd(x_data2$s)
#коэффициенты парной корреляции
r_01 = (mean(data_$sampl*x_data1$s) - mean(data_$sampl)*mean(x_data1$s))/(sd0*sd1)
r_02 = (mean(data_$sampl*x_data2$s) - mean(data_$sampl)*mean(x_data2$s))/(sd0*sd2)
r_12 = (mean(x_data2$s*x_data1$s) - mean(x_data2$s)*mean(x_data1$s))/(sd2*sd1)
cat(r_01, r_02, r_12)

#Проверим значимость коэффициентов корреляции:

#t_тест
alpha = 0.05
t_r12 <- r_12*sqrt(n - 2)/sqrt(1 - r_12*r_12)
t12 = t.test(x_data1$s, x_data2$s, alternative = "two.sided", conf.level = 1-alpha)
t12
t_r12

if (abs(t_r12) > qt_) {
  print("коэффициент r12 - значим")
} else {print("коэффициент r12 - незначим")}



t_r01 <- r_01*sqrt(n - 2)/sqrt(1 - r_01*r_01)
t01 = t.test(x_data1$s, data_$sampl, alternative = "two.sided", conf.level = 1-alpha)
t01
t_r01
if (abs(t_r01) > qt_) {
  print("коэффициент r01 - значим")
} else {print("коэффициент r01 - незначим")}



t_r02 <- r_02*sqrt(n - 2)/sqrt(1 - r_02*r_02)
t02 = t.test(data_$sampl, x_data2$s, alternative = "two.sided", conf.level = 1-alpha)
t02
t_r02
if (abs(t_r02) > qt_) {
  print("коэффициент r02 - значим")
} else {print("коэффициент r02 - незначим")}



#4 - выборочные ковариации
#ковариация  x_data1, x_data2
cov(x_data1$s, x_data2$s) 
#ковариация  x_data1, data_$sampl
cov(x_data1$s, data_$sampl) 
#ковариация  x_data2, data_$sampl
cov(x_data2$s, data_$sampl) 


cov_mat = matrix(c(data_$sampl, x_data1$s, x_data2$s), 140, 3)
print(cov(cov_mat))

#стандартное отклонение
cat("sd (выб-ое ср квадр откл по несм дисп: data_$sampl)= ", sd(data_$sampl))
cat("sd (выб-ое ср квадр откл по несм дисп: x_data1$s)= ", sd(x_data1$s))
cat("sd (выб-ое ср квадр откл по несм дисп: x_data2$s)= ", sd(x_data2$s))
sd0 = sd(data_$sampl)
sd1 = sd(x_data1$s)
sd2 = sd(x_data2$s)

r_01 = (mean(data_$sampl*x_data1$s) - mean(data_$sampl)*mean(x_data1$s))/(sd0*sd1)
r_02 = (mean(data_$sampl*x_data2$s) - mean(data_$sampl)*mean(x_data2$s))/(sd0*sd2)
r_21 = (mean(x_data2$s*x_data1$s) - mean(x_data2$s)*mean(x_data1$s))/(sd2*sd1)

cat("коэффициент парной корреляции x_data1, data_$sampl= ", r_01)
cat("коэффициент парной корреляции x_data2, data_$sampl= ", r_02)
cat("коэффициент парной корреляции x_data1, x_data2)= ", r_21)


#5 - корреляционная матрица

correl_mat = matrix(c(data_$sampl, x_data1$s, x_data2$s), 140, 3)
print(cor(correl_mat))


