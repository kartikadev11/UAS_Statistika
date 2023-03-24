library(readxl) #mengaktifkan packege xlsx
datamhs = read_xlsx("dataGame.xlsx")
library(ISLR2)

# Bagi data menjadi dua, training 70% dan testing 30%
smp = sample(c(TRUE, FALSE), nrow(datamhs), replace=TRUE, prob=c(0.7,0.3))
train = datamhs[smp, ]
test = datamhs[!smp, ]
test

library(caret)
#MELAKUKAN PREDIKSI
ctrl <- trainControl(method = "LOOCV")
modl <- train(pengeluaran ~waktu_main + usia, data = train, method = "lm", trControl = ctrl)
prd1 <- predict(modl,test)

SSE = sum((prd1 - test$pengeluaran)^2) #ERROR
SST = sum((mean(train$pengeluaran) - test$pengeluaran)^2) #TOTAL
R2 = 1 - SSE/SST #RSquared
R2 # 0.3387814 (NILAI BERUBAH SETIAP DI RUN)
RMSE = sqrt(SSE/nrow(test)) #RMSE
RMSE  #161329.1 (NILAI BERUBAH SETIAP DI RUN)

plot(prd1,test$pengeluaran)

#KOMENTAR

#RMSE digunakan untuk mengukur tingkat kesalahan sebuah model dalam memprediksi suatu nilai numerik. 
#Semakin kecil nilai RMSE, dapat dikatakan model semakin akurat dalam memprediksi.
#PADA PERHITUNGAN RMSE  #161329.1 (NILAI BERUBAH SETIAP DI RUN) INI BERARTI MODEL YANG DIBUAT TIDAK AKURAT

#Jika nilai R-squared lebih kecil, ini berarti bahwa persentase variasi dalam variabel dependen (RESPON) 
#yang dapat dijelaskan oleh variabel independen (PREDIKTOR) dalam model regresi lebih kecil. 
#Hal ini menunjukkan bahwa model kurang baik dalam menjelaskan variasi dalam variabel dependen(RESPON), 
#dan sehingga prediksi model mungkin lebih buruk daripada model yang memiliki nilai R-squared yang lebih besar. 
#PADA PERHITUNGAN R2 0.3387814 (NILAI BERUBAH SETIAP DI RUN) INI BERARTI akurasiny kecil


