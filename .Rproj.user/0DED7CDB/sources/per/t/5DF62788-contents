library(readxl)
library(tidyverse)
library(caret)
datamhs = read_xlsx("dataGame.xlsx")

attach(datamhs)
set.seed(1)
#membagi menjadi data training dan data tes
smp = sample(c(TRUE, FALSE), nrow(datamhs), replace=TRUE, prob=c(0.7,0.3))
train = datamhs[smp, ]
test = datamhs[!smp, ]

#membuat model untuk prediksi data tes
ctrl = trainControl(method = "cv", number = 10)
model = train(pengeluaran~waktu_main+usia, data = train, method = "lm", trControl = ctrl)
model


#prediksi data tes
prd1 <- predict(model, test)

#mencari nilai eror
SSE = sum((prd1 - test$pengeluaran)^2)
SST = sum((mean(train$pengeluaran) - test$pengeluaran)^2)
R2 = 1 - SSE/SST #RSquared
R2 # = 0.2647525
RMSE = sqrt(SSE/nrow(test)) #RMSE
RMSE # = 135424.5




#model untuk prediksi k=100
ctrl2 = trainControl(method = "cv", number = 100)
model2 = train(pengeluaran~waktu_main+usia, data = train, method = "lm", trControl = ctrl2)
model2

#prediksi data tes
prd2 <- predict(model2, test)

#mencari nilai eror
SSE2 = sum((prd2 - test$pengeluaran)^2)
SST2 = sum((mean(train$pengeluaran) - test$pengeluaran)^2)
R2 = 1 - SSE2/SST2 #RSquared
R2 # = 0.2647525
RMSE = sqrt(SSE2/nrow(test)) #RMSE
RMSE # = 135424.5


#NILAI RMSE DAN R2 PADA K=10 DAN 100 MEMILIKI NILAI YANG SAMA.
#Nilai RMSE sebesar 135424.5 menunjukkan bahwa rata-rata perbedaan antara 
#nilai yang diprediksi oleh model dan nilai aktual adalah 135424.5 dan R2 = 0.26.
#Hal ini menunjukan rendahnya akurasi model dan kurang baik dalam memprediksi variasi respon

#NILAI R2 DAN RMSE DAPAT BERBEDA APABILA MASING NILAI K DILAKUKAN PENGACAKAN DATA YANG BERBEDA