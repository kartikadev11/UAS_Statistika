library(readxl) #mengaktifkan packege xlsx
datamhs = read_xlsx("dataGame.xlsx")
str(datamhs)
library(ISLR2)
dim(datamhs)
summary(datamhs)

#UBAH MENJADI LOGICAL / FACTOR
datamhs$jk <- as.factor(datamhs$jk)
datamhs$platform_game <- as.factor(datamhs$platform_game)
str(datamhs)
summary(datamhs)
#KOSONGKAN ATAU HAPUS KOLOM YANG TIDAK MENGANDUNG NUMERIK
datamhs$nama<-NULL
datamhs$nama_game<-NULL
pairs(datamhs)
#View(datamhs)

#Buat model regresi logistik
model<- glm(platform_game ~ usia + semester,data = datamhs,family = "binomial")
summary(model)

#LAKUKAN PENGACAKAN DATA
set.seed(1)
#BAGI DATA MENJADI DUA (TRAINING 50% DAN SAMPLING 50%)
train = sample(dim(datamhs)[1], dim(datamhs)[1]*0.50)
test = datamhs[-train, ]

#BUAT MODEL 2 MENGGUNAKAN DATA TRAINING
model2 = glm(platform_game ~ usia + semester, data = datamhs, family = binomial, subset = train)

#MENENTUKAN NILAI PREDIKSI DAN MENGUBAH SEMUA DATA MENJADI KATEGORI MOBILE DAN MENGUBAHNYA LAGI MENJADI KATEGORI PC
#APABILA NILAI PREDIKSINYA LEBIH DARI 0.5
log.prob_def = predict(model2, test, type = "response")
log.pred_def = rep("mobile", dim(datamhs)[1]*0.50)
log.pred_def[log.prob_def > 0.5] = "PC"
table(log.pred_def, test$platform_game)
#MENCARI NILAI ERROR
mean(log.pred_def == test$platform_game)
mean(log.pred_def != test$platform_game)


#KOMENTAR : 
#PADA PENGACAKAN set.seed(1) menghasilkan tabel prediksi:
#log.pred_def Mobile PC
#     mobile     43  7
#PADA TABEL INI NILAI PREDIKSI UNTUK ITU DIBAWAH 0.5 SEMUA SEHINGGA BANYAKNYA KATEGORI PC INI TIDAK ADA
#JIKA KITA MENCARI NILAI ERROR MENGGUNAKAN mean(log.pred_def == test$platform_game) NILAI ERRORNYA AKAN BERNILAI 0
#NILAI 0 INI ARTINYA TIDAK ADA NILAI ASLI YANG TIDAK SESUAI DENGAN PREDIKSI (PREDIKSINYA BENAR)

#NILAI ERROR MENGGUNAKAN mean(log.pred_def != test$platform_game) NILAI ERRORNYA AKAN BERNILAI 1
#NILAI 1 INI BERARTI NILAI ASLINYA SAMA DENGAN PREDIKSI (PREDIKSINYA AKURAT)




#PENGULANGAN 1
#LAKUKAN PENGACAKAN DATA
set.seed(303)
#BAGI DATA MENJADI DUA (TRAINING 50% DAN SAMPLING 50%)
train = sample(dim(datamhs)[1], dim(datamhs)[1]*0.50)
test = datamhs[-train, ]

#BUAT MODEL 2 MENGGUNAKAN DATA TRAINING
model2 = glm(platform_game ~ usia + semester, data = datamhs, family = binomial, subset = train)

#Obtain a prediction of default status for each individual in the validation set by computing the posterior probability of default 
#for that individual, and classifying the individual to the default category if the posterior probability is greater than 0.5.
log.prob_def = predict(model2, test, type = "response")
log.pred_def = rep("mobile", dim(datamhs)[1]*0.50)
log.pred_def[log.prob_def > 0.5] = "PC"
table(log.pred_def, test$platform_game)
#MENCARI NILAI ERROR
mean(log.pred_def == test$platform_game)
mean(log.pred_def != test$platform_game)

#KOMENTAR : 
#PADA PENGACAKAN set.seed(303) menghasilkan tabel prediksi:
#log.pred_def Mobile PC
#     mobile     43  4
#     PC          3  0
#PADA TABEL INI NILAI PREDIKSI UNTUK ITU DIBAWAH 0.5 SEMUA SEHINGGA BANYAKNYA KATEGORI PC INI TIDAK ADA
#JIKA KITA MENCARI NILAI ERROR MENGGUNAKAN mean(log.pred_def == test$platform_game) NILAI ERRORNYA AKAN BERNILAI 0
#NILAI 0 INI ARTINYA TIDAK ADA NILAI ASLI YANG TIDAK SESUAI DENGAN PREDIKSI (PREDIKSINYA BENAR)

#NILAI ERROR MENGGUNAKAN mean(log.pred_def != test$platform_game) NILAI ERRORNYA AKAN BERNILAI 1
#NILAI 1 INI BERARTI NILAI ASLINYA SAMA DENGAN PREDIKSI (PREDIKSINYA AKURAT)


#PENGULANGAN 2
#LAKUKAN PENGACAKAN DATA
set.seed(607)
#BAGI DATA MENJADI DUA (TRAINING 50% DAN SAMPLING 50%)
train = sample(dim(datamhs)[1], dim(datamhs)[1]*0.50)
test = datamhs[-train, ]

#BUAT MODEL 2 MENGGUNAKAN DATA TRAINING
model2 = glm(platform_game ~ usia + semester, data = datamhs, family = binomial, subset = train)

#Obtain a prediction of default status for each individual in the validation set by computing the posterior probability of default 
#for that individual, and classifying the individual to the default category if the posterior probability is greater than 0.5.
log.prob_def = predict(model2, test, type = "response")
log.pred_def = rep("mobile", dim(datamhs)[1]*0.50)
log.pred_def[log.prob_def > 0.5] = "PC"
table(log.pred_def, test$platform_game)
#MENCARI NILAI ERROR
mean(log.pred_def == test$platform_game)
mean(log.pred_def != test$platform_game)


#KOMENTAR : 
#PADA PENGACAKAN set.seed(607) menghasilkan tabel prediksi:
#log.pred_def Mobile PC
#     mobile     43  5
#     PC          2  0
#PADA TABEL INI NILAI PREDIKSI UNTUK ITU DIBAWAH 0.5 SEMUA SEHINGGA BANYAKNYA KATEGORI PC INI TIDAK ADA
#JIKA KITA MENCARI NILAI ERROR MENGGUNAKAN mean(log.pred_def == test$platform_game) NILAI ERRORNYA AKAN BERNILAI 0
#NILAI 0 INI ARTINYA TIDAK ADA NILAI ASLI YANG TIDAK SESUAI DENGAN PREDIKSI (PREDIKSINYA BENAR)

#NILAI ERROR MENGGUNAKAN mean(log.pred_def != test$platform_game) NILAI ERRORNYA AKAN BERNILAI 1
#NILAI 1 INI BERARTI NILAI ASLINYA SAMA DENGAN PREDIKSI (PREDIKSINYA AKURAT)


#PENGULANGAN 3
#LAKUKAN PENGACAKAN DATA
set.seed(12345)
#BAGI DATA MENJADI DUA (TRAINING 50% DAN SAMPLING 50%)
train = sample(dim(datamhs)[1], dim(datamhs)[1]*0.50)
test = datamhs[-train, ]

#BUAT MODEL 2 MENGGUNAKAN DATA TRAINING
model2 = glm(platform_game ~ usia + semester, data = datamhs, family = binomial, subset = train)

#Obtain a prediction of default status for each individual in the validation set by computing the posterior probability of default 
#for that individual, and classifying the individual to the default category if the posterior probability is greater than 0.5.
log.prob_def = predict(model2, test, type = "response")
log.pred_def = rep("mobile", dim(datamhs)[1]*0.50)
log.pred_def[log.prob_def > 0.5] = "PC"
table(log.pred_def, test$platform_game)
#MENCARI NILAI ERROR
mean(log.pred_def == test$platform_game)
mean(log.pred_def != test$platform_game)

#KOMENTAR : 
#PADA PENGACAKAN set.seed(12345) menghasilkan tabel prediksi:
#log.pred_def Mobile PC
#     mobile     43  7
#PADA TABEL INI NILAI PREDIKSI UNTUK ITU DIBAWAH 0.5 SEMUA SEHINGGA BANYAKNYA KATEGORI PC INI TIDAK ADA
#JIKA KITA MENCARI NILAI ERROR MENGGUNAKAN mean(log.pred_def == test$platform_game) NILAI ERRORNYA AKAN BERNILAI 0
#NILAI 0 INI ARTINYA TIDAK ADA NILAI ASLI YANG TIDAK SESUAI DENGAN PREDIKSI (PREDIKSINYA BENAR)

#NILAI ERROR MENGGUNAKAN mean(log.pred_def != test$platform_game) NILAI ERRORNYA AKAN BERNILAI 1
#NILAI 1 INI BERARTI NILAI ASLINYA SAMA DENGAN PREDIKSI (PREDIKSINYA AKURAT)



#KESIMPULAN AKHIR:
#setelah dilakukan 3x perulangan didapatkan nilai eror yang sama
#terdapat 2 cara mencari nilai
#dengan membandingkan menggunakan == hasil yang diperoleh adalah 0
#hal ini berarti tidak ada nilai asli yang tidak sesuai dengan prediksi

#dengan membandingkan menggunakan != hasil yang diperoleh adalah 1
#hal ini berarti nilai prediksi sesuai dengan nilai asli (akurasi prediksi tepat)