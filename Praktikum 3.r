#membaca data populasi
populasi = read.csv("data.csv", header=TRUE) #TRUE = baris pertama dibaca sebagai header 
populasi

#Cek dimensi
dim(populasi)

#banyaknya data
N = dim(populasi)[1]
N

#untuk mencari indeks mana aja yang perempuan atau laki2
id.pr=which(populasi[,2]=="Perempuan",arr.ind= TRUE)
id.pr

id.lk=which(populasi[,2]=="Laki-Laki",arr.ind= TRUE)
id.lk

#untuk menampilkan data peremuan dan laki2
popu.pr = populasi [id.pr,]
popu.pr

popu.lk = populasi [id.lk,]
popu.lk

#jumlah populasi laki2 dan perempuan
Np = dim(popu.pr)[1]
Np
Nl = dim(popu.lk)[1]
Nl

#cara cari np nl
np= Np/N *5
np

nl = Nl/N*5
nl

#cara generate angka random
sample_pr <- sample (id.pr,np) 
sample_pr

sample_lk <- sample (id.lk,nl) 
sample_lk

#kita tampilin dulu data sampelnya
sample.pr = populasi [sample_pr,]
sample.pr

sample.lk = populasi [sample_lk,]
sample.lk

#1 dugaan rata2 sample
ratas_pr = mean(sample.pr[,3])
ratas_pr

ratas_lk = mean(sample.lk[,3])
ratas_lk

Miu = ((Nl*ratas_lk) + (Np*ratas_pr)) / N
Miu

#BoE
error = 2*sqrt(Miu)
error

#2 Total
ragam_spr = var(sample.pr[,3])
ragam_spr

ragam_slk = var(sample.lk[,3])
ragam_slk

Tou = Nl*