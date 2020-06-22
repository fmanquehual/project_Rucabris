library(vegan)
library(rich)

setwd('C:/Users/Francisco/Documents/disco_duro_extraible/rucabris/puntos_traks_finales/db/')

db.d <- read.csv('db_diurnas.csv', dec = ',')
names(db.d)[3] <- 'estacion'
head(db.d)

db.n <- read.csv('db_noche.csv', dec = ',')
head(db.n)

db.spp.ec <- read.csv('db_spp_estadoConservacion_para_merge.csv')
head(db.spp.ec)
# fin ---





# preparacion db ----
head(db.d)
dim(db.d)
str(db.d)

db.d2 <- subset(db.d, dist<=30)
dim(db.d2)

db.d2$muestreo <- 'diurno'
db.n$muestreo <- 'nocturno'

setdiff( colnames(db.d2), colnames(db.n))

db.n$estacion <- NA

db <- rbind(db.d2, db.n)
head(db)
dim(db)
str(db)

db$fecha <- as.character(db$fecha)
db$c.spp <- as.character(db$c.spp)
db$fam <- as.character(db$fam)
db$gen <- as.character(db$gen)
db$nc.spp <- as.character(db$nc.spp)
db$spp <- as.character(db$spp)

unique(db$spp)
table(db$spp%in%unique(db$spp)[47])


db1 <- subset(db, spp!=unique(db$spp)[47])
table(db1$spp%in%unique(db$spp)[47])
unique(db1$spp)

table(db1$fecha)
db1$terreno <- 0

t1 <- c('26-10-2018', '27-10-2018')
t2 <- c('23-11-2018', '24-11-2018')
t3 <- c('30-11-2018', '01-12-2018')
t4 <- c('25-01-2019', '26-01-2019')
t5 <- c('15-02-2019', '16-02-2019')
t6 <- c('29-03-2019', '30-03-2019')
t7 <- c('31-05-2019', '01-06-2019')
t8 <- c('16-06-2019', '17-06-2019')
t9 <- c('27-07-2019', '28-07-2019')
t10 <- c('17-08-2019', '18-08-2019')
t11 <- c('30-08-2019', '31-08-2019')
t12 <- c('01-09-2019')

db1$terreno[db1$fecha%in%t1] <- 1
db1$terreno[db1$fecha%in%t2] <- 2
db1$terreno[db1$fecha%in%t3] <- 3
db1$terreno[db1$fecha%in%t4] <- 4
db1$terreno[db1$fecha%in%t5] <- 5
db1$terreno[db1$fecha%in%t6] <- 6
db1$terreno[db1$fecha%in%t7] <- 7
db1$terreno[db1$fecha%in%t8] <- 8
db1$terreno[db1$fecha%in%t9] <- 9
db1$terreno[db1$fecha%in%t10] <- 10
db1$terreno[db1$fecha%in%t11] <- 11
db1$terreno[db1$fecha%in%t12] <- 12

head(db1)
table(db1$fecha)
table(db1$terreno)

db1.2 <- db1[,c('n', 'fecha', 'estacion', 'n.est', 'x.coord', 'y.coord', 'hr.ll',
                'hr.i', 'hr.t', 'clima', 'c.spp',  #'fam', 'gen', 'nc.spp', 'spp', 
                't.r', 'dist', 'perturbacion', 'terreno', 'muestreo')]
head(db1.2)
dim(db1.2)

db1.2$estacion[db1.2$terreno==1] <- 'Primavera'
db1.2$estacion[db1.2$terreno==2] <- 'Primavera'
db1.2$estacion[db1.2$terreno==3] <- 'Primavera'
db1.2$estacion[db1.2$terreno==4] <- 'Verano'
db1.2$estacion[db1.2$terreno==5] <- 'Verano'
db1.2$estacion[db1.2$terreno==6] <- 'Verano'
db1.2$estacion[db1.2$terreno==7] <- 'Otoño'
db1.2$estacion[db1.2$terreno==8] <- 'Otoño'
db1.2$estacion[db1.2$terreno==9] <- 'Invierno'
db1.2$estacion[db1.2$terreno==10] <- 'Invierno'
db1.2$estacion[db1.2$terreno==11] <- 'Invierno'
db1.2$estacion[db1.2$terreno==12] <- 'Invierno'

dim(db1.2)
table(db1.2$estacion)
sum(table(db1.2$estacion))
head(db1.2)

head(db.spp.ec)

table(db.spp.ec$codigo_sp) # bien
table(db.spp.ec$familia)  # por corregir
table(db.spp.ec$genero)  # por corregir
table(db.spp.ec$nombre_cientifico_uicn)  # bien
table(db.spp.ec$nombre_cientifico_avesdechile)  # bien
table(db.spp.ec$nombre_cientifico_mma)  # bien
table(db.spp.ec$nombre_comun)  # bien
table(db.spp.ec$estado_conservacion_uicn)  # bien
table(db.spp.ec$estado_conservacion_mma)  # bien

db.spp.ec$familia <- gsub('\t', '', db.spp.ec$familia)
table(db.spp.ec$familia)  # por corregir

db.spp.ec$genero <- gsub('\t', '', db.spp.ec$genero)
table(db.spp.ec$genero)  # por corregir

db.spp.ec2 <- db.spp.ec[,-1]
db.spp.ec2

db1.3 <- merge(db1.2, db.spp.ec2, by.x='c.spp', by.y='codigo_sp', all.x=TRUE)
head(db1.3)

dim(db1.2)
dim(db1.3)

#db1.5$tipo_recorrido <- NA
db1.3$numero_recorrido <- 0

diurno1 <- 1:13
#db1.5$tipo_recorrido[db1.5$n.est%in%diurno1] <- 'Diurno'
db1.3$numero_recorrido[db1.3$n.est%in%diurno1] <- 1

diurno2 <- 14:18
#db1.5$tipo_recorrido[db1.5$n.est%in%diurno2] <- 'Diurno'
db1.3$numero_recorrido[db1.3$n.est%in%diurno2] <- 2

diurno3 <- 19:34
#db1.5$tipo_recorrido[db1.5$n.est%in%diurno3] <- 'Diurno'
db1.3$numero_recorrido[db1.3$n.est%in%diurno3] <- 3

diurno4 <- 32:36
#db1.5$tipo_recorrido[db1.5$n.est%in%diurno4] <- 'Nocturno'
db1.3$numero_recorrido[db1.3$n.est%in%diurno4] <- 1

diurno5 <- 37:41
#db1.5$tipo_recorrido[db1.5$n.est%in%diurno5] <- 'Nocturno'
db1.3$numero_recorrido[db1.3$n.est%in%diurno5] <- 2

db1.4 <- db1.3[,c('n', 'estacion', 'fecha', 'clima', 'terreno', 'muestreo', 'numero_recorrido', 'n.est', 'x.coord', 'y.coord', 'hr.ll',
                'hr.i', 'hr.t', 'c.spp', 'familia', 'genero', 'nombre_cientifico_uicn',
                'nombre_cientifico_avesdechile', 'nombre_cientifico_mma', 'nombre_comun', 'estado_conservacion_uicn',
                'estado_conservacion_mma', 't.r', 'dist', 'perturbacion')]
head(db1.4)

db1.5 <- db1.4[order(db1.4$n),]
row.names(db1.5) <- 1:nrow(db1.5)
head(db1.5)


head(db1.5)
#write.csv(db1.5, 'db_final_corregida.csv', row.names = FALSE)
