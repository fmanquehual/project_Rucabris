library('vegan')
library('rich')
library('ggplot2')
library('xtable')

setwd('C:/Users/Francisco/Documents/disco_duro_extraible/rucabris/puntos_traks_finales/db/')
#setwd('C:/Users/Usuario/Documents/Francisco/rucabris/db/')
# Lectura de datos ----

db0 <- read.csv('base_de_datos_aves_rucamanque_3.csv')
head(db0)
dim(db0)
names(db0)
table(db0$estacion)

db.d <- subset(db0, muestreo=='diurno')
ej <- db.d[!duplicated(db.d$fecha), ]
ej2 <- ej[,c('fecha', 'estacion')]
table(ej2$estacion)
j2[order(ej2$estacion),]

db.n <- subset(db0, muestreo=='nocturno')
ej3 <- db.n[!duplicated(db.n$fecha), ]
ej4 <- ej3[,c('fecha', 'estacion')]
table(ej4$estacion)
ej4[order(ej4$estacion),]


# fin ---


# ---
db0$ley_caza_union <- paste(db0$ley_de_caza_B, db0$ley_de_caza_S, db0$ley_de_caza_E,
                           db0$ley_de_caza_S.1, sep = '')
db0$ley_caza_union <- gsub('NA', '', db0$ley_caza_union)
table(db0$ley_caza_union)

db <- db0[,c('n', 'estacion', 'nombre_comun', 'nombre_cientifico_mma', 'familia', 'muestreo', 'residencia', 'ley_caza_union')]
db$estacion <- as.character(db$estacion)
db$nombre_comun <- as.character(db$nombre_comun)
table(db$estacion)

db$estacion.numero <- 0
db$estacion.numero[db$estacion%in%'Otoño'] <- 1
db$estacion.numero[db$estacion%in%'Invierno'] <- 2
db$estacion.numero[db$estacion%in%'Primavera'] <- 3
db$estacion.numero[db$estacion%in%'Verano'] <- 4
table(db$estacion.numero)

names.i <- unique(db$nombre_comun)
db.matrix <- data.frame(nombre=names.i, Otoño=0, Invierno=0, Primavera=0, Verano=0)
db.matrix$nombre <- as.character(db.matrix$nombre)
db.matrix <- db.matrix[order(db.matrix$nombre),]
rownames(db.matrix) <- 1:nrow(db.matrix)
head(db.matrix)

for (i in 1:max(db$estacion.numero)) {# calculo abundancia por terreno
  #i <- 1
  db1 <- subset(db, estacion.numero==i)
  db.temp <- as.data.frame(unlist(table(db1$nombre_comun)))
  db.matrix[db.matrix$nombre%in%db.temp$Var1,i+1] <- db.temp$Freq
}  

db.matrix$Total <- 0  

for (j in 1:nrow(db.matrix)) {# calculo abundancia relativa mensual
  #j <- 1
  db.matrix[j,ncol(db.matrix)] <- sum(as.vector(unlist(db.matrix[j,2:ncol(db.matrix)])))
}

db.matrix


names.j <- c('Abundancia', 'Riqueza')
db.matrix2 <- data.frame(nombre=names.j, Otoño=0, Invierno=0, Primavera=0, Verano=0, Total=NA)

db.matrix3 <- rbind(db.matrix, db.matrix2)
tail(db.matrix3)

for (k in 2:ncol(db.matrix3)) {
  #k <- 1
  db.matrix3[51,k] <- sum(db.matrix3[1:50,k]) #abundancia
  
  TRUE.i <- db.matrix3[1:50,k]>0
  num.TRUE <- length(TRUE.i[TRUE.i==TRUE])
  db.matrix3[52,k] <- num.TRUE #riqueza
}

db.matrix3
db2 <- db[,c('nombre_comun', 'nombre_cientifico_mma', 'familia', 'muestreo', 'residencia', 'ley_caza_union')]
db3 <- db2[!duplicated(db2), ]

table(db3$nombre_comun)


# -
names.j <- names(table(db3$nombre_comun))
num <- as.vector(unlist(table(db3$nombre_comun)))
db.x <- data.frame(name=names.j, rep=num)

db4 <- merge(db3, db.x, by.x='nombre_comun', by.y = 'name', all.x = TRUE)
subset(db4, rep==2)

db4$Muestreo[str_detect(db4$muestreo, 'diurno')] <- 'D'
db4$Muestreo[str_detect(db4$muestreo, 'nocturno')] <- 'N'
db4$Muestreo[db4$rep%in%2] <- 'D/N'

db4
subset(db4, rep==2)

db5 <- db4[!duplicated(db4$nombre_comun), ]
row.names(db5) <- 1:nrow(db5)
db6 <- db5[,c("nombre_comun", "nombre_cientifico_mma", "familia", "Muestreo", "residencia", "ley_caza_union")]
db6
# -


db7 <- merge(db.matrix3, db6, by.x = 'nombre', by.y = 'nombre_comun', all.x = TRUE)
db7[order(db7$nombre),]
db7

db8 <- db7[,c("nombre", "nombre_cientifico_mma", "familia", "Muestreo", "residencia", "ley_caza_union", "Otoño", "Invierno", "Primavera", "Verano", "Total")]
db9 <- db8[c(2:40,42:nrow(db8), 1,41),]
row.names(db9) <- 1:nrow(db9)
names(db9) <- c('Nombre común', 'Nombre científico', 'Familia', 'Muestreo', 'Residencia', 'Ley de caza', 'Otoño', 'Invierno', 'Primavera', 'Verano', 'Total')
db9

xtable(db9, digits = 0)
