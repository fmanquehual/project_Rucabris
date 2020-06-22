library('vegan')
library('rich')
library('ggplot2')
library('xtable')
library('stringr')

setwd('C:/Users/Francisco/Documents/disco_duro_extraible/rucabris/puntos_traks_finales/db/')

# Lectura de datos ----

db <- read.csv('base_de_datos_aves_rucamanque.csv')
head(db)
db <- db[,-21]

levels(db$nombre_comun)
  
db.aves.mma <- read.csv('aves_mma_para_merge.csv')
head(db.aves.mma)

db.aves.mma2 <- db.aves.mma[,c('Nombre.Cientifico', 'Estado.Conservacion')]
names(db.aves.mma2) <- c('Nombre.Cientifico', 'estado_conservacion_mma')
head(db.aves.mma2)  

db2 <- merge(db, db.aves.mma2, by.x = 'nombre_cientifico_mma', by.y = 'Nombre.Cientifico', all.x = TRUE)
head(db2)

for (i in 1:ncol(db2)) {
  #i <- 27
  if( is.factor(db2[,i]) ){db2[,i] <- as.character(db2[,i])}
  idx <- which(db2[,i]=='')
  idy <- which(is.na(db2[,i]==''))
  db2[idx,i] <- 'NA'
  db2[idy,i] <- 'NA'
}

head(db2)
unique(db2$nombre_comun)

db3 <- db2[c('n', 'estacion', 'fecha', 'terreno', 'muestreo', 'numero_recorrido',
               'n.est', 'x.coord', 'y.coord', 'hr.ll', 'hr.i', 'hr.t', 'familia', 
               'genero', 'c.spp','nombre_cientifico_uicn', 'nombre_cientifico_avesdechile', 'nombre_cientifico_mma',
               'nombre_comun', 'estado_conservacion_uicn', 'estado_conservacion_mma', 'ley_de_caza_B',
               'ley_de_caza_S', 'ley_de_caza_E', 'ley_de_caza_N', 'ley_de_caza_C', 'ley_de_caza_S.1',
               'ley_de_caza_A', 't.r', 'dist', 'lugar_perturbacion', 'perturbacion', 'uso_suelo', 
               'clima', 'fase_lunar', 'T.seco', 'T.rocio', 'T.min', 'T.max', 'Viento_grado_Kph',
               'exposicion_al_sol_grados', 'altitud', 'pendiente_porcentaje')]
head(db3)

# write.csv(db3, 'base_de_datos_aves_rucamanque_corregido.csv', row.names = FALSE)

db4 <- db3[,c('nombre_comun', 'nombre_cientifico_mma', 'familia', 'muestreo', 'estado_conservacion_uicn', 'estado_conservacion_mma')]
head(db4)

db5 <- db4[!duplicated(db4), ]
db5$nombre_comun <- as.character(db5$nombre_comun)
db6 <- db5[order(db5$nombre_comun),]
db6$estado_conservacion_mma[db6$estado_conservacion_mma%in%'NA'] <- 'Sin clasificar'
names(db6) <- c('Nombre común', 'Nombre científico', 'Familia', 'Muestreo', 'Estado de conservación UICN', 'Estado de conservación MMA')
db6$N <- 1:nrow(db6)
db7 <- db6[,c('N', 'Nombre común', 'Nombre científico', 'Familia', 'Muestreo', 'Estado de conservación UICN', 'Estado de conservación MMA')]
head(db7)

# write.csv(db7, 'table_all_spp_Rucamanque.csv', row.names = FALSE)

names.j <- names(table(db7$`Nombre común`))
num <- as.vector(unlist(table(db7$`Nombre común`)))
db.x <- data.frame(name=names.j, rep=num)

db8 <- merge(db7, db.x, by.x='Nombre común', by.y = 'name', all.x = TRUE)
subset(db8, rep==2)

db8$Muestreo[db8$rep%in%2] <- 'D/N'
head(db8)
subset(db8, rep==2)

db9 <- db8[!duplicated(db8$`Nombre común`), ]

db10 <- db9[,c('Nombre común', 'Nombre científico', 'Familia', 'Muestreo', 'Estado de conservación UICN', 'Estado de conservación MMA')]
row.names(db9) <- 1:nrow(db9)
head(db10)

db10$`Estado de conservación MMA`[str_detect(db10$`Estado de conservación MMA`, ',')] <- 'LC, VU'
db10$`Estado de conservación MMA`[str_detect(db10$`Estado de conservación MMA`, 'Preo')] <- 'LC'
db10$`Estado de conservación MMA`[str_detect(db10$`Estado de conservación MMA`, 'NT')] <- 'NT'
db10$`Estado de conservación MMA`[str_detect(db10$`Estado de conservación MMA`, 'Sin')] <- 'S.I'
db10$Muestreo[str_detect(db10$Muestreo, 'diurno')] <- 'D'
db10$Muestreo[str_detect(db10$Muestreo, 'nocturno')] <- 'N'
tail(db10)
dim(db10)

db2.merge <- read.csv('residencia_de_spp.csv')
db2.merge2 <- db2.merge[,c('names', 'residencia')]
names(db2.merge2) <- c('names', 'Residencia')
head(db2.merge2)
dim(db2.merge2)

db11 <- merge(db10, db2.merge2, by.x='Nombre común', by.y='names')
dim(db11)
db12 <- db11[,c('Nombre común', 'Nombre científico', 'Familia', 'Muestreo', 'Residencia', 'Estado de conservación UICN', 'Estado de conservación MMA')]
tail(db12)
xtable(db12)


db.d <- subset(db, muestreo=='diurno')

db.i <- db.d
plot(unique(db.i$x.coord), unique(db.i$y.coord), col='white')
text(unique(db.i$x.coord), unique(db.i$y.coord), unique(db.i$n.est))
