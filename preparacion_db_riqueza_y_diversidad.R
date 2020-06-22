library('xlsx')
setwd('C:/Users/Usuario/Documents/Francisco/rucabris/db/')
rm(list = ls())
dev.off()

# Lectura de datos ----
db <- read.csv('base_de_datos_aves_rucamanque_corregido.csv')
head(db)
unique(db$numero_recorrido)

recorrido.d <- read.csv('riqueza_por_recorrido_diurno.csv')
head(recorrido.d)

recorrido.n <- read.csv('riqueza_por_recorrido_nocturno.csv')
head(recorrido.n)

estaciones.d <- read.csv('riqueza_por_estacion_diurno.csv')
head(estaciones.d)

estaciones.n <- read.csv('riqueza_por_estacion_nocturno.csv')
head(estaciones.n)


recorrido <- rbind(recorrido.d, recorrido.n)
recorrido$id <- paste(recorrido$recorrido, recorrido$tipo, sep = '_')
recorrido <- recorrido[,c('id', 'riqueza', 'diversidad_shannon')]
names(recorrido) <- c('id', 'riqueza_recorrido', 'diversidad_shannon_recorrido')
recorrido

estaciones <- rbind(estaciones.d, estaciones.n)
estaciones$id <- paste(estaciones$estacion, estaciones$tipo, sep = '_')
estaciones <- estaciones[,c('id', 'riqueza', 'diversidad_shannon')]
names(estaciones) <- c('id', 'riqueza_estacion', 'diversidad_shannon_estacion')
estaciones

head(db)
db$id_recorrido <- paste(db$numero_recorrido, db$muestreo, sep='_')
db$id_estaciones <- paste(db$n.est, db$muestreo, sep='_')

db2 <- merge(db, recorrido, by.x = 'id_recorrido', by.y = 'id', all.x = TRUE)
dim(db)
dim(db2)
head(db2)

db3 <- merge(db2, estaciones, by.x = 'id_estaciones', by.y = 'id', all.x = TRUE)
dim(db)
dim(db3)
head(db3)

db4 <- db3[order(db3$n),]

dbf <- db4[c('n', 'estacion', 'fecha', 'terreno', 'muestreo', 'numero_recorrido',
             'n.est', 'riqueza_recorrido', 'diversidad_shannon_recorrido', 'riqueza_estacion', 'diversidad_shannon_estacion',
             'x.coord', 'y.coord', 'hr.ll', 'hr.i', 'hr.t', 'familia', 
             'genero', 'c.spp','nombre_cientifico_uicn', 'nombre_cientifico_avesdechile', 'nombre_cientifico_mma',
             'nombre_comun', 'estado_conservacion_uicn', 'estado_conservacion_mma', 'ley_de_caza_B',
             'ley_de_caza_S', 'ley_de_caza_E', 'ley_de_caza_N', 'ley_de_caza_C', 'ley_de_caza_S.1',
             'ley_de_caza_A', 't.r', 'dist', 'lugar_perturbacion', 'perturbacion', 'uso_suelo', 
             'clima', 'fase_lunar', 'T.seco', 'T.rocio', 'T.min', 'T.max', 'Viento_grado_Kph',
             'exposicion_al_sol_grados', 'altitud', 'pendiente_porcentaje')]


write.xlsx(dbf, 'base_de_datos_aves_rucamanque_2.xlsx', row.names = FALSE)
#write.csv(dbf, 'base_de_datos_aves_rucamanque_2.csv', row.names = FALSE)

dbf2 <- read.csv('base_de_datos_aves_rucamanque_2.csv')
dbf2[c(1565:1573),]
