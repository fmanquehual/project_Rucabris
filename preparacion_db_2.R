library('stringr')

setwd('C:/Users/Francisco/Documents/disco_duro_extraible/rucabris/puntos_traks_finales/db/')

db <- read.csv('db_final_corregida.csv')
head(db)

db.fase.lunar <- read.csv('fase_lunar.csv')
head(db.fase.lunar)

db.uso <- read.csv('uso_actual.csv')
head(db.uso)

db.ley.caza <- read.csv('ley_de_caza.csv')
head(db.ley.caza)

db.noc.claudia <- read.csv('db_nocturnas_claudia.csv', dec=',')
head(db.noc.claudia)

db.diu.claudia <- read.csv('db_diurnas_claudia.csv', dec = ',')
head(db.diu.claudia)

# ---
names(db)

db1 <- merge(db, db.fase.lunar, by='fecha')
head(db1)

db2 <- merge(db1, db.uso, by.x='n.est', by.y='n_est')
head(db2)

db.ley.caza$nombre_comun <- as.character(db.ley.caza$nombre_comun)
db2$nombre_comun <- as.character(db2$nombre_comun)

setdiff(unique(db.ley.caza$nombre_comun),
         unique(db2$nombre_comun)
)

db2$nombre_comun <- gsub('Tucúquere ', 'Tucúquere', db2$nombre_comun)
db2$nombre_comun <- gsub('Diucón ', 'Diucón', db2$nombre_comun)
db2$nombre_comun <- gsub('Bandurria ', 'Bandurria', db2$nombre_comun)
db2$nombre_comun <- gsub('Tenca ', 'Tenca', db2$nombre_comun)
db.ley.caza$nombre_comun <- gsub('Codorniz ', 'Codorniz', db.ley.caza$nombre_comun)

setdiff(unique(db.ley.caza$nombre_comun),
         unique(db2$nombre_comun)
)

names(db.ley.caza)
db3 <- merge(db2, db.ley.caza, by='nombre_comun')
head(db3)



# ---
setdiff(names(db.diu.claudia), names(db.noc.claudia))

db.diu.claudia$muestreo <- 'diurno'
db.noc.claudia$muestreo <- 'nocturno'
db.diu.claudia <- db.diu.claudia[-3]
db.clau <- rbind(db.diu.claudia, db.noc.claudia)
head(db.clau)

db.clau2 <- db.clau[c('fecha', 'hr.i', 'T.seco', 'T.rocio', 'T.min', 'T.max', 'Viento_grado_Kph')]
db.clau2$fecha <- as.character(db.clau2$fecha)
db.clau2$hr.i <- as.character(db.clau2$hr.i)
head(db.clau2)

db3$fecha <- as.character(db3$fecha)
db3$hr.i <- as.character(db3$hr.i)

db.clau2$id <- paste(db.clau2$fecha, db.clau2$hr.i, sep = '_')
db.clau3 <- db.clau2[c('id', 'T.seco', 'T.rocio', 'T.min', 'T.max', 'Viento_grado_Kph')]

unique(db.clau3$id)
length(db.clau3$id)

db.clau4 <- db.clau3[!duplicated(db.clau3$id), ]
dim(db.clau4)

db3$id <- paste(db3$fecha, db3$hr.i, sep = '_')
head(db3)
dim(db3)

db4.diu <- merge(db3, db.clau4, by = 'id', all.x = TRUE)
head(db4.diu)
dim(db4.diu)

db5 <- db4.diu
# ---





# ----

db5$perturbacion <- as.character(db5$perturbacion)
patrones <- unique(db5$perturbacion)
patrones

id1 <- which(str_detect(db5$perturbacion, 'Camino principal'))
id2 <- which(str_detect(db5$perturbacion, 'Camino Principal'))

db5$perturbacion <- gsub('Camino principal de rucamanque, ', '', db5$perturbacion)
db5$perturbacion <- gsub('Camino principal de rucamanque', '', db5$perturbacion)
db5$perturbacion <- gsub('Camino principal, ', '', db5$perturbacion)
db5$perturbacion <- gsub('Camino principal ', '', db5$perturbacion)
db5$perturbacion <- gsub('Camino Principal, ', '', db5$perturbacion)
db5$perturbacion <- sub(',', '', db5$perturbacion)

db5$lugar_perturbacion <- NA
db5$perturbacion_ok <- 0

db5$lugar_perturbacion[id1] <- 'Camino principal'
db5$lugar_perturbacion[id2] <- 'Camino principal'
db5$perturbacion_ok[id1] <- 1
db5$perturbacion_ok[id2] <- 1
head(db5)

id3 <- which(str_detect(db5$perturbacion, 'Ecotono entre Rucamanque y Plantacion'))
db5$lugar_perturbacion[id3] <- 'Ecotono entre Rucamanque y Plantacion'
db5$perturbacion_ok[id3] <- 1

id4 <- which(str_detect(db5$perturbacion, 'Ecotono entre Rucamanque y Padera'))
id5 <- which(str_detect(db5$perturbacion, 'Ecotono entre Rucamanque y Pradera'))
db5$lugar_perturbacion[id4] <- 'Ecotono entre Rucamanque y Pradera'
db5$lugar_perturbacion[id5] <- 'Ecotono entre Rucamanque y Pradera'
db5$perturbacion_ok[id4] <- 1
db5$perturbacion_ok[id5] <- 1

db5$perturbacion <- gsub('Ecotono entre Rucamanque y Plantacion ', '', db5$perturbacion)
db5$perturbacion <- gsub('Ecotono entre Rucamanque y Padera', '', db5$perturbacion)
db5$perturbacion <- gsub('Ecotono entre Rucamanque y Pradera', '', db5$perturbacion)
db5$perturbacion <- gsub('Ecotono entre Rucamanque Plantacion ', '', db5$perturbacion)
db5$perturbacion <- gsub('Ecotono entre Rucamanque Pradera', '', db5$perturbacion)
db5$perturbacion <- sub('de ', '', db5$perturbacion)
db5$perturbacion <- sub('y ', '', db5$perturbacion)

db5$perturbacion <- as.character(db5$perturbacion)
patrones <- unique(db5$perturbacion)
patrones

id6 <- which(str_detect(db5$perturbacion, 'cruce'))
db5$lugar_perturbacion[id6] <- 'Cruce de Rucamanque'
db5$perturbacion_ok[id6] <- 1

# ---

names(db5)
db6 <- db5[c('n', 'estacion', 'fecha', 'clima', 'fase_lunar', 'T.seco', 'T.rocio', 'T.min', 'T.max',
             'Viento_grado_Kph', 'terreno', 'muestreo', 'numero_recorrido',
             'uso_suelo', 'n.est', 'x.coord', 'y.coord', 'hr.ll', 'hr.i', 'hr.t', 'familia', 
             'genero', 'c.spp','nombre_cientifico_uicn', 'nombre_cientifico_avesdechile', 'nombre_cientifico_mma',
             'nombre_comun', 'estado_conservacion_uicn', 'estado_conservacion_mma', 'ley_de_caza_B',
             'ley_de_caza_S', 'ley_de_caza_E', 'ley_de_caza_N', 'ley_de_caza_C', 'ley_de_caza_S.1',
             'ley_de_caza_A', 't.r', 'dist', 'lugar_perturbacion', 'perturbacion')]
setdiff( names(db6), names(db5))
head(db6)

setwd('C:/Users/Francisco/Documents/disco_duro_extraible/rucabris/puntos_traks_finales/db/')
#write.csv(db6, 'base_de_datos_aves_rucamanque_preliminar.csv', row.names = FALSE)

# hubieron arreglos manuales (ausencia datos climaticos)
# ----

library('raster')
library('rgdal')

utm18 <- "+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 18 sur con datum WGS84
dbf <- read.csv('base_de_datos_aves_rucamanque_preliminar.csv')
head(dbf)

x <- unique(dbf$x.coord)
y <- unique(dbf$y.coord)
data.i <- unique(dbf$n.est)

origen <- SpatialPoints(cbind(x, y), proj4string = CRS(utm18))
db.origen <- data.frame(id= c(1:nrow(origen@coords)), x=coordinates(origen)[,1], y=coordinates(origen)[,2])
origen.exp <- SpatialPointsDataFrame(origen, 
                                     data = db.origen, 
                                     match.ID = TRUE)
plot(origen.exp, pch=16)

setwd('C:/Users/Francisco/Documents/disco_duro_extraible/rucabris/puntos_traks_finales/coberturas/')
stack.i <- stack('09_aspect_18s.tif', '09_dem_18s.tif', '09_slope_18s.tif')
plot(stack.i)

topo.info <- extract(stack.i, origen.exp, df=TRUE)
names(topo.info) <- c('ID', 'exposicion_al_sol_grados', 'altitud', 'pendiente_porcentaje')

dbf2 <- merge(dbf, topo.info, by.x='n.est', by.y='ID')
head(dbf2)

dbf3 <- dbf2[c('n', 'estacion', 'fecha', 'terreno', 'muestreo', 'numero_recorrido',
             'n.est', 'x.coord', 'y.coord', 'hr.ll', 'hr.i', 'hr.t', 'familia', 
             'genero', 'c.spp','nombre_cientifico_uicn', 'nombre_cientifico_avesdechile', 'nombre_cientifico_mma',
             'nombre_comun', 'estado_conservacion_uicn', 'estado_conservacion_mma', 'ley_de_caza_B',
             'ley_de_caza_S', 'ley_de_caza_E', 'ley_de_caza_N', 'ley_de_caza_C', 'ley_de_caza_S.1',
             'ley_de_caza_A', 't.r', 'dist', 'lugar_perturbacion', 'perturbacion', 'uso_suelo', 
             'clima', 'fase_lunar', 'T.seco', 'T.rocio', 'T.min', 'T.max', 'Viento_grado_Kph',
             'exposicion_al_sol_grados', 'altitud', 'pendiente_porcentaje')]
head(dbf3)

for (i in 1:ncol(dbf3)) {
  #i <- 27
  if( is.factor(dbf3[,i]) ){dbf3[,i] <- as.character(dbf3[,i])}
idx <- which(dbf3[,i]=='')
dbf3[idx,i] <- 'NA'
  }

head(dbf3)

dbf4 <- dbf3[order(dbf3$fecha, dbf3$numero_recorrido),]
dbf4$n <- 1:nrow(dbf4)
head(dbf4)

setwd('C:/Users/Francisco/Documents/disco_duro_extraible/rucabris/puntos_traks_finales/db/')
# write.csv(dbf4, 'base_de_datos_aves_rucamanque.csv', row.names = FALSE)
