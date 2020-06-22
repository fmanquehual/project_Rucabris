library('ggplot2')
library('prettymapr')

setwd('C:/Users/Francisco/Documents/disco_duro_extraible/rucabris/puntos_traks_finales/db/')

db <- read.csv('base_de_datos_aves_rucamanque_2.csv')
head(db)
names(db)
dim(db)

db$nombre_comun <- as.character(db$nombre_comun)
spp <- unique(db$nombre_comun) ; spp

db.otonho <- subset(db, estacion=='Otoño')
dim(db.otonho)
db.otonho$nombre_comun <- as.character(db.otonho$nombre_comun)
spp.otonho <- unique(db.otonho$nombre_comun) ; spp.otonho

db.invierno <- subset(db, estacion=='Invierno')
dim(db.invierno)
db.invierno$nombre_comun <- as.character(db.invierno$nombre_comun)
spp.invierno <- unique(db.invierno$nombre_comun) ; spp.invierno

db.primavera <- subset(db, estacion=='Primavera')
dim(db.primavera)
db.primavera$nombre_comun <- as.character(db.primavera$nombre_comun)
spp.primavera <- unique(db.primavera$nombre_comun) ; spp.primavera

db.verano <- subset(db, estacion=='Verano')
dim(db.verano)
db.verano$nombre_comun <- as.character(db.verano$nombre_comun)
spp.verano <- unique(db.verano$nombre_comun) ; spp.verano

t1 <- data.frame(names=spp, p.otonho=NA, p.invierno=NA, p.primavera=NA, p.verano=NA)
t1

t1$p.otonho[t1$names%in%spp.otonho] <- 'O'
t1$p.invierno[t1$names%in%spp.invierno] <- 'I'
t1$p.primavera[t1$names%in%spp.primavera] <- 'P'
t1$p.verano[t1$names%in%spp.verano] <- 'V'
t1

t1$residencia <- paste(t1$p.otonho, t1$p.invierno, t1$p.primavera, t1$p.verano, sep = '')
t1$residencia <- gsub('NA', '', t1$residencia)
t1

write.csv(t1, 'residencia_de_spp.csv', row.names = F)
