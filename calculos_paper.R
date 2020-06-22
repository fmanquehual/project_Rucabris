library('raster')
library('rgeos')
library('rgdal')
library('ggplot2')
library('vegan')
library('prettymapr')

wgs84 <- "+proj=longlat +ellps=WGS84" #coordenadas geograficas WGS84
utm18 <- "+proj=utm +zone=18 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 18 sur con datum WGS84
utm19 <- "+proj=utm +zone=19 +south +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0" #utm zona 19 sur con datum WGS84

setwd('C:/Users/Francisco/Documents/disco_duro_extraible/rucabris/puntos_traks_finales/db/')

db <- read.csv('base_de_datos_aves_rucamanque_2.csv')
head(db)
dim(db)
table(db$estacion)

muestreo.i <- 'nocturno'

# otonho ----
db.otonho <- subset(db, estacion=='Otoño')
head(db.otonho)
dim(db.otonho)


# indice y riqueza

db1 <- subset(db.otonho, muestreo==muestreo.i)
head(db1)
dim(db1)

db1$c.spp <- as.character(db1$c.spp)
length(unique(db1$c.spp)) # riqueza otonho
table(db1$c.spp)

db1$nombre_cientifico_avesdechile <- as.character(db1$nombre_cientifico_avesdechile)
table(db1$nombre_cientifico_avesdechile)




db2 <- db1[,c('n.est', 'nombre_cientifico_avesdechile')]
sort(unique(db2$n.est))
db2$presencia <- 1
head(db2)
str(db2)

table(db2$nombre_cientifico_avesdechile)
table(db2$n.est)

num <- sort(unique(db2$n.est)) ; num

# de dataframe a matriz ----

for (i in num) {
  out.pre <- subset(db2, n.est==i)
  name.out <- paste('out', i, '<-out.pre', sep = '')
  eval(parse(text=name.out))
}

trans <- function(input){
  num.i <- as.vector(unlist(table(input$nombre_cientifico_avesdechile)))
  name.i <- names(table(input$nombre_cientifico_avesdechile))
  db.i <- data.frame(name=name.i, num=num.i)
  return(db.i)}



db.j <- c()

for (i in num) {
  #i <- 32
  name.k <- paste('out', i, sep = '')
  
  ej <- data.frame(name=names(table(db2$nombre_cientifico_avesdechile)), num=0)
  ej2 <- trans( eval(parse(text=name.k)) )
  
  if(i==1 | i==32){ dbf <- merge(ej, ej2, by='name', all.x = TRUE) }else(
    dbf <- merge(dbf, ej2, by='name', all.x = TRUE))
  names(dbf)[ncol(dbf)] <- c(name.k)
  
  for (j in 1:nrow(dbf)) {
    # j <- 1
    if( is.na(dbf[j,ncol(dbf)]) ){dbf[j,ncol(dbf)] <- 0}
  }
}

dbf
dbf$sum <- 0

for (k in 1:nrow(dbf)) {
  dbf$sum[k] <- sum( dbf[k, c(3:ncol(dbf))] )
}

dbf
table(db2$nombre_cientifico_avesdechile)

dbf2 <- dbf[,-c(2, ncol(dbf))]
str(dbf2)
dbf2

dbf3 <- t(dbf2)
name.l <- dbf3[1,]
dbf4 <- dbf3[-1,]

dbf5 <- as.data.frame(dbf4)
row.names(dbf5) <- 1:nrow(dbf5)
colnames(dbf5) <- name.l
dbf5

for (m in 1:ncol(dbf5)) {
  dbf5[,m] <- as.character(dbf5[,m])
  dbf5[,m] <- as.numeric(dbf5[,m])
}

str(dbf5)
dbf5

# fin ---



# riqueza de spp por estacion
num.spp <- specnumber(dbf5) ; num.spp
rich <- as.vector(unlist(num.spp))

# indice de diversidad de Shannon-Wiener (H') por estacion 
diver <- diversity(x=dbf5, index = 'shannon') ; diver
diver.i <- round(diver, 2)
sort(diver.i)

# data out
db.out0 <- db1[!duplicated(db1$x.coord), ]
db.out1 <- db.out0[,c('n.est', 'x.coord', 'y.coord')]

db.plot2 <- data.frame(estacion=num, riqueza=rich, h=diver.i)
db.plot2

db.out2 <- merge(db.plot2, db.out1, by.x = 'estacion', by.y = 'n.est', x.all = TRUE)
db.out2

# fin ---


# mapa ----
origen <- SpatialPoints(c(db.out2[4],db.out2[5]), proj4string = CRS(utm18))
db.origen <- data.frame(db.out2)
origen.otonho <- SpatialPointsDataFrame(origen, 
                                     data = db.origen, 
                                     match.ID = TRUE)
plot(origen.otonho, pch=16)













# otonho ----
db.invierno <- subset(db, estacion=='Invierno')
head(db.invierno)
dim(db.invierno)


# indice y riqueza

db1 <- subset(db.invierno, muestreo==muestreo.i)
head(db1)
dim(db1)

db1$c.spp <- as.character(db1$c.spp)
length(unique(db1$c.spp)) # riqueza otonho
table(db1$c.spp)

db1$nombre_cientifico_avesdechile <- as.character(db1$nombre_cientifico_avesdechile)
table(db1$nombre_cientifico_avesdechile)




db2 <- db1[,c('n.est', 'nombre_cientifico_avesdechile')]
sort(unique(db2$n.est))
db2$presencia <- 1
head(db2)
str(db2)

table(db2$nombre_cientifico_avesdechile)
table(db2$n.est)

num <- sort(unique(db2$n.est)) ; num

# de dataframe a matriz ----

for (i in num) {
  out.pre <- subset(db2, n.est==i)
  name.out <- paste('out', i, '<-out.pre', sep = '')
  eval(parse(text=name.out))
}

trans <- function(input){
  num.i <- as.vector(unlist(table(input$nombre_cientifico_avesdechile)))
  name.i <- names(table(input$nombre_cientifico_avesdechile))
  db.i <- data.frame(name=name.i, num=num.i)
  return(db.i)}



db.j <- c()

for (i in num) {
  #i <- 3
  name.k <- paste('out', i, sep = '')
  
  ej <- data.frame(name=names(table(db2$nombre_cientifico_avesdechile)), num=0)
  ej2 <- trans( eval(parse(text=name.k)) )
  
  if(i==1 | i==32){ dbf <- merge(ej, ej2, by='name', all.x = TRUE) }else(
    dbf <- merge(dbf, ej2, by='name', all.x = TRUE))
  names(dbf)[ncol(dbf)] <- c(name.k)
  
  for (j in 1:nrow(dbf)) {
    # j <- 1
    if( is.na(dbf[j,ncol(dbf)]) ){dbf[j,ncol(dbf)] <- 0}
  }
}

dbf
dbf$sum <- 0

for (k in 1:nrow(dbf)) {
  dbf$sum[k] <- sum( dbf[k, c(3:ncol(dbf))] )
}

dbf
table(db2$nombre_cientifico_avesdechile)

dbf2 <- dbf[,-c(2, ncol(dbf))]
str(dbf2)
dbf2

dbf3 <- t(dbf2)
name.l <- dbf3[1,]
dbf4 <- dbf3[-1,]

dbf5 <- as.data.frame(dbf4)
row.names(dbf5) <- 1:nrow(dbf5)
colnames(dbf5) <- name.l
dbf5

for (m in 1:ncol(dbf5)) {
  dbf5[,m] <- as.character(dbf5[,m])
  dbf5[,m] <- as.numeric(dbf5[,m])
}

str(dbf5)
dbf5

# fin ---



# riqueza de spp por estacion
num.spp <- specnumber(dbf5) ; num.spp
rich <- as.vector(unlist(num.spp))

# indice de diversidad de Shannon-Wiener (H') por estacion 
diver <- diversity(x=dbf5, index = 'shannon') ; diver
diver.i <- round(diver, 2)
sort(diver)

# data out
db.out0 <- db1[!duplicated(db1$x.coord), ]
db.out1 <- db.out0[,c('n.est', 'x.coord', 'y.coord')]

db.plot2 <- data.frame(estacion=num, riqueza=rich, h=diver.i)
db.plot2

db.out2 <- merge(db.plot2, db.out1, by.x = 'estacion', by.y = 'n.est', x.all = TRUE)
db.out2

# fin ---


# mapa ----
origen <- SpatialPoints(c(db.out2[4],db.out2[5]), proj4string = CRS(utm18))
db.origen <- data.frame(db.out2)
origen.invierno <- SpatialPointsDataFrame(origen, 
                                        data = db.origen, 
                                        match.ID = TRUE)
plot(origen.invierno, pch=16)

















# otonho ----
db.primavera <- subset(db, estacion=='Primavera')
head(db.primavera)
dim(db.primavera)


# indice y riqueza

db1 <- subset(db.primavera, muestreo==muestreo.i)
head(db1)
dim(db1)

db1$c.spp <- as.character(db1$c.spp)
length(unique(db1$c.spp)) # riqueza otonho
table(db1$c.spp)

db1$nombre_cientifico_avesdechile <- as.character(db1$nombre_cientifico_avesdechile)
table(db1$nombre_cientifico_avesdechile)




db2 <- db1[,c('n.est', 'nombre_cientifico_avesdechile')]
sort(unique(db2$n.est))
db2$presencia <- 1
head(db2)
str(db2)

table(db2$nombre_cientifico_avesdechile)
table(db2$n.est)

num <- sort(unique(db2$n.est)) ; num

# de dataframe a matriz ----

for (i in num) {
  out.pre <- subset(db2, n.est==i)
  name.out <- paste('out', i, '<-out.pre', sep = '')
  eval(parse(text=name.out))
}

trans <- function(input){
  num.i <- as.vector(unlist(table(input$nombre_cientifico_avesdechile)))
  name.i <- names(table(input$nombre_cientifico_avesdechile))
  db.i <- data.frame(name=name.i, num=num.i)
  return(db.i)}



db.j <- c()

for (i in num) {
  #i <- 3
  name.k <- paste('out', i, sep = '')
  
  ej <- data.frame(name=names(table(db2$nombre_cientifico_avesdechile)), num=0)
  ej2 <- trans( eval(parse(text=name.k)) )
  
  if(i==1 | i==32){ dbf <- merge(ej, ej2, by='name', all.x = TRUE) }else(
    dbf <- merge(dbf, ej2, by='name', all.x = TRUE))
  names(dbf)[ncol(dbf)] <- c(name.k)
  
  for (j in 1:nrow(dbf)) {
    # j <- 1
    if( is.na(dbf[j,ncol(dbf)]) ){dbf[j,ncol(dbf)] <- 0}
  }
}

dbf
dbf$sum <- 0

for (k in 1:nrow(dbf)) {
  dbf$sum[k] <- sum( dbf[k, c(3:ncol(dbf))] )
}

dbf
table(db2$nombre_cientifico_avesdechile)

dbf2 <- dbf[,-c(2, ncol(dbf))]
str(dbf2)
dbf2

dbf3 <- t(dbf2)
name.l <- dbf3[1,]
dbf4 <- dbf3[-1,]

dbf5 <- as.data.frame(dbf4)
row.names(dbf5) <- 1:nrow(dbf5)
colnames(dbf5) <- name.l
dbf5

for (m in 1:ncol(dbf5)) {
  dbf5[,m] <- as.character(dbf5[,m])
  dbf5[,m] <- as.numeric(dbf5[,m])
}

str(dbf5)
dbf5

# fin ---



# riqueza de spp por estacion
num.spp <- specnumber(dbf5) ; num.spp
rich <- as.vector(unlist(num.spp))

# indice de diversidad de Shannon-Wiener (H') por estacion 
diver <- diversity(x=dbf5, index = 'shannon') ; diver
diver.i <- round(diver, 2)
sort(diver)

# data out
db.out0 <- db1[!duplicated(db1$x.coord), ]
db.out1 <- db.out0[,c('n.est', 'x.coord', 'y.coord')]

db.plot2 <- data.frame(estacion=num, riqueza=rich, h=diver.i)
db.plot2

db.out2 <- merge(db.plot2, db.out1, by.x = 'estacion', by.y = 'n.est', x.all = TRUE)
db.out2

# fin ---


# mapa ----
origen <- SpatialPoints(c(db.out2[4],db.out2[5]), proj4string = CRS(utm18))
db.origen <- data.frame(db.out2)
origen.primavera <- SpatialPointsDataFrame(origen, 
                                        data = db.origen, 
                                        match.ID = TRUE)
plot(origen.primavera, pch=16)









# otonho ----
db.verano <- subset(db, estacion=='Verano')
head(db.verano)
dim(db.verano)


# indice y riqueza

db1 <- subset(db.verano, muestreo==muestreo.i)
head(db1)
dim(db1)

db1$c.spp <- as.character(db1$c.spp)
length(unique(db1$c.spp)) # riqueza otonho
table(db1$c.spp)

db1$nombre_cientifico_avesdechile <- as.character(db1$nombre_cientifico_avesdechile)
table(db1$nombre_cientifico_avesdechile)




db2 <- db1[,c('n.est', 'nombre_cientifico_avesdechile')]
sort(unique(db2$n.est))
db2$presencia <- 1
head(db2)
str(db2)

table(db2$nombre_cientifico_avesdechile)
table(db2$n.est)

num <- sort(unique(db2$n.est)) ; num

# de dataframe a matriz ----

for (i in num) {
  out.pre <- subset(db2, n.est==i)
  name.out <- paste('out', i, '<-out.pre', sep = '')
  eval(parse(text=name.out))
}

trans <- function(input){
  num.i <- as.vector(unlist(table(input$nombre_cientifico_avesdechile)))
  name.i <- names(table(input$nombre_cientifico_avesdechile))
  db.i <- data.frame(name=name.i, num=num.i)
  return(db.i)}



db.j <- c()

for (i in num) {
  #i <- 3
  name.k <- paste('out', i, sep = '')
  
  ej <- data.frame(name=names(table(db2$nombre_cientifico_avesdechile)), num=0)
  ej2 <- trans( eval(parse(text=name.k)) )
  
  if(i==1 | i==32){ dbf <- merge(ej, ej2, by='name', all.x = TRUE) }else(
    dbf <- merge(dbf, ej2, by='name', all.x = TRUE))
  names(dbf)[ncol(dbf)] <- c(name.k)
  
  for (j in 1:nrow(dbf)) {
    # j <- 1
    if( is.na(dbf[j,ncol(dbf)]) ){dbf[j,ncol(dbf)] <- 0}
  }
}

dbf
dbf$sum <- 0

for (k in 1:nrow(dbf)) {
  dbf$sum[k] <- sum( dbf[k, c(3:ncol(dbf))] )
}

dbf
table(db2$nombre_cientifico_avesdechile)

dbf2 <- dbf[,-c(2, ncol(dbf))]
str(dbf2)
dbf2

dbf3 <- t(dbf2)
name.l <- dbf3[1,]
dbf4 <- dbf3[-1,]

dbf5 <- as.data.frame(dbf4)
row.names(dbf5) <- 1:nrow(dbf5)
colnames(dbf5) <- name.l
dbf5

for (m in 1:ncol(dbf5)) {
  dbf5[,m] <- as.character(dbf5[,m])
  dbf5[,m] <- as.numeric(dbf5[,m])
}

str(dbf5)
dbf5

# fin ---



# riqueza de spp por estacion
num.spp <- specnumber(dbf5) ; num.spp
rich <- as.vector(unlist(num.spp))

# indice de diversidad de Shannon-Wiener (H') por estacion 
diver <- diversity(x=dbf5, index = 'shannon') ; diver
diver.i <- round(diver, 2)
sort(diver)

# data out
db.out0 <- db1[!duplicated(db1$x.coord), ]
db.out1 <- db.out0[,c('n.est', 'x.coord', 'y.coord')]

db.plot2 <- data.frame(estacion=num, riqueza=rich, h=diver.i)
db.plot2

db.out2 <- merge(db.plot2, db.out1, by.x = 'estacion', by.y = 'n.est', x.all = TRUE)
db.out2

# fin ---


# mapa ----
origen <- SpatialPoints(c(db.out2[4],db.out2[5]), proj4string = CRS(utm18))
db.origen <- data.frame(db.out2)
origen.verano <- SpatialPointsDataFrame(origen, 
                                        data = db.origen, 
                                        match.ID = TRUE)
plot(origen.verano, pch=16)



setwd('C:/Users/Francisco/Documents/disco_duro_extraible/rucabris/puntos_traks_finales/coberturas/')
lim.geo <- readOGR('.', 'limite_rucamanque_geo')
lim.18s <- spTransform(lim.geo, utm18)

size.circle.black <- 3
dist.to.point <- 0.4

setwd('C:/Users/Francisco/Documents/disco_duro_extraible/rucabris/puntos_traks_finales/plots_paper/')
#pdf('mapa_H_diurno_por_estacion.pdf', height = 7, width = 8)
par(mfrow=c(2,2), oma=c(2,2,2,2), mar=c(0,0,0,0))

plot(lim.18s, axes=TRUE, lty = 2, border = 'gray', xaxt='n')
plot(origen.otonho, add=TRUE, pch=16, cex=size.circle.black)
text(origen.otonho, origen.otonho$estacion, col = 'white', cex=0.6, font=2)
text(origen.otonho, origen.otonho$h, col = 'red', pos=4, cex=0.6, offset=dist.to.point)
mtext('A', side = 3, line = -1, adj=1, font=2)
addscalebar(pos = 'bottomright', label.col = 'black', plotepsg = 32718, style = 'ticks', label.cex = 0.7, widthhint = 0.25, htin = 0.08) 
addnortharrow(pos = "topleft", cols = c("black", "black"), border = 'black', text.col = 'black', scale = 0.5) 


plot(lim.18s, axes=TRUE, lty = 2, border = 'gray', xaxt='n', yaxt='n')
plot(origen.invierno, add=TRUE, pch=16, cex=size.circle.black)
text(origen.invierno, origen.invierno$estacion, col = 'white', cex=0.6, font=2)
text(origen.invierno, origen.invierno$h, col = 'red', pos=4, cex=0.6, offset=dist.to.point)
mtext('B', side = 3, line = -1, adj=1, font=2)
addscalebar(pos = 'bottomright', label.col = 'black', plotepsg = 32718, style = 'ticks', label.cex = 0.7, widthhint = 0.25, htin = 0.08) 
addnortharrow(pos = "topleft", cols = c("black", "black"), border = 'black', text.col = 'black', scale = 0.5) 

plot(lim.18s, axes=TRUE, lty = 2, border = 'gray')
plot(origen.primavera, add=TRUE, pch=16, cex=size.circle.black)
text(origen.primavera, origen.primavera$estacion, col = 'white', cex=0.6, font=2)
text(origen.primavera, origen.primavera$h, col = 'red', pos=4, cex=0.6, offset=dist.to.point)
mtext('C', side = 3, line = -1, adj=1, font=2)
addscalebar(pos = 'bottomright', label.col = 'black', plotepsg = 32718, style = 'ticks', label.cex = 0.7, widthhint = 0.25, htin = 0.08) 
addnortharrow(pos = "topleft", cols = c("black", "black"), border = 'black', text.col = 'black', scale = 0.5) 

plot(lim.18s, axes=TRUE, lty = 2, border = 'gray', yaxt='n')
plot(origen.verano, add=TRUE, pch=16, cex=size.circle.black)
#text(origen.exp, origen.exp$riqueza, col = 'red', pos=1, cex=0.7)
text(origen.verano, origen.verano$estacion, col = 'white', cex=0.6, font=2)
text(origen.verano, origen.verano$h, col = 'red', pos=4, cex=0.6, offset=dist.to.point)
mtext('D', side = 3, line = -1, adj=1, font=2)
addscalebar(pos = 'bottomright', label.col = 'black', plotepsg = 32718, style = 'ticks', label.cex = 0.7, widthhint = 0.25, htin = 0.08) 
addnortharrow(pos = "topleft", cols = c("black", "black"), border = 'black', text.col = 'black', scale = 0.5) 
# legend('bottomright', legend = c('Limite'), lty = 2, 
#        col = c('gray'), bty = 'n', merge = FALSE)
dev.off()

# ---

db.otonho <- origen.otonho@data ; head(db.otonho)
db.otonho$Estacion <- 'Otoño'
db.otonho[order(db.otonho$h),]

db.invierno <- origen.invierno@data ; head(db.invierno)
db.invierno$Estacion <- 'Invierno'
db.invierno[order(db.invierno$h),]

db.primavera <- origen.primavera@data ; head(db.primavera)
db.primavera$Estacion <- 'Primavera'
db.primavera[order(db.primavera$h),]

db.verano <- origen.verano@data ; head(db.verano)
db.verano$Estacion <- 'Verano'
db.verano[order(db.verano$h),]

e1 <- ggplot()+
  geom_line(aes(x=db.otonho$estacion, y=db.otonho$h)) +
  geom_point(aes(x=db.otonho$estacion, y=db.otonho$h)) +
  
  geom_line(aes(x=db.invierno$estacion, y=db.invierno$h), colour="red", size=1) +
  geom_point(aes(x=db.invierno$estacion, y=db.invierno$h)) +

  geom_line(aes(x=db.primavera$estacion, y=db.primavera$h), colour="blue", size=1) +
  geom_point(aes(x=db.primavera$estacion, y=db.primavera$h)) +
  
  geom_line(aes(x=db.verano$estacion, y=db.verano$h), colour="green", size=1) +
  geom_point(aes(x=db.verano$estacion, y=db.verano$h)) +
  
  theme_bw()
e1

db.all <- rbind(db.otonho, db.invierno, db.primavera, db.verano)
db.all$Estacion <- factor(db.all$Estacion, levels = c('Otoño', 'Invierno', 'Primavera', 'Verano'))

min.i <- min(db.all$estacion)
max.i <- max(db.all$estacion)
col.i <- rev(c('#292A29', '#5F5F5F', '#949594', '#CACACA'))

out1 <- ggplot(db.all, aes(x = estacion, y = h, fill = Estacion, label = h)) +
  geom_bar(stat = "identity") +
  geom_text(size = 2, position = position_stack(vjust = 0.5), col = '#FF0000', fontface='bold') +
  scale_fill_manual('Temporada', values = alpha(col.i, alpha=0.6)) +
  scale_x_discrete(name="Estación", limits=c(min.i:max.i)) +
  labs(y = 'H') +
  theme_bw()

out1

setwd('C:/Users/Francisco/Documents/disco_duro_extraible/rucabris/puntos_traks_finales/plots_paper/')
#pdf('barplot_indiceH_diurno.pdf', width = 8.5, height = 5)
out1
#dev.off()
