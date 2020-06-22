library('vegan')
library('rich')
library('ggplot2')

setwd('C:/Users/Francisco/Documents/disco_duro_extraible/rucabris/puntos_traks_finales/db/')

# Lectura de datos ----

db <- read.csv('db_final_corregida.csv')
head(db)
dim(db)
str(db)

# fin ---


# Preparacion de datos diurno ----

db1 <- subset(db, muestreo=='diurno')
head(db1)
dim(db1)

db1$c.spp <- as.character(db1$c.spp)
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
  
  if(i==1){ dbf <- merge(ej, ej2, by='name', all.x = TRUE) }else(
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




# Graficos ----

# curva acumulacion de especies

cae1 <- specaccum(dbf5, method = 'random', permutations = 1000)
plot(cae1, xlab='Estaciones de muestreo', ylab='Acumulación de especies', main='Muestreo diurno')

# rango de abundancia por especie
apply(dbf5, 2, range)

#
abun <- as.vector(unlist(table(db2$n.est)))
db.plot1 <- data.frame(var=num, val=abun)

ggplot(db.plot1, aes(var, val)) + geom_bar(stat = "identity") + labs(title = 'Muestreo diurno', 
       x = "Estación de muestreo", y = "Abundancia")# + theme(axis.text.x = element_text(angle = 0, hjust = 0))

# riqueza de spp por estacion
num.spp <- specnumber(dbf5) ; num.spp

rich <- as.vector(unlist(num.spp))
db.plot2 <- data.frame(var=num, val=rich)

ggplot(db.plot2, aes(var, val, fill=var)) + geom_bar(stat = "identity") + labs(title = 'Muestreo diurno', 
       x = "Estación de muestreo", y = "Riqueza") # + theme(axis.text.x = element_text(angle = 0, hjust = 0))

# indice de diversidad de Shannon-Wiener (H') por estacion 
diver <- diversity(x=dbf5, index = 'shannon') ; diver

sort(diver)

# fin ---













rm(list = ls())
dev.off()

# Lectura de datos ----

db <- read.csv('base_de_datos_aves_rucamanque_2.csv')
head(db)
dim(db)
str(db)

# fin ---

# Preparacion de datos nocturno ----

db1 <- subset(db, muestreo=='nocturno')
head(db1)
dim(db1)

db1$c.spp <- as.character(db1$c.spp)
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
  
  if(i==32){ dbf <- merge(ej, ej2, by='name', all.x = TRUE) }else(
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




# Graficos ----

# curva acumulacion de especies

cae1 <- specaccum(dbf5, method = 'random', permutations = 1000)
plot(cae1)

# rango de abundancia por especie
apply(dbf5, 2, range)

#
abun <- as.vector(unlist(table(db2$n.est)))
db.plot1 <- data.frame(var=num, val=abun)

ggplot(db.plot1, aes(var, val)) + geom_bar(stat = "identity") + labs(title = 'Muestreo nocturno', 
                                                                     x = "Estación de muestreo", y = "Abundancia")# + theme(axis.text.x = element_text(angle = 0, hjust = 0))

# riqueza de spp por estacion
num.spp <- specnumber(dbf5) ; num.spp

rich <- as.vector(unlist(num.spp))
db.plot2 <- data.frame(var=num, val=rich)

ggplot(db.plot2, aes(var, val)) + geom_bar(stat = "identity") + labs(title = 'Muestreo nocturno', 
                                                                     x = "Estación de muestreo", y = "Riqueza")# + theme(axis.text.x = element_text(angle = 0, hjust = 0))

# indice de diversidad de Shannon-Wiener (H') por estacion 
diver <- diversity(x=dbf5, index = 'shannon') ; diver

sort(diver)

# fin ---


