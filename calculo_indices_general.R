library('vegan')
library('rich')
library('ggplot2')

setwd('C:/Users/Francisco/Documents/disco_duro_extraible/rucabris/puntos_traks_finales/db/')

# Lectura de datos ----

db <- read.csv('base_de_datos_aves_rucamanque_corregido.csv')
head(db)
dim(db)
str(db)



# fin ---

# Muestreo diurno ----

db1 <- subset(db, muestreo=='diurno')
head(db1)
dim(db1)

plot(unique(db1$x.coord), unique(db1$y.coord), col='white')
text(unique(db1$x.coord), unique(db1$y.coord), unique(db1$n.est))

db1$nombre_comun <- as.character(db1$nombre_comun)
table(db1$nombre_comun)
length(unique(db1$nombre_comun)) # Riqueza

db1$nombre_cientifico_avesdechile <- as.character(db1$nombre_cientifico_avesdechile)
table(db1$nombre_cientifico_avesdechile)
length(unique(db1$nombre_cientifico_avesdechile)) # Riqueza


# Grafico 

db1$familia <- as.character(db1$familia)
name.fam <- names(table(db1$familia))
num.fam <- as.vector(unlist(table(db1$familia)))
db.plot1 <- data.frame(var=name.fam, val=num.fam) ; db.plot1

ggplot(db.plot1, aes(reorder(var, -val), val)) + geom_bar(stat = "identity") + labs(title = 'Muestreo diurno', x = "Familia", 
       y = "Observaciones") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# fin ---





# Muestreo nocturno ----

db1.2 <- subset(db, muestreo=='nocturno')
head(db1.2)
dim(db1.2)

db1.2$c.spp <- as.character(db1.2$c.spp)
table(db1.2$c.spp)
length(unique(db1.2$c.spp)) # Riqueza

db1.2$nombre_cientifico_avesdechile <- as.character(db1.2$nombre_cientifico_avesdechile)
unique(db1.2$nombre_cientifico_avesdechile)
length(unique(db1.2$nombre_cientifico_avesdechile)) # Riqueza


# grafico 

db1.2$familia <- as.character(db1.2$familia)
name.fam <- names(table(db1.2$familia))
num.fam <- as.vector(unlist(table(db1.2$familia)))
db.plot1 <- data.frame(var=name.fam, val=num.fam) ; db.plot1

ggplot(db.plot1, aes(reorder(var, -val), val)) + geom_bar(stat = "identity") + labs(title = 'Muestreo nocturno', x = "Familia", 
       y = "Observaciones") + theme(axis.text.x = element_text(angle = 45, hjust = 1))

# fin ---
