head(db)
dim(db)

db1 <- subset(db, muestreo=='diurno')
head(db1)
dim(db1)

x.d <- unique(db1$x.coord)
y.d <- unique(db1$y.coord)
plot(x.d, y.d)

for (i in 1:length(x.d)) {
  est <- unique(db1$n.est[db1$x.coord==x.d[i] & db1$y.coord==y.d[i]])
  msj <- paste('coordenada x:', x.d[i], ', coordenada y:', y.d[i], ', es la estacion', est, sep=' ')
  print(msj)
}


db2 <- subset(db, muestreo=='nocturno')
head(db2)
dim(db2)

x.n <- unique(db2$x.coord)
y.n <- unique(db2$y.coord)
plot(x.n, y.n)

for (i in 1:length(x.n)) {
  est <- unique(db2$n.est[db2$x.coord==x.n[i] & db2$y.coord==y.n[i]])
  msj <- paste('coordenada x:', x.n[i], ', coordenada y:', y.n[i], ', es la estacion', est, sep=' ')
  print(msj)
}
