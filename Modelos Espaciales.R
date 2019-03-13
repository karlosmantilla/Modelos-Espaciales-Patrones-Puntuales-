
load('Total_nacional(csv)/Cultivos.RData')

str(Cultivos)

Cultivos$P_S6P46<-factor(Cultivos$P_S6P46)

cultivo1<-Cultivos[Cultivos$P_S6P46=='00159201001',]

cultivo1$P_DEPTO<-factor(cultivo1$P_DEPTO)
cultivo1$P_MUNIC<-factor(cultivo1$P_MUNIC)
cultivo1$COD_VEREDA<-factor(cultivo1$COD_VEREDA)

rm(Cultivos)

library(rgdal)

veredas <- readOGR(dsn = 'Mapas', layer = 'VEREDAS_V27')

cultivo1$COD_DPTO<-cultivo1$P_DEPTO; cultivo1$P_DEPTO<-NULL
cultivo.region<-cultivo1[cultivo1$COD_DPTO=='68' | 
                           cultivo1$COD_DPTO=='05' |
                           cultivo1$COD_DPTO=='17' |
                           cultivo1$COD_DPTO=='13',c(3,6,9,23)]

region<-veredas[veredas$COD_DPTO=='68' | 
              veredas$COD_DPTO=='05' |
              veredas$COD_DPTO=='17' |
              veredas$COD_DPTO=='13',]
plot(region)

rm(cultivo1)
region.cultivo<-region[, c(12,2,3)]
rm(region)

tabla<-with(cultivo.region, as.data.frame(table(COD_VEREDA,P_S6P46)))

puntos1<-tabla[tabla$Freq!=0,]
rm(tabla)

region.df<-data.frame(Lon=coordinates(region.cultivo)[,1],
                      Lat=coordinates(region.cultivo)[,2],
                      COD_VEREDA=region.cultivo@data$CODIGO_VER)

df.cultivo<-merge(puntos1,region.df, by = 'COD_VEREDA')

with(df.cultivo,plot(Lon,Lat, pch = 19))

library(sp)

coordinates=as.matrix(df.cultivo[,c(4,5)])

myCRS<- CRS("+proj=utm +zone=17 +datum=WGS84")

mypoints=SpatialPointsDataFrame(coords=coordinates, data=df.cultivo[,c(1:3)], proj4string = myCRS)

plot(mypoints, pch = 20, col = 'red')
plot(region.cultivo, border = 'grey', add = T)

rm(region.cultivo)
departamentos <- readOGR(dsn = 'Mapas', layer = 'DepartamentosVeredas')
depart.cultivo<-departamentos[departamentos$DPTO_CCDGO=='68'|
                                departamentos$DPTO_CCDGO=='05'|
                                departamentos$DPTO_CCDGO=='17'|
                                departamentos$DPTO_CCDGO=='13',]

plot(mypoints, pch = 20, col = 'grey')
plot(depart.cultivo, border = 'red', add = T)

municipios <- readOGR(dsn = 'Mapas', layer = 'MunicipiosVeredas')
munic.cultivo<-municipios[municipios$DPTO_CCDGO=='68'|
                            municipios$DPTO_CCDGO=='05'|
                            municipios$DPTO_CCDGO=='17'|
                            municipios$DPTO_CCDGO=='13',]

plot(mypoints, pch = 20, col = 'grey', axes = T)
plot(munic.cultivo, border = 'red', add = T)
plot(depart.cultivo, border = 'black', add = T)

par(mar=rep(0,4))
plot(mypoints,pch=1,cex=log(mypoints$Freq)/2)
plot(depart.cultivo, border = 'red', add = T)

bubble(mypoints,"Freq")

muestramia<- sample(1:nrow(df.cultivo),size=500,replace=FALSE)
head(muestramia)

data <- df.cultivo[muestramia,]
names(data)
head(data)
dim(data)

sp_point <- matrix(NA, nrow=nrow(data),ncol=2)
sp_point[,1] <- jitter(data$Lon,.001)
sp_point[,2] <- jitter(data$Lat, .001)
colnames(sp_point) <- c("LONG","LAT")

data.sp <- SpatialPointsDataFrame(coords=sp_point,data,proj4string=CRS("+proj=utm +zone=48 +datum=WGS84"))

par(mar=rep(0,4))
plot(data.sp,pch=1,cex=log(data.sp$Freq)/5)

library(gstat)

plot(variogram(log(Freq)~1, locations=coordinates(sp_point), data=data.sp, cloud=T),pch=16, cex=1)

plot(variogram(log(Freq)~1, locations=coordinates(sp_point), data=data.sp, cloud=F),type="b",pch=16)

plot(variogram(log(Freq)~1, locations=coordinates(sp_point), data=data.sp, alpha=c(0,45,90,135),cloud=T),pch=16)

plot(variogram(log(Freq)~1, locations=coordinates(sp_point), data=data.sp, cutoff=.5 ,cloud=F),type="b", pch=16)

v <- variogram(log(Freq) ~ 1, locations=coordinates(sp_point), data.sp)

library(lattice)

print(xyplot(gamma ~ dist, v, ylim=c(0,max(v$gamma)+2*sd(v$gamma)), pch = 3, type = 'b', lwd = 2,
             panel = function(x, y, ...) {
               for (i in 1:100) {
                 data.sp$random <- sample(data.sp$Freq)
                 v <- variogram(log(random) ~ 1, locations=coordinates(data.sp), data.sp)
                 llines(v$dist, v$gamma, col = 'grey')
               }
               panel.xyplot(x, y, ...)
             },
             xlab = 'distance', ylab = 'semivariance'
))

v.fit <- fit.variogram(v, vgm(psill=1, model="Exp", range=1))
plot(v, v.fit, pch = 16,cex=.5)

library(maptools)

grd <- Sobj_SpatialGrid(data.sp,maxDim=200)$SG

plot(grd,axes=T,col="grey")
points(data.sp, pch = 19, col = 'red')

kr <- krige(log(Freq)~1, data.sp, grd, model=v.fit)
spplot(kr,col.regions=rev(terrain.colors(100)), names.attr=c("Prediciones","Varianza"),
       main="Kriging Ordinario,  Cultivos de  Yuca (log)",pch=2,cex=2)

grd <- Sobj_SpatialGrid(data.sp,maxDim=200)$SG

plot(grd,axes=T,col="grey")
points(data.sp, pch = 19, col = 'red')

idw.out <- idw(log(Freq)~1,data.sp,grd,idp=.2)
spplot(idw.out[1],col.regions=rev(heat.colors(100)),
       main="IDW Interpolacion, Cultivos de Yuca (log)",sub="k = 1/5")

# k=1
idw.out <- idw(log(Freq)~1,data.sp,grd,idp=1)
spplot(idw.out[1],col.regions=rev(heat.colors(100)), 
main="IDW Interpolation, Cultivos de Yuca (log)",sub="k = 1")

# k=5
idw.out <- idw(log(Freq)~1,data.sp,grd,idp=5)
spplot(idw.out[1],col.regions=rev(heat.colors(100)), 
main="IDW Interpolation, Cultivos de Yuca (log)",sub="k = 5")
