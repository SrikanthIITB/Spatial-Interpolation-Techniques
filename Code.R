
library(gstat)
library(sp)
# IDW for data given to you. Change the location of the data according to the location in your drive
sample<- read.table('pbcon.txt',header=T)
spat.samp <- sample
summary(sample)
coordinates(spat.samp) <- c('x','y')
# construct a grid of locations to predict at
grid <- expand.grid(x=seq(2.623,3.085,0.002), y=seq(0.853,1.2630,0.002))
spat.grid <- grid
# convert grid to a SpatialPoints object
coordinates(spat.grid) <- c('x','y')
# and tell sp that this is a grid
gridded(spat.grid) <- T
# default power is 2, can change by specifying idp in call to idw
sampinterp1.idw <- idw(Pb~1,spat.samp, spat.grid, idp=1)
spplot(sampinterp1.idw, 'var1.pred')
sampinterp2.idw <- idw(Pb~1,spat.samp, spat.grid, idp=2)
spplot(sampinterp2.idw, 'var1.pred')
sampinterp5.idw <- idw(Pb~1,spat.samp, spat.grid, idp=5)
spplot(sampinterp5.idw, 'var1.pred')
sampinterp10.idw <- idw(Pb~1,spat.samp, spat.grid, idp=10)
spplot(sampinterp10.idw, 'var1.pred')


sample<- read.table('pbcon.txt',header=T)
spat.samp <- sample
summary(sample)
coordinates(spat.samp) <- c('x','y')
# construct a grid of locations to predict at
grid <- expand.grid(x=seq(2.623,3.085,0.01), y=seq(0.853,1.2630,0.01))
spat.grid <- grid
coordinates(spat.grid) <- c('x','y')
# and tell sp that this is a grid
gridded(spat.grid) <- T
sample.lm <- lm(Pb ~ x + y, data=spat.samp)
sample.ts <- predict(sample.lm, newdata=spat.grid)
spat.grid<- SpatialPixelsDataFrame(spat.grid,data.frame(ts=sample.ts))
spplot(spat.grid)


# IDW for data given to you. Change the location of the data according to the location in your drive
sample<- read.table('rain.txt',header=T)
spat.samp <- sample
coordinates(spat.samp) <- c('x','y')
# construct a grid of locations to predict at
grid <- expand.grid(x=seq(-158368,132684,1000), y=seq(-107871,95408,1000))
spat.grid <- grid
# convert grid to a SpatialPoints object
coordinates(spat.grid) <- c('x','y')
# and tell sp that this is a grid
gridded(spat.grid) <- T
# default power is 2, can change by specifying idp in call to idw
sampinterp1.idw <- idw(z~1,spat.samp, spat.grid, idp=1)
spplot(sampinterp1.idw, 'var1.pred')
sampinterp2.idw <- idw(z~1,spat.samp, spat.grid, idp=2)
spplot(sampinterp2.idw, 'var1.pred')
sampinterp5.idw <- idw(z~1,spat.samp, spat.grid, idp=5)
spplot(sampinterp5.idw, 'var1.pred')
sampinterp10.idw <- idw(z~1,spat.samp, spat.grid, idp=10)
spplot(sampinterp10.idw, 'var1.pred')


sample<- read.table('rain.txt',header=T)
spat.samp <- sample
coordinates(spat.samp) <- c('x','y')
# construct a grid of locations to predict at
grid <- expand.grid(x=seq(-158368,132684,1000), y=seq(-107871,95408,1000))
spat.grid <- grid
# convert grid to a SpatialPoints object
coordinates(spat.grid) <- c('x','y')
# and tell sp that this is a grid
gridded(spat.grid) <- T
sample.lm <- lm(z ~ x + y, data=spat.samp)
sample.ts <- predict(sample.lm, newdata=spat.grid)
spat.grid<- SpatialPixelsDataFrame(spat.grid,data.frame(ts=sample.ts))
spplot(spat.grid)


rain<- read.table('rain.txt',header=T)

#Rain
spat.samp <- rain
coordinates(spat.samp) <- c('x','y')
grid <- expand.grid(x=seq(-158368,132684,1000), y=seq(-107871,95408,1000))
spat.grid <- grid
coordinates(spat.grid) <- c('x','y')
gridded(spat.grid) <- T

#Lead
lead<- read.table('pbcon.txt',header=T)
l_spat.samp <- lead
coordinates(l_spat.samp) <- c('x','y')
l_grid <- expand.grid(x=seq(2.623,3.085,0.002), y=seq(0.853,1.2630,0.002))
l_spat.grid <- l_grid
coordinates(l_spat.grid) <- c('x','y')
gridded(l_spat.grid) <- T

variog1 <- variogram(z~1, locations=~x+y,data=rain)
variog2 <- variogram(Pb~1, locations=~x+y,data=lead)

rain_model.variog <- vgm(model="Sph", psill=NA, nugget=NA, range=NA)
fit.variog <- fit.variogram(variog1, rain_model.variog)
fit.variog$psill[2] # sill
fit.variog$range[2] # range
fit.variog$psill[1] # nugget
plot(variog1, model=fit.variog)
Rain_Krig= krige(z~1, spat.samp, spat.grid, model=fit.variog)
plot(Rain_Krig)

rain_model.variog <- vgm(model="Gau", psill=NA, nugget=NA, range=NA)
fit.variog <- fit.variogram(variog1, rain_model.variog)
fit.variog$psill[2] # sill
fit.variog$range[2] # range
fit.variog$psill[1] # nugget
plot(variog1, model=fit.variog)
Rain_Krig= krige(z~1, spat.samp, spat.grid, model=fit.variog)
plot(Rain_Krig)

rain_model.variog <- vgm(model="Exp", psill=NA, nugget=NA, range=NA)
fit.variog <- fit.variogram(variog1, rain_model.variog)
fit.variog$psill[2] # sill
fit.variog$range[2] # range
fit.variog$psill[1] # nugget
plot(variog1, model=fit.variog)
Rain_Krig= krige(z~1, spat.samp, spat.grid, model=fit.variog)
plot(Rain_Krig)

lead_model.variog <- vgm(model="Sph",Cressie=TRUE)
fit.variog <- fit.variogram(variog2, lead_model.variog)
fit.variog$psill[2] # sill
fit.variog$range[2] # range
fit.variog$psill[1] # nugget
plot(variog2, model=fit.variog)
lead_Krig= krige(Pb~1, l_spat.samp, l_spat.grid, model=fit.variog)
plot(lead_Krig)

lead_model.variog <- vgm(model="Gau",Cressie=TRUE)
fit.variog <- fit.variogram(variog2, lead_model.variog)
fit.variog$psill[2] # sill
fit.variog$range[2] # range
fit.variog$psill[1] # nugget
plot(variog2, model=fit.variog)
lead_Krig= krige(Pb~1, l_spat.samp, l_spat.grid, model=fit.variog)
plot(lead_Krig)

lead_model.variog <- vgm(model="Exp",Cressie=TRUE)
fit.variog <- fit.variogram(variog2, lead_model.variog)
fit.variog$psill[2] # sill
fit.variog$range[2] # range
fit.variog$psill[1] # nugget
plot(variog2, model=fit.variog)
lead_Krig= krige(Pb~1, l_spat.samp, l_spat.grid, model=fit.variog)
plot(lead_Krig)

