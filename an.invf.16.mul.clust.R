### an.invf.16.mul.clust.R
### Analyse intertidal invertebrate data
### Multivariate analysis - current year

### Load data, define current year & extract data
load(file = "data/processed/R/inf.ts.W.Rdata")

cur.yr <- 2016

cur.dat <- subset(inf.ts.W, year == cur.yr)
rm(cur.yr,inf.ts.W)
### keep only 1 mm data
cur.dat <- subset(cur.dat, method == "1.0mm")
### remove Replicate and Year info
cur.dat <- cur.dat[, -which(names(cur.dat) %in% c("year","rep"))]

### calculate mean values
### ==========================
### remove non-numeric columns
### gets rid of P/A data
nums <- sapply(cur.dat, is.numeric)
tempdat <- cur.dat[, nums]; rm(nums)

### append sample data
#tempdat$rep <- NULL
tempdat$transect <- cur.dat$transect
tempdat$shore <- cur.dat$shore
tempdat$zone1 <- cur.dat$zone1
tempdat$zone2.1 <- cur.dat$zone2.1
tempdat$zone2.2 <- cur.dat$zone2.2
tempdat$year <- cur.dat$year
tempdat$station <-
  as.factor(paste(tempdat$transect, substring(tempdat$shore, 1, 1),sep="."))
### aggregate data
tempdat <-
  aggregate(. ~ transect + station + shore + zone1 + zone2.1 + zone2.2,
            data = tempdat,
            FUN = mean)

### remove non-numeric columns
nums <- sapply(tempdat, is.numeric)
tempdatn <- tempdat[, nums]; rm(nums)

### Remove empty columns
source("./R/summer.R")
tempdatn <- tempdatn[, sapply(tempdatn, summer)]; rm(summer); rm(sumone)

##assign rownames
rownames(tempdatn) <- tempdat$station

### Identify number of significant clusters
require(clustsig)
res <- simprof(data = tempdatn, method.distance = "braycurtis",
               method.transform = "squareroot")

par(mar=c(4,4,0,0))
pl.color <- simprof.plot(res)
summary(res)
res$significantclusters

### Tidy up
rm(cur.dat, tempdat, tempdatn, pl.color)
detach("package:clustsig", unload=TRUE)
