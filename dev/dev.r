# exploring a sample netCDF file from June 1, 2019.
dmatrix <- get.no2.colvals(2019,'06','01')
length(which(dmatrix < 0))
length(dmatrix)
length(which(dmatrix < 0))/length(dmatrix)
# I thought the data was in moles, but there appears to be over 300,000 values
# less than 0. The plot has weird patterns of very low values, and these may be
# explaining them.

# Playing around with raw data to get various NO2 value counts.
unique(dmatrix[which(dmatrix < 0)])
length(dmatrix)
length(which(dmatrix < 0))
length(which(dmatrix < -4.882633e+16))
length(which(dmatrix < -4.882653e+15))
length(which(dmatrix < -1.26765e+30))
which(dmatrix > 6.1e+16)


# The data is currently in a long vector without any assigned coordinates.
# Luckily, the data is logged in order of what cells are being measured, so we
# can iterate through knowing how many rows and columns are in total
# (1440 rows and 720 columns). These will later be transformed to latitude and
# longitude to join on the labeled dataset.

x_coord <- c()
y_coord <- c()
no2 <- c()

for(y in 1:720) {
  for(x in 1:1440) {
    i = (y-1)*1440 + x
    val <- dmatrix[i]
    x_coord[i] <- x
    y_coord[i] <- y
    no2[i] <- val
  }
}

df <- data.frame("x" = x_coord,
                 "y" = y_coord,
                 "no2" = no2
)

## dev?
df$no2.log <- log((df$no2 - min(df$no2) + 1)/1e+28)

df$no2.log <- log(df$no2)
df$no2.log[is.na(df$no2.log)] <- 0
unique(df$no2.log)
##


n <- count(df)$n

#Create a function to generate a continuous color palette
rbPal <- colorRampPalette(c('white','red'))

#color values based on the no2 level
cols <- rbPal(n)[as.numeric(cut(df$no2.log,breaks = 10))]
unique(cols)
par(mar = c(1, 1, 1, 1))
#plot(df$x, df$y, col=df$no2.log)

# Plotting

varpath <- "HDFEOS/GRIDS/ColumnAmountNO2/Data Fields/ColumnAmountNO2"
datapath <- get.omi.datapath('2020', '03', '01')

r <- raster(x=datapath, var=varpath)#, ncdf=TRUE)
#plot(r)

# https://cmr.earthdata.nasa.gov/search/concepts/C1266136111-GES_DISC.html
# According to NASA's OMI NO2 data page above, the satellite takes an NO2
# measurement of Los Angeles each time it passes by in its orbit. It orbits
# earth about 14 times a day. The weird missing data could be timing issues on
# the satellite sensors, since there appears to be about 13 and some missing
# areas with the same shape.

###################################################################

par(mfrow=c(2,2))
n=10

vals <- get.no2.colvals('2020', '03', '01')
cols <- rbPal(n)[as.numeric(cut(vals,breaks = n))]
#plot(df$x, df$y, col=cols)

datapath <- get.omi.datapath('2020', '03', '01')
r <- raster(x=datapath, var=varpath)#, ncdf=TRUE)
#plot(r)

vals <- get.no2.colvals('2020', '07', '01')
cols <- rbPal(n)[as.numeric(cut(vals,breaks = n))]
#plot(df$x, df$y, col=cols)

datapath <- get.omi.datapath('2020', '07', '01')
r <- raster(x=datapath, var=varpath)#, ncdf=TRUE)
#plot(r)
raster = get.raster('2019', '01', '01')

dev.off()
par(mfrow=c(4,6))
for(y in 2019:2020){
  for(i in 1:12){
    if(i<10){m<-paste('0', i, sep='')} else{m<-i}
    #plot(get.raster(y, m, '01'), axes=FALSE)
  }
}