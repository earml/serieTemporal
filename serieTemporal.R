#
  
  ## Read data.
  co2.dat<-read.table("K://Elisalvo//Microdados//serieEmprego.txt",h=T)
  fname = co2.dat
  m = matrix(scan(fname,skip=2),ncol=4,byrow=T)
  
  ## Perform linear fit to detrend the data.
  fit = lm(m[,1] ~ m[,2])
  
  ## Save residuals from fit as a time series object.
  q = ts(fit$residuals,start=c(1974,5),frequency=12)
  
  ## Generate the seasonal subseries plot.
  par(mfrow=c(1,1))
  monthplot(q,phase=cycle(q), base=mean, ylab="CO2 Concentrations",
            main="Seasonal Subseries Plot of CO2 Concentrations",
            xlab="Month",
            labels=c("Jan","Feb","Mar","Apr","May","Jun",
                     "Jul","Aug","Sep","Oct","Nov","Dec"))
  
  fname$Mes<-as.factor(fname$Mes)
  boxplot(fname$Mes~fname$Sergipe,data=fname, main="Car Milage Data", 
          xlab="Number of Cylinders", ylab="Miles Per Gallon")
  
  boxplot(fname)
  
  # Create a vector named "new_order" containing the desired order, and ask to boxplot using this vector !
  new_order <- with(fname, reorder(fname$Mes , fname$Sergipe, mean , na.rm=T))
  
  # And draw the boxplot
  boxplot(fname$Sergipe ~ new_order , ylab="sickness" , col="darkgreen", boxwex=0.4 , main="")
  
  monthplot(fname$Sergipe, ylab= "$ million" , xlab= "Month", xaxt= "n", main= "Seasonal deviation plot: antidiabetic drug sales")
  
  plot.ts(fname$Sergipe)
  
  library(stats)
  beer <- window(fname)
  plot(beer)
  # Autocorrelation and seasonality
  seasonplot(beer,year.labels=TRUE)
  monthplot(beer)
  lag.plot(fname$Sergipe)
  Acf(fname$Sergipe)
  
  
  