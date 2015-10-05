## IO & visualization functions
get_drivers <- function(){
	drivers <- list.dirs("drivers", full.names=FALSE);
	drivers <- drivers[drivers !=""];
	return(drivers);
}

get_trip <- function(driver, trip){
	result <- read.csv(paste0("drivers/",driver,"/",trip,".csv"));
	return(result);
}

require(ggplot2);
plottrips <- function(driver,trips){
	p<-ggplot();
	for (i in trips){
		p<- p+geom_point(data=get_trip(driver,i), aes(x=x,y=y),color=i,size=1);
	}
	return(p);

}
plottrips_fft <- function(driver,trips, points=c(1:200)){
	p<-ggplot();
	for (i in trips){
		tripdata <- get_trip(driver,i);
		npoint <- nrow(tripdata);
		datapoint <- complex(2048);
		datapoint[1:npoint] <- complex(real=tripdata$x,imaginary=tripdata$y);
		fftdata <- data.frame(x=points, y=abs(fft(datapoint))[points]);
		p<- p+geom_point(data=fftdata, aes(x=x,y=y),color=i,size=1);
	}
	return(p);

}

write_submitfile <- function(drivers,scores){
	sink(paste0("tosubmit", date(), ".csv", sep=""));
	cat("driver_trip,prob");
	for(driver in drivers){
		for(trip in c(1:200)){
			cat(paste0("\n",driver,"_",trip, ",", scores[[driver]][trip]));
		}

	}
	sink();
}

