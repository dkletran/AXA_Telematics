source("loadrawdata.R");

drivers <- get_drivers();
##############Feature construction
tmp_features <- mclapply(c(1:length(drivers)),FUN=function(k){
  cat(paste(k, "\n"));
  tmp <- matrix(nrow=200,ncol=103);
  for(trip in c(1:200)){
    thistrip <- get_trip(drivers[k], trip);
    #thistrip <- rawtrips[[k]][[trip]];
    trajlength <- sum(sqrt(diff(thistrip$x)^2 + diff(thistrip$y)^2));
    tmp[trip,1] <- nrow(thistrip);#1st feature : triplength
    tmp[trip,2] <- trajlength;#2nd feature: trajlength
    #     ts <- c(1:nrow(thistrip));
    #     splinex <- sm.spline(ts, thistrip$x)
    #       spliney <- sm.spline(ts, thistrip$x)
    # 		vx <- predict(splinex, ts, 1);
    # 		vy <- predict(spliney, ts, 1);
    vx <- diff(thistrip$x);
    vy <- diff(thistrip$y);
    v <- sqrt(vx^2+vy^2);
    
    vdis <- floor(20/pi*atan(0.5261538*v)); #O.36*19/13, max v : 130
    vdis[vdis>19] <-19;
    tmp[trip,4:23]<-sapply(c(0:19),FUN=function(x){sum(vdis==x)})
    
    ax <- diff(vx);
    ay <- diff(vy);
    as <- diff(v);
    aa2 <- -as^2 + ax^2+ay^2;
    aa2[aa2<0]<-0;
    aa <- sqrt(aa2);
    asdis <- floor(20/pi*atan(as[-1]+as[-length(as)]));
    #asdis[asdis<-10]<- -10;
    #asdis[asdis>9] <- 9; #max 3m/s2,min-3m/s2
    
    aadis <- floor(40/pi*atan(aa[-1]+aa[-length(aa)]));    #aadis[aadis>9] <-9; #max 3m/s2
    
    diffa<- diff(as);
    diffadis <- floor(20/pi*atan(diffa));
    # diffang[is.na(diffang)] <- 0;
    curv <- aa/v[-1];
    curv <- curv[!is.na(curv)];
    curv <- curv[curv < Inf];
    curvdis <- floor(40/pi*atan(20*curv));

    tmp[trip,24:43] <- sapply(c(-10:9),FUN=function(x){sum(asdis==x)})/tmp[trip,1];
    tmp[trip,44:63] <- sapply(c(0:19),FUN=function(x){sum(aadis==x)})/tmp[trip,1];
    tmp[trip,64:83] <- sapply(c(-10:9),FUN=function(x){sum(diffadis==x)})/tmp[trip,1];
    tmp[trip,84:103] <- sapply(c(0:19),FUN=function(x){sum(curvdis==x)})/tmp[trip,1];
 #   tmp[trip,104:123] <- sapply(c(0:19),FUN=function(x){Mode(vdis1[aadis==x])});
  }
  tmp[,3] <- tmp[,2]/tmp[,1];
  return(tmp)
},mc.cores=11);

features_matrix <- matrix(0,nrow=200*length(drivers),ncol=103)
for(k in c(1:length(drivers))){
  features_matrix[c(1:200)+200*(k-1),] <- tmp_features[[k]];
}

######To locally evaluate this set of features
require(randomForest)
require(pROC);

testauc <- rep(0,1)
N <- 100
nx <- 15;
set.seed(123);
for(i in c(1:1)){
  idnx <- numeric(0);
  for(k in c(1:N)){
    nt <- sample(c(2:nx),1);
    idnx <-(k-1)*200+ c(idnx,sample(c(1:200),nt));
  }
  idnx <- sample(c(1:(N*200)),nx*200);
  testdata <- features_matrix[c(1:(N*200)),]
  target <- rep(1,N*200);
  target[idnx] <-0; 
  
  testdata[idnx,] <- features_matrix[idnxa,];
  scores <- rep(0,N*200);
  
  #testdata <- scale(testdata);
  tmpscores <- mclapply(c(1:N), FUN=function(k){    
    cat(paste(k, "\n"));
    
    idx<-c(1:200)+200*(k-1);
    outidx <- sample(c(1:(N*200))[-idx],200)
    mydata <- as.data.frame(testdata[c(idx,outidx),]);
    
    labels <-as.factor(c(1:400)<=200);
    
    m <- randomForest(mydata, labels,ntree = 1000,sampsize=c("TRUE"=200,"FALSE"=200))
    
#    tmpscores <-predict(m,mydata[1:200,],type='prob')[,"TRUE"]
    tmpscores <- m$votes[1:200,"TRUE"]
    return(tmpscores);
  }, mc.cores=15);
  for(k in c(1:N)){
    idx<-c(1:200)+200*(k-1);
    scores[idx] <- tmpscores[[k]];
  }
  testauc[i] <- auc(target,scores);
}

var(testauc);
mean(testauc);

#############To generate a solution
###parallel version
require(randomForest)
N <- length(drivers)
tmpscores <- mclapply(c(1:N), FUN=function(k){
  cat(paste(k, "\n"));
  idx<-c(1:200)+200*(k-1);
  outidx <- sample(c(1:(N*200))[-idx],200)
  mydata <- as.data.frame(features_matrix[c(idx,outidx),]);
  labels <-as.factor(c(1:400)<=200);
  
  m = randomForest(mydata, labels,ntree = 1400)
  return(round(predict(m,mydata[1:200,],type='prob')[,"TRUE"],5));
  
},mc.cores=6);

###Score to submit
rf_scores<- list();
for(k in c(1:N)){
  rf_scores[[drivers[k]]] = tmpscores[[k]];
}

