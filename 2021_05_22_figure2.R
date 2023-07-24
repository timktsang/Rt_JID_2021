library(chron)
## get the epidemic curve and growth rate
rm(list = ls())
set.seed(as.numeric(Sys.time()))

setwd("/Users/timtsang/SPH Dropbox/Tim Tsang/_COVID/2019nCoV/rt_method/upload")

today <- as.Date("2020-05-08")

uu <- today - as.Date(dates("01/17/2020"))

data <- read.csv('realdata3/hkcase_20210330.csv',as.is = T)
data$type[data$classification%in%c("Imported case","Possibly local case")] <- 1
data$type[data$classification%in%c("Epidemiologically linked with imported case","Epidemiologically linked with possibly local case")] <- 2
data$type[data$classification%in%c("Local case")] <- 3
data$type[data$classification%in%c("Epidemiologically linked with local case")] <- 4

data$type[data$reclassification%in%c("Imported")] <- 1
data$type[data$reclassification%in%c("Epidemiologically linked with Imported")] <- 2
data$type[data$reclassification%in%c("Local case","Possibly local")] <- 3
data$type[data$reclassification%in%c("Epidemiologically linked with possibly local case","Epidemiologically linked with local case")] <- 4


data$onset.date <- dates(data$onset.date,format="d/m/y")
data$confirm.date <- dates(data$confirm.date,format="d/m/y") 
data$onset.date[is.na(data$onset.date)] <- data$confirm.date[is.na(data$onset.date)]+1
data <- data[data$confirm.date<=today,]
data_org <- data
#data <- data[!data$reclassification%in%c("Imported"),]
data$onset.date <- as.Date(as.character(data$onset.date),"%m/%d/%y")

data$confirm.date <- as.Date(as.character(data$confirm.date),"%m/%d/%y")


yrlimit <- 3.5
onset.date.range <- c(min(data$onset.date, na.rm=T), max(data$onset.date, na.rm=T))

unique.onset.date <- sort(unique(data$onset.date))
n.date <- today-onset.date.range[1]

data.onset <- data.frame(matrix(NA,n.date+1,5))
names(data.onset) <- c("date.onset","case","cum.case","import","cum.import")
data.onset[,1] <- onset.date.range[1]+1:(n.date+1)-1 
for (i in 1:(n.date+1)){
data.onset$case[i] <- sum(data$onset.date==data.onset$date.onset[i]&!data$reclassification%in%c("Imported"),na.rm = T)  
data.onset$import[i] <- sum(data$onset.date==data.onset$date.onset[i]&data$reclassification%in%c("Imported"),na.rm = T)  
data.onset$type1[i] <- sum(data$onset.date==data.onset$date.onset[i]&data$type==1,na.rm = T) 
data.onset$type2[i] <- sum(data$onset.date==data.onset$date.onset[i]&data$type==2,na.rm = T) 
data.onset$type3[i] <- sum(data$onset.date==data.onset$date.onset[i]&data$type==3,na.rm = T) 
data.onset$type4[i] <- sum(data$onset.date==data.onset$date.onset[i]&data$type==4,na.rm = T) 
}

data.onset$cum.case <- cumsum(data.onset$case)
data.onset$cum.import <- cumsum(data.onset$import)

date.lab.day <- gsub("(?<![0-9])0+", "", format(onset.date.range[1]+0:200,'%d'), perl=T)
date.lab.day[1:length(date.lab.day)%%7!=3] <- NA

bar.hwid=0.45

pdf("2021_05_22_figure2.pdf",width=10, height=8)
par(mar=c(5,3,2.5,0), mfrow=c(2,1))

adjset <- 0.04

#######################################################################################################
max.y1=50

bar.hwid=0.45

## here compute the shift
uu2 <- uu+17
startpt <- 24-16

rest <- read.csv("realdata3/combined_200.csv")
totaln <- (nrow(rest)-10)/2
data.onset[10:nrow(data.onset) ,c("rt1","rt1.1","rt1.2")] <- rest[10+10:nrow(data.onset)-6,1:3]
data.onset[10:nrow(data.onset) ,c("rt2","rt2.1","rt2.2")] <- rest[10+totaln+10:nrow(data.onset)-6,1:3]

### equal infectiousness

rest <- read.csv("equalinf/combined_200.csv")
#totaln <- (nrow(rest)-10)/2
#data.onset[10:nrow(data.onset) ,c("rt1","rt1.1","rt1.2")] <- rest[10+10:nrow(data.onset)-6,1:3]
data.onset[10:nrow(data.onset) ,c("rt2_equal","rt2.1_equal","rt2.2_equal")] <- rest[10+10:nrow(data.onset)-6,1:3]


#data.onset[nrow(data.onset)-(totaln-1):0,c("rt2_equal","rt2.1_equal","rt2.2_equal")] <- rest[10+1:totaln,1:3]



### equal mixed
rest <- read.csv("mixed/combined_200.csv")
#totaln <- (nrow(rest)-10)/2


data.onset[10:nrow(data.onset),c("rt2_mixed","rt2.1_mixed","rt2.2_mixed")] <- rest[10+10:nrow(data.onset)-6,1:3]







data.onset[8:14-5,c("rt1","rt1.1","rt1.2","rt2","rt2.1","rt2.2","rt11","rt1.11","rt1.21","rt12","rt1.12","rt1.22")] <- NA
data.onset[8:14-5,c("rt1","rt1.1","rt1.2","rt2","rt2.1","rt2.2","rt21","rt2.11","rt2.21","rt22","rt2.12","rt2.22")] <- NA

yrlimit <- 3.5

data.onset1 <- data.onset[15:(uu-13),]

#dataout <- data.onset[,c("date.onset","rt1","rt1.1","rt1.2","rt2","rt2.1","rt2.2")]
#names(dataout) <- c("data.onset","imported.rt.mean","imported.rt.lower","imported.rt.upper","local.rt.mean","local.rt.lower","local.rt.upper")
#write.csv(dataout,"/Users/timtsang/Dropbox/2019nCoV/Rt_method/program/rt.csv")

plot(NA, xlim=c(onset.date.range[1]-1, today), ylim=c(0,yrlimit*1.00), axes=F, ann=F)
axis(1, at=(onset.date.range[1])+0:200-0.5, lab=NA, pos=0, padj=-0.5)
axis(1, at=(onset.date.range[1])+0:200, lab=date.lab.day, pos=0, padj=-0.5, tick=F,cex.axis=1)
axis(1, at=onset.date.range[1]+c(14,43,74,104)-0.5,labels = NA, pos=0, padj=1.3,tck=-0.2)
axis(1, at=onset.date.range[1]+c(7,28.5,59.5,89,110.5)-1 , lab=c("January","February","March","April","May"), pos=0, padj=1.2, tick=F)
lines(c(onset.date.range[1]-1,onset.date.range[1]), c(0,0))
axis(2, at=0:(yrlimit*2)*0.5, las=1, pos=onset.date.range[1]-1)

lines(c(onset.date.range[1]-1, today),c(1,1),lty=2,col=rgb(0,0,0,0.3))

xvec <- data.onset1[,1]
#lines(xvec,data.onset$rt1,lty=1)
#lines(xvec,data.onset$rt1.1,lty=2)
#lines(xvec,data.onset$rt1.2,lty=2)
#polygon(c(xvec,rev(xvec)), c(data.onset$rt1.1,rev(data.onset$rt1.2) ), col=rgb(0,0,0,0.1), border=F)	

#lines(xvec,data.onset$rt11,lty=2)
#lines(xvec,data.onset$rt12,lty=3)


lines(xvec,data.onset1$rt2,lty=1,col="black")
polygon(c(xvec,rev(xvec)), c(data.onset1$rt2.1,rev(data.onset1$rt2.2) ), col=rgb(0,0,0,0.2), border=F)	
#lines(xvec,data.onset1$rt2.1,lty=2,col="black")
#lines(xvec,data.onset1$rt2.2,lty=2,col="black")

#lines(xvec,data.onset1$rt2_equal,lty=1,col="brown")
#lines(xvec,data.onset1$rt2.1_equal,lty=2,col="brown")
#lines(xvec,data.onset1$rt2.2_equal,lty=2,col="brown")

#lines(xvec,data.onset1$rt21_equal,lty=1,col="red")
#lines(xvec,data.onset1$rt2.11_equal,lty=2,col="red")
#lines(xvec,data.onset1$rt2.21_equal,lty=2,col="red")

lines(xvec,data.onset1$rt2_equal,lty=2,col="blue")
lines(xvec,data.onset1$rt2.1_equal,lty=3,col="blue")
lines(xvec,data.onset1$rt2.2_equal,lty=3,col="blue")



#polygon(c(xvec,rev(xvec)), c(data.onset$rt2.1,rev(data.onset$rt2.2) ), col=rgb(1,0,0,0.1), border=F)	

mtext("Assuming equal infectiousness for imported and local cases", 3, line=1)
mtext('Date', 1, line=2.5)
mtext('Effective reproductive number', 2, line=1)

#legend(onset.date.range[1]+60,3.7,c("Rt of local cases","Rt of imported cases")
#       ,cex=1.1,bty="n",lty=1,col=c("red","black"),border=NA,x.intersp=c(1.5))

legend(onset.date.range[1]+65,3.0,c("Estimates based on our proposed method","Estimates assumed equal infectiousness")
       ,cex=0.9,bty="n",lty=c(1,2),col=c("black","blue"),border=NA,x.intersp=c(1.5))


#polygon( c(rep(onset.date.range[1],2)+21,rep(onset.date.range[1],2)+uu),  c(0,4.5,4.5,0),col=rgb(1,0,0,0.05),border=NA)
polygon( c(rep(onset.date.range[1],2)+11,rep(onset.date.range[1],2)+43),  c(0,4.0,4.0,0),col=rgb(0,0,1,0.1),border=NA)
polygon( c(rep(onset.date.range[1],2)+65,rep(onset.date.range[1],2)+106),  c(0,4.0,4.0,0),col=rgb(0,0,1,0.1),border=NA)
polygon( c(rep(onset.date.range[1],2)+72,rep(onset.date.range[1],2)+uu),  c(0,3.3,3.3,0),col=rgb(0,1,0,0.1),border=NA)

#mtext('14-day mandatory quarantine for arriving persons', 3, line=-1.2,at=onset.date.range[1]+(uu+21)/2,cex=0.8)
#mtext('Civil servants special work arrangements', 3, line=-1.6,at=onset.date.range[1]+(uu+65)/2,cex=0.8)
mtext('Civil servants special work arrangements', 3, line=-1.1,at=onset.date.range[1]+(11+43)/2,cex=0.8)
mtext('Additional social distancing measures', 3, line=-1.9,at=onset.date.range[1]+(72+uu+4)/2,cex=0.8)



title(main="A", adj=adjset )





plot(NA, xlim=c(onset.date.range[1]-1, today), ylim=c(0,yrlimit*1.00), axes=F, ann=F)
axis(1, at=(onset.date.range[1])+0:200-0.5, lab=NA, pos=0, padj=-0.5)
axis(1, at=(onset.date.range[1])+0:200, lab=date.lab.day, pos=0, padj=-0.5, tick=F,cex.axis=1)
axis(1, at=onset.date.range[1]+c(14,43,74,104)-0.5,labels = NA, pos=0, padj=1.3,tck=-0.2)
axis(1, at=onset.date.range[1]+c(7,28.5,59.5,89,110.5)-1 , lab=c("January","February","March","April","May"), pos=0, padj=1.2, tick=F)
lines(c(onset.date.range[1]-1,onset.date.range[1]), c(0,0))
axis(2, at=0:(yrlimit*2)*0.5, las=1, pos=onset.date.range[1]-1)

lines(c(onset.date.range[1]-1, today),c(1,1),lty=2,col=rgb(0,0,0,0.3))

xvec <- data.onset1[,1]
#lines(xvec,data.onset$rt1,lty=1)
#lines(xvec,data.onset$rt1.1,lty=2)
#lines(xvec,data.onset$rt1.2,lty=2)
#polygon(c(xvec,rev(xvec)), c(data.onset$rt1.1,rev(data.onset$rt1.2) ), col=rgb(0,0,0,0.1), border=F)	

#lines(xvec,data.onset$rt11,lty=2)
#lines(xvec,data.onset$rt12,lty=3)


lines(xvec,data.onset1$rt2,lty=1,col="black")
polygon(c(xvec,rev(xvec)), c(data.onset1$rt2.1,rev(data.onset1$rt2.2) ), col=rgb(0,0,0,0.2), border=F)	
#lines(xvec,data.onset1$rt2.1,lty=2,col="black")
#lines(xvec,data.onset1$rt2.2,lty=2,col="black")

#lines(xvec,data.onset1$rt2_mixed,lty=1,col="brown")
#lines(xvec,data.onset1$rt2.1_mixed,lty=2,col="brown")
#lines(xvec,data.onset1$rt2.2_mixed,lty=2,col="brown")

lines(xvec,data.onset1$rt2_mixed,lty=2,col="red")
lines(xvec,data.onset1$rt2.1_mixed,lty=3,col="red")
lines(xvec,data.onset1$rt2.2_mixed,lty=3,col="red")

#lines(xvec,data.onset1$rt22_mixed,lty=1,col="blue")
#lines(xvec,data.onset1$rt2.11_mixed,lty=2,col="blue")
#lines(xvec,data.onset1$rt2.22_mixed,lty=2,col="blue")



#polygon(c(xvec,rev(xvec)), c(data.onset$rt2.1,rev(data.onset$rt2.2) ), col=rgb(1,0,0,0.1), border=F)	

mtext("Assuming all cases were local cases", 3, line=1)
mtext('Date', 1, line=2.5)
mtext('Effective reproductive number', 2, line=1)

#legend(onset.date.range[1]+60,3.7,c("Rt of local cases","Rt of imported cases")
#       ,cex=1.1,bty="n",lty=1,col=c("red","black"),border=NA,x.intersp=c(1.5))

legend(onset.date.range[1]+65,3.0,c("Estimates based on our proposed method","Estimates mixed local or imported cases")
       ,cex=0.9,bty="n",lty=c(1,2),col=c("black","red"),border=NA,x.intersp=c(1.5))

#polygon( c(rep(onset.date.range[1],2)+21,rep(onset.date.range[1],2)+uu),  c(0,4.5,4.5,0),col=rgb(1,0,0,0.05),border=NA)
polygon( c(rep(onset.date.range[1],2)+11,rep(onset.date.range[1],2)+43),  c(0,4.0,4.0,0),col=rgb(0,0,1,0.1),border=NA)
polygon( c(rep(onset.date.range[1],2)+65,rep(onset.date.range[1],2)+106),  c(0,4.0,4.0,0),col=rgb(0,0,1,0.1),border=NA)
polygon( c(rep(onset.date.range[1],2)+72,rep(onset.date.range[1],2)+uu),  c(0,3.3,3.3,0),col=rgb(0,1,0,0.1),border=NA)

#mtext('14-day mandatory quarantine for arriving persons', 3, line=-1.2,at=onset.date.range[1]+(uu+21)/2,cex=0.8)
#mtext('Civil servants special work arrangements', 3, line=-1.6,at=onset.date.range[1]+(uu+65)/2,cex=0.8)
mtext('Civil servants special work arrangements', 3, line=-1.1,at=onset.date.range[1]+(11+43)/2,cex=0.8)
mtext('Additional social distancing measures', 3, line=-1.9,at=onset.date.range[1]+(72+uu+4)/2,cex=0.8)



title(main="B", adj=adjset )



dev.off()

#####



