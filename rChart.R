library(ggplot2)
library(ggthemes)
library(plyr)
library(reshape2)
library(extrafont)

font_install("fontcm")
font_import()


readpre <-function(fn) {
  dv = read.table(fn, sep = ",",  header = F  )
  dv$sec <- as.numeric(dv$V1)
  return (dv)  
}

data1 = readpre("c:/tmp/3.csv")
data2 = readpre("c:/tmp/4.csv")
data5 = readpre("c:/tmp/5.csv")
data6 = readpre("c:/tmp/thr1.csv")
data7 = readpre("c:/tmp/thr1dyn.csv")
data1$engine='InnoDB'
data2$engine='MyRocks'
data5$engine='InnoDB dyno'

data6$engine='InnoDB'
data7$engine='InnoDB dyno'


data3=rbind(data1, data2)

dv = read.table("c:/tmp/1.csv", sep = ",",  header = F  )

dvr = readpre("c:/tmp/vm4.out")
dvi = readpre("c:/tmp/vm3.out")
dvr$engine='MyRocks'
dvi$engine='InnoDB'


m <- ggplot(data=subset(data3,(V11==100) & (V1>2000)), aes(x=V1,y=V3,color=as.factor(engine)))
m + geom_point(size=1)+geom_line(alpha=0.5)+
  ylab("Throughput, trx / sec")+
  xlab("--- time, sec --->")+
  expand_limits(y=0)+labs(title="InnoDB / MyRocks throughput variation, memory 100GB")+
  scale_colour_tableau(name="Engine")

  +geom_line(size=0.5,alpha=0.25)+
quantile(data1$V3)

m <- ggplot(data=subset(data3, (V1>2000)), aes(x=as.factor(V11),y=V3,color=as.factor(engine)))
m + geom_boxplot()
m + geom_violin()
m + geom_jitter(size=1,alpha=0.5)+
  labs(title="InnoDB / MyRocks throughput jitter")+
  ylab("Throughput, trx / sec")+
  xlab("Memory size, GB")+theme(legend.position="bottom")+scale_colour_tableau(name="Engine")

m + geom_boxplot()+
  labs(title="InnoDB / MyRocks throughput jitter")+
  ylab("Throughput, trx / sec")+
  xlab("Memory size, GB")+theme(legend.position="bottom")+scale_colour_tableau(name="Engine")


# InnoDB dynimized
m <- ggplot(data=subset(rbind(data1, data5), (V1>2000)), aes(x=as.factor(V11),y=V3,color=as.factor(engine)))
m + geom_boxplot()+  labs(title="Throughput vs buffer_pool_size, sysbench tpcc, tables=10 + scale=100")+
  ylab("tps")+
  xlab("innodb_buffer_pool_size, GB")+expand_limits(y=0)

# InnoDB dynimized response time
m <- ggplot(data=subset(rbind(data1, data5), (V1>2000)), aes(x=as.factor(V11),y=V8,color=as.factor(engine)))
m + geom_boxplot()+  labs(title="Throughput vs buffer_pool_size, sysbench tpcc, tables=10 + scale=100")+
  ylab("tps")+
  xlab("innodb_buffer_pool_size, GB")+expand_limits(y=0)

MeansTps <- ddply(subset(rbind(data1, data5), (V1>2000)), .(V11,engine), function(Df) c(x1 = mean(Df$V3)))
colnames(MeansTps)<-c("size","engine","tps")
MeansTps
mr<-dcast(MeansTps,size ~ engine)
mr$ratio = mr$`InnoDB dyno`/mr$InnoDB
mr

qRps <- ddply(subset(rbind(data1, data5), (V1>2000)), .(V11,engine), function(Df) c(x1 = quantile(Df$V8,prob=0.99)))
qRps
quantile(data1$V8,prob=0.99)


m <- ggplot(data=subset(rbind(data1, data5), (V11==100) & (V1>2000)), aes(x=V1,y=V3,color=as.factor(engine)))
m + geom_point(size=1)+geom_line(alpha=0.25)+
  ylab("Throughput, trx / sec")+
  xlab("time, sec")+
  expand_limits(y=0)+labs(title="Throughput variation")+scale_colour_tableau()

quantile(data1$V8,prob=0.99)

# ==============================


m <- ggplot(data=subset(rbind(data6, data7), (V1>2000)), aes(x=as.factor(V2),y=V3,color=as.factor(engine)))
m + geom_boxplot()
m + geom_jitter()

  
m + geom_violin()

m <- ggplot(data=subset(rbind(dvr, dvi),sec>2000), aes(x=as.factor(V19),y=V11/1024,color=as.factor(engine)))
m + geom_jitter()+expand_limits(y=0)
m + geom_boxplot()+expand_limits(y=0)

m <- ggplot(data=subset(rbind(dvr, dvi),sec>2000), aes(x=as.factor(V19),y=V10/1024,color=as.factor(engine)))
m + geom_jitter()+expand_limits(y=0)

m <- ggplot(data=subset(rbind(dvr, dvi),sec>2000), aes(x=as.factor(V19),y=V14,color=as.factor(engine)))
m + geom_jitter()+expand_limits(y=0)


m <- ggplot(data=dvi, aes(x=as.factor(V18),y=V10/1024))
m + geom_jitter()
m <- ggplot(data=dvi, aes(x=as.factor(V18),y=V9/1024))
m + geom_jitter()


MeansWrites <- ddply(subset(rbind(dvr, dvi),sec>2000), .(V19,engine), function(Df) c(x1 = mean(Df$V11)))
MeansWrites

dcast(MeansWrites,V19 ~ engine)

MeansReads <- ddply(subset(rbind(dvr, dvi),sec>2000), .(V19,engine), function(Df) c(x1 = mean(Df$V10)))
MeansReads

dcast(MeansReads,V19 ~ engine)


MeansIO <- ddply(subset(rbind(dvr, dvi),sec>2000), .(V19,engine), function(Df) c(ravg = mean(Df$V10),wavg=mean(Df$V11)))

colnames(MeansIO)<-c("size","engine","rio","wio")
MeansIO

MeansTps <- ddply(subset(data3, (V1>2000)), .(V11,engine), function(Df) c(x1 = mean(Df$V3)))
colnames(MeansTps)<-c("size","engine","tps")
MeansTps
dcast(MeansTps, size ~ engine)

total <- merge(MeansTps, MeansIO, by=c("size","engine"))
total$r_per_tps = total$rio / total$tps
total$w_per_tps = total$wio / total$tps
total
total[order(total$size),]

# writes per transaction
m <- ggplot(data=total, aes(x=as.factor(size),y=w_per_tps,group=engine,color=engine,shape=engine))
m+geom_line(stat="identity",position=position_dodge())
m+geom_line(size=2)+geom_point(size=4)+expand_limits(y=0)+theme_minimal()+scale_colour_tableau()+
  geom_text(aes(label = format(w_per_tps, digits=2, nsmall=2)), color="black", size = 4, hjust = 0.5, vjust=1)+
  labs(title="InnoDB / MyRocks writes per transaction")+
  ylab("Writes, KB / transaction")+
  xlab("Memory size, GB")

# reads per transaction
m <- ggplot(data=total, aes(x=as.factor(size),y=r_per_tps,group=engine,color=engine,shape=engine))
m+geom_line(size=2)+geom_point(size=4)+expand_limits(y=0)+theme_minimal()+scale_colour_tableau()+
  geom_text(aes(label = format(r_per_tps, digits=2, nsmall=2)), color="black", size = 4, hjust = 0.5, vjust=1)+
  labs(title="InnoDB / MyRocks reads per transaction")+
  ylab("Reads, KB / transaction")+
  xlab("Memory size, GB")


MeansCPU <- ddply(subset(rbind(dvr, dvi),sec>2000), .(V19,engine), function(Df) c(us = round(mean(Df$V14)),
                                                                                  sys=round(mean(Df$V15)),
                                                                                  wa=round(mean(Df$V16)),
                                                                                  id=round(mean(Df$V17))))
MeansCPU
dcpu <- melt(data = MeansCPU, id.vars = c("V19","engine"))

ggplot(data=subset(dcpu,engine=="MyRocks"), aes(x=as.factor(V19),y=value,fill=variable))+
  geom_bar(stat="identity",width = 1)+scale_fill_tableau(name="CPU state")+
  labs(title="MyRocks, CPU usage vs memory size")+
  ylab("percentage")+
  xlab("rocksdb_block_cache_size, GB")

ggplot(data=subset(dcpu,engine=="InnoDB"), aes(x=as.factor(V19),y=value,fill=variable))+
  geom_bar(stat="identity",width = 1)+scale_fill_tableau(name="CPU state")+
  labs(title="InnoDB, CPU usage vs memory size")+
  ylab("percentage")+
  xlab("innodb_buffer_pool_size , GB")



