
load(file="/ifs/sec/cpc/addhealth/users/bdomingu/transcripts/df.Rdata")
##df[!is.na(df$ea2),]->df
df[df$transcript & df$in.geno,]->df
table(df$math9)
by(df$school.hsgrad,df$scid,unique)->dist.hsgrad
df$birthyear-mean(df$birthyear,na.rm=TRUE)->df$birthyear

ifelse(df$math9 %in% c("basic","pre-algebra"),"remedial",as.character(df$math9))->df$math9b
factor(df$math9b,levels=c("remedial","algebra1","higher than algebra1"),ordered=TRUE)->df$math9b
se<-function(x) sd(x,na.rm=TRUE)/sqrt(sum(!is.na(x)))



library(gplots)
par(mfrow=c(2,1),mgp=c(2,1,0),oma=rep(.5,4))
par(mar=c(3,11,2.5,1))
ci<-M<-list()
by(df$ea2,df$math9,mean)->M
by(df$ea2,df$math9,se)->ci
barplot2(M,horiz=TRUE,xaxt="n",
         beside=TRUE,
         plot.ci=TRUE,ci.l=M-1.96*ci,ci.u=M+1.96*ci,
         xlim=c(-.6,.6),las=2,xlab="Mean Education Polygenic Score",
         col=c("lightblue")
         )
axis(side=1)
abline(v=0,col="black")
mtext(side=3,line=0,"Tracking (Grade 9 Math)")
mtext(side=3,line=1,"A",adj=0)
##
ci<-M<-list()
by(df$ea2,df$moves.up,mean)->M
by(df$ea2,df$moves.up,se)->ci
barplot2(M,horiz=TRUE,xaxt="n",
         beside=TRUE,
         plot.ci=TRUE,ci.l=M-1.96*ci,ci.u=M+1.96*ci,
         xlim=c(-.6,.6),las=2,xlab="Mean Education Polygenic Score",
         col=c("lightblue")
         )
axis(side=1)
abline(v=0,col="black")
mtext(side=3,line=0,"Persistence (# Advancing Steps)")
mtext(side=3,line=1,"B",adj=0)
