load(file="/ifs/sec/cpc/addhealth/users/bdomingu/transcripts/df.Rdata")
df[df$transcript,]->df
df$birthyear-mean(df$birthyear,na.rm=TRUE)->df$birthyear
##
df[!is.na(df$school.hsgrad),]->df
df[!is.na(df$sespc.all),]->df
df$in.geno -> df$orig

by(df$school.hsgrad,df$scid,unique)->dist.hsgrad
ifelse(df$school.hsgrad<median(dist.hsgrad,na.rm=TRUE),0,1)->df$status
#df[!is.na(df$status),]->df
quantile(dist.hsgrad,c(.25,.75),na.rm=TRUE)->qu

data.frame(ea2=seq(-2,2,by=.005),sex="female",birthyear=0,school.hsgrad=NA)->tmp.df
data.frame(ea2=-2:2,sex="female",birthyear=0,school.hsgrad=qu[1])->tmp.df1
data.frame(ea2=-2:2,sex="female",birthyear=0,school.hsgrad=qu[2])->tmp.df2
data.frame(rbind(tmp.df1,tmp.df2))->tmp.df.vals
fp<-list() #kph requested values

#############################################################################################
##tracking
library(ordinal)
clm("math9~ea2*school.hsgrad+(ea2+school.hsgrad)*(sex+birthyear)",data=df[df$orig,])->m
m

predict(m,tmp.df.vals,type="prob")->pr
data.frame(tmp.df.vals,pr)->fp$tracking

fit<-list()
for (st in qu) {
    st->tmp.df$school.hsgrad
    predict(m,tmp.df,type="prob",interval=TRUE)->pr
    pr->fit[[as.character(st)]]
}
fit->fit.track

#In order to have the same probability of being placed in Geometry as a student with an education-PGS of +1 in a high-status school, a student in a low-status school would need to have an education-PGS of 2.09 .
cbind(tmp.df$ea2,fit[[2]]$fit)->tab1
tab1[tab1[,1]==1,5]->pr.keep1
cbind(tmp.df$ea2,fit[[1]]$fit)->tab2
which.min(abs(tab2[,5]-pr.keep1))->ii
tab2[ii,]


#########################################################################
##persistence
glm("moves.up~ea2*school.hsgrad+(ea2+school.hsgrad)*(sex+birthyear)",data=df[df$orig,],family="poisson")->m
predict(m,data.frame(ea2=c(-2,2),school.hsgrad=mean(foo$school.hsgrad,na.rm=TRUE),sex='female',birthyear=mean(foo$birthyear,na.rm=TRUE)),type="response")
summary(m)
predict(m,tmp.df.vals,type="response")->pr
data.frame(tmp.df.vals,pr)->fp$persistence

for (st in qu) {
    st->tmp.df$school.hsgrad
    predict(m,tmp.df,type="response",se.fit=TRUE)->pr
    pr->fit[[as.character(st)]]
}
fit->fit.pers

#In order to have the same probability of being placed in Geometry as a student with an education-PGS of +1 in a high-status school, a student in a low-status school would need to have an education-PGS of 2.09 .
cbind(tmp.df$ea2,fit[[1]]$fit)->tab1
tab1[tab1[,1]==0,2]->pr.keep2
cbind(tmp.df$ea2,fit[[2]]$fit)->tab2
which.min(abs(tab2[,2]-pr.keep2))->ii
tab2[ii,]

#########################################################################
##last math class
ifelse(df$math12=="calc",1,0)->df$calc
ifelse(df$math12=="less than algebra2",0,1)->df$alg2
m<-list()
glm("alg2~ea2*school.hsgrad+(ea2+school.hsgrad)*(sex+birthyear)",data=df[df$orig,],family="binomial")->m[[1]]
predict(m[[1]],tmp.df.vals,type="response")->pr
data.frame(tmp.df.vals,pr)->fp$alg2
glm("calc~ea2*school.hsgrad+(ea2+school.hsgrad)*(sex+birthyear)",data=df[df$orig,],family="binomial")->m[[2]]
predict(m[[2]],tmp.df.vals,type="response")->pr
data.frame(tmp.df.vals,pr)->fp$calc

fit.end<-list()
for (i in 1:length(m)) {
    for (st in qu) {
        st->tmp.df$school.hsgrad
        predict(m[[i]],tmp.df,type="response",se.fit=TRUE)->pr
        pr->fit[[as.character(st)]]
    }
    fit->fit.end[[i]]
}



#########################################################################
##figure, main
par(mfrow=c(1,2),mar=c(3.5,3.5,1,1),mgp=c(2.5,1,0),oma=rep(.5,4))
##
fit.track->fit
levels(df$math9)->nms
col<-c("orange","blue")
plot(NULL,xlim=c(-2,2),ylim=c(0,.5),xlab="Percentiles, Distribution of Education Polygenic Score",ylab="Probability of Geometry or Above in Grade 9",main="Tracking",yaxt="n",bty="n",xaxt="n")
axis(side=1,at=-2:2,labels=round(pnorm(-2:2)*100))
axis(side=2,las=2)
for (j in 1:length(fit)) {
    col2rgb(col[j])->cc
    cc<-rgb(cc[1],cc[2],cc[3],max=255,alpha=85)
    lines(tmp.df$ea2,fit[[j]]$fit[,4],col=col[j],cex=j) #4=geom or higher
    #polygon(c(tmp.df$ea2,rev(tmp.df$ea2)),c(fit[[j]]$lwr[,4],rev(fit[[j]]$upr[,4])),col=cc)
    points(fp$tracking$ea2,fp$tracking$fit.higher.than.algebra1,col=ifelse(fp$tracking$school.hsgrad<.5,"orange","blue"),pch=19,cex=1.5)
}
legend("topleft",bty="n",fill=col,c("25th percentile school status","75th percentile school status"))
abline(h=pr.keep1,col="gray",lwd=.7)
##
fit.pers->fit
col<-c("orange","blue")
plot(NULL,xlim=c(-2,2),ylim=c(1,2.5),xlab="Percentiles, Distribution of Education Polygenic Score",ylab="Steps in Math After Grade 9",main="Persistence",yaxt="n",bty="n",xaxt="n")
axis(side=1,at=-2:2,labels=round(pnorm(-2:2)*100))
axis(side=2,las=2)
for (j in 1:length(fit)) {
    col2rgb(col[j])->cc
    cc<-rgb(cc[1],cc[2],cc[3],max=255,alpha=85)
    lines(tmp.df$ea2,fit[[j]]$fit,col=col[j],cex=j) #4=geom or higher
    lw<-fit[[j]]$fit-1.96*fit[[j]]$se.fit
    hi<-fit[[j]]$fit+1.96*fit[[j]]$se.fit
    #polygon(c(tmp.df$ea2,rev(tmp.df$ea2)),c(lw,rev(hi)),col=cc)
    points(fp$persistence$ea2,fp$persistence$pr,col=ifelse(fp$persistence$school.hsgrad<.5,"orange","blue"),pch=19,cex=1.5)
}
abline(h=pr.keep2,col="gray",lwd=.7)


##figure, si
par(mfrow=c(2,2),mar=c(3.5,3.5,1,1),mgp=c(2.5,1,0),oma=rep(.5,4))
##
fit.track->fit
levels(df$math9)->nms
col<-c("orange","blue")
plot(NULL,xlim=c(-2,2),ylim=c(0,.55),xlab="EA PGS",ylab="Probability (Geometry or above)",main="Tracking",yaxt="n",bty="n")
axis(side=2,las=2)
for (j in 1:length(fit)) {
    col2rgb(col[j])->cc
    cc<-rgb(cc[1],cc[2],cc[3],max=255,alpha=85)
    lines(tmp.df$ea2,fit[[j]]$fit[,4],col=col[j],cex=j) #4=geom or higher
    polygon(c(tmp.df$ea2,rev(tmp.df$ea2)),c(fit[[j]]$lwr[,4],rev(fit[[j]]$upr[,4])),col=cc)
}
legend("topleft",bty="n",fill=col,c("Low Status","High Status"))
abline(h=pr.keep,col="gray",lwd=.7)
##
fit.pers->fit
col<-c("orange","blue")
plot(NULL,xlim=c(-2,2),ylim=c(1,2.5),xlab="EA PGS",ylab="Steps",main="Persistence",yaxt="n",bty="n")
axis(side=2,las=2)
for (j in 1:length(fit)) {
    col2rgb(col[j])->cc
    cc<-rgb(cc[1],cc[2],cc[3],max=255,alpha=85)
    lines(tmp.df$ea2,fit[[j]]$fit,col=col[j],cex=j) #4=geom or higher
    lw<-fit[[j]]$fit-1.96*fit[[j]]$se.fit
    hi<-fit[[j]]$fit+1.96*fit[[j]]$se.fit
    polygon(c(tmp.df$ea2,rev(tmp.df$ea2)),c(lw,rev(hi)),col=cc)
}
abline(h=mean(df$moves.up),col="gray",lwd=.7)
##
for (i in 1:2) {
    fit.end[[i]]->fit
    col<-c("orange","blue")
    ifelse(i==1,"High math: at least Alg 2","High math: Calculus")->nm
    plot(NULL,xlim=c(-2,2),ylim=c(0,ifelse(i==1,1,.4)),xlab="EA PGS",ylab="Probability (Geometry or above)",main=nm,yaxt="n",bty="n")
    axis(side=2,las=2)
    for (j in 1:length(fit)) {
        col2rgb(col[j])->cc
        cc<-rgb(cc[1],cc[2],cc[3],max=255,alpha=85)
        lines(tmp.df$ea2,fit[[j]]$fit,col=col[j],cex=j) #4=geom or higher
        lw<-fit[[j]]$fit-1.96*fit[[j]]$se.fit
        hi<-fit[[j]]$fit+1.96*fit[[j]]$se.fit
        polygon(c(tmp.df$ea2,rev(tmp.df$ea2)),c(lw,rev(hi)),col=cc)
    }
    #legend("topleft",bty="n",fill=col,c("Low Status","High Status"))
    #abline(h=pr.keep,col="gray",lwd=.7)
}



