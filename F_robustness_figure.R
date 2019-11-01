##robustness analyses for pgs*ses interactions


load(file="/ifs/sec/cpc/addhealth/users/bdomingu/transcripts/df.Rdata")
df[df$transcript,]->df
df$birthyear-mean(df$birthyear,na.rm=TRUE)->df$birthyear
##
df[!is.na(df$school.hsgrad),]->df
df[!is.na(df$sespc.all),]->df
#TRUE->df$orig
df[df$in.geno,] -> df

by(df$school.hsgrad,df$scid,unique)->dist.hsgrad
ifelse(df$school.hsgrad<median(dist.hsgrad,na.rm=TRUE),0,1)->df$status
df[!is.na(df$status),]->df

#########################################################################
##more flexible
levels(df$math9)->levs
par(mfcol=c(2,length(levs)-1),mar=c(3,3.2,1,1),mgp=c(2.2,1,0),oma=rep(.5,4))
col<-c("orange","blue")
for (i in 1:(length(levs)-1)) {
    top.txt<-paste(levs[i+1]," (versus ",levs[i],")",sep="")
    df[df$math9 %in% levs[i:(i+1)],]->tmp
    ifelse(tmp$math9==levs[i],0,1)->tmp$hi
    ##
    plot(NULL,xlim=c(-3,3),ylim=c(0,1),xlab="EA PGS",ylab="Pr, LOESS",main=top.txt,bty="n",yaxt="n")
    axis(side=2,las=2)
    for (j in 0:1) {
        tmp[tmp$status==j,]->zz
        loess(hi~ea2,zz)->m
        xs<-seq(min(zz$ea2,na.rm=TRUE),max(zz$ea2,na.rm=TRUE),length.out=100)
        predict(m,newdata=data.frame(ea2=xs),se=TRUE)->m
        lines(xs,m$fit,col=col[j+1])
        col2rgb(col[j+1])->cc
        cc<-rgb(cc[1],cc[2],cc[3],max=255,alpha=85)
        polygon(c(xs,rev(xs)),c(m$fit-1.96*m$se.fit,rev(m$fit+1.96*m$se.fit)),col=cc)
    }
    ##
    glm(hi~ea2*school.hsgrad+(ea2+school.hsgrad)*(sex+birthyear),tmp,family="binomial")->m
    fit<-list()
    quantile(dist.hsgrad,c(.25,.75),na.rm=TRUE)->qu
    for (st in qu) {
        data.frame(ea2=seq(-3,3,length.out=100),school.hsgrad=st,sex="female",birthyear=0)->zz
        predict(m,zz,type="response",se.fit=TRUE)->pr
        pr->fit[[as.character(st)]]
    }
    ##
    plot(NULL,xlim=c(-3,3),ylim=c(0,1),xlab="EA PGS",ylab="Pr, Logit",main="",yaxt="n",bty="n")
    axis(side=2,las=2)
    for (j in 1:length(fit)) {
        col2rgb(col[j])->cc
        cc<-rgb(cc[1],cc[2],cc[3],max=255,alpha=85)
        lines(zz$ea2,fit[[j]]$fit,col=col[j],cex=j)
        polygon(c(zz$ea2,rev(zz$ea2)),c(fit[[j]]$fit-1.96*fit[[j]]$se.fit,rev(fit[[j]]$fit+1.96*fit[[j]]$se.fit)),col=cc)
    }
}
