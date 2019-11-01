source("/ifs/sec/cpc/addhealth/users/bdomingu/transcripts/make_tab.R")
load(file="/ifs/sec/cpc/addhealth/users/bdomingu/transcripts/df.Rdata")
##df[!is.na(df$ea2),]->df
df[df$transcript & df$in.geno,]->df



pgsL<-c(EA="ea2"
        #EAhi="eahi",
        #EAlo="ealo"
        #ADHD="adhd")#,
        ## "bmi",
        ## "extro",
        ## "asd",
        ## "menarch",
        ## "neuro"
        )

## by(df$school.hsgrad,df$scid,unique)->dist.hsgrad
## ifelse(df$school.hsgrad<median(dist.hsgrad,na.rm=TRUE),0,1)->df$status
## ifelse(df$status==0,df$ea2,NA)->df$ealo
## ifelse(df$status==1,df$ea2,NA)->df$eahi

#matrix(c(1,1,2,3),2,2,byrow=FALSE)->mat
#matL<-list()
#for (i in 1:length(pgsL)) mat+3*(i-1) -> matL[[i]]
#do.call("rbind",matL)->mat
#layout(mat)
#pdf("~/kph.pdf",width=12,height=20)
#par(mfrow=c(length(pgsL),3),mgp=c(2,1,0),mar=c(3,3,1,1),oma=c(2,5,1,1.5))

#layout(matrix(c(1,1,1,2,2,3),2,3,byrow=TRUE))
par(#mfrow=c(2,1),
    mgp=c(3,1,0),mar=c(4,4,1,1),oma=rep(1,4))

for (iii in 1:length(pgsL)) {
    pgsL[iii]->pgs
    df[[pgs]]->df$pgs
    ##pipeline plots
    out<-list()
    ff<-function(baz) {
        mean(baz$pgs,na.rm=TRUE)->M
        sd(baz$pgs,na.rm=TRUE)->S
        sum(!is.na(baz$pgs))->N
        c(N,M,S/sqrt(N))
    }
    for (i in 1:4) {
        if (i==1) {
            ifelse(df[[paste("eamsq",i,sep="")]]==0,0,1)->in.pipe
            rbind(ff(df[in.pipe==0,]),ff(df[in.pipe==1,]))->out[[i]]
            df[in.pipe==1,]->hold
        } else {
            df[!(df$aid %in% hold$aid),]->tmp
            ff(tmp)->tr1
            ifelse(hold[[paste("eamsq",i,sep="")]]==0,0,1)->in.pipe
            rbind(tr1,ff(hold[in.pipe==0,]),ff(hold[in.pipe==1,]))->out[[i]]
            hold[in.pipe==1,]->hold
        }
    }
    for (i in 1:length(out)) out[[i]][,1]/sum(out[[i]][,1])->out[[i]][,1]
    lapply(out[-1],function(x) x[3,])->x2
    do.call("rbind",x2)->x2
    rbind(out[[1]][2,],x2)->x2
    ##
    lapply(out[-1],function(x) x[2,])->x1
    do.call("rbind",x1)->x1
    rbind(out[[1]][2,],x1)->x1
    grep(pgs,pgsL)->ii
    ##paste("Mean PGS,", names(pgsL)[ii])->ylab
    paste("Mean Education Polygenic Score")->ylab
    plot(NULL,xlim=c(0.5,4.5),ylim=c(-.65,.3),ylab=ylab,xlab="",xaxt="n",las=2)
    axis(side=1,at=1:4,paste("grade",9:12))
    cols<-c("red","black")
    ##
    pc<-function(xv,yv,...) {
        args <- list(...)
        if (args$col=="red") lty<-2 else 1->lty
        points(xv,yv[,2],...,cex=.3+1.8*yv[,1],pch=19,type="b",lty=lty)
        for (i in 1:nrow(yv)) {
            arrows(xv[i],yv[i,2]-1.96*yv[i,3],xv[i],yv[i,2]+1.96*yv[i,3],...,lwd=1,length=0.1,angle=90,lty=1)
            arrows(xv[i],yv[i,2]+1.96*yv[i,3],xv[i],yv[i,2]-1.96*yv[i,3],...,lwd=1,length=0.1,angle=90,lty=1)
        }
    }
    for (i in 2:4) {
        rbind(x2[i-1,],x1[i,])->xx
        pc((i-1):i,xx,col="red")
                                        #text(i,xx[2,2],round(xx[2,1],2),pos=4)
    }
    pc(1:4,x2,col="black")
    legend("topleft",bty="n",lty=c(2,1),c("no math","takes math"))
    #mtext(side=3,adj=0,line=0,LETTERS[iii])
    #mtext(side=4,line=0,pgs)
}
for (i in seq(-1,1,by=.1)) abline(h=i,lwd=.5,lty=3)
     
