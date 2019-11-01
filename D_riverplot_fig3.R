load(file="/ifs/sec/cpc/addhealth/users/bdomingu/transcripts/df.Rdata")
df[df$transcript & df$in.geno,]->df

for (i in 1:4) {
    df[[paste("eamsq",i,sep="")]]->z
    ifelse(z %in% 1:3,1,z)->z
    ifelse(z>1,z-2,z)->z
    z->df[[paste("eamsq",i,sep="")]]
}

ifelse(df$h4ed2>13,NA,df$h4ed2)->tmp
ifelse(tmp %in% 1:2,1,NA)->df$eamsq5
ifelse(tmp %in% 3,2,df$eamsq5)->df$eamsq5
ifelse(tmp %in% 4,3,df$eamsq5)->df$eamsq5
ifelse(tmp %in% 5,4,df$eamsq5)->df$eamsq5
ifelse(tmp %in% 6,5,df$eamsq5)->df$eamsq5
ifelse(tmp %in% 7,6,df$eamsq5)->df$eamsq5
ifelse(tmp %in% 8:9,7,df$eamsq5)->df$eamsq5
ifelse(tmp>9,8,df$eamsq5)->df$eamsq5
c("Dropout","HS Grad","Some Vocational","Vocational","Some College","College","Masters","Masters +")->post.nms
post.nms[df$eamsq5]->df$eamsq5.name
table(df$h4ed2,df$eamsq5.name)



##river plot
riv<-function(df,N=NULL,thr=.01) {
    df[,paste("eamsq",1:5,sep="")]->tmp
    apply(tmp,2,function(x) table(x)/length(x))
    ##tmp[tmp[,1]==3,]->tmp
    tmp->tmp.orig
    for (i in 1:ncol(tmp)) paste(tmp[,i],i,sep=".")->tmp[,i]
    apply(tmp,2,unique)->nms
    nodes<-list()
    for (j in 1:5) for (i in sort(unique(tmp.orig[,j])))  paste(i,j,sep=".")->nodes[[paste(i,j)]]
    as.character(unlist(nodes))->nodes
    if (is.null(N)) nrow(tmp)->N
    pgs.edges<-col.edges<-edges<-list()
    for (i in 1:4) {
        paste(0:7,i,sep=".")->nms
        for (nm in nms) {
            L.tmp<-list()
            pgs.tmp<-list()
            tmp[,i]==nm->index
            tmp[index,]->foo
            table(foo[,i+1])/(N)->foo
            ifelse(foo<thr,0,foo)->foo
            if (length(foo)>0) {
                for (j in 1:length(foo)) {
                    as.numeric(foo[j])->L.tmp[[names(foo)[j]]]
                    tmp[i+1]==names(foo)[j] -> index2
                    df[index & index2,]->zz
                    mean(zz$ea2,na.rm=TRUE)->M
                    M->pgs.tmp[[names(foo)[j] ]]
                }
            }
            L.tmp->edges[[nm]]
            pgs.tmp->pgs.edges[[nm]]
        }
    }
    ##get pgs mean for nodes
    pgs.nodes<-list()
    for (i in 1:5) {
        paste("eamsq",i,sep="")->nm2
        unique(tmp[[nm2]])->nms
        for (nm in nms) {
            tmp[[nm2]]==nm -> index
            mean(df$ea2[index],na.rm=TRUE)->m
            m->pgs.nodes[nm]
        }
    }
    list(edges=edges,nodes=nodes,pgs.edges=pgs.edges,pgs.nodes=pgs.nodes)
}

##boot
bootstr<-FALSE
if (bootstr) {
    ff<-function(zz) {
        zz$edges->edg
        unlist(edg)->edg
        zz$pgs.edges->pg
        unlist(pg)->pg
        edg>0 -> test
        edg[test]->edg
        pg[test]->pg
        wtd.var(pg,edg)
    }
    library(Hmisc)
    out<-wv<-list()
    for (s in c("male","female")) {
        ts<-numeric()
        df[df$sex==s,]->tmp
        riv(tmp)->foo
        ff(foo)->wv[[s]]
        for (i in 1:100) {
            df[df$sex==s,]->tmp
            sample(1:nrow(tmp),nrow(tmp),replace=TRUE)->index
            tmp[index,]->tmp
            riv(tmp)->tmp
            ff(tmp)->ts[i]
        }
        ts->out[[s]]
    }
}


getobj<-function(df,N=NULL) {
    riv(df,N=N)->zz
    for (nm in names(zz)) assign(nm,zz[[nm]])
    ##get colors
    if (length(unique(df$sex))==1) c(-2,2)->ran else range(unlist(pgs.edges)[unlist(edges)>0])->ran
    print(ran)
    pgs.edges->col.edges
    colorRamp(c("orange", "blue"))->col.out
    for (i in 1:length(pgs.edges)) {
        for (j in 1:length(pgs.edges[[i]])) {
            pgs.edges[[i]][[j]]->m
            if (m>=ran[1] & m<=ran[2]) {
                (m-ran[1])/(ran[2]-ran[1])->m ##put M on [0,1] based on pgs mean being between -1 and 1
                col.out(m)->col
                rgb(col[1],col[2],col[3],max=255,alpha=200)->col.edges[[i]][[j]]
            } else NA->col.edges[[i]][[j]]
        }
    }
    L<-list()
    for (i in 1:length(col.edges)) {
        for (j in 1:length(col.edges[[i]])) {
            paste(names(col.edges)[i],"->",names(col.edges[[i]])[j],sep="")->nm
            list(edgecol="col",col=col.edges[[i]][[j]])->L[[nm]]
        }
    }
    L->col.edges
    ##
    list()->col.nodes
    for (i in 1:length(pgs.nodes)) {
        names(pgs.nodes)[i]->nm
        pgs.nodes[[i]]->m
        if (m<ran[1]) ran[1]->m
        if (m>ran[2]) ran[2]->m
        (m-ran[1])/(ran[2]-ran[1])->m ##put M on [0,1] based on pgs mean being between -1 and 1
        col.out(m)->col
        rgb(col[1],col[2],col[3],max=255,alpha=200)->col.nodes[[nm]]$col
    }
    ##    
    c(nodes,"10.1","10.2","10.3","10.4","10.5")->nodes
    edges$'10.1'$'10.2'<-.8
    edges$'10.2'$'10.3'<-.8
    edges$'10.3'$'10.4'<-.8
    edges$'10.4'$'10.5'<-.8
    strsplit(nodes,".",fixed=TRUE)->txt
    sapply(txt,"[",1)->xx
    as.numeric(xx)->xx
    sapply(txt,"[",2)->yy
    as.numeric(yy)->yy
    ##
    library(riverplot)
                                        #rep(c("None","Basic","General","Pre-Algebra","Algebra 1","Geometry","Algebra 2","Adv. Math","Pre-Calculus","Calculus"),4)->nms
    rep(c("None","< Algebra","Algebra 1","Geometry","Algebra 2","Adv. Math","Pre-Calculus","Calculus"),4)->nms
    c(nms,post.nms,"a","a","a","a","a")->nms
    nodes->names(nms)
    r <- makeRiver( nodes, edges,
                   node_xpos= yy,
                   node_ypos=xx+1,
                   node_labels=nms,
                   node_styles=col.nodes,
                   edge_styles=col.edges
                                        #node_labels= c( A= "Node A", B= "Node B", C= "Node C" ),
                                        #node_styles= list( A= list( col= "yellow" ))
                   )
    list(r,zz)
}

getobj(df)->tmp
#[1] -0.64  0.91
tmp[[1]]->r
## getobj(df[df$sex=="male",],N=nrow(df))->tmp
## tmp[[1]]->rm
## tmp[[2]]->zzm
## getobj(df[df$sex=="female",],N=nrow(df))->tmp
## tmp[[1]]->rf
## tmp[[2]]->zzf

## par(mfrow=c(1,2),mgp=c(2,1,0),mar=c(3,3,1,1))
## unlist(zzm$pgs.nodes)->nm
## unlist(zzf$pgs.nodes)->nf
## match(names(nm),names(nf))->index
## cbind(nm,nf[index])->tmp
## plot(tmp); abline(0,1)
## unlist(zzm$pgs.edges)->em
## unlist(zzf$pgs.edges)->ef
## match(names(em),names(ef))->index
## cbind(em,ef[index])->tmp
## plot(tmp); abline(0,1)



    
#########################################################
##on local machine
library(riverplot)

#pdf("/tmp/riv.pdf",width=10.8,height=8)
plot( r,
     plot_area=1,mar=rep(3,4),cex.txt=3.5,srt=0
    #,fix.pdf=TRUE
     )
text(0.05,-.095,"Grade 9",xpd=NA)
text(0.275,-.095,"Grade 10",xpd=NA)
text(0.5,-.095,"Grade 11",xpd=NA)
text(0.725,-.095,"Grade 12",xpd=NA)
text(0.95,-.095,"After HS",xpd=NA)
segments(0,-.05,1,-.05,xpd=NA)
#dev.off()

colorRamp(c("orange", "blue"))->col.out
ran<-c(-.65,.91)
seq(0,1,length.out=100)->mm
col<-list()
for (m in mm) {
    col.out(m)->tmp
    col[[as.character(m)]]<-rgb(tmp[1],tmp[2],tmp[3],max=255,alpha=200)
}
par(mgp=c(1,.2,0),tck=-.02)
plot(seq(ran[1],ran[2],length.out=100),rep(0,length(mm)),col=unlist(col),pch=19,cex=2,bty="n",yaxt="n",xaxt="n",xlim=c(-.75,1),ylim=c(0,1),xlab="PGS Color Codes",cex.lab=.7)
axis(side=1,at=seq(-.75,1,by=.25),cex.axis=.7)
     

## par(mfrow=c(1,2),mar=c(3,1,1,1),mgp=c(2,1,0))
## plot( rm,
##      plot_area=1,mar=rep(2,4),cex.txt=3.5,srt=0
##      )
## mtext(side=1,line=1,"males")
## ##
## plot( rf,
##      plot_area=1,mar=rep(2,4),cex.txt=3.5,srt=0
##      )
## mtext(side=1,line=1,"females")
