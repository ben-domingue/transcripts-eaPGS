##descriptives
load(file="/ifs/sec/cpc/addhealth/users/bdomingu/transcripts/df.Rdata")

by(df$ea2,df$transcript,mean,na.rm=TRUE)
by(df$ea2,df$transcript,sd,na.rm=TRUE)

    


#course-taking
df[df$transcript & df$in.geno,]->df
df[,paste("eamsq",1:4,sep="")]->tmp
apply(tmp,2,table)->tab
round(100*tab/nrow(df),1)->tab
rownames(tab)<-c("No math","Basic/Remedial","General","Pre-algebra","Algebra 1","Geometry","Algebra 2","Adv Math","Pre-calculus","Calculus")

split(df,df$math9)->tmp
f<-function(x) sum(x$eamsqh.name=="calc")/sum(!is.na(x$eamsqh.name))
lapply(tmp,f)







#number of students who dropout of math and then come back
df[,paste("eamsq",1:4,sep="")]->tmp
f<-function(x) {
    which(x==0) -> index
    if (length(index)==0) {
        tr<-FALSE
    } else {
        x[index[1]:4]->y
        any(y>0)->tr
    }
    tr
}
apply(tmp,1,f)->ret



#########################################################################
load(file="/ifs/sec/cpc/addhealth/users/bdomingu/transcripts/df.Rdata")
L<-list(all=df,transcript=df[df$transcript,],geno=df[df$in.geno,],analytic=df[df$transcript & df$in.geno,])

df[df$transcript,]->df
##
df[!is.na(df$school.hsgrad),]->df
df[!is.na(df$sespc.all),]->df
#TRUE->df$orig
df$in.geno -> df$orig
##add pedigree
load(file="/ifs/sec/cpc/addhealth/users/bdomingu/eduPGS/kin_tmp.Rdata")
merge(df,kin.tmp,all.x=TRUE)->df
df[is.na(df$pedigree),]->tmp
df[!is.na(df$pedigree),]->df
1:nrow(tmp)+10000->tmp$pedigree
data.frame(rbind(df,tmp))->df
table(df$pedigree)->tab
df$pedigree %in% names(tab)[tab>1]->df$infam
df[df$infam,]->L$family

desc<-function(df) {
    ifelse(df$sex=="female",1,0)->df$female
    vars<-c("eaogpac","w1_iq","sespc.all","school.hsgrad","female","birthyear")
    tab<-list()
    for (var in vars) {
        df[[var]]->z
        sum(is.na(z))->nna
        mean(z,na.rm=TRUE)->M
        sd(z,na.rm=TRUE)->S
        c(M,S,nna)->tab[[var]]
    }
    tab<-do.call("rbind",tab)
    colnames(tab)<-c("mean","sd","# NA")
    tab
}
lapply(L,desc)->out
lapply(L,nrow)
do.call("cbind",out)->tab
