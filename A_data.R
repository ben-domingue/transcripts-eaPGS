library(foreign)

read.xport("/ifs/sec/cpc/addhealth/addhealthdata/wave1/allwave1.xpt")->w1
w1[,c("AID","AH_PVT","H1GI20","PA12","PA1","SCID","H1GI1Y","COMMID")]->x
names(x)<-c("AID","w1_iq","grade","mom_edu","parent_gender","scid","birthyear","comid")
ifelse(x$parent_gender==1,NA,x$mom_edu)->x$mom_edu
ifelse(x$grade>12,NA,x$grade)->x$grade
ifelse(x$birthyear>83,NA,x$birthyear)->x$birthyear
ifelse(w1$BIO_SEX>2,NA,w1$BIO_SEX)->tmp
ifelse(tmp==1,"male","female")->x$sex

read.xport("/ifs/sec/cpc/addhealth/addhealthdata/wave3/education/edu1.xpt")->x2
x2[,c("AID","EAMSQH","EAOGPAC",paste("EAOGPA",1:4,sep=""),paste("EASGPA",1:4,sep=""),paste("EAMGPA",1:4,sep=""),paste("EAMSQ",1:4,sep=""))]->x2
grep("GPA",names(x2))->index
for (i in index) ifelse(x2[,i]>4,NA,x2[,i])->x2[,i]

#read.xport("/ifs/sec/cpc/addhealth/addhealthdata/wave3/education/edumsov.xpt")->x3

## 0 = No Math; 1 = Basic/Remedial Math; 2 =
## General/Applied Math; 3 = Pre-algebra; 4 = Algebra 1; 5 = Geometry; 6 = Algebra II; 7 =
## Advanced Math (Algebra III, Finite Math, Statistics); 8 = Pre-calculus (includes
## Trigonometry), and 9 = Calculus. 
proc.math<-function(y,nm) {
    y[[nm]]->z
    ifelse(z>9 ,0,z)->z
    nms<-c("none","basic","general","pre-algebra","algebra1","geometry","algebra2","advmath","precalc","calc")
    nms[1+z]->y[[paste(nm,".name",sep="")]]
    z->y[[nm]]
    y
}
for (i in 1:4) proc.math(x2,paste("EAMSQ",i,sep=""))->x2
proc.math(x2,"EAMSQH")->x2

x$AID %in% x2$AID -> x$transcript
merge(x,x2,all=TRUE)->x
table(x$transcript)

names(x)<-tolower(names(x))

load(file="/ifs/sec/cpc/addhealth/users/bdomingu/eduPGS/w1ses.Rdata")
names(ses)<-tolower(names(ses))
ses[,c("aid","sespc.all")]->ses
merge(x,ses)->x
table(x$transcript)
x->df

## read.xport("/ifs/sec/cpc/addhealth/addhealthdata/wave3/education/edupost.xpt")->y
## y[,c("AID","EPRANK","EPSECTOR")]->y
## ifelse(y$EPRANK>20,NA,y$EPRANK)->y$EPRANK
## names(y)<-tolower(names(y))
## merge(x,y,all.x=TRUE)->x

## ##smoothing data
## ifelse(df$epsector>9 | df$epsector<1,NA,df$epsector)->z
## ifelse(z %in% 1:3,"univ",z)->z
## ifelse(z %in% 4:6,"2year",z)->z
## ifelse(z %in% 7:9,"other",z)->z
## z->df$type
## ifelse(!is.na(z),1,0)->df$post

## df[df$epsector %in% 1:3,]->univ
## univ[!is.na(univ$epsector) & !is.na(univ$eprank),]->univ
## max(univ$eprank)-univ$eprank -> univ$eprank2
## (univ$eprank2) %/% 2 -> univ$decile
## univ$decile+1 -> univ$decile
## univ[,c("aid","decile")]->univ
## merge(df,univ,all.x=TRUE)->df

read.xport("/ifs/sec/cpc/addhealth/addhealthdata/wave3/wave3.xpt")->w3
w3[,c("AID","H3ED16","H3ED17")]->w3
ifelse(w3[,2] %in% c("9999997","9999996","9999998","9999999"),NA,w3[,2])->w3[,2]
ifelse(w3[,3] %in% c("9999997","9999996","9999998","9999999"),NA,w3[,3])->w3[,3]
ff<-function(x) {
    strsplit(x,".",fixed=TRUE)->tmp
    sapply(tmp,"[",1)->tmp
    ifelse(tmp %in% c(3, 4, 11, 14, 15, 26, 27, 29,30, 40, 41, 42),1,0)
}
ff(w3[,2])->w3[,2]
ff(w3[,3])->w3[,3]
w3[,2:3]->tmp
rowSums(tmp,na.rm=TRUE)->rs
data.frame(aid=w3$AID,stem.strict=ifelse(rs>0,1,0))->tmp
merge(df,tmp,all.x=TRUE)->df
##
read.xport("/ifs/sec/cpc/addhealth/addhealthdata/wave3/wave3.xpt")->w3
w3[,c("AID","H3ED16","H3ED17")]->w3
ifelse(w3[,2] %in% c("9999997","9999996","9999998","9999999"),NA,w3[,2])->w3[,2]
ifelse(w3[,3] %in% c("9999997","9999996","9999998","9999999"),NA,w3[,3])->w3[,3]
ff<-function(x) {
    strsplit(x,".",fixed=TRUE)->tmp
    sapply(tmp,"[",1)->tmp
    ifelse(tmp %in% c(3, 4, 11, 14, 15, 26, 27, 29,30, 40, 41, 42,1,45,47,51),1,0)
}
ff(w3[,2])->w3[,2]
ff(w3[,3])->w3[,3]
w3[,2:3]->tmp
rowSums(tmp,na.rm=TRUE)->rs
data.frame(aid=w3$AID,stem.loose=ifelse(rs>0,1,0))->tmp
merge(df,tmp,all.x=TRUE)->df
           
#ifelse(df$post==0,NA,df$stem.loose)->df$stem.loose
#ifelse(df$post==0,NA,df$stem.strict)->df$stem.strict


## ##genetic data
## read.table("/ifs/sec/cpc/addhealth/gwas/final/PCs/eigenStratPCs.txt",header=TRUE)->pcs
## names(pcs)<-c("FID","aid",paste("pc",1:20,sep=""),"rel")
## merge(df,pcs,all.x=TRUE)->df
## ##read.table("/ifs/sec/cpc/addhealth/users/bdomingu/freeze2/gen_euro.txt")->wh
## ##ifelse(df$aid %in% wh[,1],"euro",NA)->df$ah_race
## ##df$ah_race=="euro" & df$rel=="AddHealthUnrel"->df$
## names(df)<-tolower(names(df))
## table(df$aid %in% x$aid)
## #load(file="/cpc/addhealth/users/bdomingu/eduPGS/pgs.Rdata") #see edu_pgs
## pgs.files<-c(ea2="/ifs/sec/cpc/addhealth/users/bdomingu/eduPGS/edu2_nc_f2_nmi", #ea2
##              adhd="/ifs/sec/cpc/addhealth/users/bdomingu/pgs/adhd_nc_f2", #adhd
##              #scz="/ifs/sec/cpc/addhealth/users/bdomingu/pgs/scz_f2", #scz
##              bmi="/ifs/sec/cpc/addhealth/users/bdomingu/pgs/bmi_euro_nc_f2", #bmi
##              asd="/ifs/sec/cpc/addhealth/users/bdomingu/pgs/asd_nc", #asd
##              menarch="/ifs/sec/cpc/addhealth/users/bdomingu/pgs/menarche_nc_f2", #menarche
##              extro="/ifs/sec/cpc/addhealth/users/bdomingu/pgs/extro_nc_f2", #extroversion
##              neuro="/ifs/sec/cpc/addhealth/users/bdomingu/pgs/neuro_nc_f2" #neuro
##              )
## for (i in 1:length(pgs.files)) {
##     #load(file="/ifs/sec/cpc/addhealth/users/bdomingu/eduPGS/pgs_f2_nmi.Rdata")
##     #merge(x,df,all.x=TRUE)->df
##     ###standardizing ea2 in the analytic sample
##     #by(df$ea2,df$transcript,mean,na.rm=TRUE)
##     read.table(paste(pgs.files[i],".profile",sep=""),header=TRUE)->x
##     x[,c(2,6)]->x
##     names(pgs.files)[i]->nm
##     names(x)<-c("aid",nm)
##     merge(df,x,all.x=TRUE)->df
##     ifelse(is.na(df$transcript) | df$ah_race!="euro",NA,df[[nm]])->df[[nm]]
## ##
## }

## fun<-function(pgs,df) {
##     paste(pgs,".unstd",sep="")->nm2
##     df[[pgs]]->df[[nm2]]
##     NULL->df[[pgs]]
##     df[!is.na(df[[nm2]]),]->tmp
##     lm(paste(nm2,"~",paste(paste("pc",1:10,sep=""),collapse="+")),tmp)->mod
##     data.frame(aid=tmp$aid,mod$resid)->tmp
##     names(tmp)[2]<-pgs
##     merge(df,tmp,all.x=TRUE)->df
##     ##
##     (df[[pgs]]-mean(df[[pgs]],na.rm=TRUE))/sd(df[[pgs]],na.rm=TRUE)->df[[pgs]]
##     df
## }
## for (pgs in names(pgs.files)) fun(pgs,df)->df


read.xport("/ifs/sec/cpc/addhealth/addhealthdata/wave3/education/edueng.xpt")->eng
read.xport("/ifs/sec/cpc/addhealth/addhealthdata/wave3/education/eduhis.xpt")->hist
read.xport("/ifs/sec/cpc/addhealth/addhealthdata/wave3/education/edumsov.xpt")->sci
ifelse(eng$EAEACRC>9000,NA,eng$EAEACRC)->eng$eng.credits
eng[,c("AID","eng.credits")]->eng
ifelse(hist$EAHCRC>9000,NA,hist$EAHCRC)->hist$hist.credits
hist[,c("AID","hist.credits")]->hist
ifelse(sci$EAMCRC>9000,NA,sci$EAMCRC)->sci$math.credits
ifelse(sci$EASCRC>9000,NA,sci$EASCRC)->sci$sci.credits
sci[,c("AID","sci.credits","math.credits")]->sci
merge(eng,hist,all=TRUE)->cr
merge(cr,sci,all=TRUE)->cr
names(cr)<-tolower(names(cr))
merge(df,cr,all.x=TRUE)->df


##ea3
read.table("/ifs/sec/cpc/addhealth/gwas/PGS/SSGAC/Lee_et_al_(2018)_PGS_AddHealth.txt",header=TRUE)->sc
names(sc)[2]<-"AID"
sc[!is.na(sc$PGS_EA3_GWAS),]->sc
fun<-function(pgs,df) {
    paste(pgs,".unstd",sep="")->nm2
    df[[pgs]]->df[[nm2]]
    NULL->df[[pgs]]
    df[!is.na(df[[nm2]]),]->tmp
    lm(paste(nm2,"~",paste(paste("PC",1:10,sep=""),collapse="+")),tmp)->mod
    data.frame(AID=tmp$AID,mod$resid)->tmp
    names(tmp)[2]<-pgs
    merge(df,tmp,all.x=TRUE)->df
    ##
    (df[[pgs]]-mean(df[[pgs]],na.rm=TRUE))/sd(df[[pgs]],na.rm=TRUE)->df[[pgs]]
    df
}
fun("PGS_EA3_GWAS",sc)->pgs #the gac-created score
tolower(names(pgs))->names(pgs)
#gsub("pc","gac.pc",names(pgs))->names(pgs)
 "euro"->pgs$ah_race
-1*pgs$pgs_ea3_gwas -> pgs$ea3
pgs[,c("aid","ea3","ah_race")]->pgs #note that i'm mislabelling ea3 as ea2 just so that it works with downstream code
names(pgs)[2]<-"ea2"
NULL->df$ea2
merge(df,pgs,all.x=TRUE)->df

read.xport("/ifs/sec/cpc/addhealth/addhealthdata/wave4/wave4.xpt")->w4
w4[,c("AID","H4ED2")]->tmp
#names(tmp)[2]<-"w4_college"
ifelse(tmp[,2]>90,NA,tmp[,2])->tmp[,2]
#ifelse(tmp[,2]>6,1,0)->tmp[,2]
ifelse(tmp[,2]==1,8,NA)->e4
ifelse(tmp[,2]==2,10,e4)->e4
ifelse(tmp[,2]==3,12,e4)->e4
ifelse(tmp[,2]==4,13,e4)->e4
ifelse(tmp[,2]==5,14,e4)->e4
ifelse(tmp[,2]==6,14,e4)->e4
ifelse(tmp[,2]==7,16,e4)->e4
ifelse(tmp[,2]==8,17,e4)->e4
ifelse(tmp[,2]==9,18,e4)->e4
ifelse(tmp[,2]==10,19,e4)->e4
ifelse(tmp[,2]==11,20,e4)->e4
ifelse(tmp[,2]==12,18,e4)->e4
ifelse(tmp[,2]==13,19,e4)->e4
## ifelse(tmp[,2]==10,20,e4)->e4
## ifelse(tmp[,2]==11,22,e4)->e4
## ifelse(tmp[,2]==12,24,e4)->e4
## ifelse(tmp[,2]==13,26,e4)->e4
table(tmp[,2],e4)
e4->tmp[,2]
names(tmp)[2]<-"w4_edu"
w4$H4ED1 -> grad
ifelse(grad==2,1,0)->tmp$ged
w4$H4ED2 -> tmp$h4ed2
names(tmp)<-tolower(names(tmp))
merge(df,tmp,all.x=TRUE)->df

#table(df$w4_edu>=16,df$post)
#NULL->df$post
ifelse(df$w4_edu>12,1,0)->df$post.loose
w4[,c("AID","H4ED2")]->tmp
ifelse(tmp[,2]>90,NA,tmp[,2])->tmp[,2]
tmp[,2]->zz
ifelse(tmp[,2]>=6,1,0)->tmp[,2]
names(tmp)<-c("aid","post")
ifelse(zz<=3,"hs",NA)->z
ifelse(zz %in% 4:5,"voc",z)->z
ifelse(zz %in% 4:5,"voc",z)->z
ifelse(zz==6,"sc",z)->z
ifelse(zz==7,"cg",z)->z
ifelse(zz>7,"gs",z)->z
z->tmp$post.level
merge(df,tmp,all.x=TRUE)->df

              
##school status
library(foreign)
read.xport("/ifs/sec/cpc/addhealth/addhealthdata/inschool/inschool.xpt")->sch
ifelse(sch$S12>10,NA,sch$S12)->sch$S12
ifelse(sch$S12==10,0,sch$S12)->sch$S12
ifelse(sch$S12==9,NA,sch$S12)->sch$S12
ifelse(sch$S3>12,NA,sch$S3)->sch$S3
split(sch,sch$SSCHLCDE)->tmp
fun<-function(x) {
    f<-function(x) {
        sum(!is.na(x$S12))->N
        if (N>=10) sum(x$S12>4,na.rm=TRUE)/sum(!is.na(x$S12)) else NA
    }
    unique(x$SSCHLCDE)->id
    f(x)->M
    f2<-function(x) {
        library(reldist)
        sum(!is.na(x$S12))->N
        if (N>=10) gini(x$S12[!is.na(x$S12)]) else NA
    }
    f2(x)->g
    data.frame(scid=id,school.hsgrad=M,gini.hsgrad=g)
}
lapply(tmp,fun)->tmp
data.frame(do.call("rbind",tmp))->sch
merge(df,sch,all.x=TRUE)->df


df[,paste("eamsq",1:4,sep="")]->tmp
f<-function(x) {
    as.numeric(x)->x
    ifelse(is.na(x),-1,x)->x
    tr<-0
    if (!all(is.na(x))) {
        for (i in 2:length(x)) {
            tr<-tr+as.numeric(x[i]>max(x[1:(i-1)]))
        }
    }
    tr
}

apply(tmp,1,f)->df$moves.up
##cbind(tmp,rs)


read.table("/ifs/sec/cpc/addhealth/gwas/final/PCs/eigenStratPCs.txt",header=TRUE)->pcs
names(pcs)<-c("FID","aid",paste("pc",1:20,sep=""),"rel")
pcs[,c("aid","rel")]->tmp
tmp[tmp$rel=="AddHealthUnrel",]->tmp
!is.na(df$ea2)->test1
df$aid %in% tmp$aid -> test2
test1 & test2->df$in.geno


df[df$scid!=999,]->df

df->hold
##math9
##ceiling
df$eamsq1>=5 -> index
ifelse(index,"higher than algebra1",df$eamsq1.name)->df$eamsq1.name
ifelse(index,6,df$eamsq1)->df$eamsq1
##floor
df$eamsq1<3->index
ifelse(index,2,df$eamsq1)->df$eamsq1
ifelse(index,"basic",df$eamsq1.name)->df$eamsq1.name
factor(df$eamsq1.name,levels=c("basic","pre-algebra","algebra1","higher than algebra1"),ordered=TRUE)->df$math9
table(df$math9)
##as.factor(as.numeric(df$math9))->df$math9
##table(df$math9)
##
##math12
##ceiling
df$eamsqh==7 -> index
ifelse(index,8,df$eamsqh)->df$eamsqh
ifelse(index,"precalc",df$eamsqh.name)->df$eamsqh.name
##floor
df$eamsqh<=5->index
ifelse(index,5,df$eamsqh)->df$eamsqh
ifelse(index,"basic",df$eamsqh.name)->df$eamsqh.name
ifelse(df$eamsqh.name=="basic","less than algebra2",df$eamsqh.name)->df$eamsqh.name
factor(df$eamsqh.name,levels=c("less than algebra2","algebra2","precalc","calc"),ordered=TRUE)->df$math12
df[,c("aid","math9","math12")]->tmp
merge(hold,tmp,all.x=TRUE)->df

save(df,file="/ifs/sec/cpc/addhealth/users/bdomingu/transcripts/df.Rdata")



## save(df,file="/cpc/addhealth/users/bdomingu/transcripts/df_raw.Rdata")

## ###################################################################################
## df[!is.na(df$ea2),]->df
## #df[!is.na(df$sex) & !is.na(df$ah_race),]->df

## read.table("/cpc/addhealth/users/bdomingu/merged/euro/omni_sample_AID_sex_dupQC_hapmapQC_maf_hwe_1000GI_fwd_het_plateQC_intersection_le_ALL.kin0",header=TRUE)->kin.no
## kin.no[,c("ID1","ID2")]->grm

## nl<-1
## i<-1
## remove.ids<-character()
## while (nl>0) {
##   c(grm[,1],grm[,2])->id
##   table(id)->tab
##   which.max(tab)->index
##   names(tab)[index]->nm
##   nm->remove.ids[i]
##   (grm[,1] %in% nm) | (grm[,2] %in% nm) -> test
##   grm[!test,]->grm
##   nrow(grm)->nl
##   i<-i+1
##   print(nl)
## }

## df$aid %in% remove.ids -> df$unrel


## read.table("/cpc/addhealth/users/bdomingu/merged/euro/omni_sample_AID_sex_dupQC_hapmapQC_maf_hwe_1000GI_fwd_het_plateQC_intersection_le_ALL_three.kin0",header=TRUE)->kin.no
## kin.no[,c("ID1","ID2")]->grm

## nl<-1
## i<-1
## remove.ids<-character()
## while (nl>0) {
##   c(grm[,1],grm[,2])->id
##   table(id)->tab
##   which.max(tab)->index
##   names(tab)[index]->nm
##   nm->remove.ids[i]
##   (grm[,1] %in% nm) | (grm[,2] %in% nm) -> test
##   grm[!test,]->grm
##   nrow(grm)->nl
##   i<-i+1
##   print(nl)
## }

## df$aid %in% remove.ids -> df$unrel3 #for sensitivity analyses

## ######################################################################
##                                         #euro
## read.table("/cpc/addhealth/users/bdomingu/friends/euro/rw_ids.txt",header=FALSE)->ids
## df$aid %in% ids[,2] -> df$euro.rw






