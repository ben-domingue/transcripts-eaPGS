#mod is a list of lm objects
table.lm<-function(mod,se=TRUE) {
  lapply(mod,function(x) names(coef(x)))->nms
  unique(do.call("c",nms))->nms
  length(nms)->nr
  length(mod)->nc
  mat.pv<-mat.est<-mat.tstat<-matrix(NA,nr,nc)
  for (j in 1:nc) {
    summary(mod[[j]])$coef->foo
    for (i in 1:nr) {
      match(nms[i],rownames(foo))->index
      if (length(index)>0) {
        foo[index,1]->mat.est[i,j]
        if (!se) foo[index,3]->mat.tstat[i,j] else foo[index,2]->mat.tstat[i,j]
        foo[index,4]->mat.pv[i,j]
      }
    }
  }
  sapply(mod,function(x) length(x$y))->N
  out<-list()
  new.nms<-list()
  for (i in 1:nr) {
    rbind(mat.est[i,],mat.tstat[i,],mat.pv[i,])->out[[i]]
    new.nms[[i]]<-c(nms[i],paste(nms[i],".se",sep=""),paste(nms[i],".pv",sep=""))
  }
  do.call("rbind",out)->out
  rbind(out,N)->out
  c(do.call("c",new.nms),"N")->rownames(out)
  ##get rid of bad rows
  grep("scid",rownames(out))->i1
  grep("pedigree",rownames(out))->i2
  if (length(c(i1,i2))>0) out[-c(i1,i2),]->out
  ##
  out
}


load(file="/ifs/sec/cpc/addhealth/users/bdomingu/transcripts/df.Rdata")
df[df$transcript,]->df
df$birthyear-mean(df$birthyear,na.rm=TRUE)->df$birthyear
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


##1-3. tracking
m<-list()
##1
library(ordinal)
clm("math9~ea2+sex+birthyear",data=df[df$orig,])->m[['baseline']]
clm("math9~ea2+sespc.all+sex+birthyear",data=df[df$orig,])->m[['baseline plus ses']]
clm("math9~ea2+sespc.all+school.hsgrad+sex+birthyear",data=df[df$orig,])->m[['baseline plus ses.both']]
clm("math9~ea2*school.hsgrad+(ea2+school.hsgrad)*(sex+birthyear)",data=df[df$orig,])->m[['baseline.interaction']]
##2
ifelse(df$math9 %in% c("algebra1","higher than algebra1"),1,0)->df$alg1
ifelse(df$math9=="higher than algebra1",1,0)->df$alg1.plus
##
## glm(alg1~ea2+sex+birthyear,data=df[df$orig,],family="binomial")->m[['alg1split.baseline']]
## glm(alg1~ea2+sespc.all+sex+birthyear,data=df[df$orig,],family="binomial")->m[['alg1split.ses']]
## glm(alg1~ea2+sex+birthyear+factor(scid),data=df[df$orig,],family="binomial")->m[['alg1split.school']]
## glm(alg1~ea2+sex+birthyear+factor(pedigree),data=df[df$infam,],family="binomial")->m[['alg1split.fam']]
##
glm(alg1~ea2+sex+birthyear,data=df[df$orig & df$math9!="higher than algebra1",],family="binomial")->m[['alg1.baseline']]
glm(alg1~ea2+sespc.all+sex+birthyear,data=df[df$orig & df$math9!="higher than algebra1",],family="binomial")->m[['alg1.ses']]
glm(alg1~ea2+sex+birthyear+factor(scid),data=df[df$orig & df$math9!="higher than algebra1",],family="binomial")->m[['alg1.school']]
glm(alg1~ea2+sex+birthyear+factor(pedigree),data=df[df$infam & df$math9!="higher than algebra1",],family="binomial")->m[['alg1.fam']]
##
glm(alg1.plus~ea2+sex+birthyear,data=df[df$orig & df$alg1==1,],family="binomial")->m[['>alg1.baseline']]
glm(alg1.plus~ea2+sespc.all+sex+birthyear,data=df[df$orig & df$alg1==1,],family="binomial")->m[['>alg1.ses']]
glm(alg1.plus~ea2+sex+birthyear+factor(scid),data=df[df$orig & df$alg1==1,],family="binomial")->m[['>alg1.school']]
glm(alg1.plus~ea2+sex+birthyear+factor(pedigree),data=df[df$infam & df$alg1==1,],family="binomial")->m[['>alg1.fam']]
##
table.lm(m)->long.tab

##
f<-function(x) {
    grep("ea2",rownames(summary(x)$coef))->index
    c(summary(x)$coef[index,],length(x$y))
}
lapply(m,f)->tab
do.call("rbind",tab)->tab
write.csv(tab,"")




##4-6. persistence
m<-list()
ifelse(df$math9 %in% c("basic","pre-algebra"),-1,NA)->df$math.start
ifelse(df$math9 %in% c("higher than algebra1"),1,df$math.start)->df$math.start
ifelse(df$math9 %in% "algebra1",0,df$math.start)->df$math.start
factor(df$math.start,levels=c(0,-1,1))->df$math.start
##4.
glm(moves.up~ea2+sex+birthyear,data=df[df$orig,],family="poisson")->m[['baseline']]
glm(moves.up~ea2+sespc.all+school.hsgrad+sex+birthyear,data=df[df$orig,],family="poisson")->m[['baseline plus ses.both']]
glm(moves.up~ea2+sespc.all+school.hsgrad+sex+birthyear+math.start,data=df[df$orig,],family="poisson")->m[['baseline plus ses.both+math9']]
glm("moves.up~ea2*school.hsgrad+(ea2+school.hsgrad)*(sex+birthyear)",data=df[df$orig,],family="poisson")->m[['baseline.interaction']]
glm("moves.up~ea2*school.hsgrad+(ea2+school.hsgrad)*(sex+birthyear+math.start)",data=df[df$orig,],family="poisson")->m[['baseline.interaction.math9control']]
##5.
glm(moves.up~ea2+sex+birthyear+factor(scid),data=df[df$orig,],family="poisson")->m[['school']]
##6.
glm(moves.up~ea2+sex+birthyear+factor(pedigree),data=df[df$infam,],family="poisson")->m[['fam']]
##
table.lm(m)->long.tab

f<-function(x) {
    grep("ea2",rownames(summary(x)$coef))->index
    c(summary(x)$coef[index,],length(x$y))
}
lapply(m,f)->tab
do.call("rbind",tab)->tab
write.csv(tab,"")

## Restricting to only kids who took a math class in 9th-grade, predicting enrolling in any math in 10th grade as a function of:
## EA-PGS + family-SES + school-SES
## EA-PGS + family-SES + school-SES + 9th-grade math GPA
## Repeat for 11th-grade and 12th-grade
m <-list()
for (gr in 1:3) {
    df[[paste("eamsq",gr,sep="")]]->df$y0
    df[[paste("eamsq",gr+1,sep="")]]->df$y1
    df[!is.na(df$y0) & df$y0!=0,]->tmp
    ifelse(tmp$y1>0,1,0)->tmp$y1
    print(gr)
    print(table(tmp$y1))
    glm(y1~ea2+sespc.all+school.hsgrad+sex+birthyear,data=tmp,family="binomial")->m[[paste(gr,'1')]]
    tmp[[paste("eamgpa",gr,sep="")]]->tmp$gpa
    glm(y1~ea2+sespc.all+school.hsgrad+sex+birthyear+gpa,data=tmp,family="binomial")->m[[paste(gr,'2')]]
    lapply(m,function(x) print(summary(x)))
}
table.lm(m)->long.tab

           

## can you fit the exact same interaction model that you did for tracking in the 9th grade,
## except for two binary outcomes at the end of high school:
## (1) whether or not the student completed calculus, and
## (2) whether or not the student completed AT LEAST Algebra 2
ifelse(df$math12=="calc",1,0)->df$calc
ifelse(df$math12=="less than algebra2",0,1)->df$alg2
m<-list()
glm("alg2~ea2*school.hsgrad+(ea2+school.hsgrad)*(sex+birthyear)",data=df[df$orig,],family="binomial")->m[['alg2']]
glm("calc~ea2*school.hsgrad+(ea2+school.hsgrad)*(sex+birthyear)",data=df[df$orig,],family="binomial")->m[['calc']]
table.lm(m)->long.tab

