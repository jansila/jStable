testMLE<-function(x){
  s1<-proc.time()
  fB<-fBasics::stableFit(x,type="mle",doplot=F, trace=F)
  t1<-proc.time()-s1
  
  s2<-proc.time()
  me<-mleFit(x,init=NULL)
  t2<-proc.time()-s2
  
  ans<-list("fBasics"=list("estimate"=fB@fit$estimate,"convergence"=fB@fit$code,"time"=t1[1]),
            "myStable"=list("estimate"=me$fit$estimate,"convergence"=me$fit$code,"time"=t2[1]))
  return(ans)
}


require(tawny)
data("sp500.subset")
rnorm<-rnorm(200)
rt<-rt(200,4)
cauch<-rcauchy(200)
testingData<-matrix(cbind(rnorm,rt,cauch,as.matrix(sp500.subset,nrow=200)),nrow=200)

comp<-lapply(1:78,function(x) testMLE(testingData[,x]))

results<-data.frame(matrix(unlist(comp),ncol=12,byrow=T))
colnames(results)<-c("fBalpha","fBbeta","fBgamma","fBdelta","fBconv","fBtime","alpha","beta","gamma","delta","conv","time")

diff<-data.frame(as.matrix(results[,7:10],ncol=4)-as.matrix(results[,1:4],ncol=4))
colnames(diff)<-c("alpha","beta","gamma","delta")
diff$time<-results$fBtime/results$time
require(ggplot2)


plotComp<-function(x){
  plot(testingData[,x],denStable(testingData[,x], alpha=results$fBalpha[x], beta=results$fBbeta[x], gamma=results$fBgamma[x], delta=results$fBdelta[x]), col="blue")
  points(testingData[,x],denStable(testingData[,x], alpha=results$alpha[x],beta=results$beta[x],gamma=results$gamma[x],delta=results$delta[x]), col="blue", pch=4)
  
  points(testingData[,x],stabledist::dstable(testingData[,x], alpha=results$fBalpha[x], beta=results$fBbeta[x], gamma=results$fBgamma[x], delta=results$fBdelta[x]), col="red")
  points(testingData[,x],stabledist::dstable(testingData[,x], alpha=results$alpha[x],beta=results$beta[x],gamma=results$gamma[x],delta=results$delta[x]), col="red", pch=4)
  
}

p1<-ggplot(diff, aes(x=observations,y=alpha))+geom_point()
p2<-ggplot(diff, aes(x=observations,y=beta))+geom_point()
p3<-ggplot(diff, aes(x=observations,y=gamma))+geom_point()
p4<-ggplot(diff, aes(x=observations,y=delta))+geom_point()

multiplot(plotlist=list(p1,p3,p2,p4),cols=2)


ggplot(diff,aes(x=observations,y=time))+geom_point()


pdf(file="densityplots.pdf",paper="a4r")
for(i in 1:16){
  plotComp(order(diff$beta,decreasing=T)[i])
}
