#PREMIER EXO

ech <- scan("C:\\Users\\pierr\\Downloads\\michelson.txt")

ech
mean(ech)
hist(ech)
summary(ech)
plot(density(ech))

# utilité discutable...
GraphDensite.prog <- function(x){
  plot(density(x, width=2*(summary(x)[5]-summary(x)[2])), type="l", xlab="x", ylab=" ", main=" ")
  title("estimation de la densité de l'échantillon")
  }

GraphDensite.prog(ech)

#SHAPIRO POUR SUIT UNE LOI NORMALE
shapiro.test(ech) #test de normalité . regarde le p value (risque empirique) (on ne rejette pas à 95%)


#KOLGOMOROV POUR SUIT UNE LOI CLASSIQUE (bien pour n grand)
ks.test(ech, "pnorm",  mean(ech), sqrt(var(ech))) #test de kolmogorov : marche pour loi classique (on ne rejette pas non plus)


#STUDENT POUR ESTIMER L ESPERANCE
res <- t.test(ech, mu=990) # test de student (sans reflechir).Rq : donne les degrés de liberté
res$conf.int #intervalle de confiance (bilateral) pour mu

res2 <- t.test(ech, mu=990, alternative="less") # test de student en regardant la densité (fort rejet)
res2$conf.int #intervalle de confiance (unilateral) pour mu


#########################################################################################################################
#programme à la main pour vérifier si sigma= sigma_o sur un seul echantillon
#On calcule (n-1)Sn²/sigma_o² et on le compare à X²(n-1)

################################
# Test sur la variance         #
################################

TestVariance.prog <- function(echantillon,sigma2.0=1,alternative="greater"){
  n<-length(echantillon)
  kn<-var(echantillon)*(n-1)/sigma2.0
  if (alternative=="greater") {
    p.value<-1-pchisq(kn,df=n-1)
  }
  if (alternative=="less") {
    p.value<-pchisq(kn,df=n-1)
  }
  cat("--------------------------------",fill=T)
  cat("Test portant sur la variance d'un echantillon",fill=T)
  cat(fill=T)
  cat("H0 : vraie variance = ",sigma2.0,fill=T)
  if (alternative=="greater"){
    cat("H1 : vraie variance > ",sigma2.0,fill=T)
  }
  if (alternative=="less"){
    cat("H1 : vraie variance < ",sigma2.0,fill=T)
  }
  cat("Statistique de test Kn=",kn,"  p-value=",p.value,fill=T)
  cat("variance estimee = ",var(echantillon),fill=T)
  cat("--------------------------------",fill=T)
  list(kn=kn,p.value=p.value)
}

#-----------------------------------------
# Test de la fonction "TestVariance.prog"
#-----------------------------------------
# donnees de Michelson

var(ech)
TestVariance.prog(ech,sigma2.0=10000,alternative="greater")
#################################################################################################################

#exo 1.4 kolgomorov smirnov

plot.ecdf(ech)
plot.ecdf(rnorm(500, mean=mean(ech), sd=sqrt(var(ech))), add=T,  lty="dotted", pch=" "  )



#########################################################################################################################
#EXEMPLE DES FLACONS 1.5

ech <- scan("C:\\Users\\pierr\\Downloads\\echFlacons.txt")

#on visualise les données
mean(ech)
sd(ech)
summary(ech)
par(mfrow=c(1,2))
hist(ech)
abline(v=250, col=2, lwd=3)
boxplot(ech)
abline(h=250, col=2, lwd=3)

#fonctions de repart
plot.ecdf(ech) # fonction de repartition empirique
plot.ecdf(rnorm(5000, mean=mean(ech), sd= sd(ech)), add=T,lty="dotted", pch=" ")


shapiro.test(ech) # il est logique de tester d'abord la normalité : qui est ok!

t.test(ech, mu= 250) # on s 'eloigne significativement du 250!
t.test(ech, mu=250, alternative="less")

#kolmogorov ne teste pas la meme chose (pas utilisable ici sans sigma)

#########OPTIONNEL##############################
var(ech)
res<-TestVariance.prog(ech,sigma2.0=2,alternative="less")
res<-TestVariance.prog(ech,sigma2.0=2.5,alternative="less")
res<-TestVariance.prog(ech,sigma2.0=3,alternative="less")
res<-TestVariance.prog(ech,sigma2.0=3.5,alternative="less")
res<-TestVariance.prog(ech,sigma2.0=4,alternative="less")
