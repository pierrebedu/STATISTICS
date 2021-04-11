# ECHANTILLONS INDEPENDANTS exo 1

riche <- scan("C:\\Users\\pierr\\Documents\\R scripts\\DU INP\\Donnees\\riche.txt")

pauvre <- scan("C:\\Users\\pierr\\Documents\\R scripts\\DU INP\\Donnees\\pauvre.txt")

#on regarde

mean(pauvre)
mean(riche)

sd(pauvre)
sd(riche)

boxplot(list(riche, pauvre), names= c("riche", "pauvre"))

#test de normalité

shapiro.test(pauvre)
shapiro.test(riche)

plot(density(pauvre))
plot(density(riche))

# la normalité des DEUX echantillons étant acquise, on teste d'abord l'égalité des variances

var.test(pauvre, riche) # TEST DE FISHER : p value : 0.98 donc Ho : variances egales ok

# les variances etant égales, on teste ensuite l'égalité des moyennes

t.test(pauvre, riche, paired=FALSE, var.equal=TRUE) #TEST DE STUDENT independance par defaut (paired=FALSE) et on lui dit variances egales
#Rq : dans le cas variance inégales on ne pourrait le faire car N = 19 trop petit
#ici moyenne égales!

#on affine l'hypothèse H1 pour un test plus fin!
t.test(riche, pauvre, paired=FALSE, var.equal=TRUE, alternative="greater")
#ici elles ne le sont plus!

##################################################################################################################################
# résultat tangent donc pour confirmer on fait un test non paramétrique : Wilcoxon test des rangs

wilcox.test(riche, pauvre, alternative="greater") # confirme les moyennes poas egales si H1 choisi finement! (paired =FALSE par defaut)

###################################################################################################################################

# ECHANTILLONS APPARIES exo 2

load("C:\\Users\\pierr\\Downloads\\donneesFiltres.Rdata")
attach(donneesFiltres)# pour qu'il connaisse colonne et dataframe

#EDA
head(donneesFiltres)
summary(donneesFiltres)# pas bien! (pas le même effectif et on perd les liens indivs!)

filtre.verre> filtre.papier
boxplot(filtre.verre - filtre.papier) # box plot des différences

plot(filtre.verre, filtre.papier)#nuage de points
abline(0,1, col=2)

# test de student
shapiro.test(filtre.verre)# les echantillons ne sont pas normalement distribués mais ca n'est pas necessaire
shapiro.test(filtre.papier)

hist(filtre.verre)
hist(filtre.papier)

shapiro.test(filtre.verre - filtre.papier)# les différences sont bien normales(il le fallait!). Faire un student a du sens. 

t.test(filtre.verre - filtre.papier)#mal fait : les echantillons doivent etre considérés comme apairés
t.test(filtre.verre, filtre.papier, paired=TRUE, alternative= "greater") # dit bien que verre filtre mieux que papier car moyennes differentes

t.test(filtre.verre -filtre.papier, alternative= "greater") # EXACTEMENT LA MEME CHOSE: avec les différences sous entendu greater than zero

#SANS PRECISER APPARIES CA DEVIENT FAUX !
t.test(filtre.verre, filtre.papier, alternative= "greater")# version fausse!!!!!!!!!!!! fait comme si les echant sont non appariés

#on peut confirmer avec Wilcox

wilcox.test(filtre.verre, filtre.papier, alternative= "greater", paired=TRUE) # ça confirme le rejet de Ho


###########################################################################################################

#exo3.1 ECHANTILLON APPARIE

sourisA <- scan("C:\\Users\\pierr\\Documents\\R scripts\\DU INP\\Donnees\\echSourisA.txt")
sourisB <- scan("C:\\Users\\pierr\\Documents\\R scripts\\DU INP\\Donnees\\echSourisB.txt")


#EDA
head(sourisA)
summary(sourisA) # pas bien
summary(sourisB)
plot(sourisA, sourisB)
abline(0,1)

boxplot(sourisA-sourisB)
abline(h=0)

sum(sourisA>sourisB)

#On teste la normalité puis Ho : les moyennes sont égales
shapiro.test(sourisA-sourisB) # différences bien normales

t.test(sourisA-sourisB)
ou 
t.test(sourisA, sourisB, paired=TRUE) # même chose


#######################################################################################################
#EXO 3.2 ECHANTILLON INDEPENDANTS


malade <- scan("C:\\Users\\pierr\\Documents\\R scripts\\DU INP\\Donnees\\echCancerColon.txt")
nmalade <- scan("C:\\Users\\pierr\\Documents\\R scripts\\DU INP\\Donnees\\echCancerTemoin.txt")

head(malade)
summary(malade) # pas bien
summary(nmalade)
boxplot(list(malade, nmalade))

#On teste la normalité 
shapiro.test(malade) # les deux echantillons sont normaux
shapiro.test(nmalade)

var.test(malade, nmalade)# egalité des variances avec fisher

t.test(malade, nmalade, var.equal=TRUE, paired=FALSE) #puis Ho : les moyennes sont égales

# on confirme avec kolmogorov smirnov sur les fonctions de repartition
ks.test(malade, nmalade)# kolmogorov smirnov -> même distribution acceptée (distance sur les fcts de repartition)

#############################################################################################################
# exo 3.3 on imagine independants + pas normaux (wilcoxon direct) // OU // independants +variances pas egales


jeunes <- scan("C:\\Users\\pierr\\Documents\\R scripts\\DU INP\\Donnees\\jeunes.txt")
ages <- scan("C:\\Users\\pierr\\Documents\\R scripts\\DU INP\\Donnees\\ages.txt")

jeunes
summary(jeunes)
summary(ages)

boxplot(list(jeunes, ages))

# normalité???
hist(jeunes)
hist(ages)

shapiro.test(jeunes)
shapiro.test(ages)

#variance égales?
var.test(jeunes, ages,paired=FALSE )

#conclusion : moyennes égales
t.test(jeunes, ages, paired=FALSE, var.equal = TRUE, alternative= "less")

##############################################################################
#confirmons avec wilcoxon (rangs) et kolmogorov smirnov (fct repart)

wilcox.test(jeunes, ages, paired=FALSE, alternative ="less")

ks.test(jeunes, ages, paired=FALSE, alternative ="less")
