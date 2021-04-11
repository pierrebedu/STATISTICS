# exo 2 Xhi 2 indépendance de deux variables qualitatives

load("C:\\Users\\pierr\\Downloads\\donneesLoisir.Rdata")

head(donneesLoisir)
summary(donneesLoisir)
attach(donneesLoisir)

tab.cont <- table(Loisir, Age) # table de contingence
tab.cont

a <- round(prop.table(tab.cont, margin=1)*100, digit=1) #profil ligne avec prop.table et le parametre margin
a
round(prop.table(tab.cont, margin=2)*100, digit=1) #profil colonne


#visulaisation profil ligne d'histogramme -> on se doute que c'est dépendant
par(mfrow=c(4,2))

for (i in 1:nrow(tab.cont)){
barplot(a[i,]*100, main=rownames(tab.cont)[i], ylim=c(0,2000))
  }

#####################################################################################################
# visualisation via une AFC : ages et loisirs sont codées par une indicatrice (on enlève une categorie pour ne pas avoir de liaison linaéaire et XX' inversible)

library(PCAmixdata)
res <- PCAmix(X.quali = donneesLoisir, graph=FALSE)

par(mfrow=c(1,1))
plot(res, choice="levels")

plot(res, choice="ind")

#####################################################################################################
#test de l'independance

res.chi <- chisq.test(tab.cont) # il n'y a pas du tout indépendance
#ou alors autre commande possible
chisq.test(Loisir,Age)

# remarques on peut avoir la table et la table sous independance

res.chi$observed
res.chi$expected

############################################################################################################

#exo3 : xhi2 d'indépendance à nouveau

load("C:\\Users\\pierr\\Downloads\\departementMP.Rdata")
head(departementMP)
summary(departementMP)


tab.cont <- table(departementMP$departement, departementMP$superficie) # table de contingence
tab.cont

a <- round(prop.table(tab.cont, margin=1)*100, digit=1) #profil ligne avec prop.table et le parametre margin
round(prop.table(tab.cont, margin=2)*100, digit=1) #profil colonne


#visualisation profil ligne par histogramme 
par(mfrow=c(4,2))

for (i in 1:nrow(tab.cont)){
  barplot(a[i,]*100, main=rownames(tab.cont)[i], ylim=c(0,1000))
}

# visualisation via une AFC

library(PCAmixdata)
res <- PCAmix(X.quali = departementMP, graph=FALSE)

par(mfrow=c(1,1))
plot(res, choice="levels") #barycentre de toutes les exploitations avec telle etiquette
plot(res, choice="ind")

#test de l'independance

chisq.test(tab.cont) # il n'y a pas du tout indépendance


########################################################################################################################
#exo 4

prop.test(c(85,182), c(100,200))
prop.test(c(85,182), c(100,200), alternative="less") # proportions égales : plausible

prop.test(c(450,192), c(1000,200)) # proportions pas égales

smokers <- c(83, 90, 129, 70)
patients <- c(86, 93, 136, 82)

prop.test(smokers, patients)# pas égalité de proportions du tout

