#*********************Partie 1: Exploration des données*************************
#Pré-traitement
  data<-read.csv("C:/Users/Maaouia/Desktop/insurance data.csv")
head(data)
tail(data)
summary(data)

# Traitement des valeurs manquantes
# Vérification des valeurs manquantes
is.na(data)
any(is.na(data))
which(is.na(data))
sum(is.na(data))    #Pas de valeurs manquantes


# Traitement des valeurs aberrantes avec des boxplots

boxplot(data$age, main="Boxplot de l'âge", ylab="Âge")

boxplot(data$bmi, main="Boxplot de l'IMC", ylab="IMC")

boxplot(data$children, main="Boxplot du nombre d'enfants", ylab="Nombre d'enfants")

boxplot(data$charges, main="Boxplot des charges", ylab="Charges")

# Identifier les valeurs aberrantes pour 'bmi' et les afficher
outliers_bmi <- boxplot.stats(data$bmi)$out
outliers_bmi
# Calculer la médiane et l'IQR de 'bmi'
median_bmi <- median(data$bmi)
iqr_bmi <- IQR(data$bmi)
# Définir les limites pour identifier les valeurs aberrantes
lower_limit <- median_bmi - 1.5 * iqr_bmi
upper_limit <- median_bmi + 1.5 * iqr_bmi
# Identifier et ajuster les valeurs aberrantes de 'bmi'
data$bmi[data$bmi < lower_limit] <- lower_limit
data$bmi[data$bmi > upper_limit] <- upper_limit
#Resultat
boxplot(data$bmi, main="Boxplot de l'IMC", ylab="IMC")

#Analyse univariée
#Etude de la distribution des variables quantitatives
#*graphique
hist(data$age, main="Histogramme de l'âge", xlab="Âge", col="skyblue", border="black")
hist(data$bmi, main="Histogramme de l'IMC", xlab="IMC", col="lightgreen", border="black")
hist(data$children, main="Histogramme du nombre d'enfants", xlab="Nombre d'enfants", col="lightcoral", border="black")
hist(data$charges, main="Histogramme des charges médicales", xlab="Charges médicales", col="lightpink", border="black")
#On remarque que les histogrammes ne sont pas symétrique ce qui implique que la normalité des donńees n’est pas assurée.
#*Test statistique
shapiro_test_age <-shapiro.test(data$age)
shapiro_test_age
shapiro_test_bmi <- shapiro.test(data$bmi)
shapiro_test_bmi
shapiro_test_children <-shapiro.test(data$children)
shapiro_test_children

#Etude des modalites des variables qualitatives.
table(data$sex)
table(data$smoker)
table(data$region)

#Analyse bivariée
#Etude de la correlation entre les variables quantitatives (deux `a deux).
#Le test de shapiro indique une absence de normalité pour toutes les variables quantitaves 
#On mesure alors le coefficient de Spearman
# Corrélation entre 'age' et 'bmi'
cor_age_bmi <- cor(data$age, data$bmi, method = "spearman")
print(paste("Corrélation entre age et bmi:", cor_age_bmi))

# Corrélation entre 'age' et 'children'
cor_age_children <- cor(data$age, data$children, method = "spearman")
print(paste("Corrélation entre age et children:", cor_age_children))

# Corrélation entre 'age' et 'charges'
cor_age_charges <- cor(data$age, data$charges, method = "spearman")
print(paste("Corrélation entre age et charges:", cor_age_charges))

# Corrélation entre 'bmi' et 'children'
cor_bmi_children <- cor(data$bmi, data$children, method = "spearman")
print(paste("Corrélation entre bmi et children:", cor_bmi_children))

# Corrélation entre 'bmi' et 'charges'
cor_bmi_charges <- cor(data$bmi, data$charges, method = "spearman")
print(paste("Corrélation entre bmi et charges:", cor_bmi_charges))

# Corrélation entre 'children' et 'charges'
cor_children_charges <- cor(data$children, data$charges, method = "spearman")
print(paste("Corrélation entre children et charges:", cor_children_charges))


#Etude de la dependance de la variable cible par chacune des variables qualitative

#Vérification de la normalité et homogéniéte(sex)
test_sexe <-tapply(data$charges,data$sex,shapiro.test)
test_sexe
test_bsexe<-bartlett.test(data$charges~data$sex)
test_bsexe
# test de Wilcoxon pour comparer les charges entre les deux groupes de sexe (male/female).
charges_homme <- data$charges[data$sex == "male"]
charges_femme <- data$charges[data$sex == "female"]
wilcox_result_sex <- wilcox.test(charges_homme, charges_femme)
print(wilcox_result_sex)

#Vérification de la normalité et homogéniété(smoker)
test_fumeur <-tapply(data$charges,data$smoker,shapiro.test)
test_fumeur
test_bfumeur<-bartlett.test(data$charges~data$smoker)
test_bfumeur
# Test de Wilcoxon pour comparer les charges entre les fumeurs et les non-fumeurs.
charges_fumeur <- data$charges[data$smoker == "yes"]
charges_non_fumeur <- data$charges[data$smoker == "no"]
wilcox_result <- wilcox.test(charges_fumeur, charges_non_fumeur)
print(wilcox_result)

#Vérification de la normalité et homogéniété (region)
test_region <-tapply(data$charges,data$region,shapiro.test)
test_region
test_bregion<-bartlett.test(data$charges~data$region)
test_bregion
# Test de Kruskal-Wallis pour étudier la dépendance entre 'charges' et 'region' car region est à 3 modalités
kruskal_test_region <- kruskal.test(charges ~ region, data = data)
print(kruskal_test_region)

#Etude de la dependance de la variable cible quantitative par les deux variables qualitatives sex et smoker
#ANOVA à 2 facteurs
anova_model_dependence <- aov(charges ~ sex + smoker, data=data)
summary(anova_model_dependence)
#Etude de l’interaction entre ces variables qualitatives explicatives
anova_model_interaction <- aov(charges ~ sex * smoker, data=data)
summary(anova_model_interaction)


#*********************Modélisation linéaire*************************

#Régression linéaire multiple
# Construction du modèle linéaire multiple
modele1 <- lm(charges ~ age + bmi + children + sex + smoker +region , data=data)
# Diagnostiquer graphiquement le modèle
plot(modele1)
# Évaluer la performance
summary(modele1)
AIC(modele1)

#Régression linéaire améliorée
# Modification 1 : Relation non linéaire entre l’âge et les charges
data$age2 <- data$age^2

# Modification 2 : Convertir une variable numérique en un indicateur binaire
data$bmi30 <- ifelse(data$bmi >= 30, 1, 0)

# Modification 3 : Ajout d'effet d'interaction
data$smoker_numeric <- ifelse(data$smoker == "yes", 1, 0)
data$smoker_bmi30 <- data$smoker_numeric * data$bmi30

# Construction du modèle linéaire multiple amélioré (modele2)
modele2 <- lm(charges ~  age+age2 +bmi+children+region+sex+smoker+ bmi30 + smoker_bmi30, data = data)

# Évaluation du modele2
summary(modele2)
plot(modele2)



# Comparaison des modele1 et modele2
# R-carré ajusté pour le modèle 1
rsquared_adj_modele1 <- summary(modele1)$adj.r.squared
# R-carré ajusté pour le modèle 2
rsquared_adj_modele2 <- summary(modele2)$adj.r.squared

# Comparaison entre le modele1 et modele2
cat("R-carré ajusté pour le modèle 1 :", rsquared_adj_modele1, "\n")
cat("R-carré ajusté pour le modèle 2 :", rsquared_adj_modele2, "\n")

#Utilisation de l'AIC pour comparer les 2 modeles
AIC(modele1)
AIC(modele2)


# Régression linéaire pénalisée

library(glmnet)

# Préparez vos données
# En supposant que 'charges' est votre variable dépendante et les autres variables sont vos prédicteurs
X <- model.matrix(charges ~ age + bmi + children + sex + smoker + region, data = data)
y <- data$charges

# 1) Régression Ridge

# Ajuster le modèle de régression Ridge
ridge_model <- cv.glmnet(X, y, alpha = 0)  # alpha = 0 pour Ridge, alpha = 1 pour Lasso

# Afficher les résultats
print(ridge_model)

# Tracer les résultats
plot(ridge_model)

# Extraire les coefficients
coefficients_ridge <- coefficients(ridge_model, s = "lambda.min")

# Interprétation des résultats
cat("Coefficients de la régression Ridge:\n")
print(coefficients_ridge)

# 2) Régression Lasso

# Ajuster le modèle de régression Lasso
lasso_model <- cv.glmnet(X, y, alpha = 1)  # alpha = 1 pour Lasso

# Afficher les résultats
print(lasso_model)

# Tracer les résultats
plot(lasso_model)

# Extraire les coefficients
coefficients_lasso <- coefficients(lasso_model, s = "lambda.min")

# Interprétation des résultats
cat("Coefficients de la régression Lasso:\n")
print(coefficients_lasso)



#*********************Analyse multidimensionnelle*************************

# Sélectionnez les variables prédictives pertinentes pour l'ACP
predictors <- data[, c("age", "bmi", "children", "sex", "smoker", "region", "age2", "bmi30", "smoker_bmi30")]

# Convertir les variables catégorielles (sex, smoker et region) en format numérique
predictors <- model.matrix(~ . - 1, data = predictors)

# Effectuer l'ACP
pca_result <- prcomp(predictors, scale. = TRUE)

# Résumé de l'ACP
summary(pca_result)

# Proportion de variance expliquée par chaque composante principale
prop_var <- pca_result$sdev^2 / sum(pca_result$sdev^2)
cumulative_prop_var <- cumsum(prop_var)

# Tracer la variance cumulative expliquée
plot(cumulative_prop_var, type = "b", xlab = "Nombre de composantes principales", ylab = "Proportion cumulative de la variance expliquée")



# Extraire les composantes principales sélectionnées
selected_components <- pca_result$x[, 1:8]

# Ajouter les composantes sélectionnées avec charges
data_with_pca <- cbind(selected_components, charges = data$charges)
data_with_pca <- as.data.frame(data_with_pca)

# Construire le modèle linéaire (modele3)
modele3 <- lm(charges ~ ., data = data_with_pca)

# Résumé de modele3
summary(modele3)


rsquared_adj_modele3 <- summary(modele3)$adj.r.squared

# Comparer les AIC
AIC(modele1)
AIC(modele2)
AIC(modele3)

