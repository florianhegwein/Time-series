#----------------------------------------------------------------------------------------------------------------
# Packages utilisés
#----------------------------------------------------------------------------------------------------------------
library(caschrono)
library(forecast)


#----------------------------------------------------------------------------------------------------------------
# Série temporelle 'Pizza'
#----------------------------------------------------------------------------------------------------------------
data = read.csv("Pizza (France).csv", skip=1, col.names=c("Mois","France"), row.names=1)     # Chargement
Pizza = ts(data[1:(nrow(data)-12),], start=c(2004, 1), frequency=12)                         # Série sans 2019
Pizza2019 = ts(data[(nrow(data)-11):nrow(data),], start=c(2019, 1), frequency=12)            # Données de 2019


#----------------------------------------------------------------------------------------------------------------
# Statistique déscriptive
#----------------------------------------------------------------------------------------------------------------
# Chronogramme
#-------------
MatX = matrix(data=Pizza, nrow=12)     # La série en forme de matrix
Min = apply(MatX, 2, min)              # Déterminer les minimums par année
Max = apply(MatX, 2, max)              # Déterminer les maximums par année
YearMin = c(2005:2019)                 # Les années minimum
YearMax = c(2004:2018)                 # Les années maximum

plot(Pizza, lwd=2, main="Intérêt pour le mot 'Pizza' en France", xlab="Temps", ylab="Intérêt")     # Plot de la série
points(YearMin, Min, col="blue", type = "l")                                                       # Courbe minimum
points(YearMax, Max, col="red", type = "l")                                                        # Courbe maximum


# Month-plot
#-----------
monthplot(Pizza, main="Monthplot de 'Pizza'", ylab="Pizza")     # Month-plot de la série


# Lag-plot
#---------
lag.plot(Pizza, lags=12, main="Lag-plot de 'Pizza'")     # Lag-plot de la série


# Décomposition : modèle additive
#--------------------------------
PizzaAdd = decompose(Pizza, type="additive")     # Décomposition en modèle additif
plot(PizzaAdd)                                   # Plot de la décomposition

plot(Pizza, lwd=2, col="darkgrey",                                              # Plot de la série
     main="decompose() avec modèle additif",                                    # Titre du graphique
     xlab='Temps', ylab="Intérêt")                                              # Titre des axes
points(PizzaAdd$trend, type='l', col="red")                                     # Tendance de la série
points(PizzaAdd$trend+PizzaAdd$seasonal, type='l', col='blue')                  # Courbe de la série simulée
legend('topleft', col=c("darkgrey","red",'blue'), lwd=c(2,1,1), bg="white",     # Paramètres de la légende
       legend=c(expression(X[t]), expression(m[t]), expression(m[t]+s[t])))     # Texte de la légende


# Décomposition : modèle multiplicative
#--------------------------------------
PizzaMult = decompose(Pizza, type="multiplicative")     # Décomposition en modèle multiplicatif
plot(PizzaMult)                                         # Plot de la décomposition

plot(Pizza, lwd=2, col="darkgrey",                                               # Plot de la série
     main="decompose() avec modèle multiplicatif",                               # Titre du graphique
     xlab='Temps', ylab="Intérêt")                                               # Titre des axes
points(PizzaMult$trend, type='l', col="red")                                     # Tendance de la série
points(PizzaMult$trend*PizzaMult$seasonal, type='l', col='blue')                 # Courbe de la série simulée
legend('topleft', col=c("darkgrey","red",'blue'), lwd=c(2,1,1),  bg="white",     # Paramètres de la légende
       legend=c(expression(X[t]), expression(m[t]), expression(m[t]*s[t])))      # Texte de la légende


#----------------------------------------------------------------------------------------------------------------
# Lissage exponentielle triple : méthode de Holt-Winters
#----------------------------------------------------------------------------------------------------------------
PizzaHW_MAM = ets(Pizza, model="MAM")     # Erreur : MULT, tendence : ADD, saison : MULT 
summary(PizzaHW_MAM)                      # Paramètres du modèle

PizzaHW_MMM = ets(Pizza, model="MMM")     # Erreur : MULT, tendence : MULT, saison : MULT
summary(PizzaHW_MMM)                      # Paramètres du modèle

PizzaPredHW_MAM = forecast(PizzaHW_MAM, h=12)                                                # Prédiction sur 12 mois
plot(PizzaPredHW_MAM, xlim=c(2017,2020), ylim=c(50,100), xaxt="n")                           # Plot de la prédiction
axis(1, at=seq(2017,2020,1), labels=seq(2017,2020,1))                                        # Texte de l'abscisse
points(Pizza2019, type="l", lwd=2, col="darkgreen")                                          # Vraies valeurs
abline(v=seq(2017,2020,1), col="red", lty="dotted")                                          # Lignes verticales
legend('topleft', col=c("black","darkgreen",'blue'), lwd=c(1,2,2), cex=0.8,  bg="white",     # Paramètres de la légende
       legend=c("Anciennes valeurs","Vraies valeurs","Prédiction"))                          # Texte de la légende

PizzaPredHW_MMM = forecast(PizzaHW_MMM, h=12)                                               # Prédiction sur 12 mois
plot(PizzaPredHW_MMM, xlim=c(2017,2020), ylim=c(50,100), xaxt="n")                          # Plot de la prédiction
axis(1, at=seq(2017,2020,1), labels=seq(2017,2020,1))                                       # Texte de l'abscisse
points(Pizza2019, type="l", lwd=2, col="darkgreen")                                         # Vraies valeurs
abline(v=seq(2017,2020,1), col="red", lty="dotted")                                         # Lignes verticales
legend('topleft', col=c("black","darkgreen",'blue'), lwd=c(1,2,2), cex=0.8, bg="white",     # Paramètres de la légende
       legend=c("Anciennes valeurs","Vraies valeurs","Prédiction"))                         # Texte de la légende


#----------------------------------------------------------------------------------------------------------------
# Passage au log()
#----------------------------------------------------------------------------------------------------------------
# Chronogramme
#-------------
MatX = matrix(data=log(Pizza), nrow=12)
Min = apply(MatX, 2, min)
Max = apply(MatX, 2, max)
YearMin = c(2005:2019)
YearMax = c(2004:2018)

plot(log(Pizza), lwd=2, main="LOG de l'intérêt pour le mot 'Pizza' en France", xlab="Temps", ylab="log(Intérêt)")
points(YearMin, Min, col="blue", type = "l")
points(YearMax, Max, col="red", type = "l")


# Décomposition
#--------------
PizzaLog = decompose(log(Pizza), type="additive")     # Décomposition en modèle additif
plot(PizzaLog)                                        # Plot de la décomposition

plot(log(Pizza), lwd=2, col="darkgrey",                                                  # Plot de la série
     main="decompose() avec modèle additif du LOG",                                      # Titre du graphique
     xlab='Temps', ylab="Intérêt")                                                       # Titre des axes
points(PizzaLog$trend, type='l', col="red")                                              # Tendance de la série
points(PizzaLog$trend+PizzaLog$seasonal, type='l', col='blue')                           # Courbe de la série simulée
legend('topleft', col=c("darkgrey","red",'blue'), lwd=c(2,1,1), cex=0.8, bg="white",     # Paramètres de la légende
       legend=c(expression(log(X[t])), expression(m[t]), expression(m[t]+s[t])))         # Texte de la légende


# Lissage exponentielle triple : méthode de Holt-Winters
#-------------------------------------------------------
PizzaHWlog = ets(log(Pizza), model="AAA")     # Erreur : ADD, tendence : ADD, saison : ADD
summary(PizzaHWlog)                           # Paramètres du modèle

PizzaPredHWlog = forecast(PizzaHWlog, h=12)              # Prédiction sur 12 mois
for (i in c(2,4:7)){                                     # Rétransformation
  PizzaPredHWlog[i] = lapply(PizzaPredHWlog[i], exp)     # Appliquer exp() à tous les éléments nécessaires
}
plot(PizzaPredHWlog, xlim=c(2017,2020), ylim=c(50,100), xaxt="n")                           # Plot de la prédiction
axis(1, at=seq(2017,2020,1), labels=seq(2017,2020,1))                                       # Texte de l'abscisse
points(Pizza2019, type="l", lwd=2, col="darkgreen")                                         # Vraies valeurs
abline(v=seq(2017,2020,1), col="red", lty="dotted")                                         # Lignes verticales
legend('topleft', col=c("black","darkgreen",'blue'), lwd=c(1,2,2), cex=0.8, bg="white",     # Paramètres de la légende
       legend=c("Anciennes valeurs","Vraies valeurs","Prédiction"))                         # Texte de la légende


#----------------------------------------------------------------------------------------------------------------
# Modélisation ARMA/SARMA, ARIMA/SARIMA
#----------------------------------------------------------------------------------------------------------------
# Fonctions d'autocorrélation
#---------------------------
par(mfrow=c(1,2))
acf(Pizza, type="covariance", main="Covariance de Pizza")        # Covariance
acf(Pizza, type="correlation", main="Corrélations de Pizza")     # Corrélations
par(mfrow=c(1,1))


# Différenciation (éliminer saisonnalité et tendance)
#----------------------------------------------------
# Série originale
par(mfrow=c(1,3))
plot(diff(diff(Pizza, lag=12), lag=1))                                              # Saisonnalité 12, tendance
acf(diff(diff(Pizza, lag=12), lag=1), main="diff(diff(Pizza, lag=12), lag=1)")      # ACF
pacf(diff(diff(Pizza, lag=12), lag=1), main="diff(diff(Pizza, lag=12), lag=1)")     # PACF
par(mfrow=c(1,1))

# LOG de la série
par(mfrow=c(1,3))
plot(diff(diff(log(Pizza), lag=12), lag=1))                                                   # Saisonnalité 12, tendance
acf(diff(diff(log(Pizza), lag=12), lag=1), main="diff(diff(log(Pizza), lag=12), lag=1)")      # ACF
pacf(diff(diff(log(Pizza), lag=12), lag=1), main="diff(diff(log(Pizza), lag=12), lag=1)")     # PACF
par(mfrow=c(1,1))


# Modélisation SARIMA
#--------------------
# SARIMA(0,1,1)(0,1,0)
PizzaSARIMAlog = Arima(log(Pizza), order=c(0,1,1), seasonal=c(0,1,0))     # SARIMA(0,1,1)(0,1,0)
summary(PizzaSARIMAlog)
par(mfrow=c(1,2))
acf(PizzaSARIMAlog$residuals, main="Résidus SARIMA(0,1,1)(0,1,0)")        # ACF
pacf(PizzaSARIMAlog$residuals, main="Résidus SARIMA(0,1,1)(0,1,0)")       # PACF
par(mfrow=c(1,1))
Box.test(PizzaSARIMAlog$residuals, lag=45)                                # Test de blancheur

# SARIMA(0,1,1)(0,1,1)
PizzaSARIMAlog = Arima(log(Pizza), order=c(0,1,1), seasonal=c(0,1,1))     # SARIMA(0,1,1)(0,1,1)
summary(PizzaSARIMAlog)
par(mfrow=c(1,2))
acf(PizzaSARIMAlog$residuals, main="Résidus SARIMA(0,1,1)(0,1,1)")        # ACF
pacf(PizzaSARIMAlog$residuals, main="Résidus SARIMA(0,1,1)(0,1,1)")       # PACF
par(mfrow=c(1,1))
Box.test(PizzaSARIMAlog$residuals, lag=45)                                # Test de blancheur
t_stat(PizzaSARIMAlog)                                                    # Significativité des coefficients
cor.arma(PizzaSARIMAlog)                                                  # Autocorrélations

# Prédiction SARIMA
PizzaPredSARIMAlog = forecast(PizzaSARIMAlog, h=12)
for (i in c(4:7,9)){                                             # Rétransformation
  PizzaPredSARIMAlog[i] = lapply(PizzaPredSARIMAlog[i], exp)     # Appliquer exp() à tous les éléments nécessaires
}
plot(PizzaPredSARIMAlog, xlim=c(2017,2020), ylim=c(50,100), xaxt="n")                       # Plot de la prédiction
axis(1, at=seq(2017,2020,1), labels=seq(2017,2020,1))                                       # Texte de l'abscisse
points(Pizza2019, type="l", lwd=2, col="darkgreen")                                         # Vraies valeurs
abline(v=seq(2017,2020,1), col="red", lty="dotted")                                         # Lignes verticales
legend('topleft', col=c("black","darkgreen",'blue'), lwd=c(1,2,2), cex=0.8, bg="white",     # Paramètres de la légende
       legend=c("Anciennes valeurs","Vraies valeurs","Prédiction"))                         # Texte de la légende

# Modélisation automatique
#-------------------------
# SARIMA
PizzaAUTOlog = auto.arima(log(Pizza), ic="aic")                         # Choix automatique basé sur l'AIC
summary(PizzaAUTOlog)
par(mfrow=c(1,2))
acf(PizzaAUTOlog$residuals, main="Résidus SARIMA(2,1,2)(2,1,2)")        # ACF
pacf(PizzaAUTOlog$residuals, main="Résidus SARIMA(2,1,2)(2,1,2)")       # PACF
par(mfrow=c(1,1))
Box.test(PizzaAUTOlog$residuals, lag=45)                                # Test de blancheur
t_stat(PizzaAUTOlog)                                                    # Significativité des coefficients
cor.arma(PizzaAUTOlog)                                                  # Autocorrélations

# Prédiction
PizzaPredAUTOlog = forecast(PizzaAUTOlog, h=12)
for (i in c(4:7,9)){                                         # Rétransformation
  PizzaPredAUTOlog[i] = lapply(PizzaPredAUTOlog[i], exp)     # Appliquer exp() à tous les éléments nécessaires
}
plot(PizzaPredAUTOlog, xlim=c(2017,2020), ylim=c(50,100), xaxt="n")                         # Plot de la prédiction
axis(1, at=seq(2017,2020,1), labels=seq(2017,2020,1))                                       # Texte de l'abscisse
points(Pizza2019, type="l", lwd=2, col="darkgreen")                                         # Vraies valeurs
abline(v=seq(2017,2020,1), col="red", lty="dotted")                                         # Lignes verticales
legend('topleft', col=c("black","darkgreen",'blue'), lwd=c(1,2,2), cex=0.8, bg="white",     # Paramètres de la légende
       legend=c("Anciennes valeurs","Vraies valeurs","Prédiction"))                         # Texte de la légende


#------------------------------------------------------------------------------------------------
# Choix de modèle
#------------------------------------------------------------------------------------------------
plot(Pizza2019, lwd=2, xlim=c(2019,2019.92), xaxt="n",                       # Vraies valeurs
     main="Comparaison des modèles", xlab="Temps")                           # Titres
abline(v=seq(2019,2020,length.out=13), lty="dotted", col="grey")             # Lignes verticales
axis(1,at=seq(2019,2020,length.out=13),                                      # Textes de l'abscisse
     labels=c("Jan.", "Fév.", "Mars", 
              "Avr.", "Mai", "Juin", 
              "Juil.", "Août", "Sep.", 
              "Oct.", "Nov.", "Déc.", "Jan."))
points(PizzaPredHW_MAM$mean, type="l", col="red")                            # PizzaPredHW_MAM
points(PizzaPredHW_MMM$mean, type="l", col="green")                          # PizzaPredHW_MMM
points(PizzaPredHWlog$mean, type="l", col="blue")                            # PizzaPredHWlog
points(PizzaPredSARIMAlog$mean, type="l", col="cyan")                        # PizzaPredSARIMAlog
points(PizzaPredAUTOlog$mean, type="l", col="magenta")                       # PizzaPredAUTOlog
legend('topleft', col=seq(1:6), lwd=c(2,1,1,1,1,1), cex=0.8, bg="white",     # Paramètres de la légende
       legend=c("Vraies valeurs",                                            # Texte de la légende
                "Holt-Winters (M,A,M)",
                "Holt-Winters (M,M,M)",
                "Holt-Winters (A,A,A) - log",
                "SARIMA (0,1,1)(0,1,1) - log", 
                "SARIMA (2,1,2)(2,1,2) - log"))
