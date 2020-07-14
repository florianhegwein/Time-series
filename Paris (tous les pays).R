#----------------------------------------------------------------------------------------------------------------
# Packages utilisés
#----------------------------------------------------------------------------------------------------------------
library(caschrono)
library(forecast)


#----------------------------------------------------------------------------------------------------------------
# Série temporelle 'Paris'
#----------------------------------------------------------------------------------------------------------------
data = read.csv("Paris (tous les pays).csv", skip=1, col.names=c("Mois","France"), row.names=1)     # Chargement
Paris = ts(data[1:(nrow(data)-12),], start=c(2004, 1), frequency=12)                                # Série sans 2019
Paris2019 = ts(data[(nrow(data)-11):nrow(data),], start=c(2019, 1), frequency=12)                   # Données de 2019


#----------------------------------------------------------------------------------------------------------------
# Statistique déscriptive
#----------------------------------------------------------------------------------------------------------------
# Chronogramme
#-------------
plot(Paris, lwd=2, main="Intérêt pour le mot 'Paris' partout dans le monde", xlab="Temps", ylab="Intérêt")     # Plot de la série
points(x=c(2015, 2015.835), y=c(44, 100), pch=21, col="red", cex=2, lwd=3)                                     # Valeurs aberrantes


# Valeurs aberrantes
#-------------------
Paris[133] = (Paris[132] + Paris[134]) / 2
Paris[143] = (Paris[142] + Paris[144]) / 2

MatX = matrix(data=Paris, nrow=12)     # La série en forme de matrix
Min = apply(MatX, 2, min)              # Déterminer les minimums par année
Max = apply(MatX, 2, max)              # Déterminer les maximums par année
YearMin = c(2004:2018)                 # Les années minimum
YearMax = c(2005:2019)                 # Les années maximum

plot(Paris, lwd=2, main="Intérêt pour le mot 'Paris' partout dans le monde", xlab="Temps", ylab="Intérêt")     # Plot de la série
points(YearMin, Min, col="blue", type = "l")                                                                   # Courbe minimum
points(YearMax, Max, col="red", type = "l")                                                                    # Courbe maximum


# Month-plot
#-----------
monthplot(Paris, main="Monthplot de 'Paris'", ylab="Paris")     # Month-plot de la série


# Lag-plot
#---------
lag.plot(Paris, lags=12, main="Lag-plot de 'Paris'")     # Lag-plot de la série


# Décomposition : modèle additive
#--------------------------------
ParisAdd = decompose(Paris, type="additive")     # Décomposition en modèle additif
plot(ParisAdd)                                   # Plot de la décomposition

plot(Paris, lwd=2, col="darkgrey",                                               # Plot de la série
     main="decompose() avec modèle additif",                                     # Titre du graphique
     xlab='Temps', ylab="Intérêt")                                               # Titre des axes
points(ParisAdd$trend, type='l', col="red")                                      # Tendance de la série
points(ParisAdd$trend+ParisAdd$seasonal, type='l', col='blue')                   # Courbe de la série simulée
legend('topright', col=c("darkgrey","red",'blue'), lwd=c(2,1,1), bg="white",     # Paramètres de la légende
       legend=c(expression(X[t]), expression(m[t]), expression(m[t]+s[t])))      # Texte de la légende


# Décomposition : modèle multiplicative
#--------------------------------------
ParisMult = decompose(Paris, type="multiplicative")     # Décomposition en modèle multiplicatif
plot(ParisMult)                                         # Plot de la décomposition

plot(Paris, lwd=2, col="darkgrey",                                               # Plot de la série
     main="decompose() avec modèle multiplicatif",                               # Titre du graphique
     xlab='Temps', ylab="Intérêt")                                               # Titre des axes
points(ParisMult$trend, type='l', col="red")                                     # Tendance de la série
points(ParisMult$trend*ParisMult$seasonal, type='l', col='blue')                 # Courbe de la série simulée
legend('topright', col=c("darkgrey","red",'blue'), lwd=c(2,1,1), bg="white",     # Paramètres de la légende
       legend=c(expression(X[t]), expression(m[t]), expression(m[t]*s[t])))      # Texte de la légende


#----------------------------------------------------------------------------------------------------------------
# Lissage exponentielle triple : méthode de Holt-Winters
#----------------------------------------------------------------------------------------------------------------
ParisHW_AAA = ets(Paris, model="AAA", ic="aic")      # Erreur : ADD,  tendance : ADD,  saison : ADD
ParisHW_MAdA = ets(Paris, model="MAA", ic="aic")     # Erreur : MULT, tendance : ADD,  saison : ADD
ParisHW_MAdM = ets(Paris, model="MAM", ic="aic")     # Erreur : MULT, tendance : MULT, saison : MULT
ParisHW_MMdM = ets(Paris, model="MMM", ic="aic")     # Erreur : MULT, tendance : MULT, saison : MULT

paste0("Modèle : ", ParisHW_AAA$method, ", AIC = ", round(ParisHW_AAA$aic, 3))
paste0("Modèle : ", ParisHW_MAdA$method, ", AIC = ", round(ParisHW_MAdA$aic, 3))
paste0("Modèle : ", ParisHW_MAdM$method, ", AIC = ", round(ParisHW_MAdM$aic, 3))
paste0("Modèle : ", ParisHW_MMdM$method, ", AIC = ", round(ParisHW_MMdM$aic, 3))

ParisPredHW_MAdM = forecast(ParisHW_MAdM, h=12)                                             # Prédiction sur 12 mois
plot(ParisPredHW_MAdM, xlim=c(2017,2020), ylim=c(25,50), xaxt="n")                          # Plot de la prédiction
axis(1, at=seq(2017,2020,1), labels=seq(2017,2020,1))                                       # Texte de l'abscisse
points(Paris2019, type="l", lwd=2, col="darkgreen")                                         # Vraies valeurs
abline(v=seq(2017,2020,1), col="red", lty="dotted")                                         # Lignes verticales
legend('topleft', col=c("black","darkgreen",'blue'), lwd=c(1,2,2), cex=0.8, bg="white",     # Paramètres de la légende
       legend=c("Anciennes valeurs","Vraies valeurs","Prédiction"))                         # Texte de la légende

ParisPredHW_MMdM = forecast(ParisHW_MMdM, h=12)                                             # Prédiction sur 12 mois
plot(ParisPredHW_MMdM, xlim=c(2017,2020), ylim=c(25,50), xaxt="n")                          # Plot de la prédiction
axis(1, at=seq(2017,2020,1), labels=seq(2017,2020,1))                                       # Texte de l'abscisse
points(Paris2019, type="l", lwd=2, col="darkgreen")                                         # Vraies valeurs
abline(v=seq(2017,2020,1), col="red", lty="dotted")                                         # Lignes verticales
legend('topleft', col=c("black","darkgreen",'blue'), lwd=c(1,2,2), cex=0.8, bg="white",     # Paramètres de la légende
       legend=c("Anciennes valeurs","Vraies valeurs","Prédiction"))                         # Texte de la légende


#----------------------------------------------------------------------------------------------------------------
# Modélisation ARMA/SARMA, ARIMA/SARIMA
#----------------------------------------------------------------------------------------------------------------
# Fonctions d'autocorrélation
#---------------------------
par(mfrow=c(1,2))
acf(Paris, type="covariance", main="Covariance de Paris")        # Covariance
acf(Paris, type="correlation", main="Corrélations de Paris")     # Corrélations
par(mfrow=c(1,1))


# Différenciation (éliminer saisonnalité et tendance)
#----------------------------------------------------
par(mfrow=c(1,3))
plot(diff(diff(Paris, lag=12), lag=1))                                              # Saisonnalité 12, tendance
acf(diff(diff(Paris, lag=12), lag=1), main="diff(diff(Paris, lag=12), lag=1)")      # ACF
pacf(diff(diff(Paris, lag=12), lag=1), main="diff(diff(Paris, lag=12), lag=1)")     # PACF
par(mfrow=c(1,1))


# Modélisation SARIMA
#--------------------
# SARIMA(2,1,2)(1,1,1)
ParisSARIMA = Arima(Paris, order=c(2,1,2), seasonal=c(1,1,1))     # SARIMA(2,1,2)(1,1,1)
summary(ParisSARIMA)
par(mfrow=c(1,2))
acf(ParisSARIMA$residuals, main="Résidus SARIMA(2,1,2)(1,1,1)")        # ACF
pacf(ParisSARIMA$residuals, main="Résidus SARIMA(2,1,2)(1,1,1)")       # PACF
par(mfrow=c(1,1))
Box.test(ParisSARIMA$residuals, lag=45)                                # Test de blancheur
t_stat(ParisSARIMA)                                                    # Significativité des coefficients
cor.arma(ParisSARIMA)                                                  # Autocorrélations

# SARIMA(2,1,1)(1,1,1)
ParisSARIMA = Arima(Paris, order=c(2,1,1), seasonal=c(1,1,1))     # SARIMA(2,1,2)(1,1,1)
summary(ParisSARIMA)
par(mfrow=c(1,2))
acf(ParisSARIMA$residuals, main="Résidus SARIMA(2,1,1)(1,1,1)")        # ACF
pacf(ParisSARIMA$residuals, main="Résidus SARIMA(2,1,1)(1,1,1)")       # PACF
par(mfrow=c(1,1))
Box.test(ParisSARIMA$residuals, lag=45)                                # Test de blancheur
t_stat(ParisSARIMA)                                                    # Significativité des coefficients
cor.arma(ParisSARIMA)                                                  # Autocorrélations

# SARIMA(2,1,0)(1,1,1)
ParisSARIMA = Arima(Paris, order=c(2,1,0), seasonal=c(1,1,1))     # SARIMA(2,1,2)(1,1,1)
summary(ParisSARIMA)
par(mfrow=c(1,2))
acf(ParisSARIMA$residuals, main="Résidus SARIMA(2,1,0)(1,1,1)")        # ACF
pacf(ParisSARIMA$residuals, main="Résidus SARIMA(2,1,0)(1,1,1)")       # PACF
par(mfrow=c(1,1))
Box.test(ParisSARIMA$residuals, lag=45)                                # Test de blancheur
t_stat(ParisSARIMA)                                                    # Significativité des coefficients
cor.arma(ParisSARIMA)                                                  # Autocorrélations

# SARIMA(2,1,0)(0,1,1)
ParisSARIMA = Arima(Paris, order=c(2,1,0), seasonal=c(0,1,1))     # SARIMA(2,1,2)(1,1,1)
summary(ParisSARIMA)
par(mfrow=c(1,2))
acf(ParisSARIMA$residuals, main="Résidus SARIMA(2,1,0)(0,1,1)")        # ACF
pacf(ParisSARIMA$residuals, main="Résidus SARIMA(2,1,0)(0,1,1)")       # PACF
par(mfrow=c(1,1))
Box.test(ParisSARIMA$residuals, lag=45)                                # Test de blancheur
t_stat(ParisSARIMA)                                                    # Significativité des coefficients
cor.arma(ParisSARIMA)                                                  # Autocorrélations

# Prédiction SARIMA
ParisPredSARIMA = forecast(ParisSARIMA, h=12)                                               # Prédiction sur 12 mois
plot(ParisPredSARIMA, xlim=c(2017,2020), ylim=c(25,50), xaxt="n")                           # Plot de la prédiction
axis(1, at=seq(2017,2020,1), labels=seq(2017,2020,1))                                       # Texte de l'abscisse
points(Paris2019, type="l", lwd=2, col="darkgreen")                                         # Vraies valeurs
abline(v=seq(2017,2020,1), col="red", lty="dotted")                                         # Lignes verticales
legend('topleft', col=c("black","darkgreen",'blue'), lwd=c(1,2,2), cex=0.8, bg="white",     # Paramètres de la légende
       legend=c("Anciennes valeurs","Vraies valeurs","Prédiction"))                         # Texte de la légende

# Modélisation automatique
#-------------------------
# SARIMA
ParisAUTO = auto.arima(Paris, ic="aic")                         # Choix automatique basé sur l'AIC
summary(ParisAUTO)
par(mfrow=c(1,2))
acf(ParisAUTO$residuals, main="Résidus SARIMA(2,1,1)(2,1,1)")        # ACF
pacf(ParisAUTO$residuals, main="Résidus SARIMA(2,1,1)(2,1,1)")       # PACF
par(mfrow=c(1,1))
Box.test(ParisAUTO$residuals, lag=45)                                # Test de blancheur
t_stat(ParisAUTO)                                                    # Significativité des coefficients
cor.arma(ParisAUTO)                                                  # Autocorrélations

# Prédiction
ParisPredAUTO = forecast(ParisAUTO, h=12)                                                   # Prédiction sur 12 mois
plot(ParisPredAUTO, xlim=c(2017,2020), ylim=c(25,50), xaxt="n")                             # Plot de la prédiction
axis(1, at=seq(2017,2020,1), labels=seq(2017,2020,1))                                       # Texte de l'abscisse
points(Paris2019, type="l", lwd=2, col="darkgreen")                                         # Vraies valeurs
abline(v=seq(2017,2020,1), col="red", lty="dotted")                                         # Lignes verticales
legend('topleft', col=c("black","darkgreen",'blue'), lwd=c(1,2,2), cex=0.8, bg="white",     # Paramètres de la légende
       legend=c("Anciennes valeurs","Vraies valeurs","Prédiction"))                         # Texte de la légende

# Comparaison au modèle SARIMA
plot(ParisPredSARIMA$mean, col="blue", lwd=2, ylim=c(26,41),                                # ParisPredSARIMA
     xaxt="n", xlab="Temps", ylab="Intérêt",                                                # Textes des axes
     main="Comparaison SARIMA vs. AUTO")                                                    # Titre
axis(1,at=seq(2019,2019.917,length.out=12),                                                 # Paramètres de l'abscisse
     labels=c("Jan.", "Fév.", "Mars",                                                       # Texte de l'abscisse
              "Avr.", "Mai", "Juin",                                                        # Texte de l'abscisse
              "Juil.", "Août", "Sep.",                                                      # Texte de l'abscisse
              "Oct.", "Nov.", "Déc."))                                                      # Texte de l'abscisse
polygon(x=c(seq(2019,2019.917,length.out=12), seq(2019.917,2019,length.out=12)),            # IC 80% ParisPredAUTO
        y=c(ParisPredAUTO$upper[1:12], ParisPredAUTO$lower[12:1]),                          # IC 80% ParisPredAUTO
        col=rgb(red=1, green=0, blue=0, alpha=0.05), border=FALSE)                          # IC 80% ParisPredAUTO
points(ParisPredAUTO$mean, type="l", col="red", lwd=2)                                      # ParisPredAUTO
points(ParisPredAUTO$upper, type="l", lty="dashed", col="red")                              # Max IC 80% ParisPredAUTO
points(ParisPredAUTO$lower, type="l", lty="dashed", col="red")                              # Min IC 80% ParisPredAUTO
polygon(x=c(seq(2019,2019.917,length.out=12), seq(2019.917,2019,length.out=12)),            # IC 80% ParisPredSARIMA
        y=c(ParisPredSARIMA$upper[1:12], ParisPredSARIMA$lower[12:1]),                      # IC 80% ParisPredSARIMA
        col=rgb(red=0, green=0, blue=1, alpha=0.05), border=FALSE)                          # IC 80% ParisPredSARIMA
points(ParisPredSARIMA$upper, type="l", lty="dashed", col="blue")                           # Max IC 80% ParisPredSARIMA
points(ParisPredSARIMA$lower, type="l", lty="dashed", col="blue")                           # Min IC 80% ParisPredSARIMA
legend("bottomleft", lwd=c(2,1,2,1), lty=c("solid","dashed","solid","dashed"), cex=0.8,     # Paramètres de la légende
       col=c("blue","blue","red","red"), bg="white",                                        # Paramètres de la légende
       legend=c("Mod. SARIMA - moyenne", "Mod. SARIMA - intervalle de confiance à 80%",     # Texte de la légende
                "Mod. AUTO - moyenne", "Mod. AUTO - intervalle de confiance à 80%"))        # Texte de la légende


#------------------------------------------------------------------------------------------------
# Choix de modèle
#------------------------------------------------------------------------------------------------
plot(Paris2019, lwd=2, xlim=c(2019,2019.917), xaxt="n",                     # Vraies valeurs
     main="Comparaison des modèles", xlab="Temps")                          # Titres
abline(v=seq(2019,2020,length.out=13), lty="dotted", col="grey")            # Lignes verticales
axis(1,at=seq(2019,2020,length.out=13),                                     # Textes de l'abscisse
     labels=c("Jan.", "Fév.", "Mars", 
              "Avr.", "Mai", "Juin", 
              "Juil.", "Août", "Sep.", 
              "Oct.", "Nov.", "Déc.", "Jan."))
points(ParisPredHW_MAdM$mean, type="l", col="red")                          # ParisPredHW_MAdM
points(ParisPredHW_MMdM$mean, type="l", col="green")                        # ParisPredHW_MMdM
points(ParisPredSARIMA$mean, type="l", col="blue")                          # ParisPredSARIMA
points(ParisPredAUTO$mean, type="l", col="cyan")                            # ParisPredAUTO
legend('topright', col=seq(1:5), lwd=c(2,1,1,1,1), cex=0.8, bg="white",     # Paramètres de la légende
       legend=c("Vraies valeurs",                                           # Texte de la légende
                "Holt-Winters (M,Ad,M)",
                "Holt-Winters (M,Md,M)",
                "SARIMA (2,1,0)(0,1,1)", 
                "SARIMA (2,1,1)(2,1,1)"))
