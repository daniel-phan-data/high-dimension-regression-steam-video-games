#### EXECUTER 7 ml avant 8hypotheses_lm.R

# function to check lm assumptions
check_lm_hypotheses <- function(model, data) {
    cat("🔍 Vérification des hypothèses pour le modèle :", deparse(model$call), "\n\n")
    
    # hypothese 1: relation de linearite entre Y et X
    # le nuage de points doit etre centre autour de 0 sans motif evident
    # sinon la relation n est pas bien modelise, non-linearite possible
    plot(model, which = 1, main = "1. Résidus vs valeurs ajustées")
    
    # hypothese 2: homoscedasticite des erreurs
    # ecart type constant peu importe la valeur ajustee
    # entonnoir = heteroscedasticite
    plot(model, which = 3, main = "3. Écarts à l'effet de levier")
    
    # residus absolus vs valeurs ajustees
    # si c'est croissant/decroissant, la variance n est pas constante
    data$residu_abs <- abs(residuals(model))
    data$ajuste <- fitted(model)
    ggplot(data, aes(x = ajuste, y = residu_abs)) +
        geom_point(alpha = 0.4) +
        geom_smooth(method = "loess", col = "red") +
        labs(title = "4. Hétéroscédasticité : résidus absolus vs ajustés",
             x = "Valeurs ajustées", y = "Résidus absolus") +
        theme_minimal() -> p1
    print(p1)
    
    # hypothese 3: independance des erreurs
    # test de Durbin-Watson : attend une valeur proche de 2
    # si proche de 0 : autocorrelation positive. si > 2.5 : autocorrelation negative   
    cat("\nTest de Durbin-Watson (attendu ≈ 2) :\n")
    print(dwtest(model))
    dwtest(modele.RLM, alternative = c("two.sided"))
    # ACF : si les barres depassent, il y a autocorrelation des erreurs
    acf(residuals(model), main = "5. ACF des résidus")
    
    # hypothese 4: normalite des erreurs
    # ca doit suivre la ligne, sinon residus anormaux
    plot(model, which = 2, main = "2. QQ-plot des résidus")
    residus <- model$residuals
    #boxplot des residus
    boxplot(residus, main = "Boxplot des résidus")
    #histogramme "naif"
    hist(residus[residus < quantile(residus, 1)], breaks = 50,
         main = "Histogramme des résidus",
         xlab = "Résidus", col = "green", border = "green")
    #histogramme plus lisible en retirant le top 1% qui casse tout
    hist(residus[residus < quantile(residus, 0.99)], breaks = 50,
         main = "Histogramme des résidus (sans top 1%)",
         xlab = "Résidus (censurés à 99%)", col = "blue", border = "blue")
    
    # hypothese 5 multicolinearite
    # VIF (Variance Inflation Factor) : mesure le lien entre les variables explicatives
    # VIF > 5 = multicolinearite moderee VIF > 10 = multicolinearite severe
    cat("\nVIF (Variance Inflation Factor) :\n")
    vif_vals <- vif(model)
    print(vif_vals)
    cat("\nVariables avec VIF > 5 :\n")
    print(names(vif_vals[vif_vals > 5]))
    
    # observations influentes: distances de Cook
    # indique si certaines observations influencent beaucoup le modele
    # attention aux points au dessus de la ligne rouge
    cooks <- cooks.distance(model)
    seuil <- 4 / nrow(data)
    cat("\nObservations influentes (Cook > 4/n) :\n")
    # influents <- which(cooks > seuil)
    # print(influents)
    
    plot(cooks, type = "h",
         main = "7. Distance de Cook avec seuil 4/n",
         ylab = "Distance de Cook", xlab = "Index de l'observation")
    abline(h = seuil, col = "red", lty = 2, lwd = 2)
    legend("topright", legend = paste0("Seuil = 4/n ≈ ", round(seuil, 5)),
           col = "red", lty = 2, lwd = 2)
}

## exemple d utilisation de check_lm_hypotheses ----
# modele naif pour le test
names(gamesc)
Y <- "Average.playtime.forever"
X <- c("Peak.CCU", "Positive", "Negative", "Recommendations", "Price", "Required.age")
categories <- c("Estimated.owners")
variables <- c(Y, X, categories)  # Combined variables list
print(variables)
modele.RLM <- create_lm(gamesc, Y, X, categories)
summary(modele.RLM)

#test fonction hypothese
check_lm_hypotheses(modele.RLM, gamesc)
