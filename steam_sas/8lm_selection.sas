proc glmselect data=games outdesign=design;
   class estimated_owners; /*variables catégorielles*/
   model average_playtime_forever = estimated_owners peak_ccu price recommendations required_age positive negative / selection=none;
run;

/*combinaisons de variables de niveau 1 */
proc glmselect data=games;
   model average_playtime_forever = peak_ccu price recommendations required_age positive negative
         / selection=forward(select=aic) details=all;
run;


/* Sélection pas à pas avec AIC et jeu de validation (30 %) */
proc glmselect data=games;
   partition fraction(validate=0.3); /* 30 % des données pour la validation */
   model average_playtime_forever = peak_ccu price recommendations required_age positive negative
         / selection=stepwise(select=aic choose=validate stop=none) details=all;
run;
