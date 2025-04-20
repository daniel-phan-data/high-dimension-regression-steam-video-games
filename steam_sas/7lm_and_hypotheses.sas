/*Résidus vs valeurs ajustées, normalité, homoscédasticité, influence */
proc reg data=games;
    model Y = average_playtime_forever estimated_owners peak_ccu price recommendations required_age positive negative / 
        vif 
        r 
        influence 
        dwProb 
        collin; /* collin pour détection de multicolinéarité */
    output out=reg_out
        r=residus 
        student=student_res 
        cookd=cook_dist 
        h=lev 
        p=valeurs_ajustees;
run;

/*QQ-plot (normalité des résidus)*/
proc univariate data=reg_out normal;
    var residus;
    qqplot residus / normal(mu=est sigma=est);
run;

/*Homoscédasticité : résidus vs ajustés*/
proc sgplot data=reg_out;
    scatter x=valeurs_ajustees y=residus;
    refline 0 / axis=y lineattrs=(pattern=shortdash);
run;

/*Studentized residuals, leverage, Cook’s distance*/
proc sgplot data=reg_out;
    scatter x=lev y=student_res;
run;

proc sgplot data=reg_out;
    scatter x=_N_ y=cook_dist;
    refline %sysevalf(4 / %sysfunc(countw(%str(ton_dataset)))) / axis=y lineattrs=(color=red);
run;



