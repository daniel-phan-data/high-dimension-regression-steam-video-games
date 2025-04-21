/*Résidus vs valeurs ajustées, normalité, homoscédasticité, influence */
proc reg data=games;
    model average_playtime_forever =  peak_ccu price recommendations required_age positive negative / 
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

data reg_out_num;
    set reg_out;
    obs_id = _N_;
run;

proc sql noprint;
    select count(*) into :n_obs from reg_out;
quit;

proc sgplot data=reg_out_num;
    scatter x=obs_id y=cook_dist;
    refline %sysevalf(4 / &n_obs) / axis=y lineattrs=(color=red thickness=2 pattern=shortdash);
    title "Cook's Distance per Observation";
run;



