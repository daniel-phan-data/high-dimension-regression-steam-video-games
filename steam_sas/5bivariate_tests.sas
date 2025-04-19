#spearman 
proc corr data=games spearman;
    _all_;
run;

#Kurskal-Wallis
proc npar1way data=games wilcoxon edf;
    class groupe;          /* variable catégorielle */
    var valeur;            /* variable quantitative */
run;

