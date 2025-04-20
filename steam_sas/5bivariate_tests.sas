/*spearman (corrélation non paramétrique) */
proc corr data=games spearman;
    var _numeric_;
run;

/*Test de Kruskal-Wallis (quantitative vs catégorielle)*/
proc npar1way data=games wilcoxon edf;
    class estimated_owners;          /* variable catégorielle */
    var _numeric_;            /* variable quantitative */
run;

/*Test de Kruskal-Wallis (quantitative vs catégorielle)*/
proc npar1way data=games wilcoxon edf;
    class rating_levels;          /* variable catégorielle */
    var _numeric_;            /* variable quantitative */
run;


/*Test de normalité*/
proc univariate data=games normal;
    var _numeric_;
run;

