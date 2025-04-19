#spearman (corrélation non paramétrique)
proc corr data=games spearman;
    _numeric_;
run;

#Test de Kruskal-Wallis (quantitative vs catégorielle)
proc npar1way data=games wilcoxon edf;
    class estimated_owners rating_levels;          /* variable catégorielle */
    var _numeric;            /* variable quantitative */
run;

#Test de normalité
proc univariate data=games normal;
    _numeric_;
run;

