#spearman (corrélation non paramétrique)
proc corr data=games spearman;
    _all_;
run;

#Test de Kruskal-Wallis (quantitative vs catégorielle)
proc npar1way data=games wilcoxon edf;
    class groupe;          /* variable catégorielle */
    var valeur;            /* variable quantitative */
run;

#Test de Lilliefors (normalité)
