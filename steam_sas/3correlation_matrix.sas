ods graphics on;

proc corr data=cleaned_games spearman plots=matrix(histogram);
    var _numeric_;
run;



proc corr data=cleaned_games spearman nosimple outp=corr_out;
    var _numeric_;
run;

proc sgplot data=corr_out;
    heatmapparm x=variable y=_name_ color=r;
run;

ods graphics off 
