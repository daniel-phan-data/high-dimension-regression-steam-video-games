/* Step 1: Compute Spearman correlation matrix */
proc corr data=GAMES spearman nosimple noprint outp=corr_out;
    var _numeric_;
run;

/* Step 2: Sort before transpose */
proc sort data=corr_out(where=(_TYPE_="CORR")) out=corr_sorted;
    by _NAME_;
run;

proc transpose data=corr_sorted out=corr_long name=ColumnVar;
    by _NAME_;
run;

/* Step 3: Create a heatmap of the correlations 
red = high correlation
white = mid correlation
blue = low correlation */
proc sgplot data=corr_long noautolegend;
    heatmapparm x=ColumnVar y=_NAME_ colorresponse=COL1 /
        colormodel=(blue white red)
        outline;
    xaxis discreteorder=data display=(nolabel);
    yaxis discreteorder=data display=(nolabel);
    title "Spearman Correlation Matrix (Numeric Variables)";
run;
