/* Log-transform and clip features */
data games_log;
    set games;
    log_positive = min(log(1 + positive), 100);
    log_peak_ccu = min(log(1 + peak_ccu), 100);
run;

/* Standardize*/
proc standard data=games_log mean=0 std=1 out=games_scaled;
    var log_positive log_peak_ccu;
run;

/* Multinomial logistic regression */
proc logistic data=games_scaled;
    class estimated_owners (param=ref);
    model estimated_owners = log_positive log_peak_ccu / link=glogit;
    output out=logit_out predprobs=I;
run;

/* VIF to check multicollinearity */
proc reg data=games_scaled;
    model log_positive = log_peak_ccu / vif;
run;

proc reg data=games_scaled;
    model log_peak_ccu = log_positive / vif;
run;

/* Generate predicted class */
data pred_vs_true;
    set logit_out;
    pred_class = I_estimated_owners;
run;

/* Confusion matrix */
proc freq data=pred_vs_true;
    tables estimated_owners * pred_class / nopercent norow nocol;
run;

/* Compute accuracy */
data accuracy;
    set pred_vs_true;
    correct = (estimated_owners = pred_class);
run;

proc means data=accuracy mean;
    var correct;
run;
