

data univariatelog10;
    set GAMES;
    log_Average_playtime_forever = log10(Average_playtime_forever + 1);
    log_Peak_CCU = log10(Peak_CCU + 1);
    log_Positive = log10(Positive + 1);
    log_Negative = log10(Negative + 1);
    log_Recommendations = log10(Recommendations + 1);
    log_Price = log10(Price + 1);
run;


    %let i = 1;
    %let var = %scan(&varlist, &i, %str( ));

    %do %while(%length(&var) > 0);
        proc plot data=univariatelog10 hpercent=50;
            plot &var * log_Average_playtime_forever  / vspace=1;
            title "Plot de &var en fonction de rating";
        run;

        %let i = %eval(&i + 1);
        %let var = %scan(&varlist, &i, %str( ));
    %end;


