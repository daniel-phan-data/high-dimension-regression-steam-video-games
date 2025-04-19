
data univariate;
set GAMES;
keep Average_playtime_forever Peak_CCU 
         rating Price Recommandations Required_age 
         Positive Negative total_reviews Positive_Ratio estimated_owners;
run;

proc univariate 
data = univariate;
histogram _All_; 
run;




%let varlist=Average_playtime_forever Peak_CCU 
             rating Price Recommandations Required_age 
             Positive Negative total_reviews Positive_Ratio;

%macro plot_vars;
    %let i = 1;
    %let var = %scan(&varlist, &i, %str( ));

    %do %while(%length(&var) > 0);
        proc plot data=univariate hpercent=50;
            plot &var * Average_playtime_forever  / vspace=1;
            title "Plot de &var en fonction de rating";
        run;

        %let i = %eval(&i + 1);
        %let var = %scan(&varlist, &i, %str( ));
    %end;
%mend;

%plot_vars;

