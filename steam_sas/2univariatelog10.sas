ods graphics on;

data univariatelog10;
	set GAMES;
	log_Average_playtime_forever=log10(Average_playtime_forever + 1);
	log_Peak_CCU=log10(Peak_CCU + 1);
	log_Positive=log10(Positive + 1);
	log_Negative=log10(Negative + 1);
	log_Recommendations=log10(Recommendations + 1);
	log_Price=log10(Price + 1);
run;

%let varlist = log_Peak_CCU log_Positive log_Negative log_Recommendations log_Price;

%macro plot_vars;
	%let i = 1;
	%let var = %scan(&varlist, &i, %str( ));

	%do %while(%length(&var) > 0);
		title "Plot of &var against log_Average_playtime_forever";

		proc sgplot data=univariatelog10;
			scatter x=&var y=log_Average_playtime_forever;
			reg x=&var y=log_Average_playtime_forever / lineattrs=(color=red);
		run;

		%let i = %eval(&i + 1);
		%let var = %scan(&varlist, &i, %str( ));
	%end;
%mend;

%plot_vars;
ods graphics off;