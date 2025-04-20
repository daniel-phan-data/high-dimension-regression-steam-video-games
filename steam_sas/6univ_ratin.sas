proc freq data=games;
	tables rating_levels / out=rating_freq;
run;

proc sgplot data=rating_freq;
	vbar rating_levels / response=percent stat=sum datalabel;
	yaxis label="Proportion (%)";
	xaxis discreteorder=data;
	title "Proportion of Game Ratings";
run;