proc freq data=gamesc;
    tables rating / out=rating_freq;
run;

proc sgplot data=rating_freq;
    vbar rating / response=percent stat=sum datalabel;
    yaxis label="Proportion (%)";
    xaxis discreteorder=data;
    title "Proportion of Game Ratings";
run;
