DATA GAMES;
set STEAM_GAMES(rename=(
        'Average playtime forever'n = Average_playtime_forever
        'Peak CCU'n = Peak_CCU
        'Required age'n = Required_age
    ));
    keep Average_playtime_forever Peak_CCU 
          Price Recommandations Required_age 
         Positive Negative total_reviews Positive_Ratio categories;
/* Remplace les valeurs True et False par des valeurs numériques */
    if Windows = 'True' Then Windows = input(1, BEST12.);
    else if Windows ='False' then Windows = input(0, BEST12.) ;
    if Linux = 'True' Then Linux = input(1, BEST12.);
    else if Linux ='False' then Linux = input(0, BEST12.) ;
    if Mac = 'True' Then Mac = input(1, BEST12.);
    else if Mac ='False' then Mac = input(0, BEST12.) ;


Supported_Plateform = Windows + Mac + Linux;
Estimated_Low = scan('Estimated owners'n, 1, '-');
Estimated_High = scan('Estimated owners'n, 2, '-');
Average_Estimated = Mean(Estimated_Low, Estimated_High);
Sales_EstimatedHigh = Price * Estimated_Low;
Sales_EstimatedAverage = Price * Average_Estimated;
Sales_EstimatedLow = Price * Estimated_High;
Total_Reviews = Positive + Negative;
Positive_Ratio = Positive  / Total_Reviews;
Average_Review = (Positive / (Positive + Negative));
Supported_Languages = countw('Supported Languages'n);
Release_Year = year(Release_Date); 

if Average_Playtime_Forever > 0 ;
run;