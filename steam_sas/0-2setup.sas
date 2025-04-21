/*  run this script once per session*/

DATA GAMES;
set STEAM_GAMES(rename=(
        'Average playtime forever'n = Average_playtime_forever
        'Peak CCU'n = Peak_CCU
        'Required age'n = Required_age
        'Estimated owners'n = estimated_owners
    ));
    keep Average_playtime_forever Peak_CCU 
          Price Recommendations Required_age 
         Positive Negative total_reviews Positive_Ratio rating_levels Estimated_owners ;
 length estimated_owners $10;

Total_Reviews = Positive + Negative;
Positive_Ratio = Positive  / Total_Reviews;
Average_Review = (Positive / (Positive + Negative));
Supported_Languages = countw('Supported Languages'n);
Release_Year = year(Release_Date); 

if Average_Playtime_Forever > 0 ;


if total_reviews > 0 then
        positive_ratio = (Positive / total_reviews) * 100;
    else 
        positive_ratio = .;

    length rating_levels $30;

    if total_reviews >= 500 then do;
        if 95 <= positive_ratio <= 100 then rating_levels = "Overwhelmingly Positive";
        else if 80 <= positive_ratio < 95 then rating_levels = "Very Positive";
        else if 70 <= positive_ratio < 80 then rating_levels = "Mostly Positive";
        else if 40 <= positive_ratio < 70 then rating_levels = "Mixed";
        else if 20 <= positive_ratio < 40 then rating_levels = "Mostly Negative";
        else if 0 <= positive_ratio < 20 then rating_levels = "Overwhelmingly Negative";
    end;
    else if 50 <= total_reviews < 500 then do;
        if 80 <= positive_ratio <= 100 then rating_levels = "Very Positive";
        else if 70 <= positive_ratio < 80 then rating_levels = "Mostly Positive";
        else if 40 <= positive_ratio < 70 then rating_levels = "Mixed";
        else if 20 <= positive_ratio < 40 then rating_levels = "Mostly Negative";
        else if 0 <= positive_ratio < 20 then rating_levels = "Very Negative";
    end;
    else if 10 <= total_reviews < 50 then do;
        if 80 <= positive_ratio <= 100 then rating_levels = "Positive";
        else if 70 <= positive_ratio < 80 then rating_levels = "Mostly Positive";
        else if 40 <= positive_ratio < 70 then rating_levels = "Mixed";
        else if 20 <= positive_ratio < 40 then rating_levels = "Mostly Negative";
        else if 0 <= positive_ratio < 20 then rating_levels = "Negative";
    end;
    else rating_levels = "Not enough reviews";



    
  if Estimated_owners = "0 - 20000" then estimated_owners = "0-20k";
    else if Estimated_owners = "20000 - 50000" then estimated_owners = "20k-50k";
    else if Estimated_owners = "50000 - 100000" then estimated_owners = "50k-100k";
    else if Estimated_owners = "100000 - 200000" then estimated_owners = "100k-200k";
    else if Estimated_owners = "200000 - 500000" then estimated_owners = "200k-500k";
    else if Estimated_owners = "500000 - 1000000" then estimated_owners = "500k-1M";
    else if Estimated_owners = "1000000 - 2000000" then estimated_owners = "1M-2M";
    else if Estimated_owners = "2000000 - 5000000" then estimated_owners = "2M-5M";
    else if Estimated_owners = "5000000 - 10000000" then estimated_owners = "5M-10M";
    else if Estimated_owners = "10000000 - 20000000" then estimated_owners = "10M-20M";
    else if Estimated_owners = "20000000 - 50000000" then estimated_owners = "20M-50M";
    else if Estimated_owners = "50000000 - 100000000" then estimated_owners = "50M-100M";
    else if Estimated_owners = "100000000 - 200000000" then estimated_owners = "100M-200M";
run;
