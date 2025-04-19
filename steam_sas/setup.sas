DATA GAMES;
set STEAM_GAMES(rename=(
        'Average playtime forever'n = Average_playtime_forever
        'Peak CCU'n = Peak_CCU
        'Required age'n = Required_age
    ));
    keep Average_playtime_forever Peak_CCU 
          Price Recommandations Required_age 
         Positive Negative total_reviews Positive_Ratio ;


Total_Reviews = Positive + Negative;
Positive_Ratio = Positive  / Total_Reviews;
Average_Review = (Positive / (Positive + Negative));
Supported_Languages = countw('Supported Languages'n);
Release_Year = year(Release_Date); 

if Average_Playtime_Forever > 0 ;
run;

data GAMES;
set games ;
if total_reviews > 0 then
        positive_ratio = (Positive / total_reviews) * 100;
    else 
        positive_ratio = .;

    length review_label $30;

    if total_reviews >= 500 then do;
        if 95 <= positive_ratio <= 100 then review_label = "Overwhelmingly Positive";
        else if 80 <= positive_ratio < 95 then review_label = "Very Positive";
        else if 70 <= positive_ratio < 80 then review_label = "Mostly Positive";
        else if 40 <= positive_ratio < 70 then review_label = "Mixed";
        else if 20 <= positive_ratio < 40 then review_label = "Mostly Negative";
        else if 0 <= positive_ratio < 20 then review_label = "Overwhelmingly Negative";
    end;
    else if 50 <= total_reviews < 500 then do;
        if 80 <= positive_ratio <= 100 then review_label = "Very Positive";
        else if 70 <= positive_ratio < 80 then review_label = "Mostly Positive";
        else if 40 <= positive_ratio < 70 then review_label = "Mixed";
        else if 20 <= positive_ratio < 40 then review_label = "Mostly Negative";
        else if 0 <= positive_ratio < 20 then review_label = "Very Negative";
    end;
    else if 10 <= total_reviews < 50 then do;
        if 80 <= positive_ratio <= 100 then review_label = "Positive";
        else if 70 <= positive_ratio < 80 then review_label = "Mostly Positive";
        else if 40 <= positive_ratio < 70 then review_label = "Mixed";
        else if 20 <= positive_ratio < 40 then review_label = "Mostly Negative";
        else if 0 <= positive_ratio < 20 then review_label = "Negative";
    end;
    else review_label = "Not enough reviews";
run;