/* FIX LES DATES RAJOUTER COLONNES MOIS ANNEE
FAIRE LES COLONNES GENRES, CATEGORIE */

%let filepath = /home/u64124783/Projet Model Linéaire/games.csv

proc import datafile= "&filepath"
	dbms = csv out=STEAM_GAMES
	replace;
	guessingrows=700;
run;

DATA GAMESDROPPED;
set STEAM_GAMES;
drop  'About the game'n 'Average playtime two weeks'n 
		'Median playtime two weeks'n
		'Full audio languages'n
		Developers 'Header image'n
		'Metacritic url'n
		Notes Movies Screenshots 
		'Support email'n 'Support url'n
		Website 'Peak CCU'n
		Name Reviews 'Score rank'n Achievements Publishers
		'Estimated owners'n 'DLC count'n 'User score'n 
		Positive Negative 'Metacritic Score'n
		'Required age'n
		
;
/* Remplace les valeurs True et False par des valeurs numériques */
    if Windows = 'True' Then Windows = input(1, BEST12.);
    else if Windows ='False' then Windows = input(0, BEST12.) ;
    if Linux = 'True' Then Linux = input(1, BEST12.);
    else if Linux ='False' then Linux = input(0, BEST12.) ;
    if Mac = 'True' Then Mac = input(1, BEST12.);
    else if Mac ='False' then Mac = input(0, BEST12.) ;


Supported_Plateform = Windows + Mac + Linux;
Estimated_Low = input(scan('Estimated owners'n,1,'-'), best12.);
Estimated_High = input(scan('Estimated owners'n,2,'-'), best12.);
Average_Estimated = Mean(Estimated_Low, Estimated_High);
Average_SalesRevenue_Estimated = Price * Average_Estimated;
Average_Review = (Positive / (Positive + Negative));
Supported_Languages = countw('Supported Languages'n);
Release_Month = input(scan(Release_Date,1,'/'), best12.);
Release_Year = input(scan(Release_Date,2,'/'), best12.); 
num_genres = countw(genres, ',');
Release_Date = input('Release date'n, ANYDTDTE15.);

format Average_SalesRevenue_Estimated dollar12.2 Average_Review percent.2 
Release_Date MMYYS. Price dollar12.2 Release_Month MONTH. Release_Year YEARS.;

run;

/* CODE REALISER PAR : NATHAN OUSALEM */

