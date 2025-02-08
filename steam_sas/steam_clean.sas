/* FIX LES DATES RAJOUTER COLONNES MOIS ANNEE
FAIRE LES COLONNES GENRES, CATEGORIE */


%let datapath = '/home/u64112561/ML_STEAM/games.csv';
proc import datafile= "&datapath"
	dbms = csv out=STEAM_GAMES
	replace;
	guessingrows=4000;
run;

DATA GAMES;
set STEAM_GAMES(
drop = 'About the game'n 'Average playtime two weeks'n 
		'Median playtime two weeks'n
		'Full audio languages'n
		Developers 'Header image'n
		'Metacritic url'n
		Notes Movies Screenshots 
		'Support email'n 'Support url'n
		Website 'Peak CCU'n
		 Reviews 'Score rank'n Achievements Publishers
		 'DLC count'n 'User score'n 
		 'Metacritic Score'n
		'Required age'n Name AppID);
		
;

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
Average_SalesRevenue_Estimated = Price * Average_Estimated;
Average_Review = (Positive / (Positive + Negative));
Supported_Languages = countw('Supported Languages'n);
Release_Date = input('Release date'n, ANYDTDTE15.);
Release_Year = year(Release_Date); 
num_genres = countw(genres, ',');


format Average_Review percent.2 Release_Date MMYYS.;



run;



