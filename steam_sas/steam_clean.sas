/* last edited = 16/02/2025 19:35PM*/

/* FIX LES DATES RAJOUTER COLONNES MOIS ANNEE
FAIRE LES COLONNES GENRES, CATEGORIE */
/* 16-02 le problème est que les variables des cat1à23 ne sont pas les mêmes */
/* ex: cat2 i=1  =/ cat2 i=2 */
%let max_num_categories = 23;
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
		'Required age'n Name AppID Tags );
		
;

    
    /* Créer un tableau pour stocker les catégories extraites */
    array genre{*} $50 genre1-genre23; /* genre1 à genre23 */
    
    /* Créer un tableau pour stocker les indicateurs (1 ou 0) */
    array indicator{23} cat1-cat23; /* cat1 à cat23 */
    
    /* Initialiser les variables du tableau à vide */
    do i = 1 to &max_num_categories; /* Boucle de 1 à max_num_categories */
        genre{i} = '';
        indicator{i} = 0; /* Initialiser les indicateurs à 0 */
    end;
    
    /* Extraire les catégories et mettre à jour les indicateurs */
    i = 1;
    do while (scan(Categories, i, ',') ne ''); /* Boucle tant qu'il y a des catégories à extraire */
        genre{i} = scan(Categories, i, ',');
        indicator{i} = 1; /* Mettre à jour l'indicateur à 1 si la catégorie est présente */
        i = i + 1;
    end;
    
    drop i; /* Supprimer la variable temporaire */






    

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
Average_Review = (Positive / (Positive + Negative));
Supported_Languages = countw('Supported Languages'n);
Release_Date = input('Release date'n, ANYDTDTE15.);
Release_Year = year(Release_Date); 
num_categories = countw(categories, ',');
num_genres = countw(genres, ',');
num_tags = countw(tags, ',');


format Average_Review percent.2 Release_Date MMYYS.;

run;

proc sql;
    select max(num_categories) as max_num_categorie
    from games;
quit;

proc print data=games (obs = 10);
run;

%macro freq_tables;
    %do i = 1 %to 23; /* Boucle de 1 à 23 */
        proc freq data=games;
            tables cat&i / list missing; /* Exécute PROC FREQ pour cat&i */
        run;
    %end;
%mend freq_tables;

%freq_tables; /* Appel de la macro */

