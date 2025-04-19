#download the dataset from the link in the repo, then modify the pathfile.
pathfile = "/home/u64124783/Projet Model Linéaire/games.csv" 
proc import datafile = pathfile
	dbms = csv out=STEAM_GAMES
	replace;
	guessingrows=4000;
run;