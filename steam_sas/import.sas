proc import datafile= "/home/u64124783/Projet Model Linéaire/games.csv"
	dbms = csv out=STEAM_GAMES
	replace;
	guessingrows=4000;
run;