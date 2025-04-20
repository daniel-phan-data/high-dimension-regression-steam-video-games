/* ---------------------------------------------------------------------------------
   Instructions:
   1. Download the `games.csv` file from the Git repository.
   2. Upload it manually into SAS Studio (right-click > Upload).
   3. Go to the Files panel > right-click on the file > Properties > copy the path.
   4. Paste this path into the "pathfile" macro variable below.
   --------------------------------------------------------------------------------- */

/* TO DO: modify the path below with the path found in Properties */
%let pathfile = '/home/u64112561/ml_steam/data/games.csv';

proc import datafile = "&pathfile"
    dbms = csv
    out = steam_games
    replace;
    guessingrows = 4000;
run;
