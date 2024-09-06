% The symbol on the left is used to make a comment in Prolog
/*
This is a multi-line comment.     
*/ 
happy(dja).
playsDota(clinton).
playsDota(dja).
% comma (',') = AND operator
winsDotaGame(dja) :- happy(dja), 
                     playsDota(dja).

/*
% The statements below are conditions for an OR statement (logical disjunction)       
*/           
% winsDotaGame(clinton) :- happy(clinton).
% winsDotaGame(clinton) :- playsDota(clinton).                    

/*
% another way to represent logical disjuntion or is by using a semicolon(';') instead of a period ('.')      
*/                  
winsDotaGame(clinton) :- happy(clinton);
                        playsDota(clinton).   