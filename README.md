#CLISP CODE FOR COMPUTATIONAL INTELLIGENCE LAB NITC 2018
Author Mathew George

tictactoeminimax.lsp

	Implement the two player game Tic-Tac-Toe Using the Minimax search algorithm with fixed ply look ahead.

tictactoealphabeta.lsp
	
	Implement the two player game Tic-Tac-Toe Using the Minimax search algorithm with alpha beta pruning and fixed ply look ahead.

nqueens.lsp

	Implement N-Queens problem .
        The program should print all the board configurations of size NXN such that N Queens can be placed in the board without any conflict.(The value N should be given as input)

2sat.lsp
	
	Write a program that can find models for propositional well formed formulas given in CNF form with every clause having two terms (e.g. (AѵB)ᴧ(B ѵ┐D) ᴧ (CѵB)).  If no model exists it should return "unsatisfiable".

fwgc.lsp

	Solve the Farmer, Wolf, Goat and Cabbage Problem. 
	(Note: A farmer with his wolf, goat and cabbage come to the bank of a river which they wish to cross. There is a  boat on the bank of the river and the farmer knows to row it. But, the boat can only carry two things at a time . If the wolf is ever left alone with the goat, the wolf will eat the goat; similarly the goat will eat the cabbage if they are left alone.  The program should output a sequence of steps using which  the farmer can safely cross the river with his belongings) 

waterjug.lsp

	Solve the Water Jug Problem. 
	(Note: The problem is explained in the class. The program is expected to output the sequences of steps required to produce the required amount of water in the jug specified. The capacities of the jugs and the goal state should be given as input). 
	For searching the state space please try to use the following algorithms for the above two problems: 
		a) Depth First Search 
		b) Breadth first Search 

8puzzle.lsp

	Implement the 8-Puzzle problem using Best first search.(Use an admissible heuristic so that your algorithm becomes A*). 