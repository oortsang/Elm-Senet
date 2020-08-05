/* logic.h: header file for logic.c
 * Mostly copied from Logic.elm; contains the game logic
 *
 */

#include "board.h"


#ifndef _LOGIC_H_
#define _LOGIC_H_
// swap pawns on pn and qn
/* void pawnSwapper(GameState *gs, int pn, int qn); */

// helper function that empties the square
/* void clearSquare(GameState *gs, int square); */

// remove the pawn from the board and pawn list
// returns success status
/* int removePawn(GameState *gs, int pn); */

// finds the last square open by square 14
/* int rebirthSquare(GameState *gs); */

// send back pawns of player col
/* int sendBack(GameState *gs, Player col); */

// switch the turn color
/* void switchTurn(GameState *gs); */

// send back relevant pawns then switch turn
void skipTurn(GameState *gs);


// p==-1 to skip turn
// returns whether it worked
int makeMove(GameState *gs, int p, int roll);


#endif
