/* movesearch.h: header file for logic.c
 * store the basics for implementing expectiminimax
 *
 */

#include "board.h"
#include "logic.h"

#ifndef _MOVESEARCH_H_
#define _MOVESEARCH_H_

struct MoveState;

typedef struct MoveState {
    // Simplify memory management and ensure we don't accidentally free
    // the main copy of gs
    GameState gs;

    // Indicates whether the rest of the fields mean anything
    int isEval;
    // legal move count for each roll
    // at least 1 -- can be to skip turn
    int lmc[5];

    // Legal moves (as pawns)
    // index 0: roll; index 1: pawn choice
    Pawn moves[5][InitPawnCount];

    // Array with pointers to the resulting states
    struct MoveState *res[5][InitPawnCount];
} MoveState;


// MoveState basic functions
MoveState *newMS(GameState *gs);
int evalMS(MoveState *ms); // returns 1 if successful
MoveState *copyMS(MoveState *ms);
void freeMS(MoveState *ms);
// free the MoveState Tree except for the movestate we want to keep
void freeMST(MoveState *ms, MoveState *exception);

// Tree exploration functions
// fetches the child associated with a move
MoveState *getChild(MoveState *ms, int p, int roll);
// gets the child associated with a move and removes everything else
MoveState *stepForward(MoveState *ms, int p, int roll);

// Printing! Visualization!
void printMS(MoveState *ms);


// heuristic value function for use with expectiminimax
double gsLeafVal(GameState *gs, Player col);

// evaluate the state with ply ``ply''
double evalState(MoveState *ms, Player col, int ply);

// do the same but for a specified roll (save some computation)
// but returns the value as an outparameter
// and returns the chosen pawn
int emmChoose(MoveState *ms, int roll, Player col, int ply, double *val);

int lastPawn(MoveState *ms, int roll);

int randPawn(MoveState *ms, int roll);

#endif
