/* logic.c: does most of the heavy-lifting with regards to move legality
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "board.h"


/* swap pawns on squares pn and qn */
void pawnSwapper(GameState *gs, int pn, int qn) {
    /* check whether we need to fix the pawn lists */
    int i;
    for (i = 0; i < gs->blackPawnCount; i++) {
        if (gs->blackPawnList[i] == pn)
            gs->blackPawnList[i] = qn;
        else if (gs->blackPawnList[i] == qn)
            gs->blackPawnList[i] = pn;
    }
    for (i = 0; i < gs->whitePawnCount; i++) {
        if (gs->whitePawnList[i] == pn)
            gs->whitePawnList[i] = qn;
        else if (gs->whitePawnList[i] == qn)
            gs->whitePawnList[i] = pn;
    }

    /* do the actual swap */
    SquareState tmp = gs->board[pn];
    gs->board[pn] = gs->board[qn];
    gs->board[qn] = tmp;
}


/* sets square to 0 */
void clearSquare(GameState *gs, int square) {
    /* printf("Clearing square %d\n", square); */
    gs->board[square] = 0;
}

/* find pawn on square sq in list plist of length pcount */
int findPawn(Pawn *plist, int pcount, int sq) {
    int i;
    for (i = 0; i < pcount; i++) {
        if (plist[i] == sq)
            return i;
    }
    return -1;
}

/* remove the pawn on square pn if possible; otherwise return 0 */
/* then clear it from the records */
int removePawn(GameState *gs, int pn) {
    SquareState sq = gs->board[pn];
    int i;
    int pcount;
    Pawn *plist;
    if (sq == 0) {
        return 0;
    } else if (sq > 0) {
        // black
        plist = gs->blackPawnList;
        i = findPawn(plist, gs->blackPawnCount--, pn);
        pcount = gs->blackPawnCount;
    } else {
        plist = gs->whitePawnList;
        i = findPawn(plist, gs->whitePawnCount--, pn);
        pcount = gs->whitePawnCount;
    }
    if (i == -1)
        return 0;

    // now swap the pawn to the end of the list
    int tmp;
    tmp = plist[pcount];
    plist[pcount] = plist[i];
    plist[i] = tmp;

    clearSquare(gs, plist[pcount]);
    return 1;
}


/* find the last square open by square 14 */
int rebirthSquare(GameState *gs) {
    int i;
    for (i = 14; i >= 0; i--)
        if (gs->board[i] == 0)
            return i;
    return 0; // mathematically impossible for all of these to be filled
}

/* send back pawns of player col */
/* return the number of pawns sent back */
int sendBack(GameState *gs, Player col) {
    int i;
    int tot = 0;
    for (i = 26; i < 30; i++) {
        if (gs->board[i] == col) {
            int dst = rebirthSquare(gs);
            pawnSwapper(gs, dst, i);
            tot++;
        }
    }
    return tot;
}

/* send back pawns of player col */
/* return the number of pawns sent back */
int sendBackExcept(GameState *gs, Player col, int exception) {
    int i;
    int tot = 0;
    for (i = 26; i < 30; i++) {
        if (gs->board[i] == col && i != exception) {
            int dst = rebirthSquare(gs);
            pawnSwapper(gs, dst, i);
            tot++;
        }
    }
    return tot;
}

/* change the turn in the struct */
void switchTurn(GameState *gs) {
    gs->turn *= -1; // slick, eh?
}

/* send back relevant pawns then switch turn */
void skipTurn(GameState *gs) {
    sendBack(gs, gs->turn); // can store the output if desired
    switchTurn(gs);
}


int legalBySquareType(SquareType sq, int m) {
    if (sq == Truths || sq == Reatoum)
        return m == 30;
    return 1;
}

/* check whether the proposed move is legal */
/* basically the same as makeMove but without any modifications to gs */
int isLegal(GameState *gs, int n, int roll) {
    int m = n + roll;
    int skip = n == -1;
    int legalSqType = legalBySquareType(squareType(n), m);
    int skippedHappiness = n < 25 && m > 25;
    int attemptedLeave = m >= 30;
    int pawnCol = gs->board[n];

    int legal;

    if (skip) {
        legal = 1; // don't actually check...
    } else if (pawnCol != gs->turn) {
        legal = 0;
    } else if (!legalSqType) {
        // legal = 0;
        // act like a skipped turn
        legal = 1; // allow people to try but to get sent back immediately
    } else if (skippedHappiness) {
        legal = 0;
    } else if (attemptedLeave) {
        legal = 1;
    } else {
        int waterSendBack = squareType(m) == Water;
        int sameColor = gs->board[m] == gs->turn;

        if (waterSendBack) {
            legal = 1;
        } else {
            if (!sameColor) {
                legal = 1;
            } else {
                legal = 0;
            }
        }
    }
    return legal;
}

/* perform the move; return whether it was successful */
int makeMove(GameState *gs, int n, int roll) {
    /* GameState *backup = copyGame(gs); */
    int m = n + roll;
    int skip = n == -1;
    int legalSqType = legalBySquareType(squareType(n), m);
    int skippedHappiness = n < 25 && m > 25;
    int attemptedLeave = m >= 30;
    int pawnCol = gs->board[n];

    int legal;

    if (skip) {
        legal = 1; // don't actually check...
    } else if (pawnCol != gs->turn) {
        legal = 0;
    } else if (!legalSqType) {
        // legal = 0;
        // act like a skipped turn
        legal = 1; // allow people to try but to get sent back immediately
    } else if (skippedHappiness) {
        legal = 0;
    } else if (attemptedLeave) {
        legal = 1;
        removePawn(gs, n);
    } else {
        int waterSendBack = squareType(m) == Water;
        int sameColor = gs->board[m] == gs->turn;

        if (waterSendBack) {
            legal = 1;
            int dst = rebirthSquare(gs);
            pawnSwapper(gs, n, dst);
            switchTurn(gs);
        } else {
            if (!sameColor) {
                legal = 1;
                pawnSwapper(gs, n, m);
            } else {
                legal = 0;
            }
        }

        // check for neglected pieces
        if (legal)
            sendBackExcept(gs, gs->turn, m);
    }

    if (legal)
        switchTurn(gs);

    /* freeGame(backup); */
    return legal;
}


// return a list of all legal moves
// len is an outparameter to indicate the length
// *guaranteed to return a nonempty list with 1<=len<=InitPawnCount
Pawn *legalMoves(GameState *gs, int roll, int *len) {
    Pawn *pawns =
        gs->turn > 0
        ? gs->blackPawnList
        : gs->whitePawnList;
    int pcount =
        gs->turn > 0
        ? gs->blackPawnCount
        : gs->whitePawnCount;

    Pawn locMoves[InitPawnCount];
    int mc = 0; // moveCount

    int i;
    for (i = 0; i < pcount; i++) {
        if (isLegal(gs, pawns[i], roll)) {
            locMoves[mc++] = pawns[i];
        }
    }

    Pawn *moveList;
    if (mc) {
        moveList = malloc(mc * sizeof(Pawn));
        memcpy(moveList, locMoves, mc*sizeof(Pawn));
        *len = mc;
    } else {
        moveList = malloc(sizeof(Pawn));
        moveList[0] = (Pawn) -1; //ie, no move available
        *len = 1;
    }
    return moveList;
}


void printLegalMoves(GameState *gs, int roll) {
    int mc;
    Pawn *moves = legalMoves(gs, roll, &mc);
    printPawnList(moves, mc);
    free(moves);
}


// check whether two states are the same
int cmpState(GameState *gs, GameState *js) {
    int turn   = gs->turn == js->turn;
    int wpawns = gs->whitePawnCount == js->whitePawnCount;
    int bpawns = gs->blackPawnCount == js->blackPawnCount;
    int board  = !memcmp(gs->board, js->board, 30*sizeof(SquareState));

    return turn && wpawns && bpawns && board;
}
