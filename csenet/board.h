/* board.h: header file for board.c
 * Mostly copied from Board.elm
 */



#ifndef _BOARD_H_
#define _BOARD_H_

/* Square and Board definitions */


/* Convention: +1 is Black; -1 is White */
typedef char SquareState;

typedef enum {
    Reg,
    Rebirth,
    Happy,
    Water,
    Truths,
    Reatoum,
    Horus,
} SquareType;

// get the square type
SquareType squareType(int sq);

// get a character representation
char squareToChar(int i, SquareState s);


/* Convention: +1 is Black; -1 is White */
typedef int Player;

/* Pawn definitions */

/* typedef struct { */
/*     char square; */
/*     Player col; */
/* } Pawn; */

typedef char Pawn;

#define InitPawnCount 7

/* Game State definitions */
typedef struct {
    SquareState board[30];
    Player turn;
    int whitePawnCount;
    int blackPawnCount;
    Pawn whitePawnList[InitPawnCount];
    Pawn blackPawnList[InitPawnCount];
} GameState;

GameState *initGame();

GameState *copyGame(GameState *gs);

void freeGame(GameState *gs);

// print the representation
char *boardToString(GameState *gs);

// print directly
void printState(GameState *gs);

/* Game outcome definitions */
typedef enum {
    NotDone,
    WhiteWon,
    BlackWon
} Outcome;

Outcome isOver(GameState *gs);

#endif
