/* board.h: header file for board.c
 * Mostly copied from Board.elm
 */



#ifndef _BOARD_H_
#define _BOARD_H_

/* Square and Board definitions */

#define BLACK (1)
#define WHITE (-1)
#define NEITHER (0)

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

typedef char Pawn;

void printPawnList(Pawn *list, int len);

#define InitPawnCount 7

/* Game State definitions */
typedef struct {
    SquareState board[30];
    Player turn;
    int blackPawnCount;
    int whitePawnCount;
    Pawn blackPawnList[InitPawnCount];
    Pawn whitePawnList[InitPawnCount];
} GameState;

GameState *initGame();

GameState *copyGame(GameState *gs);

void freeGame(GameState *gs);

// print the representation
char *boardToString(GameState *gs);

// print directly
void printState(GameState *gs);

// 1  if black won
// -1 if white won
// 0 or false if the game is still going
Player isOver(GameState *gs);

// for compact passing of play information
typedef struct {
    int  roll;
    Pawn pawn;
} Move;

#endif
