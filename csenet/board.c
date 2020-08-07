/* board.c for CSenet by Oliver Tsang, Summer 2020
 *
 * C representation of the board
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include "board.h"

/* Pawn functions */
void printPawnList(Pawn *list, int len) {
    int i;
    printf("[");
    for (i = 0; i < len; i++) {
        printf("%d%s", list[i], i<len-1 ? ", " : "");
    }
    printf("]\n");
}


/* Square and Board functions */
SquareState *initBoard(int ipc) {
    /* SquareState board[30]; */
    SquareState *board = malloc(30*sizeof(SquareState));
    int i;
    for (i = 0; i < 2*ipc; i++) {
        board[i] = (i%2) ? BLACK : WHITE;
    }
    memset(board+2*ipc, 0, 30-2*ipc);
    return board;
}

GameState *initGame() {
    SquareState *board = initBoard(InitPawnCount);
    Pawn whitePawns[InitPawnCount];
    Pawn blackPawns[InitPawnCount];
    int i;
    for (i = 0; i < InitPawnCount; i++) {
        whitePawns[i] = 2*i;
        blackPawns[i] = 2*i+1;
    }

    GameState *gs = malloc(sizeof(GameState));
    gs->turn = BLACK;
    memcpy(gs->board, board, 30*sizeof(SquareState));
    memcpy(gs->whitePawnList, whitePawns, InitPawnCount*sizeof(Pawn));
    memcpy(gs->blackPawnList, blackPawns, InitPawnCount*sizeof(Pawn));
    gs->whitePawnCount = InitPawnCount;
    gs->blackPawnCount = InitPawnCount;
    free(board);

    return gs;
}

GameState *copyGame(GameState *gs) {
    GameState *js = malloc(sizeof(GameState));
    memcpy(js, gs, sizeof(GameState));
    return js;
}


void freeGame(GameState *gs) {
    // the arrays inside are fixed-length and local
    if (gs) free(gs);
}


// get the type
SquareType squareType(int sq) {
    SquareType res;
    switch (sq) {
    case 14: res = Rebirth; break;
    case 25: res = Happy;   break;
    case 26: res = Water;   break;
    case 27: res = Truths;  break;
    case 28: res = Reatoum; break;
    case 29: res = Horus;   break;
    default: res = Reg;     break;
    }
    return res;
}

// get the character representation
char squareToChar(int i, SquareState s) {
    // get the character representation
    char square[3] =  {'P', '.', 'Q'};
    char c = ' ';

    if (s == 0) {
        switch (squareType(i)) {
        case Reg:     c = '.'; break;
	case Rebirth: c = ','; break;
	case Happy:   c = '&'; break;
	case Water:   c = 'w'; break;
	case Truths:  c = '3'; break;
	case Reatoum: c = '2'; break;
        case Horus:   c = '.'; break;
        }
    } else {
        c = square[1+s];
    }
    return c;
}

char *boardToString(GameState *gs) {
    char out[72];
    char c;
    int i, j, k, m;
    int n = sprintf(out, "Board:\n");
    for (i = 0; i < 3; i++) {
        m = 0;
        char line[22];
        if (i%2==0) {
            for (j = 0; j < 10; j++) {
                k = 10*i+j;
                c = squareToChar(k, gs->board[k]);
                m += sprintf(line+m, " %c", c);
            }
        } else {
            for (j = 9; j >= 0; j--) {
                k = 10*i+j;
                c = squareToChar(k, gs->board[k]);
                m += sprintf(line+m, " %c", c);
            }
        }
        line[20] = '\n';
        line[21] = '\0';
        n += snprintf(out+n, 22, line);
    }
    out[71] = '\0';
    char *res = malloc(72*sizeof(char));
    memcpy(res, out, 72);
    return res;
}

void printState(GameState *gs) {
    printf("~~~ Game State Summary ~~~\n");
    printf("Turn: %s\n", gs->turn == BLACK ? "Black" : "White");
    printf("Black pawns (%d): ", gs->blackPawnCount);
    printPawnList(gs->blackPawnList, gs->blackPawnCount);
    printf("White pawns (%d): ", gs->whitePawnCount);
    printPawnList(gs->whitePawnList, gs->whitePawnCount);
    printf("Board:\n");
    char c;
    int i, j, k;
    for (i = 0; i < 3; i++) {
        if (i%2==0) {
            for (j = 0; j < 10; j++) {
                k = 10*i+j;
                c = squareToChar(k, gs->board[k]);
                printf(" %c", c);
            }
        } else {
            for (j = 9; j >= 0; j--) {
                k = 10*i+j;
                c = squareToChar(k, gs->board[k]);
                printf(" %c", c);
            }
        }
        printf("\n");
    }
    printf("\n");
}


Player isOver(GameState *gs) {
    if (!gs->whitePawnCount) {
        return WHITE;
    } else if (!gs->blackPawnCount) {
        return BLACK;
    }
    return NEITHER;
}
