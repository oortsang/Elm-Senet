/* main.c for CSenet by Oliver Tsang, Summer 2020
 *
 * C implementation of Senet following Kendall's rules
 * meant for more extensive AI testing
 *
 *
 * Kendall's Rules:
 *   https://www.startwithabook.org/content/pdfs/EgyptianSenetGame.pdf
 *   http://www.gamecabinet.com/history/Senet.html
 */

#include <stdio.h>
#include <stdlib.h>
#include "main.h"
#include "board.h"
#include "logic.h"


int main (int argc, char **argv) {
    int isDone  = 0;
    int moveNum = 0;
    int roll;
    int pawnSquare;

    GameState *gs = initGame();


    char *str = boardToString(gs);
    printf(str);

    while (!isDone) {
        printState(gs);
        printf("Move %d. Roll: ", 1+(int)moveNum/2);
        scanf("%d", &roll);

        printf("Legal moves:\n");
        int mc;
        Pawn *moves = legalMoves(gs, roll, &mc);
        printPawnList(moves, mc);
        free(moves);

        printf("\nPawn on square: ");
        scanf("%d", &pawnSquare);

        int leg = makeMove(gs, pawnSquare, roll);
        printf("\nThat was%s legal\n", leg ? "" : " not");


        isDone = isOver(gs);
        if (leg)
            moveNum++;
    }
    return 0;
}
