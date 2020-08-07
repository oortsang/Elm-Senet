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
#include "movesearch.h"

int human(GameState *gs, Move lastMove, int roll) {
    printf("Legal moves:\n");
    printLegalMoves(gs, roll);

    int pawnSquare;
    printf("\nPawn on square: ");
    scanf("%d", &pawnSquare);

    return pawnSquare;
}


int main (int argc, char **argv) {
    int isDone  = 0;
    int moveNum = 0;
    int roll;
    int pawnSquare;
    Move lastMove = {0, -1};

    GameState *gs = initGame();


    /* char *str = boardToString(gs); */
    /* printf(str); */

    MoveState *ms = newMS(gs);

    while (!isDone) {
        /* printState(gs); */
        evalMS(ms);
        printMS(ms);


        printf("Move %d. Roll: ", 1+(int)moveNum/2);
        scanf("%d", &roll);

        pawnSquare = human(gs, lastMove, roll);

        int leg = makeMove(gs, pawnSquare, roll);
        printf("\nThat was%s legal\n", leg ? "" : " not");

        // update...
        lastMove = (Move) {roll, pawnSquare};
        ms = stepForward(ms, pawnSquare, roll);


        isDone = isOver(gs);
        if (leg)
            moveNum++;
    }
    return 0;
}
