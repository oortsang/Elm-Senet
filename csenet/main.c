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
#include <time.h>
#include <string.h>
#include <unistd.h>

#include "main.h"
#include "board.h"
#include "logic.h"
#include "movesearch.h"

int verbose = 0;


long timediff(clock_t t1, clock_t t2) {
    // https://stackoverflow.com/questions/17250932/how-to-get-the-time-elapsed-in-c-in-milliseconds-windows
    long elapsed;
    elapsed = ((double)t2 - t1) / CLOCKS_PER_SEC * 1000;
    return elapsed;
}

int randomRoll() {
    int r = rand() % 16;
    int res;
    if      (r < 1)  res = 5;
    else if (r < 5)  res = 1;
    else if (r < 11) res = 2;
    else if (r < 15) res = 3;
    else             res = 4;

    return res;
}

int humanRoll() {
    int roll;
    printf("Enter the roll: ");
    scanf("%d", &roll);
    return roll;
}


int human(MoveState *ms, int roll) {
    if (verbose >=2) {
        printf("Legal moves:\n");
        printLegalMoves(&ms->gs, roll);
    }

    int pawnSquare;
    printf("\nPawn on square: ");
    scanf("%d", &pawnSquare);

    return pawnSquare;
}

int emm2(MoveState *ms, int roll) {
    int p = emmChoose(ms, roll, ms->gs.turn, 2, NULL);
    if (verbose >= 2) {
        printf("Expectiminimax (ply 2)... ");
        printf(" choosing pawn %d\n", p);
    }
    return p;
}

int emm4(MoveState *ms, int roll) {
    int p = emmChoose(ms, roll, ms->gs.turn, 4, NULL);
    if (verbose >= 2) {
        printf("Expectiminimax (ply 4)... ");
        printf(" choosing pawn %d\n", p);
    }
    return p;
}


int main (int argc, char **argv) {
    int isDone  = 0;
    int moveNum = 0;
    int roll;
    int pawnSquare;
    /* Move lastMove = {0, -1}; */

    time_t start, end, t;

    // seed the random number generator
    // uses nanoseconds for randomness within a second
    struct timespec spec;
    clock_gettime(CLOCK_REALTIME, &spec);
    srand((unsigned) time(&t) * spec.tv_nsec);

    GameState *gs = initGame();
    MoveState *ms = newMS(gs);

    /* Choose player 1 and player 2 */
    // human, emm2, emm4, (coming soon: randpawn, lastpawn)
    int (* pBlack) (MoveState *, int) = emm2;
    int (* pWhite) (MoveState *, int) = emm2;



    // read in arguments
    // usage: senet -v0 --pb human --pw emm2
    // for verbosity level 0, human for black, and emm2 for white
    int j;
    for (j = 0; j < argc; j++) {
        if (!strcmp(argv[j], "-v0")) {
            verbose = 0;
        } else if (!strcmp(argv[j], "-v1")) {
            verbose = 1;
        } else if (!strcmp(argv[j], "-v2")) {
            verbose = 2;
        }

        else if (!strcmp(argv[j], "--pb") && j+1 < argc) {
            char *ptype = argv[j+1];
            if (!strcmp(ptype, "human")) {
                pBlack = human;
            } else if (!strcmp(ptype, "emm2")) {
                pBlack = emm2;
            } else if (!strcmp(ptype, "emm4")) {
                pBlack = emm4;
            } else if (!strcmp(ptype, "last")) {
                pBlack = lastPawn;
            } else if (!strcmp(ptype, "rand")) {
                pBlack = randPawn;
            }
        } else if (!strcmp(argv[j], "--pw") && j+1 < argc) {
            char *ptype = argv[j+1];
            if (!strcmp(ptype, "human")) {
                pWhite = human;
            } else if (!strcmp(ptype, "emm2")) {
                pWhite = emm2;
            } else if (!strcmp(ptype, "emm4")) {
                pWhite = emm4;
            } else if (!strcmp(ptype, "last")) {
                pWhite = lastPawn;
            } else if (!strcmp(ptype, "rand")) {
                pWhite = randPawn;
            }

        }
    }


    // Start the game!
    while (!isDone) {
        evalMS(ms);

        roll = randomRoll(); // or humanRoll()

        if (verbose)
            printf("Move %d. Roll: %d\n", 1+(int)moveNum/2, roll);

        if (verbose >= 2) printMS(ms);
        else if (verbose == 1) printState(gs);

        // Decision
        start = clock();

        /* pawnSquare = emm2(ms, roll); */
        if (gs->turn == Black) {
            pawnSquare = (*pBlack)(ms, roll);
        } else {
            pawnSquare = (*pWhite)(ms, roll);
        }

        end = clock();
        if (verbose)
            printf("Thinking took: %ld ms\n\n", timediff(start, end));


        // Update the data structures
        int leg = makeMove(gs, pawnSquare, roll);
        if (!leg) {
            fflush(stdout);

            fprintf(stderr, "That was%s legal\n", leg ? "" : " not");
            fprintf(stderr, "Attempted move: (r:%d, p:%d)\n", roll, pawnSquare);

            return -1;
        }
        /* lastMove = (Move) {roll, pawnSquare}; */

        ms = stepForward(ms, pawnSquare, roll);

        isDone = isOver(gs);
        if (leg) moveNum++;
    }

    if (isDone == 1) printf("Black won!\n");
    else             printf("White won!\n");

    printState(gs);

    return 0;
}
