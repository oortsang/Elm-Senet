/* movesearch.c:  implement expectiminimax as well as several
 * heuristic algorithms like last-pawn (and also random)
 * mcts will be somewhere else
 *
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>


#include "board.h"
#include "logic.h"
#include "movesearch.h"


MoveState *newMS(GameState *gs) {
    MoveState *ms = malloc(sizeof(MoveState));
    memcpy(&ms->gs, gs, sizeof(GameState));
    ms->isEval = 0;
    return ms;
}

// Evaluates just a single depth at a time
// returns success status
int evalMS(MoveState *ms) {
    int success = 1;
    // be lazy!
    if (ms->isEval) return success;

    /* Player col = ms->gs.turn; */
    /* int pc   = col>0 ? ms->gs.blackPawnCount : ms->gs.whitePawnCount; */
    /* Pawn *ps = col>0 ? ms->gs.blackPawnList  : ms->gs.whitePawnList; */

    // find legal moves for each roll
    // run 0-4 even though roll val is 1-5 (correct by using r+1 for the roll)
    int r;
    for (r = 0; r < 5; r++) {
        int lmc;
        Pawn *tmpMoves = legalMoves(&ms->gs, (r+1), &lmc);
        ms->lmc[r] = lmc;
        memcpy(ms->moves[r], tmpMoves, lmc);

        // compute the resulting state for each possibility
        int i;
        for (i = 0; i < lmc; i++) {
            MoveState *tmpMS = newMS(&ms->gs);
            int worked = makeMove(&tmpMS->gs, tmpMoves[i], (r+1));
            ms->res[r][i] = tmpMS;

            if (!worked) {
                printf("Lackaday! Failed to make requested move (p:%d, r:%d)\n", tmpMoves[i], (r+1));
                success = 0;
            }
        }

        // cleanup time
        free(tmpMoves);
    }
    ms->isEval = 1;
    return success;
}

// copies everything except the resulting move states
MoveState *copyMS(MoveState *ms) {
    MoveState *newMS = malloc(sizeof(MoveState));
    memcpy(&newMS->gs, &ms->gs, sizeof(GameState));
    newMS->isEval = 0;
    if (!ms->isEval)
        return newMS;
    newMS->isEval = 1;
    memcpy(newMS->lmc,   ms->lmc,   5 * sizeof(int));
    memcpy(newMS->moves, ms->moves, 5 * InitPawnCount * sizeof(Pawn));
    memcpy(newMS->res,   ms->res,   5 * InitPawnCount * sizeof(MoveState *));
    return newMS;
}

// does not free any children
void freeMS(MoveState *ms) {
    if (ms) free(ms);
}


// frees all children except ``exception''
// ie, frees the MoveState Tree
void freeMST(MoveState *ms, MoveState *exception) {
    if (!ms)
        return;
    else if (!ms->isEval) {
        free(ms);
        return;
    }
    // for each roll/pawn combo
    int r, i;
    for (r = 0; r < 5; r++) {
        for (i = 0; i < ms->lmc[r]; i++) {
            // free resulting movestates
            // except for the exception
            if (ms->res[r][i] != exception)
                // delete all the children
                // won't delete exception or its children
                // if somehow there's a cycle in the move graph (wouldn't be a tree anymore)
                freeMST(ms->res[r][i], exception);
        }
    }
    free(ms);
}



// fetches the child associated with a move
MoveState *getChild(MoveState *ms, int p, int roll) {
    int r = roll-1;
    evalMS(ms);
    MoveState *res = NULL;
    int i;
    for (i = 0; i < ms->lmc[r]; i++) {
        if (ms->moves[r][i] == p) {
            res = ms->res[r][i];
            break;
        }
    }
    return res;
}

// gets the child associated with a move and removes everything else
// Note that this consumes the movestate ms!
MoveState *stepForward(MoveState *ms, int p, int roll) {
    MoveState *res = NULL;
    if (!ms->isEval) {
        // no need to free the children
        // just overwrite the current movestate
        makeMove(&ms->gs, p, roll);
        res = ms;
    } else {
        int r = roll-1;
        int i;
        for (i = 0; i < ms->lmc[r]; i++) {
            if (ms->moves[r][i] == p) {
                res = ms->res[r][i];
                break;
            }
        }

        freeMST(ms, res);
    }
    return res;
}


void printMS(MoveState *ms) {
    printf("***** Move state summary *****\n");
    printState(&ms->gs);
    if (!ms->isEval) {
        printf("Not further evaluated.\n");
    } else {
        printf("Evaluated (legal) moves:\n");
        int r, i;
        for (r=0; r<5; r++) {
            printf("\t Roll %d: ", (r+1));
            for (i=0; i < ms->lmc[r]; i++) {
                printf("%d%s", ms->moves[r][i], i+1==ms->lmc[r] ? "\n" : ", ");
            }
        }
    }
}
