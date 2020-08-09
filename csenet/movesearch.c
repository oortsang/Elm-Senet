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
        /* printf("Has been evaluated.\n"); */
        printf("Evaluated (legal) moves:\n");
        int r, i;
        for (r=0; r<5; r++) {
            printf("\t Roll %d: ", (r+1));
            for (i=0; i < ms->lmc[r]; i++) {
                printf("%d%s", ms->moves[r][i], i+1==ms->lmc[r] ? "\n" : ", ");
            }
        }
    }
    printf("\n");
}

// Evaluates from the perspective of Black
// then corrects by flipping the sign for White
double gsLeafVal(GameState *gs, Player col) {
    int colCorrection = (int) col;

    double winBounty = 1000;
    double pawnBounty = 20;
    double pawnAdv = pawnBounty * (gs->whitePawnCount - gs->blackPawnCount);

    int rebirth = rebirthSquare(gs);
    int result = isOver(gs);
    if (result) {
        return colCorrection * result * winBounty;
    }

    double sum = pawnAdv;
    int i;
    for (i = 0; i < 30; i++) {
        int sq  = gs->board[i];
        int sgn = -sq;
        double val;

        // get the value of the square given the contents
        switch (squareType(i)) {
        case Happy:
            val = sgn * -3;
            break;
        case Horus:
            val = sgn * -pawnBounty; // as if promoted
            break;
        case Reatoum:
            // 3/4 success, 1/4 failure
            val = sgn * ( 3/4 * (30-rebirth) \
                        - 1/4 * pawnBounty);
            break;
        case Truths:
            // 3/8 success, 5/8 failure
            val = sgn * ( 3/8 * (30-rebirth) \
                        - 5/8 * pawnBounty);
            break;
        default:
            if (i==24) val = sgn * 8.5; // make it a bit worse than square 22
            else       val = sgn * (30-i);
        }
        sum += val;
    }
    return colCorrection * sum;
}

// helper function to find the min or max of the list
// returns the index of the argument for minimax
int idxMM(double *vals, int lmc, int isMax) {
    if (!lmc) return -1; // shouldn't happen though

    int sgn = isMax ? 1 : -1; // find max of sgn*val then correct at the end

    double optVal  = sgn * vals[0];  // the max or min value encountered so far
    int i, k = 0;
    for (i = 0; i < lmc; i++) {
        double tmp = sgn * vals[i];
        if (optVal <= tmp) {
            optVal = tmp;
            k = i;
        }
    }
    return k;
}

// take the average weighted by the probability of each roll
double weightedAvg(double vals[5]) {
    return vals[0] * 1/4
         + vals[1] * 3/8
         + vals[2] * 1/4
         + vals[3] * 1/16
         + vals[4] * 1/16;
}


// recursively evaluate the state of gs
double evalState(MoveState *ms, Player col, int ply) {
    if (ply == 0) return gsLeafVal(&ms->gs, col);

    int isMax = col == ms->gs.turn;
    evalMS(ms);
    int r, i, k;
    double rvals [5];
    for (r = 0; r < 5; r++) {
        double pvals[InitPawnCount];
        for (i = 0; i < ms->lmc[r]; i++) {
            MoveState *next = getChild(ms, ms->moves[r][i], (r+1));
            pvals[i] = evalState(next, col, ply-1);
        }
        k = idxMM(pvals, ms->lmc[r], isMax);
        rvals[r] = pvals[k];
    }
    return weightedAvg(rvals);
}

// choose the pawn (expectiminimax) based on board values from evalState
int emmChoose(MoveState *ms, int roll, Player col, int ply, double *val) {
    // shouldn't happen
    if (ply == 0) {
        fprintf(stderr, "Oh no! evalRolledState called with ply 0!!\n");
        return -2;
    }
    int isMax = col==ms->gs.turn;
    double vals[InitPawnCount];
    int r = roll-1;

    /* printf("EMM is considering...\n"); */
    evalMS(ms);
    int i;
    for (i = 0; i < ms->lmc[r]; i++) {
        MoveState *next = getChild(ms, ms->moves[r][i], roll);
        vals[i] = evalState(next, col, ply-1);
        /* printf("Pawn %d with val %.2f\n", ms->moves[r][i], vals[i]); */
    }
    int k = idxMM(vals, ms->lmc[r], isMax);
    /* printf("I've decided to choose option %d of %d (i.e., pawn %d)\n\n", k, ms->lmc[r], ms->moves[r][k]); */
    if (val) *val = vals[k];
    return (int) ms->moves[r][k];
}


