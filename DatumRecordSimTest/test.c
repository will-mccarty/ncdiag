#include <stdio.h>
#include <time.h>
#include <sys/time.h>

#define DIM_X 10
#define DIM_Y 10
#define ITERS 100000000

int getRecord(int **record, int x, int y) {
    return record[x][y];
}

// DATUM FETCHING
// 0 - 100
// If I select (3,4), the correct position would be:
// * * * * * * * * * *
// * * * * * * * * * *
// * * * * * * * * * *
// * * * * * * * * * *
// * * * X * * * * * * (index 44 - 1 = 43)
// * * * * * * * * * *
// * * * * * * * * * *
// * * * * * * * * * *
// * * * * * * * * * *
// * * * * * * * * * *

int getDatum(int *datum, int x, int y) {
    //printf("DEBUG: datum access x=%i and y=%i, res index=%i\n", x, y, (DIM_X * y) + x);
    return datum[DIM_X * y + x];
}

int main() {
    int i, j, k, l;
    int **arrRecord;
    int *arrDatum;
    
    // Build and populate 100 element datum array
    k = 0;
    
    arrDatum = (int) malloc(sizeof(int) * (DIM_X * DIM_Y));
    
    for (i = 0; i < (DIM_X * DIM_Y); i++) {
        arrDatum[i] = k++;
    }
    
    // Build and populate 10x10 element record array
    arrRecord = (int **) malloc(sizeof(int *) * DIM_X);
    
    for (i = 0; i < DIM_X; i++) {
        arrRecord[i] = (int *) malloc(sizeof(int) * DIM_Y);
    }
    
    k = 0;
    
    for (i = 0; i < DIM_X; i++) {
        for (j = 0; j < DIM_Y; j++) {
            arrRecord[i][j] = k++;
        }
    }
    
    //////////////////////////////////////////////////////////////////
    
    struct timeval tval_before, tval_after, tval_result;
    gettimeofday(&tval_before, NULL);
    
    int tmp;
    
    // Record timing
    for (k = 0; k < ITERS; k++) {
        l = 0;
        for (i = 0; i < DIM_X; i++) {
            for (j = 0; j < DIM_Y; j++) {
                tmp = getRecord(arrRecord, i, j);
                if (tmp != l)
                    printf("[RECORD] ERROR: Invalid value! %i vs correct %i\n", tmp, l);
                l++;
            }
        }
    }
    
    gettimeofday(&tval_after, NULL);
    timersub(&tval_after, &tval_before, &tval_result);
    printf("[RECORD] Time elapsed: %ld.%06ld\n", (long int)tval_result.tv_sec, (long int)tval_result.tv_usec);
    
    /////////////////////////////////////////////////////////////////
    gettimeofday(&tval_before, NULL);
    
    // Datum timing
    for (k = 0; k < ITERS; k++) {
        l = 0;
        for (j = 0; j < DIM_Y; j++) {
            for (i = 0; i < DIM_X; i++) {
                tmp = getDatum(arrDatum, i, j);
                if (tmp != l)
                    printf("[DATUM] ERROR: Invalid value! %i vs correct %i\n", tmp, l);
                l++;
            }
        }
    }
    
    gettimeofday(&tval_after, NULL);
    timersub(&tval_after, &tval_before, &tval_result);
    printf("[DATUM]  Time elapsed: %ld.%06ld\n", (long int)tval_result.tv_sec, (long int)tval_result.tv_usec);
}
