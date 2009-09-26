// Sample program to show how to use Judy as a collision
// handler within a Hash table.
//
// cc -DHASHSIZE=256 hash.c ..

#include <stdlib.h>
#include <stdint.h>
#include <stdio.h>
#define JUDYERROR_SAMPLE 1 // use default Judy error handler
#include <Judy.h>

// Needed for timing routines
#include <sys/time.h>

// Start of timing routines ========================================

struct timeval TBeg, TEnd;

#define STARTTm gettimeofday(&TBeg, NULL)
#define ENDTm gettimeofday(&TEnd, NULL)
#define DeltaUSec \
    ( ((double)TEnd.tv_sec * 1000000.0 + (double)TEnd.tv_usec) \
    - ((double)TBeg.tv_sec * 1000000.0 + (double)TBeg.tv_usec) )

// End of timing routines ========================================
// Define Hash table size if not in compile line ===================
// Set HASHSIZE 1 for straight Judy

#ifndef HASHSIZE
#define HASHSIZE (1 << 8) // hash table size 256
#endif

// Seed for pseudo-random counter ==================================

#define INITN 123456 // first Index to store

static uint32_t // Placed here for INLINE possibility
Random(uint32_t Seed) // produce 2^32 -1 numbers by different counting
{
    if ((int32_t)Seed < 0) { Seed += Seed; Seed ^= 16611; }
    else { Seed += Seed; }
    return(Seed);
}

// Hash Table ======================================================

Pvoid_t JArray[HASHSIZE] = { NULL }; // Declare static hash table

int main(int argc, char *argv[])
{
    Word_t Count;
    Word_t Index;
    Word_t *PValue;
    Word_t NumIndexes = 10000; // default first parameter

    if (argc > 1) NumIndexes = strtoul(argv[1], NULL, 0);
    // Load up the CPU cache for small measurements:

    for (Count = 0; Count < HASHSIZE; Count++) JArray[Count] = NULL;
    printf("Begin storing %lu random numbers in a Judy scalable hash array\n",
        NumIndexes);
    Index = INITN;
    STARTTm;
    for (Count = 0; Count < NumIndexes; Count++)
    {
        Index = Random(Index);
        JLI(PValue, JArray[Index % HASHSIZE], Index/HASHSIZE);
        *PValue += 1; // bump count of duplicate Indexes
    }
    ENDTm;

    printf("Insertion of %lu indexes took %6.3f microseconds per index\n",
        NumIndexes, DeltaUSec/NumIndexes);
    Index = INITN; // start the same number sequence over
    STARTTm;

    for (Count = 0; Count < NumIndexes; Count++)
    {
        Index = Random(Index);
        JLG(PValue, JArray[Index % HASHSIZE], Index/HASHSIZE);
        if (*PValue != 1)
            printf("%lu dups of %lu\n", *PValue - 1, Index);
    }
    ENDTm;
    printf("Retrieval of %lu indexes took %6.3f microseconds per index\n",
    NumIndexes, DeltaUSec/NumIndexes);
    return(0);
}
