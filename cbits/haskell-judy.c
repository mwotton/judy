#include <Judy.h>

/* #include <stdio.h> */

/* void *hs_judy_pointer_error(void) { return PJERR; } */

void hs_judyl_free(void *ptr) {
    JudyLFreeArray(ptr, PJE0);
}

