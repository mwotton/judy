#include <Judy.h>

/* void *hs_judy_pointer_error(void) { return PJERR; } */

void hs_judyl_free(void *ptr) { JudyLFreeArray(ptr, PJE0); }

