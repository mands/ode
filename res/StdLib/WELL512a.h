/* ***************************************************************************** */
/* Copyright:      Francois Panneton and Pierre L'Ecuyer, University of Montreal */
/*                 Makoto Matsumoto, Hiroshima University                        */
/* Notice:         This code can be used freely for personal, academic,          */
/*                 or non-commercial purposes. For commercial purposes,          */
/*                 please contact P. L'Ecuyer at: lecuyer@iro.UMontreal.ca       */
/* ***************************************************************************** */
#ifndef WELL512A_H
#define WELL512A_H

extern void InitWELLRNG512a(const uint32_t* const restrict init);
extern double WELLRNG512a(void);

#endif // WELL512A_H
