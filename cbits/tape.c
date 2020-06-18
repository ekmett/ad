#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <stdio.h>

typedef struct
{
  int idx;
  int offset;
  int size;
  double* val;
  int* lnk;
} tape_t;

int tape_offset(void *p)
{
  tape_t *pTape = (tape_t*)p;
  return pTape->offset;
}

void* tape_alloc(int offset, int size)
{
  tape_t *pTape = malloc( sizeof(tape_t) );
  pTape->size = size;
  pTape->idx = 0;
  pTape->offset = offset;
  pTape->val = malloc( size*sizeof(double) );
  pTape->lnk = malloc( size*sizeof(int) );

  return pTape;
}

int tape_push(void* p, int i_l, int i_r, double d_l, double d_r)
{
  tape_t *pTape = (tape_t*)p;

  if (pTape->idx * 2 >= pTape->size)
  {
    int newSize = pTape->size * 2;

    double *val2 = malloc( newSize*sizeof(double) );
    memcpy(val2, pTape->val, pTape->size*sizeof(double));
    free(pTape->val);
    pTape->val = val2;

    int *lnk2 = malloc( newSize*sizeof(int) );
    memcpy(lnk2, pTape->lnk, pTape->size*sizeof(int));
    free(pTape->lnk);
    pTape->lnk = lnk2;

    pTape->size = newSize;
  }

  int i = pTape->idx++;

  pTape->val[i*2] = d_l;
  pTape->val[i*2 + 1] = d_r;

  pTape->lnk[i*2] = i_l;
  pTape->lnk[i*2 + 1] = i_r;

  return (i + pTape->offset);
}

void tape_backPropagate(void* p, int start, double* out)
{
  tape_t *pTape = (tape_t*)p;

  double* buffer = calloc( pTape->offset + pTape->idx, sizeof(double) );
  buffer[start] = 1.0;
  int idx = 1 + start - pTape->offset;

  while (--idx >= 0)
  {
    double v = buffer[idx + pTape->offset];
    if (v == 0.0) continue;

    int i = pTape->lnk[idx*2];
    double x = pTape->val[idx*2];
    if (x != 0.0)
      buffer[i] += v*x;

    int j = pTape->lnk[idx*2 + 1]; 
    double y = pTape->val[idx*2 + 1];
    if (x != 0.0)
      buffer[j] += v*y;
  }
  
  memcpy(out, buffer, pTape->offset * sizeof(double) );
  free(buffer);
}

void tape_free(void* p)
{
  tape_t *pTape = (tape_t*)p;

  free(pTape->val);
  free(pTape->lnk);

  free(pTape);
}