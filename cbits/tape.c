#include <stdlib.h>
#include <math.h>
#include <string.h>
#include <stdio.h>

typedef struct Tape
{
  int idx;
  int offset;
  int size;
  int variables;
  double *val;
  int *lnk;
  struct Tape* prev; 
} tape_t;

int tape_variables(void *p)
{
  tape_t *pTape = (tape_t*)p;
  return pTape->variables;
}

void* tape_alloc(int variables, int size)
{
  void* p = malloc(sizeof(tape_t) + size*2*(sizeof(double) + sizeof(int)) );
  tape_t *pTape = (tape_t*)p;

  pTape->size = size;
  pTape->idx = 0;
  pTape->offset = variables;
  pTape->variables = variables;

  pTape->val = p + sizeof(tape_t);
  pTape->lnk = p + sizeof(tape_t) + size*2*sizeof(double);
  pTape->prev = 0;

  return pTape;
}

int tape_push(void* p, int i_l, int i_r, double d_l, double d_r)
{
  tape_t *pTape = (tape_t*)p;

  // time to allocate new block?
  if (pTape->idx >= pTape->size)
  {
    int newSize = pTape->size * 2;

    p = malloc( sizeof(tape_t) + newSize*2*(sizeof(double) + sizeof(int)) );
    
    tape_t *pNew = (tape_t*)p;
    *pNew = *pTape;

    pTape->idx = 0;
    pTape->val = p + sizeof(tape_t);
    pTape->lnk = p + sizeof(tape_t) + newSize*2*sizeof(double);
    pTape->offset = pNew->offset + pNew->size;
    pTape->size = newSize;
    pTape->prev = pNew;
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

  int variables = pTape->variables;

  double* buffer = calloc( pTape->offset + pTape->idx, sizeof(double) );
  buffer[start] = 1.0;
  
  int idx = 1 + start;

  while (pTape)
  {
    idx -= pTape->offset;

    while (--idx >= 0)
    {
      double v = buffer[idx + pTape->offset];
      if (v == 0.0) continue;

      int i = pTape->lnk[idx*2];
      double x = pTape->val[idx*2];
      if (x != 0.0)
      {
        buffer[i] += v*x;
      }

      int j = pTape->lnk[idx*2 + 1]; 
      double y = pTape->val[idx*2 + 1];
      if (y != 0.0)
      {
        buffer[j] += v*y;
      }
    }
    idx += 1 + pTape->offset;
    pTape = pTape->prev;
  }
  
  memcpy(out, buffer, variables * sizeof(double) );
  free(buffer);
}

void tape_free(void* p)
{
  tape_t *pTape = (tape_t*)p;

  while (pTape)
  {
    p = pTape;
    pTape = pTape->prev;
    free(p);
  }
}