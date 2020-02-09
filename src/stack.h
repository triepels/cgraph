/*
Copyright 2020 Ron Triepels

Licensed under the Apache License, Version 2.0 (the "License");
you may not use this file except in compliance with the License.
You may obtain a copy of the License at

    http://www.apache.org/licenses/LICENSE-2.0

Unless required by applicable law or agreed to in writing, software
distributed under the License is distributed on an "AS IS" BASIS,
WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
See the License for the specific language governing permissions and
limitations under the License.
*/

#ifndef STACK_H
#define STACK_H

#define R_NO_REMAP

#include <R.h>
#include <Rinternals.h>

/*
 * STACK STRUCTURE
 */

typedef struct
{
  int top;
  int size;
  SEXP *data;
} cg_stack_t;

/*
 * INLINE FUNCTIONS
 */

inline int cg_stack_is_empty(const cg_stack_t *stack)
{
  return stack->top < 0;
}

inline int cg_stack_is_full(const cg_stack_t *stack)
{
  return stack->top >= stack->size - 1;
}

inline void cg_stack_push(cg_stack_t *stack, const SEXP x)
{
  if(cg_stack_is_full(stack))
  {
    int size = (stack->size > 0) ? 2 * stack->size : 1;

    SEXP *data = R_Realloc(stack->data, size, SEXP);

    stack->data = data;
    stack->size = size;
  }

  stack->data[++stack->top] = x;
}

inline SEXP cg_stack_top(const cg_stack_t *stack)
{
  if(cg_stack_is_empty(stack))
  {
    Rf_errorcall(R_NilValue, "unable to retrieve top element because the stack is empty");
  }

  return stack->data[stack->top];
}

inline void cg_stack_pop(cg_stack_t *stack)
{
  if(cg_stack_is_empty(stack))
  {
    Rf_errorcall(R_NilValue, "unable to pop the top element because the stack is empty");
  }

  stack->top--;
}

/*
 * PUBLIC CONSTRUCTORS
 */

cg_stack_t* cg_stack_allocate(const int size);

#endif
