/*
Copyright 2019 Ron Triepels

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

#define R_NO_REMAP

#include <R.h>
#include <Rinternals.h>

#include "stack.h"

/*
 * PUBLIC METHODS
 */

cg_stack_t* cg_stack_allocate(const int size)
{
  cg_stack_t *stack = (cg_stack_t*)R_alloc(1, sizeof(cg_stack_t));

  int *data = (int*)R_alloc(size, sizeof(int));

  stack->top = -1;
  stack->size = size;
  stack->data = data;

  return stack;
}

int cg_stack_is_empty(const cg_stack_t *stack)
{
  return stack->top < 0;
}

int cg_stack_is_full(const cg_stack_t *stack)
{
  return stack->top >= stack->size - 1;
}

void cg_stack_push(cg_stack_t *stack, const int x)
{
  if(cg_stack_is_full(stack))
  {
    int size = (stack->size > 0) ? 2 * stack->size : 1;

    int *data = R_Realloc(stack->data, size, int);

    stack->data = data;
    stack->size = size;
  }

  stack->data[++stack->top] = x;
}

int cg_stack_top(const cg_stack_t *stack)
{
  if(cg_stack_is_empty(stack))
  {
    Rf_errorcall(R_NilValue, "unable to retrieve top element because the stack is empty");
  }

  return stack->data[stack->top];
}

void cg_stack_pop(cg_stack_t *stack)
{
  if(cg_stack_is_empty(stack))
  {
    Rf_errorcall(R_NilValue, "unable to pop the top element because the stack is empty");
  }

  stack->top--;
}
