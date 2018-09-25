/*
Copyright 2018 Ron Triepels

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

#include <R.h>
#include <Rinternals.h>

#include <stdio.h>
#include <stdlib.h>

#include "stack.h"

stack* stack_allocate(int size)
{
  stack *s;

  int *data;

  if(size < 0)
  {
    Rf_errorcall(R_NilValue, "invalid stack size provided");
  }

  s = malloc(sizeof(stack));

  data = malloc(size * sizeof(int));

  if(s == NULL || data == NULL)
  {
    Rf_errorcall(R_NilValue, "unable to allocate stack of %d elements", size);
  }

  s->top = -1;
  s->size = size;
  s->data = data;

  return s;
}

void stack_destroy(stack *s)
{
  free(s->data);
  free(s);
}

int stack_is_empty(stack *s)
{
  return s->top < 0;
}

int stack_is_full(stack *s)
{
  return s->top >= s->size - 1;
}

void stack_push(stack *s, int x)
{
  if(stack_is_full(s))
  {
    const int size = s->size == 0 ? 1 : 2 * s->size;

    s->data = realloc(s->data, size * sizeof(int));

    if(s->data == NULL)
    {
      Rf_errorcall(R_NilValue, "unable to reallocate stack of %d elements", size);
    }

    s->size = size;
  }

  s->data[++s->top] = x;
}

int stack_peek(stack *s)
{
  if(stack_is_empty(s))
  {
    Rf_errorcall(R_NilValue, "unable to peek the stack because it is empty");
  }

  return s->data[s->top];
}

void stack_remove(stack *s)
{
  if(stack_is_empty(s))
  {
    Rf_errorcall(R_NilValue, "unable to remove the top element of the stack because it is empty");
  }

  s->top--;
}

int stack_pop(stack *s)
{
  if(stack_is_empty(s))
  {
    Rf_errorcall(R_NilValue, "unable to pop the stack because it is empty");
  }

  return s->data[s->top--];
}
