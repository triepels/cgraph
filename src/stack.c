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

void stack_add(stack *s, int x)
{
  if(stack_is_full(s))
  {
    if(s->size == 0)
    {
      s->size = 1;
    }
    else
    {
      s->size *= 2;
    }

    s->data = realloc(s->data, s->size * sizeof(int));

    if(s->data == NULL)
    {
      Rf_errorcall(R_NilValue, "unable to reallocate stack of %d elements", s->size);
    }
  }

  s->data[++s->top] = x;
}

int stack_current(stack *s)
{
  if(stack_is_empty(s))
  {
    Rf_errorcall(R_NilValue, "unable to retrieve the current element of the stack because it is empty");
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

int stack_get(stack *s)
{
  if(stack_is_empty(s))
  {
    Rf_errorcall(R_NilValue, "unable to get the first element of the stack because it is empty");
  }

  return s->data[s->top--];
}
