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

  if(size < 0)
  {
    Rf_errorcall(R_NilValue, "invalid stack size provided");
  }

  s = malloc(sizeof(stack));

  if(s == NULL)
  {
    Rf_errorcall(R_NilValue, "unable to allocate stack");
  }

  s->top = -1;

  PROTECT_WITH_INDEX(s->data = Rf_allocVector(VECSXP, size), &s->index);

  return s;
}

void stack_destroy(stack *s)
{
  UNPROTECT_PTR(s->data);

  free(s);
}

int stack_is_empty(stack *s)
{
  return s->top < 0;
}

int stack_is_full(stack *s)
{
  return s->top >= LENGTH(s->data) - 1;
}

void stack_add(stack *s, SEXP x)
{
  if(stack_is_full(s))
  {
    int size = LENGTH(s->data);

    if(size == 0)
    {
      REPROTECT(s->data = Rf_lengthgets(s->data, 1), s->index);
    }
    else
    {
      REPROTECT(s->data = Rf_lengthgets(s->data, size * 2), s->index);
    }
  }

  SET_VECTOR_ELT(s->data, ++s->top, x);
}

SEXP stack_current(stack *s)
{
  if(stack_is_empty(s))
  {
    Rf_errorcall(R_NilValue, "unable to retrieve the current element of the stack because it is empty");
  }

  return VECTOR_ELT(s->data, s->top);
}

void stack_remove(stack *s)
{
  if(stack_is_empty(s))
  {
    Rf_errorcall(R_NilValue, "unable to remove the top element of the stack because it is empty");
  }

  s->top--;
}

SEXP stack_get(stack *s)
{
  if(stack_is_empty(s))
  {
    Rf_errorcall(R_NilValue, "unable to get the first element of the stack because it is empty");
  }

  return VECTOR_ELT(s->data, s->top--);
}
