/*
Copyright (C) 2018 Ron Triepels

This program is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/

#include <stdio.h>
#include <stdlib.h>

#include <R.h>
#include <Rinternals.h>

#include "stack.h"

stack stack_init(int maxSize)
{
  int *data;

  stack s;

  data = malloc(maxSize * sizeof(int));

  if(data == NULL)
  {
    error("insufficient memory to initialize stack");
  }

  s.top = -1;
  s.maxSize = maxSize;
  s.data = data;

  return s;
}

void stack_destroy(stack *s)
{
  free(s->data);

  s->top = -1;
  s->maxSize = 0;
  s->data = NULL;
}

int stack_is_empty(stack *s)
{
  return s->top < 0;
}

int stack_is_full(stack *s)
{
  return s->top >= s->maxSize - 1;
}

void stack_push(stack *s, int x)
{
  if(stack_is_full(s))
  {
    s->data = realloc(s->data, 2 * s->maxSize * sizeof(int));

    if(s->data == NULL)
    {
      error("insufficient memory to initialize stack");
    }

    s->maxSize *= 2;
  }

  s->data[++s->top] = x;
}

int stack_peek(stack *s)
{
  if(stack_is_empty(s))
  {
    error("stack is empty");
  }

  return s->data[s->top];
}

void stack_remove(stack *s)
{
  if(stack_is_empty(s))
  {
    error("stack is empty");
  }

  s->top--;
}

int stack_pop(stack *s)
{
  if(stack_is_empty(s))
  {
    error("stack is empty");
  }

  return s->data[s->top--];
}
