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

typedef struct {
  int top;
  int maxSize;
  int *data;
} stack;

stack stack_init(int maxSize);

void stack_destroy(stack *s);

int stack_is_empty(stack *s);

int stack_is_full(stack *s);

void stack_push(stack *s, int x);

int stack_peek(stack *s);

void stack_remove(stack *s);

int stack_pop(stack *s);
