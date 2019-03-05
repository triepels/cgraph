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

#ifndef STACK_H
#define STACK_H

#define R_NO_REMAP

#include <R.h>
#include <Rinternals.h>

/*
 * PUBLIC STRUCTS
 */

typedef struct {
  int top;
  int size;
  int *data;
} cg_stack;

/*
 * PUBLIC METHODS
 */

cg_stack* cg_stack_allocate(const int size);

void cg_stack_destroy(cg_stack *stack);

int cg_stack_is_empty(const cg_stack *stack);

int cg_stack_is_full(const cg_stack *stack);

void cg_stack_push(cg_stack *stack, const int x);

int cg_stack_top(const cg_stack *stack);

void cg_stack_pop(cg_stack *stack);

#endif
