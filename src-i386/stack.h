typedef struct {
  int top;
  int maxSize;
  int *data;
} stack;

void stack_init(stack *s, int maxSize);

void stack_destroy(stack *s);

int stack_is_empty(stack *s);

int stack_is_full(stack *s);

void stack_push(stack *s, int x);

int stack_peek(stack *s);

void stack_remove(stack *s);

int stack_pop(stack *s);
