#include <stdlib.h>
#include <stdio.h>
#define EMPTY NULL
typedef struct listS List;
List *empty();
List *cons(void *data, List *next);
int isempty(List *lst);
void* first(List *next);
List* rest(List *next);
void* nth(List *lst, int n);
int length(List *lst);
List *append(List *A, List *B);
List *reverse (List *lst);
List *map(List *lst, void *(f)(void *));
List *filter(List *lst, int *(f)(void *));

