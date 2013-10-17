#include "ll.h"
#include <assert.h>
void* sqr(void *x) {
	int *n = (int*) x;
	(*n) *= (*n);
	return n;
}
int main() {
	List* lst = empty();
	assert(lst == EMPTY);
	assert(isempty(lst));
	int d1 = 1;
	lst = cons(&d1, lst);
	assert(!isempty(lst));
	assert(*(int*)first(lst) == 1);
	assert(*(int*)nth(lst, 0) == 1);
	int d2 = 2;
	lst = cons(&d2, lst);
	assert(*(int*)first(lst) == 2);
	assert(*(int*)first(rest(lst)) == 1);
	int d3 = 3;
	lst = cons(&d3, lst);
	assert(*(int*)first(rest(lst)) == 2);
  assert(*(int*)first(rest(rest(lst))) == 1);
	assert(*(int*)first(lst) == 3);
	int d4 = 4;
	lst = cons(&d4, lst);
	int d5 = 5;
	lst = cons(&d5, lst);
	assert(*(int*) nth(lst, 0) == 5);
	assert(*(int*) first(rest(lst)) == 4);
	assert(*(int*) nth(lst, 1) == 4);
	assert(*(int*) nth(lst, 2) == 3);
	assert(*(int*) nth(lst, 3) == 2);
	assert(*(int*) nth(lst, 4) == 1);
	assert( nth(lst, 5) == NULL);
	assert(length(lst) == 5);
	lst = reverse(lst);
	assert(nth(lst, 5) == EMPTY);
	assert(*(int*) nth(lst, 4) == 5);
  assert(*(int*) nth(lst, 3) == 4);
  assert(*(int*) nth(lst, 2) == 3);
  assert(*(int*) nth(lst, 1) == 2);
  assert(*(int*) nth(lst, 0) == 1);
	map(lst, &sqr);
	assert(*(int*) nth(lst, 4) == 25);
  assert(*(int*) nth(lst, 3) == 16);
  assert(*(int*) nth(lst, 2) == 9);
  assert(*(int*) nth(lst, 1) == 4);
  assert(*(int*) nth(lst, 0) == 1);		
}

