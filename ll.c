#include "ll.h"
struct listS{
	void *data;
	struct listS *next;
};
// The empty list is null
List *cons(void *data, List *next) {
	List *pl;
	pl = (List*) malloc (sizeof(struct listS));
	pl->data = data;
	pl->next = next;
	return pl;
}
List *empty() {
	return EMPTY;
}
int isempty(List *lst) {
	return lst == EMPTY;
}
void* first(List *next) {
	return next->data;
}
List* rest(List *next) {
	return next->next;
}
void* nth(List *lst, int n) {
	if(n < 0) {
		fprintf(stderr, "Out of Bounds: Negative N\n");
		return EMPTY;
	}
	if(isempty(lst)) {
		fprintf(stderr, "Out of Bounds: Too Big\n");
		return EMPTY;
	}
	else if(n == 0) return first(lst);
	else return nth(rest(lst), n-1);
}
int length(List *lst) {
	int len = 0;
	while (lst != NULL) {
		len++;
		lst=rest(lst);
	}
	return len;
}
List *append(List *A, List *B) {
	List *lp = A;
	if (isempty(lp)) return B;
	while(!isempty(rest(lp))) {
		lp = rest(lp);
	}
	lp->next = B;
	return A;
}
List *reverse (List *lst) {
	List *pl = EMPTY;
	List *next = EMPTY;
	while(!isempty(lst)) {
		next = rest(lst);
		lst->next = pl;
		pl = lst;
		lst = next;
	}
	return pl;
}
List *map(List *lst, void *(f)(void *)) {
	List *pl = lst;
	while(!isempty(pl)) {
		f(first(pl));
		pl = rest(pl);
	}
	return lst;
}
List *filter(List *lst, int *(f)(void *)) {
	List *prev = lst;
	while(!isempty(prev) && !f(first(prev))) {
		prev = rest(prev);
	}
	if(isempty(prev)) return NULL;
	List *pl = rest(prev);
	while(!isempty(pl)) {
		if(f(first(lst))) {
			pl = rest(pl);
		} else {
			prev->next = rest(pl);
			pl = rest(pl);
		}
	}
	return lst;
}
