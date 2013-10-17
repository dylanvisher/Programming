#include <stdio.h>
#include <stdlib.h>
typedef double type;
struct vectS {
	int dim;
	type *arr;
} *vect;

vect new_vect(unsigned int size, type values) {
	vect v = (vect) malloc(struct vectS);
	v->arr = malloc(size * sizeof(type));
	memset(v->arr, values, size * sizeof(type));
	v->dim = size;
	return vect;
}

vect vectorify (type *arr, unsigned int dim) {
	vect = new_vect(dim, 0);
	memcpy(vect->arr, arr, dim * sizeof(type));
	return vect;
}

type dot (vect u, vect v) {
	if (u.dim != v.dim) {
		return 0;
	}
	else {
		u.arr[i] * u.arr[v];
	}
}
