#include <stdio.h>
// don't include the last element that is the length
int partition(int* arr, int start, int end) {
	// end is the last element to look at
	int split = arr[end-1];
	// swap everything but that splitter 
  int i = start; int j = end-2;
	while(i < j) {
		while(arr[i] <= split) i++;
		while(arr[j] > split) j--;
		if (i < j) {
			int temp = arr[i];
			arr[i] = arr[j];
			arr[j] = temp;
		}
	}
	if(i != end) {
	// switch the split with i which now points ot the higher side
  arr[end-1] = arr[i];
  arr[i] = split;
	}
	return i;
}
void qsort(int* arr, int start, int end) {
	if(start >= end-1) return;
	int q = partition(arr, start, end);
	printf("start: %d\tend: %d\tq: %d\t", start, end, q);
  for(int i = 0; i < 21; i++) {
    printf("%d, ", arr[i]);
  }
  putchar('\n');
	qsort(arr, start, q-1);
	qsort(arr, q+1, end);
}

int main() {
	int arr[] = { 1, -1, 6, 3, 5, 4, 7, 5, -1, -6, -4, -9, -10, -3, -3, -2, -5, 6, 4, -3, 8 };
  for(int i = 0; i < 21; i++) {
    printf("%d, ", arr[i]);
  }
  putchar('\n');
	int size = 21;
	qsort(arr, 0, 21);
	for(int i = 0; i < 21; i++) {
		printf("%d, ", arr[i]);
	}
	putchar('\n');
	return 0;
}
