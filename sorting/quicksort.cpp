#include <stdio.h>
int Partition(int arr[], int start, int pivot_loc);
void QuickSort(int arr[], int start, int pivot) {
  if (start < pivot) {
		// new_pivot is the location of the pivot that is now inplace
	  int new_pivot = Partition(arr, start, pivot);
    // quick sort the two partitioned arrays
		// ignoring the pivot because that is guarenteed to be inplace by Partition
		QuickSort(arr, start, new_pivot-1);
		QuickSort(arr, new_pivot+1, pivot);
  }
}
// returns the location of the pivot of the partition
int Partition(int arr[], int start, int pivot_loc) {
	int pivot = arr[pivot_loc];
  int i = start - 1;
  // start at the beggining of the list
  // if j should be before the pivot earlier swap it with i
  for (int j = start; j < pivot_loc; j++) {
    // every time you find one that you want to be earlier
		// swap it with i (i is the inplace element)
		// the Invarient is that all elements up to i are in place
		if (arr[j] <= pivot) {
			i++;
			int temp = arr[i];
			arr[i] = arr[j];
			arr[j] = temp;
    }
  }
  i++;
  // swap the pivot to be in place
	int temp = arr[i];
  arr[i] = arr[pivot_loc];
  arr[pivot_loc] = temp;
  return i;
}

int main() {
  int arr[] = { 10, 4, 7, 83, -1, -5, 8, 7 };
  int size = 8; 
  QuickSort(arr, 0, size);
  for (int i = 0; i < size; i++) printf("%d, ", arr[i]);
  printf("\n");
  return 0;
}
