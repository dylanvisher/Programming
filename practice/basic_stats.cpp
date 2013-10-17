#include <cstdio>
#include <cmath>
#include <vector>
#include <queue>
using namespace std;
/* Goal specified on programming praxis.com
Read a file containing integers, one integer per line. At the end of the file, write the number of integers in the file, their sum, and the mean, median, mode, minimum and maximum of the integers in the file.
*/
// There are several approaches to this problem, but 
// since we have to find the median of the sample set it would be beneficial
// to load it all into memory
// the best way to do this will be to use insertion sort
// because we are getting the input one by one
// and insertion into a sorted list will be O(logn)

int main(int argc, char **argv) {
	if (argc != 2) {
		fprintf(stderr, "Improper arguments : stats filename\n");
		return 1;
	}
	FILE *pfile;
	pfile = fopen(argv[1], "r+");
	if (pfile == NULL) {
		fprintf(stderr, "Invalid file specified");
		return 1;
	}
	double data;
	double mean;
	double median;
	
	double nread; // number of elements read
	double prev = nan("");
	double total; // total adding all the elements together
	priority_queue<double, vector<double>, greater<double> > min_heap;
	priority_queue<double, vector<double>, less<double> > max_heap;	
	while(!feof(pfile)) {
		fscanf(pfile, "%lf", &data);
		// update information for mean
		nread++;
		total += data;
		// update the information for median // note this is a lower median
		// implementation using two heaps
		// worst case is if it is sorted in descending order
		if(max_heap.size() == 0) {
			max_heap.push(data);
		} else {
			// need to add to max_heap
			if(max_heap.size() > min_heap.size()) {
				if(max_heap.top() < data) {
					min_heap.push(data);
				} else {
					min_heap.push(max_heap.top());
					max_heap.pop();
					max_heap.push(data);
				}
			} else {
				if(min_heap.top() < data) {
					max_heap.push(data);
				} else {
					max_heap.push(min_heap.top());
					min_heap.pop();
					min_heap.push(data);
				}
			}
		}
	}
	printf("mean : %lf, median : %lf\n",
				total/nread, max_heap.top());
	return 0;
}
