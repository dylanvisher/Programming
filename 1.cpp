#include <stdio.h>
#include <math.h>
#include <iostream>
#include <vector>
// factorize
vector<int> factorize(b) {
	vector<int> factors;
	for(int i = 2; i < sqrt(b); i++) {
		int temp = 0;
		while((temp = (b % i)) == 0) {
			b = temp;
			b /= i;
		}
		if (temp != 0) {
			factors.push_back(i);
		}
	}
	return factors;
}
int primes(int a, int b) {
	vector<int> factors = factorize(b);
	vector<int>::iterator iter;
	int count = 0;
	for(int i = 0; i < (1<<factors.size())-1; i++) {
	}
	// for each factor of b check to see how many times it goes into a
	for(iter = factors.begin(); iter != factors.end; iter++) {
		count += a \ (*iter);
	}
}
// Reply to M questions of the following type
//   2 natural number a, b, what is the number of natural numbers <= a and prime with b
// So I need to check primality with b
// check primality by doing if gcd x , b = 0 then mutually prime
int gcd(int a, int b) {
	if (b == 0) {
		return a;
	}
	else return gcd(b, a % b);
}
int prime(int a, int b) {
	int count = 0;
	for(int i = 0; i <= a; i++) {
		if(gcd(i, b) == 1) {
			count++;
		}
	} 
	return count;
}
// I could divide b into prime factors and then check mod each of those != 0
// This way I compute b only once

int main() {
	std::cout << prime(1234, 132567) << std::endl;
}
