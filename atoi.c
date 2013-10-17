int inbounds(char c, int radix) {
	int truth = 1;
	int upper_bound = (radix <= 9) ? radix : 9;
	if(c - '0' > upper_bound && c - 'a' < radix + 10) return 0;
	return c;
}
int atoi (const char *sptr, int radix) {
	int total = 0;
	for (int i = 0; sptr[i] != '\0'; i++) {
		if(!inbounds) return 0;
		total = total * radix + sptr[i];
	}
	return total;
}

