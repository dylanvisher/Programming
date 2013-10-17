#include <stdio.h>
#include <list>
#include <vector>

using std::list;
using std::vector;
using std::string;

class Hash {
	unsigned int size_;
	unsigned int nelem_;
	vector< list <char*>* > table_;
 public:
	Hash(unsigned int size = 10) {
		size_ = size;
		nelem_ = 0;
		table_.resize(10);
		for(int i = 0; i < table_.size(); i++) {
			list<char*> *temp = new list<char*>;
			table_[i] = temp;
		}
	}
	~Hash() {
		vector< list <char*>*>::iterator iter;
		for(iter = table_.begin(); iter != table_.end(); ++iter)
			delete *iter;
	}
	//where value is a null terminated string
	unsigned long int Key(char *value) {
		unsigned long int key = 5167;
		for(unsigned int i = 0; i < strlen(value); i++) {
			key *= (key * 4679) % size_;
		} 
		return key % size_;
	}
	int hash_it(char* value) {
		if(nelem_ > 1.5*table_.size()) {
			size_ *= 3;
			table_.resize(size_);
		}
		std::list<char*>::iterator iter;
		list<char*>* list = table_[Key(value)];
		for(iter = list->begin(); iter != list->end() && strcmp(*iter, value) < 0; iter++);
		if(!strcmp(value, *iter)) {
			printf("value already found\n");
			return 0;
		}
		list->insert(iter, value);
		printf("value insterted\n");
		nelem_++;
		return 1;
	}
	int Find(char* value) {
		std::list<char*>::iterator iter;
    list<char*>* list = table_[Key(value)];
    for(iter = list->begin(); iter != list->end() && strcmp(*iter, value) < 0; iter++);
    if(!strcmp(value, *iter)) {
			return 1;
		}
		printf("values are not the same: %s vs. %s\n", value, *iter);
    return 0;
	}
};


