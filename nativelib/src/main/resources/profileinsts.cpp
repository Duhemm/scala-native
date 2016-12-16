#include <stdlib.h>
#include <stdio.h>
#include <string>
#include "CountingMap.hpp"
#include "gc.h"

CountingMap insts;

extern "C" {
	void log_call() {
		insts.insert_occ(1, 1);
	}

	void log_load() {
		insts.insert_occ(2, 1);
	}

	void log_store() {
		insts.insert_occ(3, 1);
	}

	void log_elem() {
		insts.insert_occ(4, 1);
	}

	void log_extract() {
		insts.insert_occ(5, 1);
	}

	void log_insert() {
		insts.insert_occ(6, 1);
	}

	void log_stackalloc() {
		insts.insert_occ(7, 1);
	}

	void log_bin() {
		insts.insert_occ(8, 1);
	}

	void log_comp() {
		insts.insert_occ(9, 1);
	}

	void log_conv() {
		insts.insert_occ(10, 1);
	}

	void log_select() {
		insts.insert_occ(11, 1);
	}

	void log_improved() {
		insts.insert_occ(12, 1);
	}

        void log_dispatch_miss() {
		insts.insert_occ(13, 1);
	}

        void log_miss_info(long readPtr, void* origPtr, int key) {
            fprintf(stdout, "MISS: Read %lu in %p at %d\n", readPtr, origPtr, key);
            fflush(stdout);
        }

	void profile_insts_dump() {
		insts.print(stdout);
	}

}
