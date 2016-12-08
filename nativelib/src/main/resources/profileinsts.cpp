#include <stdlib.h>
#include <stdio.h>
#include <string>
#include "CountingMap.hpp"
#include "gc.h"

CountingMap insts;

extern "C" {
	void log_call() {
		insts.insert_occ("call", 1);
	}

	void log_load() {
		insts.insert_occ("load", 1);
	}

	void log_store() {
		insts.insert_occ("store", 1);
	}

	void log_elem() {
		insts.insert_occ("elem", 1);
	}

	void log_extract() {
		insts.insert_occ("extract", 1);
	}

	void log_insert() {
		insts.insert_occ("insert", 1);
	}

	void log_stackalloc() {
		insts.insert_occ("stackalloc", 1);
	}

	void log_bin() {
		insts.insert_occ("bin", 1);
	}

	void log_comp() {
		insts.insert_occ("comp", 1);
	}

	void log_conv() {
		insts.insert_occ("conv", 1);
	}

	void log_select() {
		insts.insert_occ("select", 1);
	}

	void profile_insts_dump() {
		insts.print(stdout);
	}
}
