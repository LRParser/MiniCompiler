TOP ?= $(shell pwd)

HOST=$(shell hostname)
ASSIGNMENT=A3
PYTHON=python
INTERPRET=interpreterext.py
PROGRAMEXT=programext.py

TEST_DIR=test
TEST_OUTPUT_DIR1=$(TEST_DIR)/output1
TEST_ANSWER_DIR1=$(TEST_DIR)/answers1
TEST_INPUT_DIR1=$(TEST_DIR)/SampleInputs1

TESTER1=runtest1.py
RUN_TEST1=$(PYTHON) $(TEST_DIR)/$(TESTER1)
LINT_FILE=pylint.rc

ifneq (,$(findstring tux,$(HOST)))
	SIM=~jjohnson/bin/ram
else
	SIM=./ram
endif

TRANS_OUT_FILE=symOut.txt

#non-optimized versions
NON_OPT_MEM_FILE=mem-dump.txt
NON_OPT_PROGRAM=program-non-opt.txt

#optimized versions
OPT_PROGRAM=program-opt.txt
OPT_MEM_FILE=mem-dump-opt.txt

OUTPUT_FILES = $(TRANS_OUT_FILE) $(NON_OPT_PROGRAM) $(NON_OPT_MEM_FILE) $(OPT_PROGRAM) $(OPT_MEM_FILE)

SIM_CODE_DIR=sim/
BUILD_SIM=$(SIM_CODE_DIR)ram.cpp $(SIM_CODE_DIR)main.cpp -I $(SIM_CODE_DIR)

RELEASE_DIR=release
RELEASE_FILE=$(ASSIGNMENT).tar.gz

.PHONY : clean test lint build tags view-trans view-link view-op

# define the run-sim function
# takes two arguments... progFile memFile
run-simulator = $(SIM) $(1) $(2)

view:
	@more $(INTERPRET) $(PROGRAMEXT)
ram:
	@g++ $(BUILD_SIM) -o $(SIM)

run: compile
	@$(call run-simulator,$(NON_OPT_PROGRAM),$(NON_OPT_MEM_FILE))

run-op: compile
	@$(call run-simulator,$(OPT_PROGRAM),$(OPT_MEM_FILE))


view-trans:
	@cat $(TRANS_OUT_FILE)

view-link:
	@cat $(NON_OPT_PROGRAM)

view-op:
	@cat $(OPT_PROGRAM)

lint: clean
	-pylint $(INTERPRET) $(PROGRAMEXT) --rcfile $(TEST_DIR)/$(LINT_FILE)
	-pychecker $(INTERPRET) $(PROGRAMEXT)

# For emacs users...
tags:
	@etags *.py

# This is the idea... but it needs to be cleaned up to handle a growing number of tests
test-part1: clean
	@$(RUN_TEST1)
	@echo "Checking answers"
	@diff $(TEST_ANSWER_DIR1) $(TEST_OUTPUT_DIR1)



test: test-part1

clean:
	@rm -f *.pyc *.out parsetab.py $(OUTPUT_FILES)
	@rm -rf $(TEST_OUTPUT_DIR1)
	@-rm -rf $(RELEASE_DIR)


build : clean

compile: clean SIM
	@$(PYTHON) $(INTERPRET)

release: clean
	@-rm ram
	@cd ..; \
	cp -R $(TOP) $(ASSIGNMENT); \
	tar -zcf $(RELEASE_FILE) --exclude .git $(ASSIGNMENT); \
	rm -rf $(ASSIGNMENT); \
	mkdir $(TOP)/$(RELEASE_DIR); \
	mv $(RELEASE_FILE) $(TOP)/$(RELEASE_DIR)
