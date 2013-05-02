PYTHON=python
INTERPRET=interpreterext.py
INTERPRETGC=interpreterextgc.py
PROGRAMEXT=programext.py
PROGRAMEXTGC=programextgc.py

TEST_DIR=test
TEST_OUTPUT_DIR1=$(TEST_DIR)/output1
TEST_ANSWER_DIR1=$(TEST_DIR)/answers1
TEST_INPUT_DIR1=$(TEST_DIR)/SampleInputs1

TESTER1=runtest1.py
RUN_TEST1=$(PYTHON) $(TEST_DIR)/$(TESTER1)
LINT_FILE=pylint.rc

SIM=./ram
SYMBOL_FILE=symOut.txt
MEM_FILE=memOut.txt
MEM_OPT_FILE=optimizedOut.txt
LINK_FILE=linkedOut.txt

SIM_CODE_DIR=sim/
BUILD_SIM=$(SIM_CODE_DIR)ram.cpp $(SIM_CODE_DIR)main.cpp -I $(SIM_CODE_DIR)

.PHONY : clean test lint build tags

# define the run-sim function
# takes two arguments... progFile memFile
run-simulator = $(SIM) $(1) $(2)

ram:
	@g++ $(BUILD_SIM) -o $(SIM)

lint: clean
	-pylint $(INTERPRET) $(PROGRAMEXT) --rcfile $(TEST_DIR)/$(LINT_FILE)
	-pychecker $(INTERPRET) $(PROGRAMEXT)

# For emacs users...
tags:
	@etags *.cpp *.h *.py

# This is the idea... but it needs to be cleaned up to handle a growing number of tests
test-part1: clean
	@$(RUN_TEST1)
	@echo "Checking answers"
	@diff $(TEST_ANSWER_DIR1) $(TEST_OUTPUT_DIR1)



test: test-part1

clean:
	@rm -f *.pyc *.out parsetab.py ram $(SYMBOL_FILE) $(MEM_FILE) $(MEM_OPT_FILE) $(LINK_FILE)
	@rm -rf $(TEST_OUTPUT_DIR1)


build : clean

compile: clean ram
	@$(PYTHON) $(INTERPRET)
	$(call run-simulator,$(LINK_FILE),$(MEM_FILE))
#	$(call run-simulator,$(MEM_OPT_FILE),$(MEM_FILE))
