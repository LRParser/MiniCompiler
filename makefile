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


.PHONY : clean test lint build tags

ram:
	@g++ ram.cpp main.cpp -o ram

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
	@rm -f *.pyc *.out parsetab.py ram 
	@rm -rf $(TEST_OUTPUT_DIR1)


build : clean

compile: clean
	@$(PYTHON) $(INTERPRET)

