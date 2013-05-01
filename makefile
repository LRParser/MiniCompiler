PYTHON=python
INTERPRET=interpreterext.py
INTERPRETGC=interpreterextgc.py
PROGRAMEXT=programext.py
PROGRAMEXTGC=programextgc.py

TEST_DIR=test
TEST_OUTPUT_DIR1=$(TEST_DIR)/output1
TEST_ANSWER_DIR1=$(TEST_DIR)/answers1
TEST_INPUT_DIR1=$(TEST_DIR)/SampleInputs1

TEST_OUTPUT_DIR2=$(TEST_DIR)/output2
TEST_ANSWER_DIR2=$(TEST_DIR)/answers2
TEST_INPUT_DIR2=$(TEST_DIR)/SampleInputs2

TESTER1=runtest1.py
TESTER2=runtest2.py
RUN_TEST1=$(PYTHON) $(TEST_DIR)/$(TESTER1)
RUN_TEST2=$(PYTHON) $(TEST_DIR)/$(TESTER2)
LINT_FILE=pylint.rc

FUNC1=$(TEST_INPUT_DIR1)/recLen.p
FUNC2=$(TEST_INPUT_DIR1)/iterList.p

.PHONY : clean test lint build view-part1 view-part2 view-func1 view-func2


lint: clean
	-pylint $(INTERPRET) $(PROGRAMEXT) --rcfile $(TEST_DIR)/$(LINT_FILE)
	-pychecker $(INTERPRET) $(PROGRAMEXT)


# This is the idea... but it needs to be cleaned up to handle a growing number of tests
test-part1: clean
	@$(RUN_TEST1)
	@echo "Checking answers"
	@diff $(TEST_ANSWER_DIR1) $(TEST_OUTPUT_DIR1)

test-part2: clean
	@$(RUN_TEST2)
	@echo "Checking answers"
	@diff $(TEST_ANSWER_DIR2) $(TEST_OUTPUT_DIR2)


test: test-part1 test-part2

clean:
	@rm -f *.pyc *.out parsetab.py
	@rm -rf $(TEST_OUTPUT_DIR1)
	@rm -rf $(TEST_OUTPUT_DIR2)

view-part1 : clean
	@more $(INTERPRET) $(PROGRAMEXT)

view-part2 : clean
	@more $(INTERPRETGC) $(PROGRAMEXTGC)

view-func1: clean
	@more $(FUNC1)

view-func2: clean
	@more $(FUNC2)

build : clean

run-part1: clean
	@$(PYTHON) $(INTERPRET)

run-part2: clean
	@$(PYTHON) $(INTERPRETGC)
