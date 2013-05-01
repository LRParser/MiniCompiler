CC=python
INTERPRET=interpreterext.py
PROGRAMEXT=programext.py

TEST_IN=SampleInputs
TEST_DIR=test
TEST_OUTPUT_DIR=$(TEST_DIR)/output
TEST_ANSWER_DIR=$(TEST_DIR)/answers
TESTER=runtest.py
RUN_TEST=$(CC) $(TEST_DIR)/$(TESTER)
LINT_FILE=pylint.rc


.PHONY : clean test lint


lint: clean
	-pylint $(INTERPRET) $(PROGRAMEXT) --rcfile $(TEST_DIR)/$(LINT_FILE)
	-pychecker $(INTERPRET) $(PROGRAMEXT)


# This is the idea... but it needs to be cleaned up to handle a growing number of tests
test: clean
	@$(RUN_TEST)
#Add tests here
	diff $(TEST_ANSWER_DIR)/add.p $(TEST_OUTPUT_DIR)/add.p
	diff $(TEST_ANSWER_DIR)/assignlist1.p $(TEST_OUTPUT_DIR)/assignlist1.p
	diff $(TEST_ANSWER_DIR)/assignlist2.p $(TEST_OUTPUT_DIR)/assignlist2.p
	diff $(TEST_ANSWER_DIR)/carTest.p $(TEST_OUTPUT_DIR)/carTest.p
	diff $(TEST_ANSWER_DIR)/cons.p $(TEST_OUTPUT_DIR)/cons.p


clean:
	@rm -f *.pyc *.out parsetab.py
	@rm -rf $(TEST_OUTPUT_DIR)
