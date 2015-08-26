FSC = fsharpc

STEP1_BASE=types.fs printer.fs reader.fs
STEP1_SRC=$(STEP1_BASE) step1_read_print.fs

STEP2_BASE=$(STEP1_BASE)
STEP2_SRC=$(STEP2_BASE) step2_eval.fs

step0_repl.exe: step0_repl.fs
	$(FSC) $<

step1_read_print.exe: $(STEP1_SRC)
	$(FSC) $^

step2_eval.exe: $(STEP2_SRC)
	$(FSC) $^

.PHONY: clean
clean:
	rm *.exe
