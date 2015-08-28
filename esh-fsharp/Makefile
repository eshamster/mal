FSC = fsharpc

STEP1_BASE=types.fs printer.fs reader.fs
STEP1_SRC=$(STEP1_BASE) step1_read_print.fs

STEP2_BASE=$(STEP1_BASE)
STEP2_SRC=$(STEP2_BASE) step2_eval.fs

STEP3_BASE=$(STEP2_BASE) env.fs
STEP3_SRC=$(STEP3_BASE) step3_env.fs

STEP4_BASE=$(STEP3_BASE) env.fs
STEP4_SRC=$(STEP4_BASE) step4_if_fn_do.fs

step0_repl.exe: step0_repl.fs
	$(FSC) $<

step1_read_print.exe: $(STEP1_SRC)
	$(FSC) $^

step2_eval.exe: $(STEP2_SRC)
	$(FSC) $^

step3_env.exe: $(STEP3_SRC)
	$(FSC) $^

step4_if_fn_do.exe: $(STEP4_SRC)
	$(FSC) $^

.PHONY: clean
clean:
	rm *.exe
