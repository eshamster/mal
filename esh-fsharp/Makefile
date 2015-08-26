FSC = fsharpc

STEP1_SRC=types.fs printer.fs reader.fs step1_read_print.fs

step0_repl.exe: step0_repl.fs
	$(FSC) $<

step1_read_print.exe: $(STEP1_SRC)
	$(FSC) $^

.PHONY: clean
clean:
	rm *.exe
