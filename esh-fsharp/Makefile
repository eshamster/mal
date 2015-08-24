FSC = fsharpc

step0_repl.exe: step0_repl.fs
	$(FSC) $<

step1_read_print.exe: reader.fs step1_read_print.fs
	$(FSC) reader.fs step1_read_print.fs

.PHONY: clean
clean:
	rm *.exe
