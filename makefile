
make:
	clang runtime/runtime.c assembly/output.s -o output

clean:
	rm assembly/* output & rm -r compiled/

