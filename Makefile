.PHONY: clean wc

clean:
	rm -f *.fasl */*.fasl *~

wc:
	wc -l *.asd *.lisp */*.lisp
