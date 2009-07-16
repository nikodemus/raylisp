.PHONY: clean wc

clean:
	rm *.fasl */*.fasl *~ */*~

wc:
	wc -l *.asd *.lisp */*.lisp
