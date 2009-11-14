VERSION ?= dev

.PHONY: clean wc

clean:
	rm -f *.fasl */*.fasl *~ */*~ \#* */\#*

wc:
	wc -l *.asd *.lisp */*.lisp

release: clean
	mkdir -p raylisp-$(VERSION)/models
	cp -r Makefile LICENCE README TODO cameras formats gui kernel lights normals objects patterns shaders *.asd *.lisp raylisp-$(VERSION)/
	cp models/utah-teapot.obj raylisp-$(VERSION)/models/
	tar -czvf raylisp-$(VERSION).tar.gz raylisp-$(VERSION)
	rm -rf raylisp-$(VERSION)
