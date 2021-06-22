all: README.org

clean:
	rm -f README.org *.fasl

README.org: weave.lisp
	sbcl --script weave.lisp < weave.lisp > README.org

