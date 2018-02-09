all: line.fasl
	sbcl --script line.fasl

line.fasl: line.lisp
	sbcl --non-interactive --eval '(compile-file "line.lisp")' > /dev/null

clean:
	rm -f *~ *.fasl *.ppm
