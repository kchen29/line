all: main.fasl
	sbcl --script main.fasl

%.fasl:
	sbcl --non-interactive --eval '(compile-file "$*.lisp")' > /dev/null

main.fasl: main.lisp draw.fasl display.fasl

draw.fasl: draw.lisp display.fasl

display.fasl: display.lisp

clean:
	rm -f *~ *.fasl *.ppm
