all:
	sbcl --script line.lisp

clean:
	rm -f *~ *.fasl *.ppm
