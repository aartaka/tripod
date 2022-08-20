LISP ?= sbcl
SBCL_FLAGS =
ifeq ($(LISP), sbcl)
	SBCL_FLAGS = --dynamic-space-size 1024 --non-interactive
endif
LISP_FLAGS ?= $(SBCL_FLAGS)
DESTDIR ?= /usr/bin
QUICKLISP=false

.PHONY: all install clean

all: tripod

clean:
	rm tripod

ifeq ($(QUICKLISP), true)
tripod:
	$(LISP) $(LISP_FLAGS) --eval '(require "asdf")' \
	--load tripod.asd --eval '(ql:quickload :tripod)' \
	--eval '(asdf:make :tripod)' \
	--eval '(quit)'
else
tripod:
	$(LISP) $(LISP_FLAGS) --eval '(require "asdf")' \
	--load tripod.asd --eval '(asdf:load-system :tripod)' \
	--eval '(asdf:make :tripod)' \
	--eval '(quit)'
endif



install: tripod
	cp tripod $(DESTDIR)/
