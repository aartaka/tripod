LISP ?= sbcl
SBCL_FLAGS =
ifeq ($(LISP), sbcl)
	SBCL_FLAGS = --dynamic-space-size 1024 --no-userinit --non-interactive
endif
LISP_FLAGS ?= $(SBCL_FLAGS)
DESTDIR ?= /usr/bin

.PHONY: all install clean

all: tripod

clean:
	rm tripod

tripod:
	$(LISP) $(LISP_FLAGS) --eval '(require "asdf")' --load tripod.asd --eval '(asdf:load-system :tripod)' --eval '(asdf:make :tripod)' --eval '(quit)'

install: tripod
	cp tripod $(DESTDIR)/
