# -*- makefile -*-

srcdir ?= ../../

include ../../Makefile.config

default: check-config byte-compile

check-config:
ifeq ($(configure_options),)
	$(error Note: you need to compile and install notmuch before installing notmuch-pick.)
endif
ifeq ($(HAVE_EMACS),0)
	$(error You need emacs to use notmuch-pick.)
endif

byte-compile:
	emacs --quick --directory ../../emacs -batch \
		-f batch-byte-compile notmuch-pick.el

install: check-config byte-compile
	mkdir -p "$(DESTDIR)$(emacslispdir)"
	install -m0644 notmuch-pick.el "$(DESTDIR)$(emacslispdir)"
	install -m0644 notmuch-pick.elc "$(DESTDIR)$(emacslispdir)"

clean:
	 rm -f notmuch-pick.elc

../../Makefile.config:
	@echo "Note: you need to compile and install notmuch before installing notmuch-pick."