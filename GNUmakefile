# GNUmakefile
# Perthon GNU Makefile
# David Manura, 2003-11

PERL=perl

TEST_FILES=$(wildcard t/*.t)

all: perthon

perthon: lib/Perthon/PerthonImpl.pm

lib/Perthon/PerthonImpl.pm: lib/Perthon/grammar.pl
	$(PERL) -MParse::RecDescent - $^ Perthon::PerthonImpl
	mv PerthonImpl.pm $@

doc: html/language.html

html/language.html: html/pod/language.pod
	pod2html -v $^ | $(PERL) bin/pod2html_cleanup.pl > $@

test:
	$(PERL) -MExtUtils::Command::MM -e "test_harness(0, 'lib')" \
            $(TEST_FILES)

tidy:
	rm -f `find . -name '*~'`

clean: tidy
	rm -f sl sl.bat blib Makefile html/language.html

