LIBDIR=`erl -eval 'io:format("~s~n", [code:lib_dir()])' -s init stop -noshell`
PKGNAME=giza
VERSION=0.1.0

all: compile docs

ebin:
	mkdir ebin

compile: ebin
	cd src;erl -make
	cp src/gen_nb_server.app ebin

package: clean
	mkdir ebin
	@mkdir $(PKGNAME)-$(VERSION)/ && cp -rf ebin Makefile README.markdown src t $(PKGNAME)-$(VERSION)
	@COPYFILE_DISABLE=true tar zcf $(PKGNAME)-$(VERSION).tgz $(PKGNAME)-$(VERSION)
	@rm -rf $(PKGNAME)-$(VERSION)/

doc:
	mkdir -p doc

docs:	doc doc/*.html

doc/*.html:
	erl -eval 'edoc:files(["./src/gen_nb_server.erl"])' -noshell -s init stop
	mv *.html erlang.png stylesheet.css edoc-info doc
clean:
	rm -f *.tgz *.tar.gz edoc-info *.html erlang.png erl_crash.dump
	rm -rf $(PKGNAME)-$(VERSION)
	rm -rf ebin
	rm -rf doc
	rm -f t/*.beam
	rm -rf include
