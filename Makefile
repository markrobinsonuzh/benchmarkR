R = R_LIBS=/home/Shared/Rcode/benchmarkR/Rlibs/:/home/Shared/Rlib/release-3.0-lib/ /usr/local/R/R-3.1.1/bin/R
PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)
all: build check doc clean

build:
	cd ..;\
	$(R) CMD build --resave-data $(PKGSRC)

check:
	cd ..;\
	$(R) CMD check $(PKGNAME)_$(PKGVERS).tar.gz

doc:
	cd ..;\
	mkdir -m 777 pkg_install;\
	$(R) CMD INSTALL -l pkg_install $(PKGNAME)_$(PKGVERS).tar.gz;\
	cp ./pkg_install/benchmarkR/doc/benchmarkR.pdf ./benchmarkR/vignettes/

clean:
	cd ..;\
	$(RM) -r pkg_install;\
	$(RM) -r $(PKGNAME).Rcheck/