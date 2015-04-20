PKGNAME := $(shell sed -n "s/Package: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGVERS := $(shell sed -n "s/Version: *\([^ ]*\)/\1/p" DESCRIPTION)
PKGSRC  := $(shell basename `pwd`)

all: build check doc clean

build:
	cd ..;\
	R CMD build $(PKGSRC)

check:
	cd ..;\
	R CMD check $(PKGNAME)_$(PKGVERS).tar.gz

doc:
	cd ..;\
	mkdir -m 777 pkg_install;\
	R_LIBS=pkg_install R CMD INSTALL $(PKGNAME)_$(PKGVERS).tar.gz;\
	cp ./pkg_install/benchmarkR/doc/benchmarkR.pdf ./benchmarkR/vignettes/

clean:
	cd ..;\
	$(RM) -r pkg_install;\
	$(RM) -r $(PKGNAME).Rcheck/