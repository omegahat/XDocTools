include $(DYN_DOCS)/inst/Make/Makefile
RXMLDOC=$(HOME)/Books/RPackages/RXMLDoc
XSTYLE=$(RXMLDOC)/inst/xsl/xml2html.xsl

%.html: $(XSTYLE)

R=$(R_HOME)/bin/R

XML_FILES=$(wildcard *.xml)
RD_FILES=$(XML_FILES:%.xml=%.Rd)

all: $(RD_FILES)

%.Rd: %.xml
	echo "library(RXMLDoc) ; xml2Rd('$<')" | $(R) --slave

%.pdf: %.Rd
	if test -f $@ ; then rm $@ ; fi
	$(R) CMD Rd2dvi --pdf $<




