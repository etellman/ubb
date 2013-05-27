# Makefile

TARGETS = hw13.pdf 

default: $(TARGETS)

%.pdf : %.dvi
	dvipdf $<

%.dvi : %.tex
	latex $<

clean:
	rm -f *.dvi *.aux *.log *.pdf *~ *.bak

