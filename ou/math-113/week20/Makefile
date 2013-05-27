# Makefile

TARGETS = hw16.pdf practice.pdf 

default: $(TARGETS)

%.pdf : %.dvi
	dvipdf $<

%.dvi : %.tex
	latex $<

clean:
	rm -f *.dvi *.aux *.log *~

