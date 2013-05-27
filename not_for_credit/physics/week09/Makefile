# Makefile

TARGETS = hw07.pdf

default: $(TARGETS)

%.pdf : %.dvi
	dvipdf $<

%.dvi : %.tex
	latex $<

clean:
	rm -f *.dvi *.aux *.log *.pdf *~

# hw06.dvi: hw06.tex
# 	latex hw06.tex
