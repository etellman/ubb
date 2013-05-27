# Makefile

TARGETS = chapter_five_exam.pdf correction.pdf

default: $(TARGETS)

%.pdf : %.dvi
	dvipdf $<

%.dvi : %.tex
	latex $< && latex $<

clean:
	rm -f *.dvi *.aux *.log *.pdf *~

