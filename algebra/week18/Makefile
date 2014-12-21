# Makefile

TARGETS = chapter-five-exam.pdf cumulative-review.pdf

default: $(TARGETS)

%.pdf : %.dvi
	dvipdf $<

%.dvi : %.tex
	latex $<

clean:
	rm -f *.dvi *.aux *.log *~

