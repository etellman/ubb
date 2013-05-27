# Makefile

TARGETS = practice_1.pdf final_1.pdf final_2.pdf final_3.pdf final_4.pdf final_5.pdf final_6.pdf final_7.pdf

default: $(TARGETS)

%.pdf : %.dvi
	dvipdf $<

%.dvi : %.tex
	latex $<

clean:
	rm -f *.dvi *.aux *.log *.pdf *~ *.bak

