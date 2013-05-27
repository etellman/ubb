# Makefile

TARGETS = study_guide.pdf study_guide_ch2.pdf study_guide_ch3.pdf study_guide_ch4.pdf

default: $(TARGETS)

%.pdf : %.dvi
	dvipdf $<

%.dvi : %.tex
	latex $<

clean:
	rm -f *.dvi *.aux *.log *.pdf *~ *.bak

