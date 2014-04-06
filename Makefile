# Makefile

default: $(TARGETS)

%.pdf : %.dvi
	dvipdf $<

# doesn't work perfectly 
%.pdf : %.tex
	pdflatex $<

%.dvi : %.tex
	latex $<

clean:
	rm -f *.dvi *.aux *.log *~ *.toc 

