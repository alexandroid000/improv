all: \
    	Nilles_poster.pdf

%.pdf: %.tex
	pdflatex $<
	bibtex $*
	pdflatex $<
	pdflatex $<

clean:
	rm *.aux *.blg *.log *.xml *.bbl
