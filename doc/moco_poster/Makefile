all: \
    	MOCO_poster.pdf \
    	slide.pdf

%.pdf: %.md
	pandoc -s --filter=pandoc-citeproc --to=beamer --highlight-style=kate -o slide.tex $<
	pandoc -s --filter=pandoc-citeproc --to=beamer --highlight-style=kate -o slide.pdf $<

%.pdf: %.tex
	pdflatex $<
	bibtex $*
	pdflatex $<
	pdflatex $<


clean:
	rm *.aux *.blg *.log *.xml *.bbl
