all: \
    	moco_paper.pdf

%.pdf: %.md
	pandoc --data-dir=/home/alli/common --standalone \
	--variable classoption=twocolumn --filter=pandoc-citeproc --to=latex -o $@ $<
