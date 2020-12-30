.PHONY : clean cleanpdfs cleandist all

all: slides.pdf

slides: slides.pdf
slides.pdf: $(wildcard *.tex)
	- pdflatex -halt-on-error -shell-escape slides.tex
	pdflatex -halt-on-error -shell-escape slides.tex

clean:
	rm -f *.eps
	rm -f *.aux *.log *.out *.bbl *.blg *~ *.bak *.ps
	rm -f *.brf *.lof *.lot *.toc *.glo *.ist
	rm -f *.nav *.snm *.vrb
	rm -rf _minted-* .auctex-auto

cleanpdfs:
	rm -f slides.pdf

cleandist: clean cleanpdfs
