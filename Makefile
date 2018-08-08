PDFS=$(patsubst %.lhs,%.pdf,$(wildcard *.lhs))

all: $(PDFS)

%.pdf: %.lhs
	pdflatex -shell-escape $<
