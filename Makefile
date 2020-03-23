all: maker.R usefulrainbowfuncs.R
	R CMD BATCH maker.R  # creates the PDF diagrams

clean:
	rm -f *.pdf *.Rout *.aux *.bbl *.log .RData
