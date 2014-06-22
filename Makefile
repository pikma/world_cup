all: data/poisson_model.csv predict_score

data/poisson_model.csv: poisson_model.R data/all_matches.csv
	# Training model (this might take a minute).
	R CMD BATCH poisson_model.R

data/all_matches.csv: Match.hs Stats.hs extract_matches.hs data/friendly/*.txt
	# Extracting the list of matches.
	runhaskell extract_matches.hs

predict_score: predict_score.hs
	ghc -O2 predict_score.hs

clean:
	rm -f data/*.csv
	rm -f *.Rout
	rm -f predict_score
