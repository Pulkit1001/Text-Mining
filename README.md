# Text-Mining
Part of Speech tagging and Word Frequency

Using POS tagging and Word Frequency can help further in identifying the theme of the sentences. Here i use openNLPmodens.en and openNLP for POS tagging.

Installation
You can install packages from CRAN:

install.packages("tm")
install.packages("openNLP")

For openNLPmodels use datacube repos.
install.packages("openNLPmodels.en", repos = "http://datacube.wu.ac.at/", type = "source")

Before using NLP packages tidy the text of any whitespaces, numbers, special characters.
I have used tm package for this.

