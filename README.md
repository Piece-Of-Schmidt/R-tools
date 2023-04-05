# R-tools
Simple and sometimes not too simple funcs that helped me working with text data R. I am happy to share my scripts publicly, so they will maybe help others, too.


## progrss.R

This function will allow you to track the progress of your loop. Whether you are using a simple for loop or an sapply or lapply function, ´´´progress.indicate()´´´ lets you know how fast your code is running. The best part is, you don't need to install any packages!

To use this function, you will first need to initialize a progress tracker. To do so, simply type ´´´progress.initialize(object_you_are_iterating_over)´´´. Then, you can insert ´´´progress.indicate()´´´ into your loop. The progrss.R file shows two examples of how this could be done.

## similar_texts.R

This function helps you find duplicates and similar documents in your text data. Specifically, the function splits your texts into ngrams using the ´´´quanteda´´´ library. By using ngrams, the function ensures that the order of the words is taken into account, not just the usage of similar words. When two documents share a large number of ngrams, this is highly indicative of them being duplicates or at least similar texts.

The function requires the installation of several packages (quanteda, dplyr, proxyC, Matrix)
