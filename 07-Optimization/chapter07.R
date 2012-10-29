# Changes
# made modify.cipher a bit shorter
# 24th snippet - changed the way the data frame is built up (preallocated memory) to make the code
# slightly faster ... still very, very slow though, primary bottleneck appears to be the lookup table

# File-Name:       chapter07.R           
# Date:            2012-02-10                                
# Author:          Drew Conway (drew.conway@nyu.edu) and John Myles White (jmw@johnmyleswhite.com)                                                                    
# Purpose:         
# Data Used:       data/01_heights_weights_genders.csv, data/lexical_database.Rdata
# Packages Used:   n/a

# All source code is copyright (c) 2012, under the Simplified BSD License.  
# For more information on FreeBSD see: http://www.opensource.org/licenses/bsd-license.php

# All images and materials produced by this code are licensed under the Creative Commons 
# Attribution-Share Alike 3.0 United States License: http://creativecommons.org/licenses/by-sa/3.0/us/

# All rights reserved.

# NOTE: If you are running this in the R console you must use the 'setwd' command to set the 
# working directory for the console to whereever you have saved this file prior to running.
# Otherwise you will see errors when loading data or saving figures!

# First code snippet - A linear function which predicts weight from height given parameters a, b. We wish to
# optimize this function by adjusting a and b.
height.to.weight <- function(height, a, b)
{
  return(a + b * height)
}

# Second code snippet - Create height.weights dataframe. Examine preferred values of a,b of the provided lm function.
heights.weights <- read.csv(file.path('data', '01_heights_weights_genders.csv'))

coef(lm(Weight ~ Height, data = heights.weights))
#(Intercept) Height
#-350.737192 7.717288

# Third code snippet - Takes a (heights/weights) dataset and values for intercept and slope a,b and returns the squared error
# of the height.to.weight function on the dataset when using those values of a,b.
squared.error <- function(heights.weights, a, b)
{
  predictions <- with(heights.weights, height.to.weight(Height, a, b))
  errors <- with(heights.weights, Weight - predictions)
  return(sum(errors ^ 2))
}

# Fourth code snippet - Try out some a's and b's
for (a in seq(-1, 1, by = 1))
{
  for (b in seq(-1, 1, by = 1))
  {
    print(squared.error(heights.weights, a, b))
  }
}

# Fifth code snippet
optim(c(0, 0),
      function (x)
      {
        squared.error(heights.weights, x[1], x[2])
      })
#$par
#[1] -350.786736    7.718158
#
#$value
#[1] 1492936
#
#$counts
#function gradient 
#     111       NA 
#
#$convergence
#[1] 0
#
#$message
#NULL

# Sixth code snippet
a.error <- function(a)
{
  return(squared.error(heights.weights, a, 0))
}

# Seventh code snippet
curve(sapply(x, function (a) {a.error(a)}), from = -1000, to = 1000)

# Eighth code snippet
b.error <- function(b)
{
  return(squared.error(heights.weights, 0, b))
}

curve(sapply(x, function (b) {b.error(b)}), from = -1000, to = 1000)

# Ridge Regression

# Ninth code snippet - The ridge regression error function. 
ridge.error <- function(heights.weights, a, b, lambda)
{
  predictions <- with(heights.weights, height.to.weight(Height, a, b))
  errors <- with(heights.weights, Weight - predictions)
  return(sum(errors ^ 2) + lambda * (a ^ 2 + b ^ 2))
}

# Tenth code snippet
lambda <- 1

optim(c(0, 0),
      function (x)
      {
        ridge.error(heights.weights, x[1], x[2], lambda)
      })

#$par
#[1] -340.434108    7.562524
#
#$value
#[1] 1612443
#
#$counts
#function gradient 
#     115       NA 
#
#$convergence
#[1] 0
#
#$message
#NULL

# Eleventh code snippet
a.ridge.error <- function(a, lambda)
{
  return(ridge.error(heights.weights, a, 0, lambda))
}
curve(sapply(x, function (a) {a.ridge.error(a, lambda)}), from = -1000, to = 1000)

b.ridge.error <- function(b, lambda)
{
  return(ridge.error(heights.weights, 0, b, lambda))
}
curve(sapply(x, function (b) {b.ridge.error(b, lambda)}), from = -1000, to = 1000)

# Twelfth code snippet - An example of an error function which the book says does not work well with optim
# Seems to work just fine though ...
absolute.error <- function(heights.weights, a, b)
{
  predictions <- with(heights.weights, height.to.weight(Height, a, b))
  errors <- with(heights.weights, Weight - predictions)
  return(sum(abs(errors)))
}

# Thirteenth code snippet
a.absolute.error <- function(a)
{
  return(absolute.error(heights.weights, a, 0))
}

curve(sapply(x, function (a) {a.absolute.error(a)}), from = -1000, to = 1000)

# Code Breaking as Optimization

# Fourteenth code snippet
english.letters <- c('a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k',
                     'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
                     'w', 'x', 'y', 'z')

caesar.cipher <- list()

inverse.caesar.cipher <- list()

for (index in 1:length(english.letters))
{
  caesar.cipher[[english.letters[index]]] <- english.letters[index %% 26 + 1]
  inverse.caesar.cipher[[english.letters[index %% 26 + 1]]] <- english.letters[index]
}

print(caesar.cipher)

# Fifteenth code snippet - encrypt/decrypt functions
apply.cipher.to.string <- function(string, cipher)
{
  output <- ''

  for (i in 1:nchar(string))
  {
  output <- paste(output, cipher[[substr(string, i, i)]], sep = '')
  }
  
  return(output)
}

apply.cipher.to.text <- function(text, cipher)
{
  output <- c()
  
  for (string in text)
  {
    output <- c(output, apply.cipher.to.string(string, cipher))
  }
  
  return(output)
}

apply.cipher.to.text(c('sample', 'text'), caesar.cipher)

# Sixteenth code snippet - Functions for creating and modifying ciphers
generate.random.cipher <- function()
{
  cipher <- list()
  
  inputs <- english.letters
  
  outputs <- english.letters[sample(1:length(english.letters), length(english.letters))]
  
  for (index in 1:length(english.letters))
  {
    cipher[[inputs[index]]] <- outputs[index]
  }
  
  return(cipher)
}

modify.cipher <- function(cipher, input, output)
{
  new.cipher <- cipher
  
  new.cipher[[input]] <- output
  
  old.output <- cipher[[input]]
  
  collateral.input <- names(which(cipher==output))  # cipher is an array indexed by characters
                                                    # find the character which mapped to output
  
  new.cipher[[collateral.input]] <- old.output
  
  return(new.cipher)
}

propose.modified.cipher <- function(cipher)
{
  input <- sample(names(cipher), 1)
  
  output <- sample(english.letters, 1)
  
  return(modify.cipher(cipher, input, output))
}

# Seventeenth code snippet - Load the lexical database
load(file.path('data', 'lexical_database.Rdata'))

# Eighteength code snippet
lexical.database[['a']]
lexical.database[['the']]
lexical.database[['he']]
lexical.database[['she']]
lexical.database[['data']]

# Nineteenth code snippet - Return the probability of a given one gram (i.e. a single word) occurring with respect to a lexical database
one.gram.probability <- function(one.gram, lexical.database = list())
{
  lexical.probability <- lexical.database[[one.gram]]
  
  if (is.null(lexical.probability) || is.na(lexical.probability))
  {
    return(.Machine$double.eps)
  }
  else
  {
    return(lexical.probability)
  }
}

# Twentieth code snippet - Decrypts a text according to a given cypher and returns the probability 
# of the result being a real message based on the probability of occurrence of it's 1-grams.
log.probability.of.text <- function(text, cipher, lexical.database = list())
{
  log.probability <- 0.0
  
  for (string in text)
  {
    decrypted.string <- apply.cipher.to.string(string, cipher)
    log.probability <- log.probability +
    log(one.gram.probability(decrypted.string, lexical.database))
  }
  
  return(log.probability)
}

# Twenty-first code snippet
metropolis.step <- function(text, cipher, lexical.database = list())
{
  proposed.cipher <- propose.modified.cipher(cipher)
  
  lp1 <- log.probability.of.text(text, cipher, lexical.database)
  lp2 <- log.probability.of.text(text, proposed.cipher, lexical.database)
  
  if (lp2 > lp1)
  {
    return(proposed.cipher)
  }
  else
  {
    a <- exp(lp2 - lp1)  # acceptance ratio P(new)/P(old)
    x <- runif(1)  # get one (1) random number between 0.0 and 1.0 (non-inclusive), which are the default min, max for runif (random uniform)
    
    if (x < a)
    {
      return(proposed.cipher)
    }
    else
    {
      return(cipher)
    }
  }
}

# Twenty-second code snippet
decrypted.text <- c('here', 'is', 'some', 'sample', 'text')

# Twenty-third code snippet
encrypted.text <- apply.cipher.to.text(decrypted.text, caesar.cipher)

# Twenty-fourth code snippet - Run the Metropolis algorithm
set.seed(1)

cipher <- generate.random.cipher()

number.of.iterations <- 1000

results <- data.frame(Iteration=numeric(number.of.iterations),
                      LogProbability=numeric(number.of.iterations),
					  CurrentDecryptedText=character(number.of.iterations),
					  CorrectText=character(number.of.iterations),
					  stringsAsFactors=FALSE)

for (iteration in 1:number.of.iterations)
{
  log.probability <- log.probability.of.text(encrypted.text,
                                             cipher,
                                             lexical.database)
  
  current.decrypted.text <- paste(apply.cipher.to.text(encrypted.text,
                                                       cipher),
                                  collapse = ' ')
  
  correct.text <- as.numeric(current.decrypted.text == paste(decrypted.text,
                                                             collapse = ' '))
  results[iteration, ] <- c(iteration, log.probability, current.decrypted.text, correct.text)

  
  cipher <- metropolis.step(encrypted.text, cipher, lexical.database)
}

write.table(results,
            file = file.path('data/results.tsv'),
            row.names = FALSE,
            sep = '\t')
