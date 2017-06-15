---
title       : Coursera Capstone Project- SwiftKey
subtitle    : Next word prediction
author      : Uma Venkat
framework   : io2012 # {io2012, html5slides, shower, dzslides, ...}
highlighter : highlight.js # {highlight.js, prettify, highlight}
hitheme     : tomorrow # 
widgets     : [bootstrap, interactive] # {mathjax, quiz, bootstrap}
ext_widgets : [bootstrap]
mode        : selfcontained # {standalone, draft, selfcontained}
knit        : slidify::knit2slides
--- .quote .segue bg:#ebf442
<style>
.title-slide {
  background-color: #41f4e8
}
</style>

<b>Concept</b>
<p>The idea is the build a model that will predict the next word in a sentence with relative accuracy and such that a sentence can be formed in accordance with grammatical accuracy and accurate words to express the sentiments of the user.</p>

<b>Features of the application</b>

The application 
- Will predict words for start of a sentence.
- will redict follow-up words in a sentence.
- Will render appropriate capitalization of letters such as beginning or after a period, question mark or exclamation mark.
- Will display contraction words appropriately.
- Is child friendly in the sense that no profanity will be predicted.


--- .quote .segue bg:#ebf442
<b>How it works</b>

- Load corpus (text(s)) 
- If the corpus is large, break into smaller chunks. You can set the size of the data while calling the function. I set it to "10000" rows for my machine to process quickly. You may set it to "1000" if you use a smaller machine. Else clean the data. If the files are broken you may use the execclean function to process you smaller broken down files.
- Once cleaned load you cleaned text file(s)
- You can then use the exexgram function to produce the n-grams. By default you will have unigrams, bigrams & trigrams. If you want more then you can set it to a different value. The n-grams will produce a consolidated list of unique values even if you have multiple cleaned data files


--- .quote .segue bg:#ebf442
<b>How to use the application</b>

- Choose from the list of words predicted for the start of a sentence
- As you type the model will list words that you could use to finish the word
- Choose from the list of words predicted for the following word in the sentence
- Give it a try: https://ultraviolet.shinyapps.io/SwiftKey/ 

--- .quote .segue bg:#ebf442
<b>Location of the project files</b>
- Shiny Repo: https://ultraviolet.shinyapps.io/SwiftKey/
- Github: https://github.com/ultraviolet3/Capstone

<b>Thanks to</b>
- www.stackoverflow.com; www.coursera.com; Johs Hopkins University;
- N-Grams: https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip
- Contractions: https://en.wikipedia.org/wiki/Wikipedia:List_of_English_contractions
- Profanity: www.noswearing.com/dictionary



