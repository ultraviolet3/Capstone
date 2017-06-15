
Data Science Capstone Project- SwiftKey Next word prediction
========================================================
author: Uma Venkat
date: Jun 2017
autosize: true
css: custom.css


What is it about?
========================================================
<small>
The idea is predict the next possible word(s) for you to use while you type your sentence. So I built a model that will predict the next word in a sentence with relative accuracy. The model can form a  sentence that is grammatically correct and with approppriate words.

<b>Features of the application: </b>

The application
- Will predict words for start of a sentence and after every word in a sentence.
- Will render appropriate capitalization of letters such as beginning or after a period, question mark or exclamation mark.
- Will display contraction words appropriately.
- Is child friendly in the sense that no profanity will be predicted.
</small>

What does the application do?
========================================================
<small>
- Load corpus (text(s))
- If the corpus is large, break into smaller chunks. You can set the size of the data while calling the function.
- Else clean the data. If the files are broken you may use the execclean function to process you smaller broken down files.
- Once cleaned load you cleaned text file(s)
- You can then use the exexgram function to produce the n-grams. By default you will have unigrams, bigrams & trigrams. If you want more then you can set it to a different value. The n-grams will produce a consolidated list of unique values even if you have multiple cleaned data files
- Finally use pred function to predict the next word for your sentence</small>

How to use the application?
========================================================
<small>
- Choose from the list of words predicted for the start of a sentence
- As you type the model will list words that you could use to finish the word
- Choose from the list of words predicted for the following word in the sentence

Give it a try: https://ultraviolet.shinyapps.io/SwiftKey/
</small>

Finally
========================================================
<b>Thanks to</b>
<small>
- www.stackoverflow.com; www.coursera.com; Johs Hopkins University;
- Data for N-Grams: https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip
- Data for Contractions: https://en.wikipedia.org/wiki/Wikipedia:List_of_English_contractions
- Data for Profanity: www.noswearing.com/dictionary
</small>
