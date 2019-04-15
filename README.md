# TextAnalysis
This is a ShinyApp which can take as a input 2 different PDFS, the user can explore the distances between words that pass the filter and visualize different topics the file talks about on the clustering section, as well as comparing both

With this app a user can compare 2 pdf files to see how terms appear together by a metric of mean standart deviation or mean of both

It can also cluster words by this metric so the user can see how words are tied together in the document

Both PDFS can be compared between each other to check for similarities in their content

to run this app if you have already R and its related dependencies you can use
 
 
 shiny::runGitHub(repo="TextAnalysis",username="R-S-P-MODELS")

You may also clone the repository and use docker to build a docker image
