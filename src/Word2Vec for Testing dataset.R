library(devtools)
install_github("mukul13/rword2vec")

library(rword2vec)
ls("package:rword2vec")

model=word2vec(train_file = "E://study metirial//admproject//email.txt",output_file = "vec.bin",binary=1)


ana=word_analogy(file_name =
                   "E://study metirial//admproject//vec.bin",search_words = "taco quesadillas
                 tamales",num = 20)