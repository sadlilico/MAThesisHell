article_number = input("Enter article number: ")
sentence_number = input("Enter number of total sentences: ")
if article_number.isnumeric() and sentence_number.isnumeric():
    for x in range(1, int(sentence_number)+1):
        if x < 10:
            print("article0" + article_number + "_00" + str(x))
        else:
            print("article0" + article_number + "_0" + str(x))
else:
    print("Invalid input")
