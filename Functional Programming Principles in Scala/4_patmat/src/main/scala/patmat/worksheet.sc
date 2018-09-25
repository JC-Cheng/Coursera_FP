import patmat.Huffman._

val Secret_word = decodedSecret

val S = "I'm going to make him an offer he can't refuse.".toLowerCase()
val my_tree = createCodeTree(string2Chars(S))

convert(my_tree)

val new_text = string2Chars("the rat has a mug.")
val c1 = encode(my_tree)(new_text)
val c2 = quickEncode(my_tree)(new_text)

c1 == c2