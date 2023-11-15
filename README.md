# Brzozowski-derivative-and-Pattern-Matching-in-OCaml

 This program returns the Brzozowski derivative of a regular expression with respect to a character, 
  using a randomly generated regular expression and an input symbol over the lowercase Latin alphabet.
 Then, using the derivative, this program checks whether the input symbol (to be extended to a finite sequence) is contained in the randomly generated regular expression.
The process is divided into three steps:
(1) generating a random expression randomly and accepting standard input,
(2) finding the Brzozowski derivative of the regular expression with respect to the input symbol, and 
(3) checking if the obtained derivative of the regular expression contains the empty string.


REFERENCES:
-Brzozowski, J.A.(1964).Derivatives of Regular Expressions. Journal of the ACM,11(4), pp481-494,https://dl.acm.org/doi/10.1145/321239.321249,
-Krishnaswami, Neel.(2013,November 7).Antimirov Derivatives for Regular Expressions,
https://semantic-domain.blogspot.com/2013/11/antimirov-derivatives-for-regular.html,
-Goldstein, Harrison.(2017)regex-deriv.hs, https://gist.github.com/hgoldstein95/0fe2def7591b44391521d988f28abf03
