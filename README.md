# data-types-a-la-carte

- Original
  - Swierstra W, "Data types à la carte", Journal of Functional Programming, Vol 18, No 4, pp423--436 (2008)
  - URL: <https://www.cambridge.org/core/journals/journal-of-functional-programming/article/data-types-a-la-carte/14416CB20C4637164EA9F77097909409>

- お試し (Ch 5 まで)

    ```sh
    $ stack repl
    ghci> eval addExample -- ex: addition (Ch 3)
    1337
    ghci> eval ex41 -- ex: addition (Ch 4)
    31337
    ghci> eval ex51 -- ex: addition and multiplication (Ch 5)
    404
    ghci> eval ex52 -- ex: multiplication (Ch 5)
    42
    ghci> pretty ex54 -- ex: pretty-print (Ch 5)
    "(10 * (20 + 30))"
    ghci> pretty $ rewriteExpr distr ex54 -- ex: pretty-print (Ch 5)
    "((10 *20) + (10* 30))"
    ghci> pretty ex55 -- ex: pretty-print (Ch 5)
    "((20 + 30) *10)"
    ghci> pretty $ rewriteExpr distr ex55 -- ex: pretty-print (Ch 5)
    "((20* 10) + (30 * 10))"
    ghci> pretty ex56 -- ex: pretty-print (Ch 5)
    "((10 + 20) *(30 + 40))"
    ghci> pretty $ rewriteExpr distr ex56 -- ex: pretty-print (Ch 5)
    "(((10* 30) + (20 *30)) + ((10* 40) + (20 * 40)))"
    ```
