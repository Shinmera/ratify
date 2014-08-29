About Ratify
------------
Ratify is a collection of utilities to perform validation checks and parsing. The main intention of usage for this is in web-applications in order to check form inputs for correctness and automatically parse them into their proper representations or return meaningful errors.

How To
------
Ratify has a load of `TEST-*` functions, each accompanied by a predicate equivalent. The `TEST-*` functions will signal errors of type `RATIFICATION-ERROR` if the test fails, whereas the predicates will simply return `NIL`. Both will return the passed argument unmodified on success. Some of the tests have an equivalent `PARSE-*` function in order to turn the string into a more useful representation.

The main interaction with Ratify is not supposed to be directly with the `TEST-*` and `PARSE-*` functions however, but rather through the `TEST` and `PARSE` wrapper functions. Both also have a macro shorthand to perform many tests and parsings at once, `PERFORM-COMBINED-TESTS` and `WITH-PARSED-FORMS`. These macros will perform as many tests as possible and only signal an error right after all tests have been made. This error is of type `COMBINED-ERROR`, which contains all the errors that occurred during the testing.

```
(ratify:perform-combined-tests
  (:integer "45")
  (:date "2014-08-01")
  (:ratio "566/21"))

(ratify:perform-combined-tests
  (:integer "4.5" "e")
  (:date "2014-08-01" "2014" "2014-55-99")
  (:ratio "566/21" "5.6/21"))
; Evaluation aborted on #&lt;RATIFY-TESTING:COMBINED-ERROR {10074E51D3}&gt;.
```
Or to perform parsing:

```
(let ((int "45")
      (url "http://foo.bar/baz.jp?what=ever#hashtag")
      (uri "things-are://sometimes:complicated@with-all.these/damn?protocols=i'm#telling+you!")
      (dt "2014-08-01T21:23:01"))
  (ratify:with-parsed-forms ((:integer int) (:url url) (:uri uri) (:datetime dt))
    (list int url uri dt)))
```

If an error occurs, the `ERRORS` function gives access to the list of errors that the `COMBINED-ERROR` contains. For a complete list of testing and parsing functions, please see the [symbol index](http://shinmera.github.io/ratify). The syntax grammar used to describe the valid values in the docstrings of each test is regex with the addition of `<brackets>` to refer to other tests, sometimes accompanied with a second line that describes limits of the values.
