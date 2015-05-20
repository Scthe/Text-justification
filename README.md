#Text justification ([Demo](http://scthe.github.io/Text-justification/resources/public/)) [![Build Status](https://travis-ci.org/Scthe/Text-justification.svg?branch=master)](https://travis-ci.org/Scthe/Text-justification)

Using dynamic programming to make the text look pretty. You can find a nice explanation of the problem in one of Erik Demaine's classes [here](http://ocw.mit.edu/courses/electrical-engineering-and-computer-science/6-006-introduction-to-algorithms-fall-2011/lecture-videos/lecture-20-dynamic-programming-ii-text-justification-blackjack/).

## How to run

1. Install [leiningen](http://leiningen.org/#install)
2. Run one of:
  * `lein cljsbuild once min` to build the app, check the **resources\public** directory afterwards
  * `lein figwheel` for [figwheel](https://github.com/bhauman/lein-figwheel) awesomeness
  * `lein cljsbuild test` to run tests
  * `lein clean`


## Dependencies

(Leiningen takes care of them)

- [clojurescript](https://github.com/clojure/clojurescript)
- [leiningen](http://leiningen.org/)
- [figwheel](https://github.com/bhauman/lein-figwheel)
- [Reagent](https://reagent-project.github.io/)
- [Skeleton](http://getskeleton.com/)
- [jQuery](https://jquery.com/)
