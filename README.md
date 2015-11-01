Code, stories, and commentary for National Novel Generation Month 2015.

I'm starting from Warren Sack's 1992 Common Lisp reconstruction of James Meehan's program in _Inside_Computer_Understanding:_Five_Programs_Plus_Miniatures_, Roger Schank and Christopher Riesbeck (eds.). The original Sack version may be found at http://eliterature.org/images/microtalespin.txt, and I've also included it in this repo.

Prior to NaNoGenMo2015 kickoff, I cheated a bit by starting to look at the code and tweak it in some relatively small ways. The first thing I did was to separate the event simulation/generation and text output components into separate files, which I've call micro-talesim.lisp and micro-mumble.lisp respectively. I then moved global variables into their own file (ao-globals.lisp), moved story "initial conditions" into a separate file (stories.lisp), and, in a small wave to Common Lisp coding practice evolution over the past 23 years, introduced a package and a load file. All this will be changing over the course of the month.

To give a sense of what my "before the starting gun" changes did, I've also included two brief generated stories that start from the same initial conditions. original-story5.txt is created using the Sack micro-talespin, and revised-story5.txt was created using my pre-November version.

I expect to maintain some kind of notes file as the month goes on, and will try to remember to update this README to reflect major changes as well.
