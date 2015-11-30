Code, stories, and commentary for National Novel Generation Month 2015.

I started from Warren Sack's 1992 Common Lisp reconstruction of James Meehan's program in _Inside_Computer_Understanding:_Five_Programs_Plus_Miniatures_, Roger Schank and Christopher Riesbeck (eds.). The original Sack version may be found at http://eliterature.org/images/microtalespin.txt, and I've also included it in this repo.

Prior to NaNoGenMo2015 kickoff, I cheated a bit by starting to look at the code and tweak it in some relatively small ways. The first thing I did was to separate the event simulation/generation and text output components into separate files, which I've call micro-talesim.lisp and micro-mumble.lisp respectively. I then moved global variables into their own file (ao-globals.lisp), moved story "initial conditions" into a separate file (stories.lisp), and, in a small wave to Common Lisp coding practice evolution over the past 23 years, introduced a package and a load file. All this will be changing over the course of the month.

To give a sense of what my "before the starting gun" changes did, I've also included two brief generated stories that start from the same initial conditions. original-story5.txt is created using the Sack micro-talespin, and revised-story5.txt was created using my pre-November version.

It's 11/30/2015 and I'm downing tools, pushing up the current version of the code (including some last minute kludges to get closer to the word count target, which I have not hit), and the last generation attempt. That last attempt is in Final Story Run.odt.

If you're crazy enough to want to attempt to use the code, you can take a look at minimal-instructions.txt to get started. For a top-down view, it's probably best to start at multi-episode.lisp, in particular the spin-episode function.

Generally speaking, the simulator is in micro-talesim.lisp, the generator in micro-mumble.lisp, and the planning mechanism in nngm-plans.lisp.

It was a hoot, even if I didn't get to set the "completed" tag. Looking forward to next year.

