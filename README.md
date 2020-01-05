# Puretext

When my sis set up a Raspberry Pi as a development platform, she asked for text editor recommendations after realizing neither of hers ran on it.
I realized I didn't have any good recommendations (she's learning programming, I'm not going to throw vim at her at the same time).
So I got nerd-sniped, and finally got serious learning how text editors are implemented.

My goal here was/is to build a pure-functional text editor.
Support multi-select, fuzzy completion, run as much as possible off plugins, and don't worry about the interface.

It's only to the proof-of-concept on data structures at this point, and doesn't even support more than one line of editing or loading/saveing a file.
Nevertheless, I'm putting it down for now, since there are more useful things I can build, even if my current editors now kinda annoy me.
Coming back, I think the first step will be in getting `TextTest.hs` to manage multiple lines (and multi-line selections) through
implementing the currently commented cases in the `Cell` datatype in `Select/Cell.hs`.
Multiple selections should be fairly easily possible from there.

