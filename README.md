# BASIC for .NET

Building a BASIC compiler from scratch targetting .NET Core (v3) using VB.NET.

## History

In the past I've built a 100% language-compatible version of the classic GW-BASIC MS-DOS environment.  It was made 
available through Windows Phone and as a SilverLight web-hosted application.  (Man, I really picked two "winners" there...)

The GW-BASIC project was built mostly through "brute force" without any thought to making the project (code) publically 
visible.  As such, I've never released the code.

Fast forward a few years to now...

## What is this?

The original GW-BASIC project was the classic/retro "IDE" and an interpreter (REPL).  This project is going to take this 
all in a very different direction by starting with the compiler as the ultimate goal.  I'm sure that some of the original 
project will make it's way into this one... but for the most part, this project is "starting from scratch" with the 
ultimate goal of having a pretty good representation of "classic" BASIC targetting .NET Core!

For now, yes, I'm planning on adding support for line numbers. ;-)

## Why???

Why not?

## Progress...

So far I've worked through the first two episodes of the video series (see below)... only 12 more to go (as of 6/2/2019).

## Credits

As I already stated, much of the work on this project will be "borrowed" from my previous work; however, I'm also borrowing
(learning) from Immo Landwerth's video series on the subject of "building a compiler".  He's not claiming to be a "master"; 
however, he's certainly has more knowledge on the subject than I do and a good amount of experience on Roslyn (and .NET) that
I feel I can definately learn from and am treating his video series as a sort of "master class" on the subject. ;-)

Although not the same compiler, as I said the *process* of building this compiler is inspired in part by [Minsk by Immo Landwerth (YouTube)](https://www.youtube.com/watch?v=wgHIkdUQbp0&list=PLRAdsfhKI4OWNOSfS7EUu5GRAVmze1t2y).  If you are interested in how to build a C-style compiler for .NET, please see his [video series](https://www.youtube.com/watch?v=wgHIkdUQbp0&list=PLRAdsfhKI4OWNOSfS7EUu5GRAVmze1t2y).  Some key differences are:

- A language similar to BASIC instead of C/JScript.
- Using Visual Studio 2019 Community Edition (instead of Visual Studio Code).
- Using GitHub Tools for Visual Studio instead of command line tools.

I highly recommend [Immo's video series](https://www.youtube.com/watch?v=wgHIkdUQbp0&list=PLRAdsfhKI4OWNOSfS7EUu5GRAVmze1t2y)... it's awesome!

You can also find the repo for Immo's version [here](https://github.com/terrajobst/minsk/).

One additional note is that although not inspired by the comment he has at about 11 minutes into his first video, this project does attempt to answer the request someone made in his first live stream. ;-)
