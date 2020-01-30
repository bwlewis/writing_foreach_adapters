# Writing adapters for R's foreach package

This repository includes guides, code examples, and notes on writing
adapters for R's foreach package: https://cran.r-project.org/package=foreach
It's a work in progress. I've got a draft of a basic introductory section
and plan to add other, more detailed, sections over time.

The foreach package defines an interface between R programs and back-end
systems that can, for example, evaluate code in parallel.

The adapters in these notes are very simple and basic. They are designed
to illustrate the foreach API by example and to provide a simple guide
to writing new adapters.

You can view raw R Markdown files in the repository, and also see example
HTML output of them at these links:

- Introduction https://bwlewis.github.io/writing_foreach_adapters/introduction.html
- Code-time foreach parameters and their use
- Reproducible parallel RNG
- Foreach loop composition for efficient nested parallel routines
- Set comprehensions
- Some issues with foreach and comparison with the future package
