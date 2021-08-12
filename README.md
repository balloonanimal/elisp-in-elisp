# Elisp In Elisp
The goal of this repository is to implement as much of the emacs lisp language in elisp itself. While this project is largely intended to be a learning experience, the recent arrival of native compilation for elisp opens up the door for more of the emacs code base to be written in elisp itself.

The goal of this project is *not* to re-implement emacs primitives, nor is it to make any changes to the language design of elisp itself. Both are beyond the scope of what is doable in this project. There is still a ton of elisp code used every day and any breaking changes to this code would immediately disqualify this project from ever getting upstreamed. The end goal is to have an indistinguishable drop in replacement for the default elisp implementation.

## Checklist
- [ ] read (in progress)
- [ ] print
- [ ] eval
- [x] byte-compile (already elisp upstream)
- [x] native-compile (already elisp upstream)
