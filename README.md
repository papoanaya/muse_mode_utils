muse_mode_utils
===============

The exporters directory contains exporters for Emacs Muse mode:

- muse-mm.el : MUSE driver for groff Memorandum Macros (-mm).  Even though
  muse already has a groff mode, that mode  uses the MOM and WWW macros
  which seems to be geared to literary works (i. e. books).
  The MM macros are catered for professional communications
  (Memorandum, White Papers, Business Letters, etc.)
  This MUSE driver may be rough around the edges but it works good
  enough for me.
- muse-man.el : MUSE driver for groff manual pages (-man). 
- muse-groff.el : MUSE driver for groff MOM macros. (-mom) This is my personal
  version that matches the latest version of the MOM macros. One is
  included in the muse distribution.
- muse-md.el : MUSE driver for Markdown. Still early stages.
