====
comment
====

// This is a comment
/* This is also a comment */
/* this comment /* // /** ends here: */

---

(program (comment) (comment) (comment))

===
comments and literals
===

123;
// comment
---
(program (integer_literal (decimal_integer_literal)) (comment))
