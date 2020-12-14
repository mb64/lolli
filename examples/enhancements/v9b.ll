MODULE v9b.

test :- ((a,erase) & b), d.

LINEAR a.
LINEAR c.
LINEAR d.

b :- c  & (write_sans "Hi", nl, a).
b :- a, c.
