%% File: switch.ll

%% A simple switch.

setsw(V,G) <- sw(_) x (sw(V) -> G).
getsw(V)   <- sw(V) x true.
toggle(G)  <- sw(on)  x (sw(off) -> G).
toggle(G)  <- sw(off) x (sw(on)  -> G).

test(1,V)  <- sw(on) -> toggle(setsw(off,getsw(V))).
test(2,V)  <- sw(on) -> toggle(toggle(getsw(V))).
test(3,V)  <- sw(on) -> toggle(toggle(setsw(V,getsw(off)))).
test(4,V)  <- sw(on) -> setsw(V,toggle(toggle(getsw(off)))).
  %% This last test asks the question "How should a switch be set
  %% so that after it is toggled twice the switch is off?"
