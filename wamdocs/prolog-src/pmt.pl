%% File: ?- consult("pmt.lisp").

property(fulham, 6500000).
property(chelsea, 10500000).
property(mayfair, 9000000).
property(kensington, 4000000).
property(knightsbridge, 6000000).

pmt(Local, Price, Down, I, NY, Payment) :-
     property(Local, Price),
     P is 0.7 * Price,
     Down is 0.3 * Price,
     R is I/1200,
     N is NY * 12,
     RN is (1 + R)** N,
     Payment is R * P * RN / (RN - 1).

