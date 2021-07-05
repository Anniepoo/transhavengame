:- module(rules, []).

global p(0-40, 0-20).

on $up do p(0, --).
on $down do p(0, ++).
on $left do p(
$up / py-- .
$down / py++ .
$left / px-- .
$right / px++ .

$plant * 5 /


