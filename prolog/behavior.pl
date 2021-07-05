:- module(behavior, []).



tile(grass).
tile(cropped_grass).

:- op(1200, xfx, +> ).

'+>'(_, _).

grass, graze +>  grass + crop.
grass + crop, graze +> grass + crop + crop.
grass + crop + crop, graze +> cropped_grass.



tile_action(grass, graze(1), cropped_grass).
tile_action(cropped_grass, time(4), grass).
tile_action(grass, plow, plowed).
tile_action(plowed, plant(veggies), planted_veggies).
tile_action(planted_veggies, time(2),

