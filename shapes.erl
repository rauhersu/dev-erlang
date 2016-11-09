% A simple erlang module
% export every function as test1..testN
%
% Example of use:
%
% $ erl
% 1> cd("location-of-shapes.erl"). 
% 2> c(shapes).
% 3> shapes:test1().

-module(shapes).
-export([test1/0]).

area({rectangle_, Width, Height}) -> Width * Height;
area({circle_, Radius})           -> 3.14159 * Radius * Radius;
area({square_, Side})             -> Side * Side.

test1() ->
    Arectangle = {rectangle_,{width_,10},{height_,5}},
    {Id_,_,_} = Arectangle,
    {_,{_,Width_},_} = Arectangle,
    {_,_,{_,Height_}} = Arectangle,
    Area = area({Id_,Width_,Height_}),
    io:format("Area of a rectangle is ~w~n",[Area]).




