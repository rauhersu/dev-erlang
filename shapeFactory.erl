% A simple erlang module
% export every function as test1..testN
%
% Example of use:
%
% $ erl
% 1> cd("location-of-shapes.erl"). 
% 2> c(shapeFactory).
% 3> shapeFactory:test1().

-module(shapeFactory).
-export([test1/0]).

newRectangle(Width,Height) -> 
    {rectangle_,Width,Height}.

newCircle(Radius) -> 
    {circle_,Radius}.

%% This was an attempt to make 'registerShape' recursive
%% but the syntax for map comprehensions has not been implemented
%% yet. See: 
%%
%%   + https://github.com/erlang/eep/blob/master/eeps/eep-0043.md
%%   + http://learnyousomeerlang.com/maps
%%
%% registerShape(Id, CreateFun, #{}) -> #{Id => CreateFun};
%% registerShape(Id, CreateFun, ShapeMap) -> 
%%     % 1. get the keys
%%     kk = [Keys || Keys := _ <- ShapeMap],
%%     [Key|RestOfkeys] = Keys,
%%     Val = ShapeMap#{Key},
%%     % 2. remove that key
%%     ShapeMapTmp = #{ K => V || K := V <- ShapeMap, K =/= Key },
%%     registerShape(Id, CreateFun, ShapeMapTmp) #{Key => Val}.

registerShape(Id, CreateFun, ShapeMap) -> 
    ShapeMapNew = ShapeMap #{Id => CreateFun}.

 test1() ->    
    ShapeMap1 = registerShape(rectangle_, fun newRectangle/2,#{}),
    ShapeMap2 = registerShape(circle_, fun newCircle/1,ShapeMap1),

    % Creates a rectangle
    %
    #{rectangle_ := CreateRectangleFun} = ShapeMap2,
    Rectangle = CreateRectangleFun(1,2),
    {_,_,Height}= Rectangle,
    {_,Width,_} = Rectangle,
    io:format("A rectangle was created with Height [~w] and Width [~w]~n",[Height,Width]),

    % Creates a circle
    %
    #{circle_ := CreateCircleFun} = ShapeMap2,
    Circle = CreateCircleFun(3),
    {_,Radius} = Circle,
    io:format("A Circle was created with Radius [~w]~n",[Radius]).




