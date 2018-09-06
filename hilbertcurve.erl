-module(hilbertcurve).
-export([get/2, test/1]).

%%% This module allows you to calculate the position of a 1d point in 2d space
%%% using a hilbert curve. 
%%%
%%% For more info watch 3blue1brows video on this topic:
%%% https://www.youtube.com/watch?v=3s7h2MHQtxc

%% The number that these positions have represents the order in which a line
%% would go though them. This is usefull for some calculations.
-define(BOTTOM_LEFT, 0).
-define(TOP_LEFT, 1).
-define(TOP_RIGHT, 2).
-define(BOTTOM_RIGHT, 3).

orderArea(Order) -> trunc(math:pow(4, Order)).
orderWidth(Order) -> trunc(math:pow(2, Order)).

%% Calculate the quadrant position of a given point.
quadrantContainingPoint(Point1d, QuadrantArea)
        when Point1d < QuadrantArea * 4 ->
    % This calculation takes advantage for 2 nice properties:
    %  1. The quadrant positions are defined in the order in which a line would
    %     go though them.
    %  2. The area of a quadrant is qual to the anoumt of point of a quadrant
    %     in 1d space.
    trunc(Point1d / QuadrantArea).

%% Get the starting point of a quadrant in 1d space.
quadrant1dOffset(QuadrantPosition, QuadrantArea)
        when QuadrantPosition < 4 ->
    % This calculation takes advantage of the same nice properties as described
    % in the `quadrantContainingPoint` comment.
    QuadrantPosition * QuadrantArea.

%% Flip a point horizontal while not moving the field.
flipHorizontal({X, Y}, Width) ->
    {(-X) + (Width - 1), Y}.

%% If the quadrant needs to be flipped, flip the quadrant in the appropiate
%% way.
flipQuadrant(?BOTTOM_LEFT, Point, QuadrantWidth) ->
    % Flipping a quadrant which is in the bottom left position is the same as
    % flipping a quadrant in the bottom right if we flip and unflip the
    % quadrant horizontally first.
    HFlippedPoint = flipHorizontal(Point, QuadrantWidth),
    FlippedHFlippedPoint = flipQuadrant(?BOTTOM_RIGHT, HFlippedPoint, QuadrantWidth),
    FlippedPoint = flipHorizontal(FlippedHFlippedPoint, QuadrantWidth),
    FlippedPoint;
flipQuadrant(?TOP_LEFT, Point, _QuadrantWidth) -> Point;
flipQuadrant(?TOP_RIGHT, Point, _QuadrantWidth) -> Point;
flipQuadrant(?BOTTOM_RIGHT, {X, Y}, _QuadrantWidth) -> {Y, X}.

%% Map a point of a single quadrant to its position in the containing quadrant.
mapPoint(?BOTTOM_LEFT, {X, Y}, QuadrantWidth) ->
    {X, Y + QuadrantWidth};
mapPoint(?TOP_LEFT, {X, Y}, _QuadrantWidth) ->
    {X, Y};
mapPoint(?TOP_RIGHT, {X, Y}, QuadrantWidth) ->
    {X + QuadrantWidth, Y};
mapPoint(?BOTTOM_RIGHT, {X, Y}, QuadrantWidth) ->
    {X + QuadrantWidth, Y + QuadrantWidth}.

%% Get the position of a 1d point in 2d space using a hilbert curve.
get(?BOTTOM_LEFT, 1) -> {0, 1};
get(?TOP_LEFT, 1) -> {0, 0};
get(?TOP_RIGHT, 1) -> {1, 0};
get(?BOTTOM_RIGHT, 1) -> {1, 1};
get(Point1d, Order) when Order > 1 ->
    QuadrantArea = orderArea(Order - 1),
    QuadrantWidth = orderWidth(Order - 1),

    Quadrant = quadrantContainingPoint(Point1d, QuadrantArea),

    RawPoint = get(
        Point1d - quadrant1dOffset(Quadrant, QuadrantArea),
        Order - 1
    ),
    FlippedPoint = flipQuadrant(Quadrant, RawPoint, QuadrantWidth),

    mapPoint(Quadrant, FlippedPoint, QuadrantWidth).

%% Print all of the 2d positions of a given order.
test(Order) ->
    Area = orderArea(Order),
    GetOrder2 = fun (Point1d) -> io:format("~p~n", [get(Point1d, Order)]) end,
    lists:foreach(GetOrder2, lists:seq(0, Area - 1)).
