-module(hilbert).
-export([get/2, test/0]).
-define(BOTTOM_LEFT, 0).
-define(TOP_LEFT, 1).
-define(TOP_RIGHT, 2).
-define(BOTTOM_RIGHT, 3).

getQuadrant1d(Point1d, QuadrantSize) ->
    getQuadrant1d(Point1d, QuadrantSize, 0).

getQuadrant1d(Point1d, QuadrantSize, QuadrantToCheck)
        when Point1d < QuadrantSize * 4 ->
    if Point1d < QuadrantSize * (QuadrantToCheck + 1) ->
        QuadrantToCheck
     ; Point1d >= QuadrantSize * (QuadrantToCheck + 1) ->
        getQuadrant1d(Point1d, QuadrantSize, QuadrantToCheck + 1)
    end.

flipLeft({PointX, PointY}, Width) ->
    HorizontalFlippedPoint = {(-PointX) + (Width - 1), PointY},
    {RightFlippedPointX, RightFlippedPointY} = flipRight(HorizontalFlippedPoint),
    FlippedPoint = {(-RightFlippedPointX) + (Width - 1), RightFlippedPointY},
    FlippedPoint.

flipRight({PointX, PointY}) -> {PointY, PointX}.

flipQuadrant(?BOTTOM_LEFT, Point, LineSize) -> flipLeft(Point, LineSize);
flipQuadrant(?TOP_LEFT, Point, _LineSize) -> Point;
flipQuadrant(?TOP_RIGHT, Point, _LineSize) -> Point;
flipQuadrant(?BOTTOM_RIGHT, Point, _LineSize) -> flipRight(Point).

mapPoint(?BOTTOM_LEFT, {X, Y}, LineSize) -> {X, Y + LineSize};
mapPoint(?TOP_LEFT, {X, Y}, _LineSize) -> {X, Y};
mapPoint(?TOP_RIGHT, {X, Y}, LineSize) -> {X + LineSize, Y};
mapPoint(?BOTTOM_RIGHT, {X, Y}, LineSize) -> {X + LineSize, Y + LineSize}.

get(?BOTTOM_LEFT, 1) -> {0, 1};
get(?TOP_LEFT, 1) -> {0, 0};
get(?TOP_RIGHT, 1) -> {1, 0};
get(?BOTTOM_RIGHT, 1) -> {1, 1};

get(Point1d, Order) when Order > 1 ->
    Size = trunc(math:pow(4, Order)),
    LineSize = trunc(math:pow(2, Order - 1)),
    QuadrantSize = trunc(Size / 4),
    Quadrant = getQuadrant1d(Point1d, QuadrantSize),

    RawPoint = get(Point1d - (Quadrant * QuadrantSize), Order - 1),
    FlippedPoint = flipQuadrant(Quadrant, RawPoint, LineSize),

    mapPoint(Quadrant, FlippedPoint, LineSize).

test() ->
    GetOrder2 = fun (Point1d) -> io:format("~p~n", [get(Point1d, 2)]) end,
    lists:foreach(GetOrder2, lists:seq(0, 15)).
