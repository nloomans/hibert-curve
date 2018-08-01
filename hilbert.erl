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
    HorizontalFlippedPoint = {PointX, (-PointY) + (Width - 1)},
    {RightFlippedPointX, RightFlippedPointY} = flipRight(HorizontalFlippedPoint),
    FlippedPoint = {RightFlippedPointX, (-RightFlippedPointY) + (Width - 1)},
    FlippedPoint.

flipRight({PointX, PointY}) -> {PointY, PointX}.

mapPoint(?BOTTOM_LEFT, {PointX, PointY}, LineSize) ->
    {PointX + LineSize, PointY};
mapPoint(?TOP_LEFT, Point, _) ->
    Point;
mapPoint(?TOP_RIGHT, {PointX, PointY}, LineSize) ->
    {PointX, PointY + LineSize};
mapPoint(?BOTTOM_RIGHT, {PointX, PointY}, LineSize) ->
    {PointX + LineSize, PointY + LineSize}.

get(Point1d, 1) when Point1d < 4 ->
    if Point1d == ?BOTTOM_LEFT -> {1, 0};
       Point1d == ?TOP_LEFT -> {0, 0};
       Point1d == ?TOP_RIGHT -> {0, 1};
       Point1d == ?BOTTOM_RIGHT -> {1, 1}
    end;

get(Point1d, Order) when Order > 1 ->
    Size = trunc(math:pow(4, Order)),
    LineSize = trunc(math:pow(2, Order - 1)),
    QuadrantSize = Size div 4,
    Quadrant = getQuadrant1d(Point1d, QuadrantSize),

    RawPoint = get(Point1d - (Quadrant * QuadrantSize), Order - 1),

    FlippedPoint =
        if Point1d < QuadrantSize * 1 -> flipLeft(RawPoint, LineSize)
         ; Point1d < QuadrantSize * 2 -> RawPoint
         ; Point1d < QuadrantSize * 3 -> RawPoint
         ; Point1d < QuadrantSize * 4 -> flipRight(RawPoint)
        end,

    mapPoint(Quadrant, FlippedPoint, LineSize).

test() ->
    GetOrder2 = fun (Point1d) -> io:format("~p~n", [get(Point1d, 2)]) end,
    lists:foreach(GetOrder2, lists:seq(0, 15)).
