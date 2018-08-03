-module(hilbert).
-export([get/2, test/0]).
-define(BOTTOM_LEFT, 0).
-define(TOP_LEFT, 1).
-define(TOP_RIGHT, 2).
-define(BOTTOM_RIGHT, 3).

getOrderSize(Order) ->
    trunc(math:pow(4, Order)).

getOrderWidth(Order) ->
    trunc(math:pow(2, Order)).

getQuadrant(Point1d, QuadrantSize) when Point1d < QuadrantSize * 4 ->
    trunc(Point1d / QuadrantSize).

flipHorizontal({X, Y}, Width) ->
    {(-X) + (Width -1), Y}.

flipQuadrant(?BOTTOM_LEFT, Point, QuadrantWidth) ->
    HFlippedPoint = flipHorizontal(Point, QuadrantWidth),
    FlippedHFlippedPoint = flipQuadrant(?BOTTOM_RIGHT, HFlippedPoint, QuadrantWidth),
    FlippedPoint = flipHorizontal(FlippedHFlippedPoint, QuadrantWidth),
    FlippedPoint;
flipQuadrant(?TOP_LEFT, Point, _QuadrantWidth) -> Point;
flipQuadrant(?TOP_RIGHT, Point, _QuadrantWidth) -> Point;
flipQuadrant(?BOTTOM_RIGHT, {X, Y}, _QuadrantWidth) -> {Y, X}.

mapPoint(?BOTTOM_LEFT, {X, Y}, QuadrantWidth) ->
    {X, Y + QuadrantWidth};
mapPoint(?TOP_LEFT, {X, Y}, _QuadrantWidth) ->
    {X, Y};
mapPoint(?TOP_RIGHT, {X, Y}, QuadrantWidth) ->
    {X + QuadrantWidth, Y};
mapPoint(?BOTTOM_RIGHT, {X, Y}, QuadrantWidth) ->
    {X + QuadrantWidth, Y + QuadrantWidth}.

get(?BOTTOM_LEFT, 1) -> {0, 1};
get(?TOP_LEFT, 1) -> {0, 0};
get(?TOP_RIGHT, 1) -> {1, 0};
get(?BOTTOM_RIGHT, 1) -> {1, 1};

get(Point1d, Order) when Order > 1 ->
    QuadrantSize = getOrderSize(Order - 1),
    QuadrantWidth = getOrderWidth(Order - 1),

    Quadrant = getQuadrant(Point1d, QuadrantSize),

    RawPoint = get(Point1d - (Quadrant * QuadrantSize), Order - 1),
    FlippedPoint = flipQuadrant(Quadrant, RawPoint, QuadrantWidth),

    mapPoint(Quadrant, FlippedPoint, QuadrantWidth).

test() ->
    GetOrder2 = fun (Point1d) -> io:format("~p~n", [get(Point1d, 2)]) end,
    lists:foreach(GetOrder2, lists:seq(0, 15)).
