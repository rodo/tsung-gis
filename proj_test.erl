%% proj4erl
proj_test()->
    {ok, WGS84} = proj4:init("+init=epsg:4326"),
    {ok, CRS2180} = proj4:init("+init=epsg:2180"),
    P = {21.049804687501, 52.22900390625},
    {ok, P2} = proj4:transform(WGS84, CRS2180, P),
    ?assertEqual({639951.5695094677, 486751.7840663176}, P2).

