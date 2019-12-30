# buoy_bench

### bench table 
```
erl -pa deps/*/ebin -pa ebin -s buoy_bench

F = fun()-> httpc:request("http://192.168.3.231:8080/index") end.

% httpc_proxy pool size = 16
httpc_proxy:start_link(16).

G  =  fun()-> httpc_proxy:request("http://192.168.3.231:8080/index") end.

% buoy_proxy pool size = 16
buoy_proxy:start_link(16).

H = fun()-> buoy_proxy:request("http://192.168.3.231:8080/index") end.

> buoy_bench:bench("bench_buoy_proxy",H,10,10,[]).
> bench_buoy_proxy: 775.080 us (1290.19 per second)
```

| name | threads | trials | output | qps |
| --- | --- |--- | --- | --- |
| ```bench_httpc``` | 10 | 10 | ```bench_httpc: 1711.670 us (584.22 per second) ``` |584 |
| ```bench_httpc``` | 100 | 100 | ```bench_httpc: 763.951 us (1308.98 per second) ``` |1308 |
| ```bench_httpc``` | 1000 | 100 | ```bench_httpc: 491.622 us (2034.08 per second)``` |2034 |
| ```bench_httpc_proxy``` | 10 | 10 | ```bench_httpc_proxy: 479.000 us (2087.68 per second)``` |2087 |
| ```bench_httpc_proxy``` | 100 | 100 | ```bench_httpc_proxy: 416.317 us (2402.02 per second)``` |2402 |
| ```bench_httpc_proxy``` | 1000 | 100 | ```bench_httpc_proxy: 379.730 us (2633.45 per second)``` |2633 |
| ```bench_buoy_proxy``` | 10 | 10 | ``` bench_buoy_proxy: 775.080 us (1290.19 per second)``` | 1290 |
| ```bench_buoy_proxy``` | 100 | 100 | ``` bench_buoy_proxy: 58.476 us (17101.06 per second)``` | 17101 |
| ```bench_buoy_proxy``` | 1000 | 10 | ``` bench_buoy_proxy: 60.581 us (16506.83 per second)``` | 16506 |
| ```bench_buoy_proxy``` | 1000 | 100 | ```bench_buoy_proxy: 7.086 us (141118.36 per second)``` | 141118 |
| ```bench_buoy_proxy``` | 10000 | 1 | ```bench_buoy_proxy: 57.757 us (17314.04 per second)``` | 17314 |
