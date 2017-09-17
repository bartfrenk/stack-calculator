run:
	stack exec server

bench:
	ab -m PATCH -c 10 -n 100000 http://localhost:4444/1/push/1;
