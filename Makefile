run:
	stack exec server

bench:
	ab -m PATCH -c 4 -n 1000 http://localhost:4444/1/push/1;
