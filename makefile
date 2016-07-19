install:
	raco exe hostblocker.rkt
	cp hostblocker /usr/bin/

clean:
	rm hostblocker
