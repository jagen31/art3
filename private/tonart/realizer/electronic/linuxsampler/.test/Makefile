CFLAGS += -std=c++11 -Ilib/linuxsampler
LDFLAGS+=-L./bin/ -llinuxsampler.6

all:
	g++ $(LDFLAGS) -o test $(CFLAGS) *.cpp
