CXX=g++
CXXFLAGS=-Wall -pthread
LDFLAGS=-lpthread

all: sikradio-receiver sikradio-sender

sikradio-receiver: sikradio-receiver.o
	$(CXX) $^ -o $@ $(LDFLAGS)

sikradio-sender: sikradio-sender.o
	$(CXX) $^ -o $@ $(LDFLAGS)

%.o: %.cpp
	$(CXX) $(CXXFLAGS) -c $<

clean:
	rm -f sikradio-receiver sikradio-sender *.o