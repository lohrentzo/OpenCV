//
//  cvboh.cpp
//  Bilateral Object Handler
//  
//  Project asyno
//
//  Miki Fossati and Lorenzo Benoni 09/12/2016.
//
//
#include "zmq.hpp"
#include <iostream>

int main( void )
{
    zmq::context_t context (1);
    zmq::socket_t transmitter (context, ZMQ_PUSH);
    zmq::socket_t receiver (context, ZMQ_PULL);
    transmitter.connect ("tcp://127.0.0.1:5553");
    receiver.bind ("tcp://*:5554");
    std::cout << "Cvproc server starting..." << std::endl;

// Sending 0 and waiting for answer
    zmq::message_t zero(1);
    memcpy(zero.data(), "0", 1);
    std::cout << "Sending zero..." << std::endl;
    transmitter.send(zero);
    zmq::message_t answer;
    receiver.recv (&answer);
    std::cout << "Answer received! Ready to go..." << std::endl;
// Ok, answer received, we're good to go

    unsigned nb_frames = 0;

    while (true) {
        zmq::message_t request;
        receiver.recv (&request);
        transmitter.send(request);
        std::cout << nb_frames << '\r' << std::flush;
        ++nb_frames;
    }
}